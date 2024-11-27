(ns scheduling-tbd.web.websockets
  "Set up a websockets and message routing for async communication with the client using ring.websocket.async."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.core.async       :as async :refer [<! >! go]]
   [clojure.edn              :as edn]
   [clojure.spec.alpha       :as s]
   [mount.core               :as mount :refer [defstate]]
   [promesa.core             :as p]
   [promesa.exec             :as px]
   [ring.websocket.async     :as wsa]
   [scheduling-tbd.specs     :as spec]
   [scheduling-tbd.sutil     :as sutil :refer [elide]]
   [scheduling-tbd.util      :refer [now util-state]] ; util-state for mount
   [taoensso.telemere        :refer [log! event!]]))

(def ^:diag diag (atom {}))
(def socket-channels "Indexed by a unique client-id provided by the client." (atom {}))
(declare domain-expert-says send-to-chat dispatch)

(defn make-ws-channels
  "Create channels and store them keyed by the unique ID provided by the client."
  [id]
  (let [chans {:in  (async/chan)
               :out (async/chan)
               :err (async/chan)
               ;; alive? is occassionally set to false by close-inactive-channels, e.g. when connection to new clients is made.
               ;; close-inactive-channels then wait for an ack (:confirm-alive) which sets it back to true
               ;; ...or if no timely response, closes the channel.
               :alive? true
               :exit? false}]     ; Used to exit from go loops. Important!
    (swap! socket-channels  #(assoc % id chans))
    chans))

(defn exiting? [client-id] (get-in @socket-channels [client-id :exit?]))

(defn ^:diag inject-stop
  "Send a message to incoming on the socket to exit the go loop.
   I wrote this because I don't see it happening in close-ws-channels above."
  [client-id]
  (let [{:keys [in]} (get @socket-channels client-id)]
    (go (>! in (str {:dispatch-key :stop})))))

(def inactive-channels-process
  "This is used to ensure that there is at most one close-ws-channels. That's not essential, but it seems correct to have at most one."
  (atom nil))

(defn close-ws-channels [client-id]
  (when (contains? @socket-channels client-id)
    (let [{:keys [in out err]} (get @socket-channels client-id)]
      (log! :debug (str "Closing websocket channels for inactive client " client-id " " (now)))
      ;; Set exit? and send something so go loop will be jogged and see it.
      (swap! socket-channels #(assoc-in % [client-id :exit?] true))
      ;;(go (>! in (str {:dispatch-key :stop}))) ; <======== ToDo: Needs investigation.
      ;; Keep delay high to be sure :stop is seen. (p/submit! is probably helpful here; promises are executed async and out of order.
      (-> (p/delay 3000)
          (p/then (fn [_]
                    (async/close! in)
                    (async/close! out)
                    (async/close! err)
                    (Thread/sleep 1000)
                    (swap! socket-channels #(dissoc % client-id))
                    (reset! inactive-channels-process nil)))))))

(declare clear-promises! clear-keys select-promise)

(def ping-dates  "Indexed by client-id, value is last time it pinged. Possibly only useful for diagnostics"
  (atom {}))

(defn forget-client
  "Close the channel and forget the promises associated with the client.
   This is typically from a client unmount."
  [client-id]
  (log! :debug (str client-id " closes its websocket."))
  (swap! ping-dates #(dissoc % client-id))
  (close-ws-channels client-id)
  (clear-promises! client-id))

(defn close-channel
  "This is called from a client (that is closing)."
  [{:keys [client-id]}]
  (forget-client client-id))

(defn ping-confirm
  "Create a ping confirmation for use in middle of a round-trip."
  [{:keys [client-id]}] ; also ping-id
  (log! :debug "confirming ping.")
  (swap! ping-dates #(assoc % client-id (now)))
  ;; Returnin a confirm here doesn't change the situation for ws keep-alive, so we don't bother.
  ;; When it is useful to debugging the client, return it instead of nil (which isn't sent, of course).
  {:dispatch-key :ping-confirm}
  #_nil)

(defn ^:diag recent-client!
  "Return the client-id of the client that pinged most recently.
   This should only used in development, I think!"
  []
  (->> @ping-dates seq (sort-by second) reverse first first))

(defn close-inactive-channels
  "Close channels that don't respond to :alive? with an :alive-confirm."
  []
  (let [clients (keys @socket-channels)]
    (doseq [client-id clients]
      (swap! socket-channels #(assoc-in % [client-id :alive?] false))
      (send-to-chat {:client-id client-id :dispatch-key :alive?}))
    (Thread/sleep 300000) ; 5 minutes to respond.
    (doseq [client-id clients]
      (when-not (get-in @socket-channels [client-id :alive?])
        (forget-client client-id)))))

(defn error-listener
  "Start a go loop for listening for errors from the client."
  [client-id]
  (if-let [{:keys [err exit?]} (get @socket-channels client-id)]
    (go
      (loop []
        (when-let [msg (<! err)]
          (log! :warn (str "Client reports " (type msg) ": client-id = " client-id))
          (forget-client client-id)
          (when-not exit? (recur)))))
    (log! :error (str "error-listener: Cannot find client-id " client-id))))

;;; NOTE: You have to recompile handler.clj and restart when you change this!
;;; This is the only thing that listens on in.
;;; https://clojure.org/news/2013/06/28/clojure-clore-async-channels:
;;;   go is a macro that takes its body and examines it for any channel operations.
;;;   It will turn the body into a state machine.
;;;   Upon reaching any blocking operation, the state machine will be 'parked' and the actual thread of control will be released.
;;;
;;; https://github.com/clojure/core.async/blob/master/examples/walkthrough.clj :
;;; The `go` macro asynchronously executes its body in a special pool of threads.
;;; Channel operations that would block will pause execution instead, blocking no threads.

;;; go blocks should not (either directly or indirectly) perform operations
;;; that may block indefinitely. Doing so risks depleting the fixed pool of
;;; go block threads, causing all go block processing to stop.

;;; https://medium.com/@reetesh043/difference-between-completablefuture-and-future-in-java-4f7e00bcdb56
;;; Interesting. However, what that post says about blocking doesn't seem to be true. I can block (see llm/run-long)
;;; and throw exceptions (see llm/throw-it) and nothing bad happens.
(defn dispatching-loop
  "Run the listening and dispatching loop for the client's channel."
  [client-id]
  (let [{:keys [in out]} (get @socket-channels client-id)]
    (go
      (loop []
        (when-let [msg (<! in)] ; Listen for messages.
          (let [msg (edn/read-string msg)]
            (when-not (#{:ping :alive-confirm} (:dispatch-key msg)) (log! :debug (str "Received message: "  msg)))
            (if (= :stop (:dispatch-key msg))
              (swap! socket-channels #(assoc-in % [client-id :exit?] true))
              (let [prom (px/submit! (fn [] (dispatch msg)))]
                (-> prom
                    (p/then (fn [r] (when r (go (>! out (str r)))))) ; Some, like-resume-conversation-plan don't return anything themselves.
                    (p/catch (fn [err]
                               (let [trace (->> (.getStackTrace err) (map str) (interpose "\n") (apply str))]
                                 (log! :error (str "Error dispatching: msg = " msg " err = " err "\ntrace:\n" trace))))))))
            (when-not (exiting? client-id) (recur)))))
      ;; There are many reasons a websocket connection might be dropped.
      ;; It is absolutely necessary that it exits the go loop when the socket closes; a race condition my occur otherwise.
      ;; The socket is closes when, for example, there is a WebSocketTimeoutException from the client (see error-listener).
      ;; I think there is still value in looking for inactive sockets and closing them. I implemented alive? because the client
      ;; will have to make another websocke request otherwise, and it doesn't seem to notice that the sever isn't listening to it!
      (log! :debug (str "Exiting dispatching loop: " client-id)))))

(defn establish-websocket-handler
  "Handler for http:/ws request. Returns nothing interesting.
   Set up web socket in, out, and err channels.
   This is called for each client. Client can call it more than once if, for example,
   it finds that the channel has become unusable (e.g. using (.-readyState) in JS.
   In that case, the old channel will eventually get destroyed by close-inactive-channels.
   Returns a map with value for key :ring.websocket/listener."
  [request]
  (log! :debug (str "Establishing ws handler for " (-> request :query-params (update-keys keyword) :client-id)))
  (when-not @inactive-channels-process (reset! inactive-channels-process (future (close-inactive-channels))))
  (if-let [client-id (-> request :query-params (update-keys keyword) :client-id)]
    (let [{:keys [in out err]} (make-ws-channels client-id)]
      (swap! ping-dates #(assoc % client-id (now)))
      (error-listener client-id)   ; This and next are go loops,
      (dispatching-loop client-id) ; which means they are non-blocking.
      {:ring.websocket/listener (wsa/websocket-listener in out err)})
    (log! :error "Websocket client did not provide id.")))

;;; ----------------------- Promise management -------------------------------
;;;   * By design, the server waits for a response before sending another question (except for maybe some future where there is a "Are you still there?").
;;;     - Questions always are transmitted over the websocket; they are the beginning of something new.
;;;   * The domain-expert, however might be able to send multiple chat responses before the next question.
;;;   * promise-keys are an attempt to match the question to the responses.
;;;     - When the server asks a question, it adds a promise key to the question; the client manages a set of these keys.
;;;     - When the domain-expert does a domain-expert-says, it tells the server what keys it has.
;;;     - As the system processes a domain-expert-says, it decides what keys to tell the client to erase.
;;;     - The system uses the keys to match a question to a domain-expert-says.
;;;       + It picks the key top-most on its stack that is also in the set sent by the client with domain-expert-says.
;;;     - When the system has decided where to route the question, it can also tell the client to remove that key from its set.
;;;       + This last step is important because the server needs to get old promise-keys off the stack.
;;;
;;; Note that the whole chat process is run in websockets. REST is only used for less interactive UI activities (e.g. changing projects).
(s/def ::promise (s/keys :req-un [::prom ::p-key ::client-id ::timestamp]))
(s/def ::prom p/promise?)
(s/def ::p-key keyword?)
(s/def ::client-id string?)
(s/def ::timestamp #(instance? java.util.Date %))

(def promise-stack "A stack of promise objects. The are objects with :prom, :p-key, :client-id, and :timestamp." (atom ()))

(defn clear-promises!
  ([] (reset! promise-stack '()))
  ([client-id]
   (doseq [p (filter #(= client-id (:client-id %)) @promise-stack)]
     (log! :info (str "Clearing (rejecting) promise " p))
     (try (p/reject! (:prom p) (ex-info "client forgotten" {}))
          (catch Exception _e nil)))
   (swap! promise-stack #(remove (fn [p] (= (:client-id p) client-id)) %))))

(defn remove-promise!
  "Remove the promise identified by the argument :p-key from the promise-stack."
  [p-key]
  (swap! promise-stack (fn [stack] (remove #(= p-key (:p-key %)) stack))))

(defn new-promise
  "Return a vector of [key, promise] to a new promise.
   We use this key to match returning responses from ws-send :domain-expert-says, to know
   what is being responded to." ; ToDo: This is not fool-proof.
  [client-id]
  (let [k (-> (gensym "promise-") keyword)
        p (p/deferred)
        obj {:prom p :p-key k :client-id client-id :timestamp (java.util.Date.)}]
    (swap! promise-stack #(conj % obj))
    ;; Because I saw it messed up once...
    (when-not (every? #(s/valid? ::promise %) @promise-stack)
      (throw (ex-info "Promise stack is messed up" {:stack @promise-stack})))
    obj))

(defn select-promise
  "Return the promise that is top-most on the stack and also in the argument set."
  [client-keys]
  (let [in-client-set? (set client-keys)
        stack @promise-stack]
    (or (some #(when (in-client-set? (:p-key %)) %) stack)
        (log! :warn (str "Couldn't find promise for any of" client-keys)))))

(defn clear-keys
  "Clear the argument promises (their keys are provided) from the stack and
   send a message to the client to do similar."
  [client-id client-keys]
  (doseq [k client-keys] (remove-promise! k))
  (if-let [out (->> client-id (get @socket-channels) :out)]
    (let [msg {:dispatch-key :clear-promise-keys
               :promise-keys client-keys}]
      (go (>! out (str msg))))
    (log! :error (str "Could not find out async channel for client " client-id))))

;;;--------------------- Receiving a response from a client -----------------------
(defn domain-expert-says
  "Handle websocket message with dispatch key :domain-expert-says. This will typically clear a promise-key.
   Note that we call it 'domain-expert' rather than 'user' because the role is just that, and it can be
   filled by a human or surrogate expert."
  [{:keys [msg-text client-id promise-keys] :as msg}]
  (log! :debug (str "domain-expert-says: " msg))
  (if-let [prom-obj (select-promise promise-keys)]
    (do (log! :debug (str "Before resolve!: prom-obj = " prom-obj))
        (p/resolve! (:prom prom-obj) msg-text)
        (clear-keys client-id [(:p-key prom-obj)]))
    (log! :error "domain-expert-says: no p-key (e.g. no question in play)")))

;;;-------------------- Sending questions etc. to a client --------------------------
(defn send-to-chat
  "Send the argument structure to the client.
   If :promise?=true, return a promise that is resolved when the domain-expert responds to the message.
   The only keys of the argument map that are required are :client-id. and :dispatch-key.
   :promise? defaults to true only when the dispatch key is :tbd-says."
  [{:keys [client-id promise? dispatch-key] :as content}]
  (s/assert ::spec/chat-msg-obj content)
  (when-not client-id (throw (ex-info "ws/send: No client-id." {})))
  (if-let [out (->> client-id (get @socket-channels) :out)]
    (let [{:keys [prom p-key]} (when promise? (new-promise client-id))
          msg-obj (cond-> content
                    p-key               (assoc :p-key p-key)
                    true                (assoc :timestamp (now)))]
      (when-not (= :alive? dispatch-key)
        (event! ::send-to-client {:level :debug :msg (elide (str "send-to-chat: msg-obj =" msg-obj) 80)}))
      (go (>! out (str msg-obj)))
      prom)
    (log! :error (str "Could not find out async channel for client " client-id))))

;;; A map from keys to functions used to call responses from clients.
;;; This is defonce so that it doesn't get blown away when websockets.clj is reloaded.
;;; Other namespaces update it (with register-ws-dispatch).
(defonce dispatch-table (atom nil))

(defmacro register-ws-dispatch
  "Add a function to the websocket dispatch table."
  [k func]
  `(do
     (assert (fn? ~func))
     (swap! dispatch-table #(assoc % ~k ~func))
     (log! :info (str "Registered function for websocket: " ~k))))

(defn client-confirms-alive
  "Because the ws go loop can drop, we use this as part of reconnecting."
  [{:keys [client-id]}]
  (event! ::alive {:level :debug :msg (str "client confirms alive: " client-id)})
  (swap! socket-channels #(assoc-in % [client-id :alive?] true))
  nil)

(defn init-dispatch-table!
  "A map from keyword keys (typically values of :dispatch-key) to functions for websockets.
   Other entries in this table include (they are added by register-ws-dispatch):
       :resume-conversation-plan plan/resume-conversation
       :surrogate-call           sur/start-surrogate
       :ask-llm                  llm/llm-directly."
  []
  (reset! dispatch-table {:ping                 ping-confirm
                          :domain-expert-says   domain-expert-says
                          :alive-confirm        client-confirms-alive
                          :close-channel        close-channel}))

;;; When you recompile this, recompile surrogate.clj, interviewers.clj and llm.clj.
(defn dispatch [{:keys [dispatch-key] :as msg}]
  (when-not (#{:ping :alive-confirm} dispatch-key)
    (event! ::dispatch {:level :debug :msg (str "dispatch: Received msg: " msg)}))

  (let [res (cond (= dispatch-key :stop)                        nil ; What needs to be done has already been done.
                  (contains? @dispatch-table dispatch-key)      ((get @dispatch-table dispatch-key) msg)
                  :else                                         (log! :error (str "No dispatch function for " msg "."
                                                                                  " Functions were unregistered? "
                                                                                  " Search for 'register-ws-dispatch' and recompile.")))]
    (when (map? res)
      (when-not (= (:dispatch-key res) :ping-confirm)
        (event! ::response-from-dispatch {:level :debug :msg (str "dispatch: Sending response: " res)}))
      res)))

;;;------------------- Starting and stopping ---------------
(defn wsock-start []
  (clear-promises!)
  (reset! ping-dates {})
  (init-dispatch-table!)
  ;; The following have ws/register-ws-dispatch, which need to be re-established.
  (mount/start (find-var 'scheduling-tbd.llm/llm-tools))
  (mount/start (find-var 'scheduling-tbd.surrogate/surrogates))
  (mount/start (find-var 'scheduling-tbd.interviewers/iviewers))
  [:socket-started])

(defn wsock-stop []
  (doseq [id (keys @socket-channels)]
    (forget-client id))
  (doseq [prom-obj @promise-stack]
    (p/reject! (:prom prom-obj) :restarting))
  (mount/stop  (find-var 'scheduling-tbd.llm/llm-tools))
  (mount/stop  (find-var 'scheduling-tbd.surrogate/surrogates))
  (mount/stop  (find-var 'scheduling-tbd.interviewers/iviewers))
  (reset! promise-stack '())
  [:closed-sockets])

(defstate wsock
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (wsock-start)
  :stop  (wsock-stop))
