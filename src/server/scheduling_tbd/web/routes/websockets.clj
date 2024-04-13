(ns scheduling-tbd.web.routes.websockets
  "Set up a websockets and message routing for async communication with the client using ring.websocket.async."
  (:refer-clojure :exclude [send])
  (:require
   ;[clj-async-profiler.core  :as prof]
   [clojure.core.async       :as async :refer [<! >! >!! go]]
   [clojure.edn              :as edn]
   [clojure.spec.alpha       :as s]
   [clojure.walk             :as walk  :refer [keywordize-keys]]
   ;;[cognitect.transit        :as transit]
   [mount.core               :as mount :refer [defstate]]
   [promesa.core             :as p]
   [promesa.exec             :as px]
   [ring.websocket.async     :as wsa]
   [scheduling-tbd.specs     :as spec]
   [scheduling-tbd.util      :refer [now]]
   [taoensso.timbre          :as log]))

(def ^:diag diag (atom {}))
(def socket-channels "Indexed by a unique client-id provided by the client." (atom {}))
(declare user-says send-to-chat dispatch)

(defn make-ws-channels
  "Create channels and store them keyed by the unique ID provided by the client."
  [id]
  (let [chans {:in  (async/chan)
               :out (async/chan)
               :err (async/chan)
               :alive? true
               :exit? false}]
    (swap! socket-channels  #(assoc % id chans))
    chans))

(defn exiting? [client-id] (get-in @socket-channels [client-id :exit?]))

(defn close-ws-channels [client-id]
  (when (contains? @socket-channels client-id)
    (let [{:keys [in out err]} (get @socket-channels client-id)]
      (log/info "Closing websocket channels for inactive client" client-id (now))
      ;; Set exit? and send something so go loop will be jogged and see it.
      (swap! socket-channels #(assoc-in % [client-id :exit?] true))
      (go (>! in (str {:dispatch-key :stop}))) ; <====================================================================== I don't see this!
      ;; Keep delay high to be sure :stop is seen. (p/submit! is probably helpful here; promises are executed async an out of order.
      (-> (p/delay 3000)
          (p/then (fn [_]
                    (async/close! in)
                    (async/close! out)
                    (async/close! err)
                    (Thread/sleep 1000)
                    (swap! socket-channels #(dissoc % client-id))))))))

(defn ^:diag inject-stop
  "Send a message to incoming on the socket to exit the go loop.
   I wrote this because I don't see it happening in close-ws-channels above."
  [client-id]
  (let [{:keys [in]} (get @socket-channels client-id)]
    (go (>! in (str {:dispatch-key :stop})))))

(declare clear-promises! clear-keys select-promise)

(defn forget-client
  "Close the channel and forget the promises associated with the client.
   This is typically from a client unmount."
  [{:keys [client-id]}]
  (log/info client-id "closes its websocket.")
  (close-ws-channels client-id)
  (clear-promises! client-id))

(def ping-dates  "Indexed by client-id, value is last time it pinged."  (atom {}))
(defn ping-confirm
  "Create a ping confirmation for use in middle of a round-trip."
  [{:keys [client-id]}] ; also ping-id
  ;(log/info "confirming ping.")
  (swap! ping-dates #(assoc % client-id (now)))
  {:dispatch-key :ping-confirm}
  nil) ; <========================================================================================================================== Does confirming help in any way? Should I let the client close?

;;; Not to be confused with sending an :alive? dispatch-key
(defn alive?
  "Return true if client had recent ping activity.
   option :recent defaults to 30 seconds."
  [client-id & {:keys[recent] :or {recent 30}}]
  (if-let [last-date (get @ping-dates client-id)]
    (< (- (-> (now)     .toInstant .toEpochMilli)
          (-> last-date .toInstant .toEpochMilli))
       (* recent 1000))
    (do (swap! ping-dates #(dissoc % client-id)) false)))

(defn ^:diag recent-client!
  "Return the client-id of the client that pinged most recently.
   This should only used in development, I think!"
  []
  (->> @ping-dates seq (sort-by second) reverse first first))

(defn close-inactive-channels
  "Close channels that don't respond to :alive?."
  []
  (doseq [client-id (keys @socket-channels)]
    (when-not (alive? client-id)
      (swap! socket-channels #(assoc-in % [client-id :exit?] true))
      (close-ws-channels client-id))))

(defn error-listener
  "Start a go loop for listening for errors from the client."
  [client-id]
  (if-let [{:keys [err exit?]} (get @socket-channels client-id)]
    (go
      (loop []
        (when-let [msg (<! err)]
          (log/error "Client reports" (type msg) ": client-id =" client-id)
          (close-ws-channels client-id)
          (when-not exit? (recur)))))
    (log/error "error-listener: Cannot find client-id" client-id)))

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

;;; Why I'm using CompleteableFutures (promesa) rather than Futures (clojure.core):
;;; https://medium.com/@reetesh043/difference-between-completablefuture-and-future-in-java-4f7e00bcdb56
(defn dispatching-loop
  "Run the listening and dispatching loop for the client's channel."
  [client-id]
  (let [{:keys [in out]} (get @socket-channels client-id)]
    (go
      (loop []
        (when-let [msg (<! in)] ; Listen for messages.
          (log/info "In loop msg =" msg)
          (let [msg (edn/read-string msg)]
            (if (= :stop (:dispatch-key msg))
              (swap! socket-channels #(assoc-in % [client-id :exit?] true))
              (let [prom (px/submit! (fn [] (dispatch msg)))]
                (-> prom
                    (p/then (fn [r] (log/info "dispatch returning:" r) r))
                    (p/then (fn [r] (when r (go (>! out (str r)))))) ; Some, like :resume-conversation don't return anything themselves.
                    (p/catch (fn [err] (log/error "Error dispatching on socket: client-id =" client-id "err =" err))))))
            (when-not (exiting? client-id) (recur)))))
      ;; There are many reasons a websocket connection might be dropped. It is absolutely necessary that it exits the go loop
      ;; when the socket closes. It closes when, for example, there is a WebSocketTimeoutException from the client (see error-listener).
      (log/info "Exiting dispatching loop:" client-id))))

(defn establish-websocket-handler
  "Handler for http:/ws request. Returns nothing interesting.
   Set up web socket in, out, and err channels.
   This is called for each client. Client can call it more than once if, for example,
   it finds that the channel has become unusable (e.g. using (.-readyState) in JS.
   In that case, the old channel will eventually get destroyed by close-inactive-channels."
  [request]
  (log/info "Establishing ws handler for" (-> request :query-params keywordize-keys :client-id))
  (close-inactive-channels) ; <==============================================================================================
  (if-let [client-id (-> request :query-params keywordize-keys :client-id)]
    (try (close-ws-channels client-id)
         (let [{:keys [in out err]} (make-ws-channels client-id)]
           (error-listener client-id)   ; This and next are go loops,
           (dispatching-loop client-id) ; which means they are non-blocking.
           {:ring.websocket/listener (wsa/websocket-listener in out err)})
         (catch Exception e ; ToDo: Never happens
           ;(close-ws-channels client-id) ; <=========================================================================
           (log/error "Error in ws loop:" (type e))))
    (log/error "Websocket client did not provide id.")))

;;; ----------------------- Promise management -------------------------------
;;;   * By design, the server waits for a response before sending another question (except for maybe some future where there is a "Are you still there?").
;;;     - Questions always are transmitted over the websocket; they are the beginning of something new.
;;;   * The user, however might be able to send multiple chat responses before the next question.
;;;   * promise-keys are an attempt to match the question to the responses.
;;;     - When the server asks a question, it adds a promise key to the question; the client manages a set of these keys.
;;;     - When the user does a user-says, it tells the server what keys it has.
;;;     - As the system processes a user-says, it decides what keys to tell the client to erase.
;;;     - The system uses the keys to match a question to a user-says.
;;;       + It picks the key top-most on its stack that is also in the set sent by the client with user-says.
;;;     - When the system has decided where to route the question, it can also tell the client to remove that key from its set.
;;;       + This last step is important because the server needs to get old promise-keys off the stack.
;;;
;;; Note that the whole chat process is run in websockets. REST is only used for less interactive UI activities (e.g. changing projects).
(s/def ::promise (s/keys :req-un [::prom ::p-key ::client-id ::timestamp]))
(s/def ::prom p/promise?)
(s/def ::p-key keyword?)
(s/def ::client-id string?)
(s/def ::timestamp #(instance? java.util.Date %))

(def promise-stack "A stack of promise objects." (atom ()))

(defn clear-promises!
  ([] (reset! promise-stack '()))
  ([client-id] (swap! promise-stack #(remove (fn [p] (= (:client-id p) client-id)) %))))

(defn remove-promise!
  "Remove the promise identified by the argument :p-key from the promise-stack."
  [p-key]
  (swap! promise-stack (fn [stack] (remove #(= p-key (:p-key %)) stack))))

(defn new-promise
  "Return a vector of [key, promise] to a new promise.
   We use this key to match returning responses from ws-send :user-says, to know
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
        (log/warn "Couldn't find promise for any of" client-keys))))

(defn clear-keys
  "Clear the argument promises (their keys are provided) from the stack and
   send a message to the client to do similar."
  [client-id client-keys]
  (doseq [k client-keys] (remove-promise! k))
  (if-let [out (->> client-id (get @socket-channels) :out)]
    (let [msg {:dispatch-key :clear-promise-keys
               :promise-keys client-keys}]
      (go (>! out (str msg))))
    (log/error "Could not find out async channel for client" client-id)))

;;;--------------------- Receiving a response from a client -----------------------
(defn user-says
  "Handle websocket message with dispatch key :user-says. This will typically clear a promise-key."
  [{:keys [msg-text client-id promise-keys] :as msg}]
  (log/info "User-says:" msg)
  (if-let [prom-obj (select-promise promise-keys)]
    (do (log/info "Before resolve!: prom-obj =" prom-obj)
        (reset! diag prom-obj)
        (p/resolve! (:prom prom-obj) msg-text)
        (clear-keys client-id [(:p-key prom-obj)]))
    (log/error "user-says: no p-key (e.g. no question in play)")))

;;;-------------------- Sending questions to client --------------------------
(defn send-to-chat
  "Send the argument message-vec to the current project (or some other destination if a client-id is provided.
   Return a promise that is resolved when the user responds to the message, by whatever means (http or ws).
     msg-vec is is a vector of ::spec/msg-text-elem and :spec/msg-link-elem. See specs.cljs."
  [{:keys [promise? client-id dispatch-key msg-vec] :or  {promise? true} :as content}]
  (s/assert ::spec/chat-msg-vec msg-vec)
  (let [content (cond-> content ; Just so that we can uses s/assert below!
                  (not (contains? content :dispatch-key))  (assoc :dispatch-key :tbd-says))]
    (s/assert ::spec/chat-msg-obj content)
    (log/info "send-to-chat: content =" content)
    (when-not client-id (throw (ex-info "ws/send: No client-id." {})))
    (if-let [out (->> client-id (get @socket-channels) :out)]
      (let [{:keys [prom p-key]} (when promise? (new-promise client-id))
            msg-obj (cond-> content
                      true                (assoc :dispatch-key (or dispatch-key :tbd-says))
                      p-key               (assoc :p-key p-key)
                      true                (assoc :timestamp (now)))]
        (go (>! out (str msg-obj)))
        prom)
      (log/error "Could not find out async channel for client" client-id))))

(def dispatch-table
  "A map from keys to functions used to call responses from clients."
  (atom nil))

(defn register-ws-dispatch
  "Add a function to the websocket dispatch table."
  [k func]
  (assert (fn? func))
  (swap! dispatch-table #(assoc % k func))
  (log/info "Registered function for websocket:" k))

(defn client-confirms-alive
  "Because the ws go loop can drop, we use this as part of reconnecting."
  [{:keys [client-id]}]
  (swap! socket-channels #(assoc-in % [client-id :alive] true)))

(defn init-dispatch-table!
  "A map from keyword keys (typically values of :dispatch-key) to functions for websockets.
   Other entries in this table include (they are added by register-ws-dispatch):
       :start-a-new-project plan/interview-for-new-proj
       :surrogate-call      sur/start-surrogate
       :ask-llm             llm/llm-directly."
  []
  (reset! dispatch-table {:ping                 ping-confirm
                          :user-says            user-says
                          :alive-confirm        client-confirms-alive
                          :close-channel        forget-client}))

(defn dispatch [{:keys [dispatch-key] :as msg}]
  (when-not (= :ping dispatch-key)  (log/info "dispatch: Received msg:" msg))
  (let [res (cond (= dispatch-key :stop)                        nil ; What needs to be done has already been done.
                  (contains? @dispatch-table dispatch-key)      ((get @dispatch-table dispatch-key) msg)
                  :else                                         (log/error "No dispatch function for " msg))]
    (when (map? res)
      (when-not (= (:dispatch-key res) :ping-confirm)
        (log/info "dispatch: Sending response:" res))
      res)))

;;;------------------- Starting and stopping ---------------
(defn wsock-start []
  (clear-promises!)
  (reset! ping-dates {})
  (init-dispatch-table!))

(defn wsock-stop []
  (doseq [id (keys @socket-channels)]
    (close-ws-channels id))
  (doseq [prom-obj @promise-stack]
    (p/reject! (:prom prom-obj) :restarting))
  (reset! promise-stack '())
  [:closed-sockets])

(defstate wsock
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (wsock-start)
  :stop  (wsock-stop))
