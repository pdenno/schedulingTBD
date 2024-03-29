(ns scheduling-tbd.web.routes.websockets
  "Set up a websockets and message routing for async communication with the client using ring.websocket.async."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.core.async       :as async :refer [<! >! <!! >!! go]]
   [clojure.edn              :as edn]
   [clojure.spec.alpha       :as s]
   [clojure.walk             :as walk  :refer [keywordize-keys]]
   ;;[cognitect.transit        :as transit]
   [mount.core               :as mount :refer [defstate]]
   [promesa.core             :as p]
   [ring.websocket.async     :as wsa]
   [scheduling-tbd.specs     :as spec]
   [scheduling-tbd.util      :refer [now]]
   [taoensso.timbre          :as log]))

(def ^:diag diag (atom {}))
(def socket-channels "Indexed by a unique client-id provided by the client." (atom {}))

(def client-current-proj "Current project (keyword) indexed by client-id (string)."  (atom {}))
(defn clear-client-current-proj [] (reset! client-current-proj {}))
(defn set-current-project
  "We track client's current project, but since clients are ephemeral, it isn't a DB thing."
  [client-id pid]
  (when-not (some #(= client-id %) (keys @socket-channels))
    (log/warn "Nothing know about client when setting current project. client-id =" client-id))
  (swap! client-current-proj #(assoc % client-id pid)))

(declare user-says send-to-chat dispatch)

;;; ToDo: move to devl?
(defn ^:diag any-client!
  "Return the client-id of a any client, hoping there is just one.
   This is only used in development, I think."
  []
  (-> @socket-channels keys first))

(defn make-ws-channels
  "Create channels and store them keyed by the unique ID provided by the client."
  [id]
  (let [chans {:in  (async/chan)
               :out (async/chan)
               :err (async/chan)
               :exit-atm (atom nil)}]
    (swap! socket-channels  #(assoc % id chans))
    chans))

;;; ToDo: I think, before closing, I want to send something to the client so that it check it's exit-atm
(defn close-ws-channels [id]
  (when (contains? @socket-channels id)
    (let [{:keys [in out err exit-atm]} (get @socket-channels id)]
      (log/info "Closing websocket channels for " id (now))
      ;; Set exit-atm and send something so go loop will be jogged and see it.
      (reset! exit-atm true)
      (>!! in (str {:dispatch-key :stop}))
      ;; (>!! err "STOP") ; ToDo: Why do things stall when I do this?
      (-> (p/delay 500)
          (p/then (fn [_]
                    (async/close! in)
                    (async/close! out)
                    (async/close! err)
                    (swap! socket-channels #(dissoc % id))))))))

(declare clear-promises!)
(defn forget-client
  "Close the channel and forget the promises associated with the client.
   This is typically from a client unmount."
  [{:keys [client-id]}]
  (log/info client-id "closes its websocket.")
  (close-ws-channels client-id)
  (clear-promises! client-id))

(defn ping-diag
  "Create a ping confirmation for use in middle of a round-trip."
  [{:keys [client-id ping-id]}]
  ;(log/info "Ping" ping-id "from" client-id)
  {:dispatch-key :ping-confirm})

(declare clear-keys select-promise)

;;; ToDo: I'm using blocking versions (>!!, <!!) so you'd think I'd be checking for timeout.
;;;       But some are closed and return nil. I suppose I should put the the test in a future just in case???
(defn close-inactive-channels
  "Close channels that don't respond to :alive?."
  []
  (doseq [id (keys @socket-channels)]
    (let [{:keys [in out _err]} (get @socket-channels id)
          res (do (>!! out (str {:dispatch-key :alive?})) (<!! in))]
      (when-not res
        ;;(log/info "Closing non-alive channel" id)
        (close-ws-channels id)))))

(defn error-listener
  "Start a go loop for listening for errors from the client."
  [client-id]
  (if-let [{:keys [err exit-atm]} (get @socket-channels client-id)]
    (go
      (loop []
        (when-let [msg (<! err)]
          (log/error "Error channel reports:" msg "client-id =" client-id)
          (when-not @exit-atm (recur)))))
    (log/error "error-listener: Cannot find client-id" client-id)))

(defn establish-websocket-handler
  "Handler for http:/ws request.
   Set up web socket in, out, and err channels."
  [request]
  (close-inactive-channels)
  (if-let [client-id (-> request :query-params keywordize-keys :client-id)]
    (do (close-ws-channels client-id)
        (let [{:keys [in out err exit-atm]} (make-ws-channels client-id)]
          ;;(log/info "Starting websocket handler for " id (now))
          (go ; This was wrapped in a try with (finally (close-ws-channels id)) but closing wasn't working out.
            (loop []
              (when-let [msg (<! in)] ; Listen fo messages
                (reset! diag msg)
                (>! out (-> msg edn/read-string dispatch str)))
              (when-not @exit-atm (recur))))
          (log/warn "Exiting go loop.")
          (reset! diag :exiting!)      ; ToDo: I expected to see this, but instead I get "{:dispatch-key :stop}"...
          (error-listener client-id)   ; ...from from close-ws-channels above.
          {:ring.websocket/listener (wsa/websocket-listener in out err)}))
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
;;; Note that the whole process is run in websockets. REST is only used for less interactive UI activities (e.g. changing projects).
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
  "Handle web-socket message with dispatch key :user-says. This will typically clear a promise-key."
  [{:keys [msg-text client-id promise-keys] :as msg}]
  (log/info "User-says:" msg)
  (if-let [prom-obj (select-promise promise-keys)]
    (do (clear-keys client-id [(:p-key prom-obj)])
        (log/info "Before resolve!: msg-text =" msg-text)
        (p/resolve! (:prom prom-obj) msg-text))
    (log/error "user-says: no p-key (e.g. no question in play)")))

;;;-------------------- Sending questions to client --------------------------
;;; (ws/send-to-chat "Hey are you alive?" :promise? false :client-id (ws/any-client!)
(defn send-to-chat
  "Send the argument message-vec to the current project (or some other destination if a client-id is provided.
   Return a promise that is resolved when the user responds to the message, by whatever means (http or ws).
     msg-vec is is a vector of ::spec/msg-text-elem and :spec/msg-link-elem. See specs.cljs."
  [{:keys [msg-vec promise? client-id dispatch-key]
    :or {msg-vec [] promise? true dispatch-key :tbd-says} :as content}]
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
    (log/error "Could not find out async channel for client" client-id)))

(def dispatch-table
  "A map from keys to functions used to call responses from clients."
  (atom nil))

(defn register-ws-dispatch
  "Add a function to the websocket dispatch table."
  [k func]
  (swap! dispatch-table #(assoc % k func))
  (log/info "Registered function for websocket:" k))

(defn init-dispatch-table!
  "A map from keyword keys (typically values of :dispatch-key) to functions for websockets.
   Other entries in this table include (they are added by register-ws-dispatch):
       :start-a-new-project plan/interview-for-new-proj
       :surrogate-call      sur/start-surrogate
       :ask-llm             llm/directly."
  []
  (reset! dispatch-table {:ping                 ping-diag
                          :user-says            user-says
                          :close-channel        forget-client}))

(defn dispatch [{:keys [dispatch-key] :as msg}]
  ;(when-not (= :ping dispatch-key)  (log/info "Received msg:" msg))
  ;(log/info "Received msg:" msg)
  (cond (= dispatch-key :stop)                        nil ; What needs to be done has already been done.
        (contains? @dispatch-table dispatch-key)      ((get @dispatch-table dispatch-key) msg)
        :else                                         (log/error "No dispatch function for " msg)))

;;;------------------- Starting and stopping ---------------
(defn wsock-start []
  (clear-promises!)
  (clear-client-current-proj) ; ToDo: Restarting the server means we lose knowledge of their current project. Ok?
  (init-dispatch-table!))

(defn wsock-stop []
  (doseq [id (keys @socket-channels)]
    (close-ws-channels id))
  [:closed-sockets])

(defstate wsock
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (wsock-start)
  :stop  (wsock-stop))
