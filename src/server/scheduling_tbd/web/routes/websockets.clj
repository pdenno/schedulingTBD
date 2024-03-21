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
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.specs     :as spec]
   [scheduling-tbd.surrogate :as sur]
   [scheduling-tbd.util      :refer [now]]
   [taoensso.timbre          :as log]))

(def ^:diag diag (atom {}))
(def socket-channels "Indexed by a unique ID provided by the client." (atom {}))
(def current-client-id "UUID identifying client. This is for diagnostics." (atom nil))
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
  (reset! current-client-id id)
  (let [chans {:in  (async/chan)
               :out (async/chan)
               :err (async/chan)}]
    (swap! socket-channels  #(assoc % id chans))
    chans))

(defn close-ws-channels [id]
  (when (contains? @socket-channels id)
    (let [{:keys [in out err]} (get @socket-channels id)]
      ;;(log/info "Closing websocket channels for " id (now))
      (async/close! in)
      (async/close! out)
      (async/close! err)
      (swap! socket-channels #(dissoc % id)))))

(defn close-channel-by-msg
  "Close the channel. This is typically from a client unmount."
  [{:keys [id]}]
  (close-ws-channels id))

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

(defn establish-websocket-handler [request]
  (close-inactive-channels)
  (if-let [id (-> request :query-params keywordize-keys :client-id)]
    (do (close-ws-channels id)
        (let [{:keys [in out err]} (make-ws-channels id)]
          ;;(log/info "Starting websocket handler for " id (now))
          (go ; This was wrapped in a try with (finally (close-ws-channels id)) but closing wasn't working out.
            (loop []
              (when-let [msg (<! in)]
                (>! out (-> msg edn/read-string dispatch str)) ; For round-trip messages from client.
                (recur))))
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

(def promise-stack "A stack of promise objects." (atom ()))
(defn clear-promises! [] (reset! promise-stack '()))

(defn remove-promise!
  "Remove the promise identified by the argument :prom-key from the promise-stack."
  [prom-key]
  (swap! promise-stack (fn [stack] (remove #(= prom-key (:prom-key %)) stack))))

(s/def ::promise (s/keys :req-un [::prom ::prom-key ::client-id ::timestamp]))
(s/def ::prom p/promise?)
(s/def ::prom-key keyword?)
(s/def ::client-id string?)
(s/def ::timestamp #(instance? java.util.Date %))

(defn new-promise
  "Return a vector of [key, promise] to a new promise.
   We use this key to match returning responses from ws-send :user-says, to know
   what is being responded to." ; ToDo: This is not fool-proof.
  [client-id]
  (let [k (-> (gensym "promise-") keyword)
        p (p/deferred)
        obj {:prom p :prom-key k :client-id client-id :timestamp (java.util.Date.)}]
    (swap! promise-stack #(conj % obj))
    ;; Because I saw it messed up once...
    (when-not (every? #(s/valid? ::promise %) @promise-stack)
      (throw (ex-info "Promise stack is messed up" {:stack @promise-stack})))
    obj))

(defn ^:diag lookup-promise
  "Return the promise associated with the argument key."
  [k]
  (or (some #(when (= k (:prom-key %)) %) @promise-stack)
      (log/error "Could not find promise with key" k)))

(defn select-promise
  "Return the promise that is top-most on the stack and also in the argument set."
  [client-keys]
  (let [in-client-set? (set client-keys)
        stack @promise-stack]
    (or (some #(when (in-client-set? (:prom-key %)) %) stack)
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
    (do (clear-keys client-id [(:prom-key prom-obj)])
        (log/info "Before resolve!: msg-text =" msg-text)
        (p/resolve! (:prom prom-obj) msg-text))
    (log/error "user-says: no prom-key")))

;;;-------------------- Sending questions to client --------------------------
;;; (ws/send-to-chat "Hey are you alive?" :promise? false :client-id (ws/any-client!)
(defn send-to-chat
  "Send the argument message text to the current project (or some other destination if a client-id is provided.
   Return a promise that is resolved when the user responds to the message, by whatever means (http or ws).
     msg-obj is is a vector of ::spec/msg-text-elem and :spec/msg-link-elem. See specs.cljs."
  [msg-obj & {:keys [promise? client-id] :or {promise? true}}]
  (s/assert ::spec/chat-msg msg-obj)
  (when-not client-id (throw (ex-info "ws/send: No client-id." {})))
  (if-let [out (->> client-id (get @socket-channels) :out)]
    (let [{:keys [prom prom-key]} (when promise? (new-promise client-id))
          msg-obj (cond-> {:dispatch-key :tbd-says
                           :msg-obj msg-obj
                           :client-id client-id
                           :timestamp (now)}
                    prom-key (assoc :p-key prom-key))]
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
                          :close-channel        close-channel-by-msg}))

(defn dispatch [{:keys [dispatch-key] :as msg}]
  (when-not (= :ping dispatch-key)  (log/info "Received msg:" msg))
  (if (contains? @dispatch-table dispatch-key)
    ((get @dispatch-table dispatch-key) msg)
    (log/error "No dispatch function for " msg)))


;;;------------------- Starting and stopping ---------------
(defn wsock-start []
  (clear-promises!)
  (init-dispatch-table!))

(defn wsock-stop []
  (doseq [id (keys @socket-channels)]
    (close-ws-channels id))
  [:closed-sockets])

(defstate wsock
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (wsock-start)
  :stop  (wsock-stop))
