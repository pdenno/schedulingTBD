(ns scheduling-tbd.web.routes.websockets
  "Set up a websockets and message routing for async communication with the client using ring.websocket.async."
  (:require
   [clojure.core.async   :as async :refer [<! >! <!! >!! go]]
   [clojure.edn          :as edn]
   [clojure.walk         :as walk  :refer [keywordize-keys]]
   [mount.core           :as mount :refer [defstate]]
   [promesa.core         :as p]
   [ring.websocket.async :as wsa]
   [scheduling-tbd.util  :refer [now]]
   [taoensso.timbre      :as log]))

;;; Though this system is only exploratory code that will probably run single-user in a Docker instance,
;;; I'm writing this mostly as though the server handles multiple clients.
;;; I think it is just a little less error-prone this way while doing development.

(def ^:diag diag (atom {}))
(def socket-channels "Indexed by a unique ID provided by the client." (atom {}))
(def current-client-id "UUID identifying client. Text above notwithstanding, this is a single-user idea." (atom nil))

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
      (log/info "Closing websocket channels for " id (now))
      (async/close! in)
      (async/close! out)
      (async/close! err)
      (swap! socket-channels #(dissoc % id)))))

(defn close-channel-by-msg
  "Close the channel. This is typically from a client unmount."
  [{:keys [client-id]}]
  (close-ws-channels client-id))

(defn ping-diag
  "Create a ping confirmation for use in middle of a round-trip."
  [{:keys [_client-id _ping-id]}]
  ;(log/info "Ping" _ping-id "from" _client-id)
  {:dispatch-key :ping-confirm})

(def dispatch-table
  "A map from keyword keys (typically values of :dispatch-key) to functions for websockets."
  {:ping          ping-diag
   :close-channel close-channel-by-msg})

(defn dispatch [{:keys [dispatch-key] :as msg}]
  (when-not (= :ping dispatch-key) (log/info "Received msg:" msg))
  (if (contains? dispatch-table dispatch-key)
    ((get dispatch-table dispatch-key) msg)
    (log/error "No dispatch function for " msg)))

;;; ToDo: I'm using blocking versions (>!!, <!!) so you'd think I'd be checking for timeout.
;;;       But some are closed and return nil. I suppose I should put the the test in a future just in case???
(defn close-inactive-channels
  "Close channels that don't respond to :alive?."
  []
  (doseq [id (keys @socket-channels)]
    (let [{:keys [in out _err]} (get @socket-channels id)
          res (do (>!! out (str {:dispatch-key :alive?})) (<!! in))]
      (when-not res
        (log/info "Closing non-alive channel" id)
        (close-ws-channels id)))))

(defn establish-websocket-handler [request]
  (close-inactive-channels)
  (if-let [id (-> request :query-params keywordize-keys :client-id)]
    (do (close-ws-channels id)
        (let [{:keys [in out err]} (make-ws-channels id)]
          (log/info "Starting websocket handler for " id (now))
          (go ; This was wrapped in a try with (finally (close-ws-channels id)) but closing wasn't working out.
            (loop []
              (when-let [msg (<! in)]
                (>! out (-> msg edn/read-string dispatch str)) ; For round-trip messages from client.
                (recur))))
          {:ring.websocket/listener (wsa/websocket-listener in out err)}))
    (log/error "Websocket client did not provide id.")))

;;; ----------------------- Promise management -------------------------------
(def promise-stack "A stack of promise objects." (atom ()))
(defn clear-promises! [] (reset! promise-stack '()))

(defn remove-promise!
  "Remove the promise identified by the argument :prom-key from the promise-stack."
  [prom-key]
  (swap! promise-stack (fn [stack] (remove #(= prom-key (:prom-key %)) stack))))

(defn new-promise
  "Return a vector of [key, promise] to a new promise.
   We use this key to match returning responses (which may be HTTP/user-says) to know
   what is being responded to." ; ToDo: This is not fool-proof.
  [client-id]
  (let [k (-> (gensym "promise-") keyword)
        p (p/deferred)
        obj {:prom p :prom-key k :client-id client-id :timestamp (java.util.Date.)}]
    (swap! promise-stack #(conj % k obj))
    ;(log/info "Adding promise" (:prom-key obj))
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

;;;-------------------- Sending questions to client --------------------------
(defn msg4chat
  "Format a message, typically a question, for transmission to a client."
  ([msg-text] (msg4chat msg-text nil))
  ([msg-text p-key]
   (cond-> {:dispatch-key :tbd-says
            :msg msg-text
            :timestamp (now)}
     p-key (assoc :promise-key p-key))))

(defn send
  "Send the argument message text to the current project (or some other destination if a client-id is provided.
   Return a promise that is resolved when the user responds to the message, by whatever means (http or ws)."
  [msg-text & {:keys [promise? client-id]
               :or {promise? true client-id @current-client-id}}]
  (when-not client-id (throw (ex-info "ws/send: No client-id." {})))
  (if-let [out (->> client-id (get @socket-channels) :out)]
    (if promise?
      (let [{:keys [prom prom-key]} (new-promise client-id)]
        (go (>! out (-> msg-text (msg4chat prom-key) str)))
        ;(log/info "ws-send: promise-key = " prom-key)
        prom)
      (go (>! out (-> msg-text msg4chat str))))
    (log/error "Could not find out async channel for client" client-id)))

;;;------------------- Starting and stopping ---------------
(defn wsock-start []
  (clear-promises!)
  :started!)

(defn wsock-stop []
  (doseq [id (keys @socket-channels)]
    (close-ws-channels id))
  [:closed-sockets])

(defstate wsock
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (wsock-start)
  :stop (wsock-stop))
