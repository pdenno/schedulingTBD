(ns stbd-app.ws
  "Implement websocket, except for reading, which happens in the chat component, owing to React hooks."
  (:require
   [stbd-app.util   :as util :refer [client-id]]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))
(def ws-url (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))
(def channel (atom nil))
(def connected? (atom false))

(def ping-id (atom 0))
(defn ping!
  "Ping the server to keep the socket alive."
  []
  (if-let [chan @channel]
    (when (= 1 (.-readyState chan)) ; 0=connecting, 1=open, 2=closing, 3=closed.
      (.send chan (str {:dispatch-key :ping,
                        :client-id client-id
                        :ping-id (swap! ping-id inc)})))
    (log/error "Couldn't send ping; channel isn't open.")))

(defn send-msg
  "Send the message to the server. msg can be any Clojure object but if it is a map we add the :client-id.
   Example usage: (ws-send-msg! {:dispatch-key :ping})"
  [{:keys [dispatch-key] :as msg-obj}]
  (assert (#{:users-says :alive? :ping :start-a-new-project} dispatch-key))
  (let [msg-obj (assoc msg-obj :client-id client-id)]
    (if-let [chan @channel]
      ;; readystate:  0=connecting, 1=open, 2=closing, 3=closed.
      (if (= 1 (.-readyState chan))
        (.send chan (pr-str msg-obj))
        (log/warn "Channel not ready to send. msg-obj = " msg-obj))
      (throw (ex-info "Couldn't send message; no channel." {:msg-obj msg-obj})))))
