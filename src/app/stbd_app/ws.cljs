(ns stbd-app.ws
  "Implement websocket, except for reading, which happens in the chat component, owing to React hooks."
  (:require
   [stbd-app.util   :as util :refer [client-id]]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))
(def ws-url (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))
(def channel (atom nil))
(def connected? (atom false))
(def pending-promise-keys "These are created by the server to track what is being responded to." (atom #{}))

;;; ToDo: A problem scenario is that the server has put up multiple messages and the user-says just once.
;;;       I think I'm going to have to have a ws message from the server just about clearing a key.
;;;       Don't have the client clear it; only have it pick one that it is responding to.
(defn add-promise-key    [k]  (when k (swap! pending-promise-keys conj k)))
(defn clear-promise-keys! [ks]
  (doseq [k ks] (when k (swap! pending-promise-keys disj k))))

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
  (let [msg-obj (-> msg-obj
                    (assoc :client-id client-id)
                    (assoc :promise-keys @pending-promise-keys))]
    (if-let [chan @channel]
      ;; readystate:  0=connecting, 1=open, 2=closing, 3=closed.
      (if (= 1 (.-readyState chan))
        (.send chan (pr-str msg-obj))
        (log/warn "Channel not ready to send."))
      (throw (ex-info "Couldn't send message; no channel." {:msg-obj msg-obj})))))

;;; :tbd-says isn't in this because it sets the set-system-text hook.
(defn dispatch-msg
  "Call a function depending on the value of :dispatch-key in the message."
  [{:keys [dispatch-key promise-keys] :as _msg}]
  (cond (= dispatch-key :clear-promise-keys) (clear-promise-keys! promise-keys)
        (= dispatch-key :alive?)             (send-msg {:dispatch-key :alive? :alive? true})))
