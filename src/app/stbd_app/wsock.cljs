(ns stbd-app.wsock
  (:require
   [cljs.reader     :as edn]
   [stbd-app.util   :as util]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(defonce channel (atom nil)) ; Object required to communicate on websocket."
(defonce ws-url (str "ws://localhost:" util/server-port "/ws?id=" util/client-id))
(defonce ping-process (atom nil)) ; Thing on which js/window.clearInterval is run on reload. ToDo: Its an integer ?!?

;;; ToDo: Is there a keep-alive?
(defn ping!
  "Ping the server to keep the socket alive."
  []
  (if-let [chan @channel]
    (.send chan (str {:dispatch-key :ping, :client-id util/client-id}))
    (log/error "Couldn't send ping; channel isn't open.")))

;;; https://javascript.info/websocket#:~:text=A%20simple%20example,also%20encrypted%20wss%3A%2F%2F%20protocol.
;;; Example useage: (do (connect! <some callback fn>) (send-message! "abc"))
(defn connect!
  "Initiate a websocket connection with the server. Ping it to keep alive.
   Example usage: (connect! ws-handler)"
  [receive-handler]
  (if-let [chan (js/WebSocket. ws-url)]
    (do
      (.log js/console "Connected!")
      (set! (.-onmessage chan) #(->> %
                                     .-data
                                     edn/read-string
                                     receive-handler))
      (reset! channel chan)
      ;; For reasons described here (though that is a Python thing):  https://websockets.readthedocs.io/en/stable/topics/timeouts.html
      (reset! ping-process (js/window.setInterval (fn [] (ping!)) 20000)))
    (throw (ex-info "Websocket Connection Failed!" {:url ws-url}))))

(defn send-message!
  "Send the message to the server. msg can be any Clojure object but if it is a map we add the :client-id.
   Example usage: (send-message! {:dispatch-key :ping})"
  [msg]
  (let [msg (if (map? msg) (assoc msg :client-id util/client-id) msg)]
    (if-let [chan @channel]
      (.send chan (pr-str msg))
      (throw (ex-info "Couldn't send message, channel isn't open!" {:message msg})))))
