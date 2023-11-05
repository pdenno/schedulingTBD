(ns stbd-app.wsock
  (:require
   [cljs.reader :as edn]))

(defonce channel (atom nil))

;;; I guess this is my receive-handler.
(defn im-here
  [args]
  (.log js/console "I'm here (in the receive-handler)! args = " args))

;;; https://javascript.info/websocket#:~:text=A%20simple%20example,also%20encrypted%20wss%3A%2F%2F%20protocol.
;;; When new WebSocket(url) is created, it starts connecting immediately.
;;; Example useage: (do (connect! "ws://localhost:3300/ws" im-here) (send-message! "abc"))
(defn connect! [url receive-handler]
  (if-let [chan (js/WebSocket. url)]
    (do
      (.log js/console "Connected!")
      (set! (.-onmessage chan) #(->> %
                                     .-data
                                     edn/read-string
                                     receive-handler))
      (reset! channel chan))
    (throw (ex-info "Websocket Connection Failed!" {:url url}))))

(defn send-message! [msg]
  (if-let [chan @channel]
    (.send chan (pr-str msg))
    (throw (ex-info "Couldn't send message, channel isn't open!" {:message msg}))))
