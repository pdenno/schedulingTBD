(ns stbd-app.wsock
  "This is web-socket stuff I'm not using anymore. I'll keep it around just in case."
  (:require
   [cljs.reader                :as edn]
   [stbd-app.util              :as util]
   [taoensso.timbre            :as log :refer-macros [info debug]]))

(defonce channel (atom nil)) ; A clojure.core.async.impl.channels.ManyToManyChannel implementing a web socket.
(defonce ping-process (atom nil)) ; Thing on which js/window.clearInterval is run on reload. ToDo: Its an integer ?!?
(def ^:diag diag (atom nil))
;;;(defonce keep-alive? (atom true))
(def client-id "A random uuid naming this client. It changes on disconnect." (str (random-uuid)))
(def ws-url (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))

(defn ^:diag no-op [& _arg])
(defn confirm-ping [msg] (log/info "Confirmed ping to server:" msg))
(defn ws-tbd-says [msg] (log/info "ws-tbd-says:" msg))

(def dispatch-table
  "A map from keyword keys (typically values of :dispatch-key) to functions for websockets."
  {:ping-confirm  confirm-ping
   :tbd-says      ws-tbd-says})

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

(defn ^:diag ws-dispatch [{:keys [dispatch-key] :as msg}]
  (reset! diag msg)
  (if-let [dfn (get dispatch-table dispatch-key)]
    (dfn msg)
    (log/error "No dispatch function for " msg)))

(defn dispatch-on-channel [event receive-handler]
  (let [data (.-data event)]
    (try
      (->> data edn/read-string receive-handler)
      (catch :default e (log/error "handling failed on WS data =" data "err =" e)))))

;;; https://javascript.info/websocket#:~:text=A%20simple%20example,also%20encrypted%20wss%3A%2F%2F%20protocol.
;;; https://medium.com/pragmatic-programmers/multi-user-with-websockets-839f1459fe81
(defn connect!
  "Initiate a websocket connection with the server. Ping it to keep alive. If it closes start a new one.
   Example usage: (connect! ws-dispatch)"
  [receive-handler]
  (if-let [chan (js/WebSocket. ws-url)]
    (do
      (log/info "Websocket Connected!")
      (reset! channel chan)
      (set! (.-onmessage chan) (fn [event] (dispatch-on-channel event receive-handler)))
      ;;(set! (.-onclose chan) (fn [& _] (reconnect-if-possible chan receive-handler)))
      (set! (.-onerror chan) (fn [& arg] (log/error "Error on socket: arg=" arg)))
      (reset! ping-process (js/window.setInterval (fn [] (ping!)) 10000))) ; Ping to keep-alive.
    (throw (ex-info "Websocket Connection Failed:" {:url ws-url}))))


(defn send-message
  "Send the message to the server. msg can be any Clojure object but if it is a map we add the :client-id.
   Example usage: (send-message {:dispatch-key :ping})"
  ([msg] (send-message msg @channel client-id))
  ([msg chan client-id]
   (let [msg (if (map? msg) (assoc msg :client-id client-id) msg)]
     (if chan
       (.send chan (pr-str msg))
       (throw (ex-info "Couldn't send message; no channel." {:message msg}))))))

(defn ^:diag reconnect-if-possible [chan receive-handler]
  (if (= 3 (.-readyState chan))
    (do (log/info "======== Making new connection on WS close.")
        (connect! receive-handler))
    (log/error "Web socket not ready to reconnect.")))
