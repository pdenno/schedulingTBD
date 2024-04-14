(ns stbd-app.ws
  "Implement websocket, except for reading, which happens in the chat component, owing to React hooks."
  (:require
   [clojure.edn     :as edn]
   [promesa.core    :as p]
   [stbd-app.util   :as util]
   [taoensso.timbre :as log  :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))
(def client-id "UUID for this instance of the app. Doesn't change except when re-connect!-ing." (atom nil))
(def channel "The JS Websocket object being used for communication with the server." (atom nil))
(def reconnecting? "True only from the time reconnect! is called until socket is once again ready." (atom false))
(def check-process "A process that is run every second once problems are encountered to check socket readiness." (atom nil))
(def check-count "After a certain number of check for readiness, if still not ready, we try connect! again." (atom 0))
(def ping-process "A process run intermittently by js/window.setInterval" (atom nil))

;;; ToDo: A more comprehensive registration method, like the server has for WS dispatch. Useful for share too.
(def change-proj-fn
  "We store this function -- which is set in Top and closes over some refs -- on an atom so we don't have send it around."
  (atom nil))
(def set-tbd-obj-fn
  "Rationale for this is similar to change-proj-fn; set elsewhere, needed by websocket."
  (atom nil))

;;; readystate:  0=connecting, 1=open, 2=closing, 3=closed.
(defn channel-ready? [] (and @channel (= 1 (.-readyState @channel))))

(defn reset-state
  "Reset the state of websocket things. Especially useful for hot reload."
  []
  (when-let [proc @ping-process ] (js/window.clearInterval proc)) ; clear old ping-process, if any.
  (when-let [proc @check-process] (js/window.clearInterval proc))
  (reset! ping-process nil)
  (reset! check-process nil)
  (reset! check-count 0))

(defn ws-url [client-id] (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))

;;; --------------------------promise-key management ---------------------------------------
(def pending-promise-keys "These are created by the server to track what is being responded to." (atom #{}))

(defn remember-promise
  "When the server sends a message that is part of a conversation and requires a response, it adds a keyword
   that associates to a promise on the server side and allows the server to continue the conversation,
   interpreting the :user-says response which repeat this promise-key as answer to the :tbd-says ws message.
   This function just adds to the list, which in most cases will be empty when the argument key is added here."
  [k]  (when k (swap! pending-promise-keys conj k)))

(defn clear-promise-keys! [ks]
  (log/info "Clear promise keys:" ks)
  (doseq [k ks] (when k (swap! pending-promise-keys disj k))))

(declare send-msg)
(def recv-msg-type?
  #{:clear-promise-keys ; Server tells you to forget a promise.
    :alive?             ; Server is asking whether you are alive.
    :reload-proj        ; Server created new current project (e.g. starting, surrogates).
    :ping-confirm       ; Server confirms your ping.
    :tbd-says})         ; Message for the chat, a question, typically.

(defn dispatch-msg
  "Call a function depending on the value of :dispatch-key in the message."
  [{:keys [p-key dispatch-key promise-keys new-proj-map] :as msg}]
  (if-not (recv-msg-type? dispatch-key)
    (log/error "Invalid message type from server:" msg)
    (case dispatch-key
      :clear-promise-keys (clear-promise-keys! promise-keys)
      :alive?             (send-msg {:dispatch-key :alive-confirm})
      :reload-proj        (@change-proj-fn new-proj-map)
      :ping-confirm       #_:ok (log/info "Ping confirm")
      :tbd-says           (do (when p-key (remember-promise p-key))
                              (log/info "tbd-says msg:" msg)
                              (@set-tbd-obj-fn msg))
      "default")))

(def ping-id (atom 0))
(defn ping!
  "Ping the server to keep the socket alive."
  []
  (when-not @reconnecting?
    ;;(log/info "Ping attempt.")
    (send-msg {:dispatch-key :ping, :ping-id (swap! ping-id inc)})))

(defn connect! []
  (reset! client-id (str (random-uuid))) ; ToDo: Use bare random-uuid when we start using transit.
  (reset-state)
  (if-let [chan (js/WebSocket. (ws-url @client-id))]
    (do (log/info "Websocket Connected!" @client-id)
        (reset! channel chan)
        (reset! reconnecting? false)
        (set! (.-onmessage chan)
              (fn [event]       ;; ToDo: Consider transit rather then edn/read-string.
                (try (-> event .-data edn/read-string dispatch-msg)
                     (catch :default e (log/warn "Error in reading from websocket:" e)))))
        (set! (.-onerror chan) (fn [& arg] (log/error "Error on socket: arg=" arg))) ; ToDo: investigate why it gets these.
        ;; Ping to keep-alive the web-socket. 10 sec is not enough; 3 is too little???
        (reset! ping-process (js/window.setInterval (fn [] (ping!)) 4000)))
    (throw (ex-info "Websocket Connection Failed:" {:client-id @client-id}))))


(defn check-channel
  "This gets called by send-msg when (channel-ready?) is not true."
  [prom]
  (log/info "Check-channel: state =" (.-readyState @channel))
  (if (channel-ready?)
    (do (log/info "Channel is READY!")
        (js/window.clearInterval @check-process)
        (reset! reconnecting? false)
        (reset! check-count 0)
        (p/resolve! prom))
    (do (swap! check-count inc)
        (when (> @check-count 9) ; Wait 10 seconds before asking for a new connection.
          (reset! check-count 0)
          (connect!)))))

(defn reconnect!
  "Wait Try to connect! and when successful, deliver on the promise."
  [prom]
  (log/info "Call to reconnect!")
  (when-not @reconnecting?
    (log/info "Starting interval functions.")
    (when-let [proc @check-process] (js/window.clearInterval proc))
    (reset! check-process (js/window.setInterval (fn [] (check-channel prom)) 1000))
    (reset! reconnecting? true)))

(def send-msg-type?
  #{:alive-confirm          ; Like a ping but initiated from server, and only when it seems things have inadvertently disconnected. ToDo: Remove it? Not implemented.
    :ask-llm                ; User asked a "LLM:..." question at the chat prompt.
    :start-surrogate        ; User wrote "SUR: <some product type> at the chat prompt, something like :resume-conversation.
    :resume-conversation    ; Restart the planner (works for :START-A-NEW-PROJECT too).
    :close-channel          ; Close the ws. (Typically, client is ending.) ToDo: Only on dev recompile currently.
    :ping                   ; Ping server.
    :user-says              ; User wrote at the chat prompt (typically answering a question).
    :run-long               ; diagnostic
    :throw-it})             ; diagnostic

(defn send-msg
  "Add client-id and send the message to the server over the websocket.
   Example usage: (send-msg {:dispatch-key :ping})"
  [{:keys [dispatch-key] :as msg-obj}]
  (if-not (send-msg-type? dispatch-key)
    (log/error "Invalid message type in" msg-obj)
    (let [msg-obj (assoc msg-obj :client-id @client-id)]
      (if @channel
        (if (channel-ready?)
          (do (.send @channel (str msg-obj))
              (when-not (= :ping dispatch-key) (log/info "send-msg: SENT msg-obj =" msg-obj)))
          (let [prom (p/deferred)]
            (reconnect! prom)
            (-> prom
                (p/then (fn [_] (log/info "After wait state is" (.-readyState @channel))))
                (p/then (fn [_] (.send @channel (str msg-obj)))))))
        (throw (ex-info "Couldn't send message; no channel." {:msg-obj msg-obj}))))))
