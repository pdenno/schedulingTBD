(ns stbd-app.ws
  "Implement websocket, except for reading, which happens in the chat component, owing to React hooks."
  (:require
   [clojure.edn     :as edn]
   [promesa.core    :as p]
   [stbd-app.util   :as util]
   [taoensso.timbre :as log  :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))
(def channel (atom nil))
(def connected? (atom false))
(def change-proj-fn
  "We store this function -- which is set in Top and closes over some refs -- on an atom so we don't have send it around."
  (atom nil))
(def set-tbd-obj-fn
  "Rationale for this is similar to change-proj-fn; set elsewhere, needed by websocket."
  (atom nil))
(def client-id "UUID for this instance of the app. Doesn't change except when re-connect!-ing." (atom nil))

(defn ws-url [client-id] (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))

;;; --------------------------promise-key management ---------------------------------------
(def pending-promise-keys "These are created by the server to track what is being responded to." (atom #{}))

(defn remember-promise ; Silly name?
  "When the server sends a message that is part of a conversation and requires a response, it adds a keyword
   that associates to a promise on the server side and allows the server to continue the conversation,
   interpreting the :user-says response which repeat this promise-key as answer to the :tbd-says ws message.
   This function just adds to the list, which in most cases will be empty when the argument key is added here."
  [k]  (when k (swap! pending-promise-keys conj k)))

(defn clear-promise-keys! [ks]
  (doseq [k ks] (when k (swap! pending-promise-keys disj k))))

(declare send-msg)
(def recv-msg-type?
  #{:clear-promise-keys ; Server tells you to forget a promise.
    :alive              ; Server is asking whether you are alive.
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
      :alive?             (send-msg {:dispatch-key :alive? :alive? true})
      :reload-proj        (@change-proj-fn new-proj-map)
      :ping-confirm       :ok
      :tbd-says           (do (when p-key (remember-promise p-key))
                              (log/info "msg-obj =" msg)
                              (@set-tbd-obj-fn msg))
      "default")))

(def send-tries "Counts how many consecutive times send failed." (atom 0))
(defn connect! []
  (reset! send-tries 0)
  (reset! client-id (str (random-uuid)))
  (if-let [chan (js/WebSocket. (ws-url @client-id))]
    (do (log/info "Websocket Connected!")
        (reset! channel chan)
        (reset! connected? true)
        (set! (.-onmessage chan)
              (fn [event]       ;; ToDo: Consider transit rather then edn/read-string.
                (try (-> event .-data edn/read-string dispatch-msg)
                     (catch :default e (log/warn "Error in :tbd-says socket reading:" e)))))
        (set! (.-onerror chan) (fn [& arg] (log/error "Error on socket: arg=" arg)))) ; ToDo: investigate why it gets these.
    (throw (ex-info "Websocket Connection Failed:" {:url ws-url}))))

(def send-msg-type?
  #{:ask-llm                ; User asked a "LLM:..." question at the chat prompt.
    :close-channel          ; Close the ws. (Typically, client is ending.)
    :ping                   ; Ping server.
    :resume-conversation    ; Restart the planner (works for :START-A-NEW-PROJECT too).
    :start-surrogate        ; User wrote "SUR: <some product type> at the chat prompt, something like :resume-conversation.
    :user-says})            ; User wrote at the chat prompt (typically answering a question).


(defn send-msg
  "Send the message to the server. msg can be any Clojure object but if it is a map we add the :client-id.
   Example usage: (ws-send-msg! {:dispatch-key :ping})"
  [{:keys [dispatch-key] :as msg-obj}]
  (if-not (send-msg-type? dispatch-key)
    (log/error "Invalid message type in" msg-obj)
    (let [msg-obj (assoc msg-obj :client-id @client-id)]
      (when (> @send-tries 10)
        (log/info "send-msg: send-tries = " @send-tries "reconnecting.")
        (connect!)
        (p/delay 1000))
      ;; readystate:  0=connecting, 1=open, 2=closing, 3=closed.
      (if-let [chan @channel]
        (if (= 1 (.-readyState chan))
          (do (.send chan (str msg-obj))
              (when-not (= :ping dispatch-key) (log/info "send-msg: SENT msg-obj =" msg-obj))
              (reset! send-tries 0))
          (-> (p/delay 1000)
              (p/then (fn [_] (log/info "send-msg not ready.Starting a ping process.")))
              (p/then (fn [_] (swap! send-tries inc)))
              (p/then (fn [_] (send-msg msg-obj)))))
        (throw (ex-info "Couldn't send message; no channel." {:msg-obj msg-obj}))))))

(def ping-id (atom 0))
(defn ping!
  "Ping the server to keep the socket alive."
  []
  (log/info "Ping!")
  (send-msg {:dispatch-key :ping, :client-id @client-id :ping-id (swap! ping-id inc)}))
