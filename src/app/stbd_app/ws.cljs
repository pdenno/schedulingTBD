(ns stbd-app.ws
  "Implement websocket, except for reading, which happens in the chat component, owing to React hooks."
  (:require
   [clojure.edn     :as edn]
   [promesa.core    :as p]
   [stbd-app.util   :as util :refer [client-id]]
   [taoensso.timbre :as log  :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))
(def ws-url (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))
(def channel (atom nil))
(def connected? (atom false))

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
;;; :tbd-says isn't in this because it sets the set-system-text hook.
(defn dispatch-msg
  "Call a function depending on the value of :dispatch-key in the message."
  [{:keys [dispatch-key promise-keys new-proj-map] :as _msg} change-proj-fn]
  (case dispatch-key
    :clear-promise-keys (clear-promise-keys! promise-keys)
    :alive?             (send-msg {:dispatch-key :alive? :alive? true})
    :reload-proj        (change-proj-fn new-proj-map)
    :ping-confirm       :ok
    "default"))

(defn connect! [change-proj-fn set-tbd-obj-fn] ; 2024-03-19: This really does seem to need to be inside the component!
  (if-let [chan (js/WebSocket. ws-url)]
    (do (log/info "Websocket Connected!")
        (reset! channel chan)
        (reset! connected? true)
        (set! (.-onmessage chan)
              (fn [event]       ;; ToDo: Consider transit rather then edn/read-string.
                (try (let [{:keys [p-key dispatch-key] :as msg-obj} (-> event .-data edn/read-string)]
                       (dispatch-msg msg-obj change-proj-fn)
                       (when (= :tbd-says dispatch-key)
                         (when p-key (remember-promise p-key))
                         (log/info "msg-obj =" msg-obj)
                         (set-tbd-obj-fn msg-obj)))
                     (catch :default e (log/warn "Error in :tbd-says socket reading:" e)))))
        (set! (.-onerror chan) (fn [& arg] (log/error "Error on socket: arg=" arg)))) ; ToDo: investigate why it gets these.
    (throw (ex-info "Websocket Connection Failed:" {:url ws-url}))))


(def ping-id (atom 0))
(defn ping!
  "Ping the server to keep the socket alive."
  []
  (log/info "Ping!")
  (if-let [chan @channel]
    (if (= 1 (.-readyState chan)) ; 0=connecting, 1=open, 2=closing, 3=closed.
      (.send chan (str {:dispatch-key :ping,
                        :client-id client-id
                        :ping-id (swap! ping-id inc)}))
      (log/warn "Channel not ready for ping. state =" (.-readyState chan)))
    (log/error "Couldn't send ping; channel isn't open.")))

(def msg-type?
  #{:alive?                 ; Server is asking whether you are alive
    :ask-llm                ; User asked a "LLM:..." question at the chat prompt.
    :close-channel          ; Close the ws. (Typically, client is ending.)
    :ping                   ; Ping server.
    :resume-conversation    ; Restart the planner (works for :START-A-NEW-PROJECT too).
    :start-surrogate        ; User wrote "SUR: <some product type> at the chat prompt, something like :resume-conversation.
    :user-says})            ; User wrote at the chat prompt (typically answering a question).

;;; ToDo: I'd like to do a connect! after a few tries, but currently connect needs some functions from chat!
(def send-tries (atom 0))
(defn send-msg
  "Send the message to the server. msg can be any Clojure object but if it is a map we add the :client-id.
   Example usage: (ws-send-msg! {:dispatch-key :ping})"
  [{:keys [dispatch-key] :as msg-obj}]
  (if-not (msg-type? dispatch-key)
    (log/error "Invalid message type in" msg-obj)
    (let [msg-obj (assoc msg-obj :client-id client-id)]
      ;; readystate:  0=connecting, 1=open, 2=closing, 3=closed.
      (if-let [chan @channel]
        (if (= 1 (.-readyState chan))
          (do (.send chan (str msg-obj))
              (log/info "send-msg: SENT msg-obj =" msg-obj)
              (reset! send-tries 0))
          (-> (p/delay 1000) ; This was intended to solve a not-ready problem, but screws things up.
              (p/then (fn [_] (log/info "send-msg not ready.Starting a ping process.")))
              (p/then (fn [_] (swap! send-tries inc)))
              (p/then (fn [_] (send-msg msg-obj)))))
        (throw (ex-info "Couldn't send message; no channel." {:msg-obj msg-obj}))))))
