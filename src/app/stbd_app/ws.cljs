(ns stbd-app.ws
  "Implement websocket, except for reading, which happens in the chat component, owing to React hooks."
  (:require
   [clojure.edn       :as edn]
   [promesa.core      :as p]
   [stbd-app.util     :as util :refer [dispatch-table lookup-fn register-fn update-common-info!]]
   [taoensso.telemere :refer [log!]]))

(def ^:diag diag (atom nil))
(def client-id "UUID for this instance of the app. Doesn't change except when re-connect!-ing." (str (random-uuid)))
(def channel "The JS Websocket object being used for communication with the server." (atom nil))
(def reconnecting? "True only from the time reconnect! is called until socket is once again ready." (atom false))
(def check-process "A process that is run every second once problems are encountered to check socket readiness." (atom nil))
(def check-count "After a certain number of check for readiness, if still not ready, we try connect! again." (atom 0))
(def ping-process "A process run intermittently by js/window.setInterval" (atom nil))

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
   interpreting the :domain-expert-says response which repeat this promise-key as answer to the :iviewr-says ws message.
   This function just adds to the list, which in most cases will be empty when the argument key is added here."
  [k]  (when k (swap! pending-promise-keys conj k)))

(defn clear-promise-keys! [ks]
  (log! :info (str "Clear promise keys: " ks))
  (doseq [k ks] (when k (swap! pending-promise-keys disj k))))

(declare send-msg)
(defn recv-msg-type? [k] (contains? @dispatch-table k))

;;; These are some of the functions registered. Others are chat.cljs, core.cljs, db_access.cljs, graph.cljs and maybe other places.
;;; The ones here correspond to dispatch keys, but other might be used to break-out React hooks or avoid namespace cycles.
(register-fn :clear-promise-keys    (fn [obj] (-> obj :promise-keys clear-promise-keys!)))

(register-fn :alive?                (fn [_] (send-msg {:dispatch-key :alive-confirm})))

(register-fn :ping-confirm          (fn [_] :ok))

;;; The server uses this one after the client sends it :start-surrogate (which creates the surrogate's DB).
(register-fn :load-proj             (fn [{:keys [new-proj-map]}] ; New projects start on :process
                                      (update-common-info! (assoc new-proj-map :cid :process))
                                      (when-let [f (lookup-fn :set-current-project)] (f new-proj-map))
                                      (when-let [f (lookup-fn :get-conversation)]    (f :project/id new-proj-map))))

(register-fn :domain-expert-submits-table (fn [table]
                                            (send-msg {:dispatch-key :domain-expert-says
                                                       :table-string (str table)})))

;;; GraphPane/:set-graph-cmd might not be set, in which case we go through core/set-graph. Ditto for load-table.
(register-fn :load-graph
             (fn [{:keys [graph]}]
               ;;(log! :info (str "ws: Call to :load-graph: graph = " graph))
               (when-let [f (lookup-fn :set-rhs-pane)] (f :graph))
               (when-let [f (or (lookup-fn :set-graph-pane) (lookup-fn :set-graph))]
                 (f graph))))

(register-fn :load-table
             (fn [{:keys [table]}]
               ;;(log! :info (str "ws: Call to :load-table: table = " table))
               (when-let [f (lookup-fn :set-rhs-pane)] (f :table))
               (when-let [f (or (lookup-fn :set-table-pane) (lookup-fn :set-table))]
                 (f table))))

(defn dispatch-msg
  "Call a function depending on the value of :dispatch-key in the message."
  [{:keys [dispatch-key] :as msg}]
  (if-not (recv-msg-type? dispatch-key)
    (log! :error (str "Invalid message type from server: " msg))
    ((lookup-fn dispatch-key) msg)))

(def ping-id (atom 0))
(defn ping!
  "Ping the server to keep the socket alive."
  []
  (when-not @reconnecting?
    (send-msg {:dispatch-key :ping, :ping-id (swap! ping-id inc)})))

(def error-info (atom nil))
(def error-info-2 (atom nil))

(defn connect! []
  ;; 2024-05-31: I was getting
  ;; *ERROR [scheduling-tbd.web.websockets:287] - Could not find out async channel for client [uuid]*
  ;; in long-running interactions. I don't think I had good rationale for resetting client-id!
  ;;(reset! client-id (str (random-uuid))) ; ToDo: Use bare random-uuid if/when we start using transit.
  (reset-state)
  (if-let [chan (js/WebSocket. (ws-url client-id))]
    (do (log! :info (str "Websocket Connected! " client-id))
        (reset! channel chan)
        (reset! reconnecting? false)
        (set! (.-onmessage chan)
              (fn [event]       ;; ToDo: Consider transit rather then edn/read-string.
                (try (let [data (.-data event)]
                       (when (not-empty data) (-> data edn/read-string dispatch-msg)))
                     (catch :default e
                       (reset! error-info {:event event :error e :data (.-data event)})
                       (log! :warn (str "Error reading from websocket: " e))))))
        (set! (.-onerror chan)
              (fn [& arg]
                (reset! error-info-2 {:event arg})
                (log! :error (str "Error on socket: arg = " arg)))) ; ToDo: investigate why it gets these.
        ;; Ping to keep-alive the web-socket. 10 sec is not enough; 3 is too little???
        (reset! ping-process (js/window.setInterval (fn [] (ping!)) 30000 #_2000)))
    (throw (ex-info "Websocket Connection Failed:" {:client-id client-id}))))

(defn check-channel
  "This gets called by send-msg when (channel-ready?) is not true."
  [prom]
  (log! :info (str "Check-channel: state = " (.-readyState @channel)))
  (if (channel-ready?)
    (do (log! :debug "Channel is READY!")
        (js/window.clearInterval @check-process)
        (reset! reconnecting? false)
        (reset! check-count 0)
        (p/resolve! prom))
    (do (swap! check-count inc)
        (when (> @check-count 3) ; Wait +10+ 3 seconds before asking for a new connection.
          (reset! check-count 0)
          (connect!)))))

(defn reconnect!
  "Wait Try to connect! and when successful, deliver on the promise."
  [prom]
  (log! :debug "Call to reconnect!")
  (when-not @reconnecting?
    (log! :debug "Starting interval functions.")
    (when-let [proc @check-process] (js/window.clearInterval proc))
    (reset! check-process (js/window.setInterval (fn [] (check-channel prom)) 1000))
    (reset! reconnecting? true)))

(def send-msg-type?
  #{:alive-confirm             ; Like a ping but initiated from server, and only when it seems things have inadvertently disconnected. ToDo: Remove it? Not implemented.
    :ask-llm                   ; User asked a "LLM:..." question at the chat prompt.
    :start-surrogate           ; User wrote "SUR: <some product type> at the chat prompt, something like :resume-conversation
    :surrogate-follow-up       ; User wrote "SUR?" <some question about dialog to date>
    :start-conversation        ; Start a conversation. All that is needed is the client-id, I think.
    :resume-conversation       ; Restart the planner. Done when client recieves a :load-proj, and when switches conversation.
    :close-channel             ; Close the ws. (Typically, client is ending.) ToDo: Only on dev recompile currently.
    :ping                      ; Ping server.
    :domain-expert-says        ; Human user wrote at the chat prompt (typically answering a question).
    :interviewer-busy?         ; Enable/disable various UI features depending on whether interviewer is busy.
    :run-long                  ; diagnostic
    :user-returns-table        ; User submitted table data
    :throw-it})                ; diagnostic

(defn send-msg
  "Add client-id and send the message to the server over the websocket.
   Example usage: (send-msg {:dispatch-key :ping})"
  [{:keys [dispatch-key] :as msg-obj}]
  (if-not (send-msg-type? dispatch-key)
    (log! :error (str "Invalid message type in " msg-obj))
    (let [msg-obj (assoc msg-obj :client-id client-id)]
      (if @channel
        (if (channel-ready?)
          (do (.send @channel (str msg-obj))
              (when-not (or (= dispatch-key :ping)
                            (= dispatch-key :alive-confirm))
                (log! :info (str "send-msg: SENT msg-obj = " msg-obj))))
          (let [prom (p/deferred)]
            (reconnect! prom)
            (-> prom
                (p/then (fn [_] (log! :info (str "After wait, state is " (.-readyState @channel)))))
                (p/then (fn [_] (.send @channel (str msg-obj)))))))
        (throw (ex-info "Couldn't send message; no channel." {:msg-obj msg-obj}))))))
