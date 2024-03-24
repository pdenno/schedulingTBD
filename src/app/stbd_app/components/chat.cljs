(ns stbd-app.components.chat
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [applied-science.js-interop :as j]
   [clojure.edn                :as edn]
   [helix.core                 :refer [defnc $]]
   [helix.hooks                :as hooks]
   ["@mui/icons-material/Send$default" :as Send]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/material/LinearProgress$default" :as LinearProgress]
   ["@mui/material/Link$default" :as Link]
   ["@mui/material/Stack$default" :as Stack]
   ["react-chat-elements/dist/main"    :as rce]
   [stbd-app.components.share :as share :refer [ShareUpDown]]
   [stbd-app.ws         :as ws]
   [scheduling-tbd.util :as sutil :refer [timeout-info #_invalidate-timeout-info]]
   [taoensso.timbre     :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

;;; -------------------- This stuff maybe for later (its from RADmapper) ---------------------------
(def progress-handle
  "The thing that can be called by js/window.clearInterval to stop incrementing progress under js/window.setInterval."
  (atom nil))

(def progress-atm "Percent allowed duration for eval-cell. 100% is a timeout." (atom 0))

(defn compute-progress
  "Use either progress-atm or timeout-info to return a percent done."
  []
  (let [now (.getTime (js/Date.))
        info @timeout-info
        timeout-at (:timeout-at info)
        res (if (:valid? info)
              (if (> now timeout-at)
                100
                (int (* 100.0  (- 1.0 (double (/ (- timeout-at now) (:max-millis info)))))))
              (+ @progress-atm 2))]
    res))

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

;;; :tbd-says isn't in this because it sets the set-system-text hook.
(defn dispatch-msg
  "Call a function depending on the value of :dispatch-key in the message."
  [{:keys [dispatch-key promise-keys] :as _msg}]
  (cond (= dispatch-key :clear-promise-keys) (clear-promise-keys! promise-keys)
        (= dispatch-key :alive?)             (ws/send-msg {:dispatch-key :alive? :alive? true})))

;;; ------------------------- active stuff ------------------------------------
(def item-keys "Atom for a unique :key of some UI object." (atom 0))

(defn make-link
  "Create a MUI link. (Works for react-chat-elements too.)
   Example usage: (make-link 'https://en.wikipedia.org/wiki/%22Hello,_World!%22_program' 'Hello, world!'})"
  [href text]
  ($ Link {:key (swap! item-keys inc)
           :href href}
     text))

(def msg-style {:system {:name "TBD" :color "Red"   :position "left"}
                :user   {:name "You" :color "Green" :position "right"}})

;;; ToDo: Remove this and it usages?
(def msg-index "This might be a waste of time. WS won't' print the same message twice in a row."  (atom 0))


(defn msg-vec2rce
  "Rewrite the conversation DB-style messages to objects acceptable to the RCE component.
   Does not do clj->js on it, however."
  [msg-vec msg-owner]
  (let [{:keys [name color position]} (get msg-style msg-owner)]
    (-> {:type "text"}
        (assoc :id (swap! msg-index inc))
        (assoc :title name)
        (assoc :titleColor color)
        (assoc :position position)
        (assoc :text (reduce (fn [res elem]
                               (if (contains? elem :msg-link/uri)
                                 (conj res (make-link (:msg-link/uri elem) (:msg-link/text elem)))
                                 (conj res (:msg-text/string elem))))
                             []
                             msg-vec)))))

(defn add-msg [msg-list msg]
  (-> msg-list js->clj (conj msg) clj->js))

;;; ========================= Component ===============================
(defn make-resize-fns
  "These are used by ShareUpDown. The argument is a Hook state variable set- function."
  [set-height-fn]
  {:on-resize-up (fn [_parent _width height] (when height (set-height-fn height)))})

(defnc Chat [{:keys [chat-height conv-map]}]
  (let [[msg-list set-msg-list]         (hooks/use-state (->> conv-map :conv (mapv #(msg-vec2rce (:message/content %) (:message/from %))) clj->js))
        [progress set-progress]         (hooks/use-state 0)
        [progressing? _set-progressing] (hooks/use-state false)
        [user-text     set-user-text]   (hooks/use-state "")                         ; Something the user said, plain text.
        [tbd-obj set-tbd-obj]           (hooks/use-state "")                         ; Something the system said, a dispatch-obj with :msg-vec.
        [box-height set-box-height]     (hooks/use-state (int (/ chat-height 2.0)))
        input-ref                       (hooks/use-ref nil)
        resize-fns (make-resize-fns set-box-height)]
    ;; ------------- talk through web socket; server-initiated.
    (letfn [(connect! [] ; 2024-03-19: This really does seem to need to be inside the component!
              (if-let [chan (js/WebSocket. ws/ws-url)]
                (do (log/info "Websocket Connected!")
                    (reset! ws/channel chan)
                    (reset! ws/connected? true)
                    (set! (.-onmessage chan)
                          (fn [event]       ;; ToDo: Consider transit rather then edn/read-string.
                            (try (let [{:keys [p-key dispatch-key] :as msg-obj} (-> event .-data edn/read-string)]
                                   (dispatch-msg msg-obj) ; Just for :clear-promise-keys messages currently.
                                   (when (= :tbd-says dispatch-key)
                                     (when p-key (remember-promise p-key))
                                     (log/info "msg-obj =" msg-obj)
                                     (set-tbd-obj msg-obj)))
                                 (catch :default e (log/warn "Error in :tbd-says socket reading:" e)))))
                    (set! (.-onerror chan) (fn [& arg] (log/error "Error on socket: arg=" arg)))) ; ToDo: investigate why it gets these.
                (throw (ex-info "Websocket Connection Failed:" {:url ws/ws-url}))))]
    (hooks/use-effect :once ; Start the web socket.
      (when-not @ws/connected? (connect!)))
    (hooks/use-effect [conv-map] ; Put the entire conversation into the chat.
      (set-msg-list (->> conv-map :conv (mapv #(msg-vec2rce (:message/content %) (:message/from %))) clj->js)))
    (hooks/use-effect [tbd-obj]  ; Put TBD's (server's) message into the chat.
      (when (not-empty tbd-obj)
        (let [new-msg (-> tbd-obj :msg-vec (msg-vec2rce :system) clj->js)]
          (set-msg-list (add-msg msg-list new-msg)))))
    (hooks/use-effect [user-text] ; Entered by user with arrow button. Send user-text to the server, and put it in the chat.
       (when (not-empty user-text)
         (log/info "In user-text hook: user-text =" user-text)
         (let [[ask-llm? question] (re-matches #"\s*LLM:(.*)" user-text)
               [surrogate? surrogate-role] (re-matches #"\s*SUR:(.*)" user-text)
               msg (cond  ask-llm?      {:dispatch-key :ask-llm :question question}
                          surrogate?    {:dispatch-key :surrogate-call :role surrogate-role}
                          :else         {:dispatch-key :user-says :msg-text user-text :promise-keys @pending-promise-keys})]
           (log/info "Before ws/send-msg: msg =" msg)
           (ws/send-msg msg))))
      ;; -------------- progress stuff (currentl not hooked up)
    (hooks/use-effect [progressing?] ; This shows a progress bar while waiting for server response.
       (reset! progress-atm 0)
       (reset! progress-handle
               (js/window.setInterval
                (fn []
                  (let [percent (compute-progress)]
                    (if (or (>= progress 100) (not progressing?))
                      (do (set-progress 0) (js/window.clearInterval @progress-handle))
                      (set-progress (reset! progress-atm percent)))))
                200)))
      ;; ----------------- component UI structure.
      ($ ShareUpDown
         {:init-height chat-height
          :share-fns resize-fns
          :up
          ($ Box {:sx ; This work!
                  #js {:overflowY "auto"
                       :display "flex"    ; So that child can be 100% of height. See https://www.geeksforgeeks.org/how-to-make-flexbox-children-100-height-of-their-parent-using-css/
                       :height box-height ; When set small enough, scroll bars appear.
                       :flexDirection "column"
                       :bgcolor "#f0e699"}} ; "#f0e699" is the yellow color used in MessageList. (see style in home.html).
             ($ rce/MessageList {:dataSource msg-list
                                 ; :lockable true ; Does nothing.
                                 :toBottomHeight "100%" ; https://detaysoft.github.io/docs-react-chat-elements/docs/messagelist I'd like it to scroll to the bottom.
                                 :style #js {:alignItems "stretch" ; :style is helix for non-MUI things. I think(!)
                                             :display "flex"}}))   ; "stretch" is just for horizontal??? ToDo: Everything here ignored?
          :dn
          ($ Stack {:direction "column"}
                 ($ LinearProgress {:variant "determinate" :value progress})
                 ($ Stack {:direction "row" :spacing "0px"}
                    ($ rce/Input {:referance input-ref ; <==== Yes, rilly!
                                  :value user-text
                                  :min-width "800px"
                                  :placeholder "Type here..."
                                  :multiline true})
                    ($ IconButton {:onClick #(when-let [iref (j/get input-ref :current)]
                                               (when-let [text (not-empty (j/get iref :value))]
                                                 (set-msg-list (add-msg msg-list (msg-vec2rce [{:msg-text/string text}] :user)))
                                         (j/assoc! iref :value "")
                                         (set-user-text text)))}
                       ($ Send))))}))))
