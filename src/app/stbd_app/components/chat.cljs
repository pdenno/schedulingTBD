(ns stbd-app.components.chat
   "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   ;;[applied-science.js-interop :as j]
   [clojure.spec.alpha :as s]
   [helix.core                 :refer [defnc $]]
   [helix.hooks                :as hooks]
   ["@chatscope/chat-ui-kit-react/dist/cjs/ChatContainer$default"           :as ChatContainer]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MainContainer$default"           :as MainContainer]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Message$default"                 :as Message]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Message/MessageHeader$default"   :as MessageHeader]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageInput$default"            :as MessageInput]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageList$default"             :as MessageList]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageSeparator$default"        :as MessageSeparator]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Stack$default" :as Stack]
   ;;["@chatscope/chat-ui-kit-react/dist/cjs/TypingIndicator$default"         :as TypingIndicator]
   [scheduling-tbd.util :refer [now]]
   [stbd-app.components.share :as share :refer [ShareUpDown]]
   [stbd-app.util       :refer [register-dispatch-fn]]
   [stbd-app.ws         :as ws]
   [taoensso.timbre     :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

;;; ----------------------------- formatting ChatScope messages --------------------------------------
(def today "A string like 'Sat May 04 2024'" (-> (js/Date. (.now js/Date)) str (subs 0 15)))

(defn inst2date "A string like 'Sat May 04 2024 12:33:14'" [inst] (-> (js/Date. inst) str (subs 0 25)))

(defn dyn-msg-date
  "Return a string 'just now', '2 minutes ago' of if it was yesterday a string '[Day Mon dd year]'."
  [instant]
  (reset! diag instant)
  (let [msg-num (inst-ms instant)
        now-num (now)
        diff (- now-num msg-num)]
    (cond (< 60000  diff) "just now"
          (> 60000  diff 120000)   "1 minute ago"
          (> 120001 diff 3600000)  (str (quot diff 60000) " minutes ago")
          :else (-> (inst2date instant) (subs 0 15)))))

(defn prepend-surrogate-expert
  "If the message is from surrogate expert, prepend the role."
  [txt role]
  (if (= role :surrogate)
    (str "<b>Surrogate Expert</b><br/> " txt)
    txt))

(def key-atm (atom 0))
(defn new-key [] (swap! key-atm inc) (str "msg-" @key-atm))

(defn msgs2cs ; cs = ChatScope, https://chatscope.io/
  "Create ChatScope structures (Message, MessageHeader, MessageSeparator, etc.) for a collection of messages."
  [msgs]
  (let [new-date (atom today)]
    (reduce (fn [r msg]
              (let [{:message/keys [content from time] :or {time (now)}} msg
                    content (prepend-surrogate-expert content from)
                    msg-date (-> time inst2date (subs 0 15))]
                (as-> r ?r
                  (if (= @new-date msg-date)
                    ?r
                    (do (reset! new-date msg-date)
                        (conj ?r ($ MessageSeparator {:key (new-key)} msg-date))))
                  (conj ?r ($ Message
                              {:key (new-key)
                               :model #js {:position "single" ; "single" "normal", "first" and "last"
                                           :direction (if (= :system from) "incoming" "outgoing")
                                           :type "html"
                                           :payload content}}
                              ;; ToDo Use dyn-msg-date here (currently broken).
                              ($ MessageHeader {:sender (str "TBD, " (inst2date time))})))))) ;  They only appear for TBD, which is probably good!
            []
            msgs)))

;;; ========================= Component ===============================
(defn make-resize-fns
  "These are used by ShareUpDown. The argument is a Hook state variable set- function."
  [set-height-fn]
  {:on-resize-up (fn [_parent _width height] (when height (set-height-fn height)))})

(defnc Chat [{:keys [chat-height conv-map proj-info]}]
  (let [[msg-list set-msg-list]         (hooks/use-state (-> conv-map :conv msgs2cs))
        [user-text     set-user-text]   (hooks/use-state "")                         ; Something the user said, plain text.
        [sur-text      set-sur-text]    (hooks/use-state "")                         ; Something said by a surrogate, different path of execution than user-text.
        [tbd-text set-tbd-text]         (hooks/use-state "")                         ; Something the system said, a dispatch-obj with :msg.
        [box-height set-box-height]     (hooks/use-state (int (/ chat-height 2.0)))
        resize-fns (make-resize-fns set-box-height)]
    ;; ------------- Talk through web socket, initiated below.
    (hooks/use-effect :once ; These are used outside the component scope.
      (register-dispatch-fn :set-tbd-text set-tbd-text)
      (register-dispatch-fn :set-sur-text set-sur-text))
    (hooks/use-effect [conv-map] ; Put the entire conversation into the chat.
      (set-msg-list (-> conv-map :conv msgs2cs)))
    (hooks/use-effect [tbd-text]  ; Put TBD's (server's) message into the chat.
      (when (not-empty tbd-text)
        (let [new-msg (msgs2cs [{:message/content tbd-text :message/from :system}])]
          (set-msg-list (into msg-list new-msg)))))
    (hooks/use-effect [sur-text]  ; Put TBD's (server's) message into the chat.
      (when (not-empty sur-text)
        (let [new-msg (msgs2cs [{:message/content sur-text :message/from :surrogate}])]
          (set-msg-list (into msg-list new-msg)))))
    (hooks/use-effect [user-text] ; Entered by user with send button. Send user-text to the server, and put it in the chat.
       (when (not-empty user-text)
         (log/info "In user-text hook: user-text =" user-text)
         (let [[ask-llm? question] (re-matches #"\s*LLM:(.*)" user-text)
               [surrogate? product] (re-matches #"\s*SUR:(.*)" user-text)
               [sur-follow-up? q] (re-matches #"\s*SUR\?:(.*)" user-text)
               msg (cond  ask-llm?       {:dispatch-key :ask-llm :question question}
                          surrogate?     {:dispatch-key :start-surrogate :product product}
                          sur-follow-up? {:dispatch-key :surrogate-follow-up :pid (:project/id proj-info) :question q}
                          :else          {:dispatch-key :domain-expert-says :msg-text user-text :promise-keys @ws/pending-promise-keys})]
           ;; ToDo: Human-injected questions, though some of them are stored, don't store the human-injected annotation.
           ;;       In fixing this, keep the annotation separate from the question because if a surrogate sees it, it will be confused.
           (set-msg-list (into msg-list (msgs2cs [{:message/content (str "<b>[Human-injected question]</b><br/>"(or question q))
                                                   :message/from :system}])))
           (log/info "Before ws/send-msg: msg =" msg)
           (ws/send-msg msg))))
    ;; ----------------- component UI structure.
    ($ ShareUpDown
       {:init-height chat-height
        :share-fns resize-fns
        :up ($ Box {:sx ; This work!
                    #js {:overflowY "auto"
                         :display "flex"    ; So that child can be 100% of height. See https://www.geeksforgeeks.org/how-to-make-flexbox-children-100-height-of-their-parent-using-css/
                         :height box-height ; When set small enough, scroll bars appear.
                         :flexDirection "column"
                         :bgcolor "#f0e699"}} ; "#f0e699" is the yellow color used in MessageList. (see style in home.html).
               ;; https://github.com/chatscope/use-chat-example/blob/main/src/components/Chat.tsx (See expecially :onChange and :onSend.)
               ($ MainContainer
                  ($ ChatContainer
                     ($ MessageList
                        {#_#_:typingIndicator ($ TypingIndicator "TBD is typing") ; ToDo: insert this when it is useful.
                         :style #js {:height "500px"}}
                        msg-list))))
        :dn ($ Stack {:direction "row" :spacing "0px"}
               ($ MessageInput {:placeholder "Type message here...."
                                :onSend #(do (log/info "onSend:" %)
                                             (set-user-text %))
                                :fancyScroll false
                                ;;:autoFocus false ; ToDo: Needs investigation. I don't know what it does.
                                ;; It sets height to whatever you'd like with px, but doesn't expand scroll bars. It doesn't respond to :height <percent> either.
                                :style #js {#_#_:height "200px" :width "90%"}}))})))
