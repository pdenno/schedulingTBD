(ns stbd-app.components.chat
   "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [helix.core                 :refer [defnc $]]
   [helix.hooks                :as hooks]
   ["@chatscope/chat-ui-kit-react/dist/cjs/ChatContainer$default"           :as ChatContainer]
   ["@chatscope/chat-ui-kit-react/dist/cjs/ConversationList$default"        :as ConversationList]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Conversation$default"            :as Conversation]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MainContainer$default"           :as MainContainer]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Message$default"                 :as Message]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Message/MessageHeader$default"   :as MessageHeader]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageInput$default"            :as MessageInput]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageList$default"             :as MessageList]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageSeparator$default"        :as MessageSeparator]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Sidebar$default"                 :as Sidebar]
   ["@chatscope/chat-ui-kit-react/dist/cjs/TypingIndicator$default"         :as TypingIndicator]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/ButtonGroup$default" :as ButtonGroup]
   ["@mui/material/Stack$default" :as Stack]
   [promesa.core    :as p]
   [scheduling-tbd.util :refer [now]]
   [stbd-app.components.attachment-modal :as attach :refer [AttachmentModal]]
   [stbd-app.components.share :as share :refer [ShareUpDown]]
   [stbd-app.db-access  :as dba]
   [stbd-app.util       :as util :refer [register-fn lookup-fn common-info update-common-info!]]
   [stbd-app.ws         :as ws]
   [taoensso.timbre     :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

;;; ----------------------------- formatting ChatScope messages --------------------------------------
(def today "A string like 'Sat May 04 2024'" (-> (js/Date. (.now js/Date)) str (subs 0 15)))
(def start-of-day-millis (-> today js/Date.parse))

(defn inst2date "A string like 'Sat May 04 2024 12:33:14'" [inst] (-> (js/Date. inst) str (subs 0 25)))

(defn dyn-msg-date
  "Return a string 'just now', '2 minutes ago' etc. for the argument Instant relative to now."
  [instant]
  (let [msg-epoch-millis (inst-ms instant)
        now-epoch-millis (now)
        diff (- now-epoch-millis msg-epoch-millis)]
    (cond (< diff 60000) "just now"
          (<= 60000  diff 120000)   "1 minute ago"
          (<= 120001 diff 3600000)  (str (quot diff 60000) " minutes ago")
          (<= 3600000 diff 7200000) "1 hour ago"
          (< start-of-day-millis msg-epoch-millis) (str (quot diff 3600000) " hours ago")
          :else (-> (inst2date instant) (subs 0 15)))))

(defn msg-with-title
  [content from]
  (cond (= from :surrogate)            (str "<b>Surrogate Expert</b><br/>" content)
        (= from :developer-injected)   (str "<b>Developer Injected Question</b><br/>" content)
        :else                          content))

(def key-atm (atom 0))
(defn new-key [] (swap! key-atm inc) (str "msg-" @key-atm))

(defn msgs2cs ; cs = ChatScope, https://chatscope.io/
  "Create ChatScope structures (Message, MessageHeader, MessageSeparator, etc.) for a collection of messages."
  [msgs]
  (let [new-date (atom today)]
    (reduce (fn [r msg]
              (let [{:message/keys [content from time] :or {time (js/Date. (now))}} msg
                    content (msg-with-title content from)
                    msg-date (-> time inst2date (subs 0 15))]
                (as-> r ?r
                  (if (= @new-date msg-date)
                    ?r
                    (do (reset! new-date msg-date)
                        (conj ?r ($ MessageSeparator {:key (new-key)} msg-date))))
                  (conj ?r ($ Message
                              {:key (new-key)
                               :model #js {:position "single" ; "single" "normal", "first" and "last"
                                           :direction (if (#{:system :developer-injected} from) "incoming" "outgoing") ; From perspective of user.
                                           :type "html"
                                           :payload content}}
                              ($ MessageHeader {:sender (str "Interviewer, " (dyn-msg-date time))})))))) ;  They only appear for Interviewer, which is probably good!
            []
            msgs)))

;;; ========================= Component ===============================
(defn make-resize-fns
  "These are used by ShareUpDown. The argument is a Hook state variable set- function."
  [set-height-fn]
  {:on-resize-up (fn [_parent _width height] (when height (set-height-fn height)))})

(def msgs-atm "This has to be out here for :get-msg-list to work!" (atom nil))
(def update-msg-dates-process "A process run by js/window.setInterval" (atom nil))

(defn update-msg-list
  "Update the dates on the msg list."
  []
  (log/info "update-msg-list")
  ((lookup-fn :set-cs-msg-list) ((lookup-fn :get-msg-list))))

;;; This is called by project.cljs, core.cljs/top, and below. It is only in chat below that it would specify conv-id.
;;; In the other cases, it takes whatever the DB says is current.
(defn get-conversation
  ([pid] (get-conversation pid nil))
  ([pid conv-id]
   (-> (dba/get-conversation-http pid conv-id)
       (p/then (fn [{:keys [conv conv-id]}]
                 (log/info "chat/get-conversation (return from promise): conv-id =" conv-id "count =" (count conv))
                 (let [conv (if (empty? conv)
                              [#:message{:content "No discussion here yet.", :from :system, :time (js/Date. (.now js/Date))}]
                              conv)]
                   (reset! msgs-atm conv)
                   ((lookup-fn :set-cs-msg-list) conv)
                   ((lookup-fn :set-active-conv) conv-id)
                   (ws/send-msg {:dispatch-key :resume-conversation-plan :pid pid :conv-id conv-id})
                   (update-common-info! {:project/id pid :conv-id conv-id}))))
       (p/catch (fn [e]
                  (log/info "get-conversation failed:" e))))))

(register-fn :get-conversation get-conversation)

(defnc Chat [{:keys [chat-height proj-info]}]
  (let [[msg-list set-msg-list]         (hooks/use-state [])
        [box-height set-box-height]     (hooks/use-state (int (/ chat-height 2.0)))
        [cs-msg-list set-cs-msg-list]   (hooks/use-state nil)
        [active-conv set-active-conv]   (hooks/use-state nil) ; active-conv is a keyword
        [busy? set-busy?]               (hooks/use-state nil)
        resize-fns (make-resize-fns set-box-height)]
    (letfn [(change-conversation-click [to]
              (when-not busy?
                (if-let [pid (:project/id @common-info)]
                  (get-conversation pid to)
                  (log/info "change-conversation-click fails: common-info =" @common-info))))
            (process-user-input [text]
              (when (not-empty text)
                (let [[ask-llm? question]  (re-matches #"\s*LLM:(.*)" text)
                      [surrogate? product] (re-matches #"\s*SUR:(.*)" text)
                      [sur-follow-up? q]   (re-matches #"\s*SUR\?:(.*)" text)
                      msg (cond  ask-llm?       {:dispatch-key :ask-llm :question question}
                                 surrogate?     {:dispatch-key :start-surrogate :product product}
                                 sur-follow-up? {:dispatch-key :surrogate-follow-up :pid (:project/id proj-info) :question q}
                                 :else          {:dispatch-key :domain-expert-says :msg-text text :promise-keys @ws/pending-promise-keys})]
                  ;; ToDo: Human-injected questions, though some of them are stored, don't store the human-injected annotation.
                  ;;       In fixing this, keep the annotation separate from the question because if a surrogate sees it, it will be confused.
                  (set-msg-list (conj msg-list {:message/content (str "<b>[Human-injected question]</b><br/>"(or question q))
                                                :message/from :system}))
                  (ws/send-msg msg))))]
      ;; ------------- Talk through web socket, initiated below.
      (hooks/use-effect :once ; These are used outside the component scope.
        (register-fn :set-tbd-text (fn [text] (set-msg-list (conj @msgs-atm {:message/content text :message/from :system}))))
        (register-fn :set-sur-text (fn [text] (set-msg-list (conj @msgs-atm {:message/content text :message/from :surrogate}))))
        (register-fn :set-active-conv set-active-conv)
        (register-fn :set-busy? set-busy?)
        (register-fn :get-msg-list  (fn [] @msgs-atm))                               ; These two used to update message time.
        (register-fn :set-cs-msg-list (fn [msgs] (set-cs-msg-list (msgs2cs msgs))))  ; These two used to update message time.
        (reset! update-msg-dates-process (js/window.setInterval (fn [] (update-msg-list)) 60000)))
      (hooks/use-effect [msg-list]
        (reset! msgs-atm msg-list)
        (set-cs-msg-list (msgs2cs msg-list)))
      ;; ----------------- component UI structure.
      ($ ShareUpDown
         {:init-height chat-height
          :share-fns resize-fns
          :up ($ Box {:sx ; This work!
                      #js {:overflowY "auto"  ; Creates a scroll bar
                           :display "flex"    ; So that child can be 100% of height. See https://www.geeksforgeeks.org/how-to-make-flexbox-children-100-height-of-their-parent-using-css/
                           :height box-height ; When set small enough, scroll bars appear.
                           :flexDirection "column"
                           :bgcolor "#f0e699"}} ; "#f0e699" is the yellow color used in MessageList. (see style in home.html).
                 ;; https://github.com/chatscope/use-chat-example/blob/main/src/components/Chat.tsx (See expecially :onChange and :onSend.)
                 ($ MainContainer
                    ($ Sidebar {:position "left" :sx #js {:maxWidth "100px"}}
                       ($ ConversationList
                          ($ Conversation {:name "Process"
                                           :active (= active-conv :process)
                                           :onClick (fn [_] (change-conversation-click :process))})
                          ($ Conversation {:name "Data"
                                           :active (= active-conv :data)
                                           :onClick (fn [_] (change-conversation-click :data))})
                          ($ Conversation {:name "Resources"
                                           :active (= active-conv :resource)
                                           :onClick (fn [_] (change-conversation-click :resource))})))
                    ($ ChatContainer
                       ($ MessageList
                          {:typingIndicator (when busy? ($ TypingIndicator {:content "Interviewer is typing"}))
                           :style #js {:height "500px"}}
                          cs-msg-list))))
          :dn ($ Box {:sx #js {:width "95%"}} ; This fixes a sizing bug!
                 ($ Stack {:direction "row" :spacing "0px"}
                    ($ ButtonGroup
                       ($ AttachmentModal {:post-attach-fn #(log/info "attach-fn: args =" %)}))
                    ($ MessageInput {:placeholder "Type message here...."
                                     :onSend #(do (log/info "onSend:" %)
                                                  (process-user-input %))
                                     :attachButton false
                                     :fancyScroll false
                                     ;; It sets height to whatever you'd like with px, but doesn't expand scroll bars. It doesn't respond to :height <percent> either.
                                     :style #js {#_#_:height "200px" :width "90%"}})))}))))
