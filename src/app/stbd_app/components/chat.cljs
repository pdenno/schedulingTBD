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
   ["@chatscope/chat-ui-kit-react/dist/cjs/Message/MessageCustomContent$default"   :as MessageCustomContent]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Message/MessageHtmlContent$default"   :as MessageHtmlContent]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageInput$default"            :as MessageInput]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageList$default"             :as MessageList]
   ["@chatscope/chat-ui-kit-react/dist/cjs/MessageSeparator$default"        :as MessageSeparator]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Sidebar$default"                 :as Sidebar]
   ["@chatscope/chat-ui-kit-react/dist/cjs/TypingIndicator$default"         :as TypingIndicator]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/material/ButtonGroup$default" :as ButtonGroup]
   ["@mui/material/Stack$default" :as Stack]
   [promesa.core    :as p]
   [stbd-app.components.attachment-modal :refer [AttachmentModal]]
   [stbd-app.components.share :as share :refer [ShareUpDown]]
   [stbd-app.components.graph-modal   :refer [GrapheModal]]
   [stbd-app.components.table-modal   :refer [TableModal]]
   [stbd-app.db-access  :as dba]
   [stbd-app.util       :as util :refer [register-fn lookup-fn common-info update-common-info!]]
   [stbd-app.ws         :as ws :refer [remember-promise]]
   [taoensso.telemere          :as tel :refer-macros [log!]]))

(def ^:diag diag (atom nil))

;;; ----------------------------- formatting ChatScope messages --------------------------------------
;;; BTW: (.now js/Date) is an integer.
;;;      (js/Date. (.now js/Date)) is an #Inst.
(def today "A string like 'Sat May 04 2024'" (-> (js/Date. (.now js/Date)) str (subs 0 15)))
(def start-of-day-millis (-> today js/Date.parse))

(defn inst2date "A string like 'Sat May 04 2024 12:33:14'" [inst] (-> (js/Date. inst) str (subs 0 25)))

(defn dyn-msg-date
  "Return a string 'just now', '2 minutes ago' etc. for the argument Instant relative to now."
  [instant]
  (let [msg-epoch-millis (inst-ms instant)
        now-epoch-millis (.now js/Date)
        diff (- now-epoch-millis msg-epoch-millis)]
    (cond (< diff 60000) "just now"
          (<= 60000  diff 120000)   "1 minute ago"
          (<= 120001 diff 3600000)  (str (quot diff 60000) " minutes ago")
          (<= 3600000 diff 7200000) "1 hour ago"
          (< start-of-day-millis msg-epoch-millis) (str (quot diff 3600000) " hours ago")
          :else (-> (inst2date instant) (subs 0 15)))))

(defn msg-with-title
  [content from]
  (assert (#{:surrogate :system :developer-interjected :human} from))
  (case from
      :surrogate               (str "<b>Surrogate Expert</b><br/>" content)
      :developer-interjected   (str "<b>Developer Interjected Question</b><br/>" content)
      content))

(def key-atm (atom 0))
(defn new-key [] (swap! key-atm inc) (str "msg-" @key-atm))

(defn show-table
  [table]
  (log! :info (str "Show table: " table)))

(defn show-graph
  [graph]
  (log! :info (str "Show graph: " graph)))


(defn msgs2cs ; cs = ChatScope, https://chatscope.io/
  "Create ChatScope structures (Message, MessageHeader, MessageSeparator, etc.) for a collection of messages."
  [msgs]
  (let [new-date (atom today)]
    (reduce (fn [r msg]
              (let [{:message/keys [content from time table graph] :or {time (js/Date. (.now js/Date))}} msg
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
                                           :direction (if (#{:system :developer-interjected} from) "incoming" "outgoing") ; From perspective of user.
                                           :type "custom"}}
                              ($ MessageHeader {:sender (str "Interviewer, " (dyn-msg-date time))}) ;  They only appear for Interviewer, which is probably good!
                              ($ MessageCustomContent {}
                                 ($ MessageHtmlContent {:html content})
                                 (when (or table graph)
                                   ($ ButtonGroup {}
                                      (when table ($ TableModal {:table table}))
                                      (when graph ($ GraphModal {:graph graph}))))))))))
            []
            msgs)))

;;; ========================= Component ===============================
(defn make-resize-fns
  "These are used by ShareUpDown. The argument is a Hook state variable set- function."
  [set-height-fn]
  {:on-resize-up (fn [_parent _width height] (when height (set-height-fn height)))})

(def msgs-atm "A vector of messages in the DB format." (atom nil)) ; ToDo: Revisit keeping this out here. I was accidentally calling the get as a function. <========================
(def update-msg-dates-process "A process run by js/window.setInterval" (atom nil))

(defn update-msg-times
  "Update the dates on the msg list."
  []
  (when (and (not ((lookup-fn :get-busy?))) (> (count @msgs-atm) 0))
    (log! :debug (str "update-msg-times: msg-count = " (count @msgs-atm)))
    ((lookup-fn :set-cs-msg-list) @msgs-atm))) ; Consider use of ((lookup-fn :get-msg-list)) here???

;;; This is called by project.cljs, core.cljs/top, and below. It is only in chat below that it would specify cid. <======================== So why not put it in db_access.cljs? Answer: msgs-atm but see above!
;;; In the other cases, it takes whatever the DB says is current.
(defn get-conversation
  "Using an HTTP GET, get the conversation, and also the code, if any."
  ([pid] (get-conversation pid nil)) ; nil -> You start based on what the DB says was most recent.
  ([pid cid]
   (log! :info (str "chat/get-conversation: pid = " pid " cid = " cid))
   (-> (dba/get-conversation-http pid cid)
       (p/catch (fn [e] (log! :error (str "get-conversation failed: " e))))
       (p/then (fn [{:keys [conv cid code]}]
                 (log! :info (str "chat/get-conversation (return from promise): cid = " cid " count = " (count conv)))
                 (reset! msgs-atm conv)
                 (when (not-empty code) ((lookup-fn :set-code) code))
                 ((lookup-fn :set-cs-msg-list) conv)
                 ((lookup-fn :set-active-conv) cid)
                 (ws/send-msg {:dispatch-key :resume-conversation :pid pid :cid cid})
                 (update-common-info! {:pid pid :cid cid}))))))

(register-fn :get-conversation get-conversation)

(defn add-msg
  "Add messages to the msgs-atm.
   This is typically used for individual messages that come through :iviewr-says or :sur-says,
   as opposed to bulk update through get-conversation."
  [text from]
  (assert (#{:system :surrogate :human :developer-interjected} from))
  (let [msg-id (inc (or (apply max (->> @msgs-atm (map :message/id) (filter identity))) 0))]
    (swap! msgs-atm conj {:message/content text :message/from from :id msg-id :time (js/Date. (.now js/Date))})
    ((lookup-fn :set-cs-msg-list) @msgs-atm)))

(register-fn :interviewer-busy?     (fn [{:keys [value]}]
                                      ((lookup-fn :set-busy?) value)))

(register-fn :iviewr-says           (fn [{:keys [p-key text table]}]
                                      (when p-key (remember-promise p-key))
                                      (add-msg text :system)
                                      (when table
                                        ((lookup-fn :set-table) table))))

(register-fn :sur-says              (fn [{:keys [p-key msg]}]
                                      (when p-key (remember-promise p-key))
                                      (log! :info (str "sur-says msg: " msg))
                                      (add-msg msg :surrogate)))

;;; There is just one Chat instance in our app. It is switched between different conversations.
(defnc Chat [{:keys [chat-height]}]
  (let [[msg-list set-msg-list]         (hooks/use-state [])
        [box-height set-box-height]     (hooks/use-state (int (/ chat-height 2.0)))
        [cs-msg-list set-cs-msg-list]   (hooks/use-state nil)
        [active-conv set-active-conv]   (hooks/use-state nil) ; active-conv is a keyword
        [busy? set-busy?]               (hooks/use-state nil) ; Have to go through common-info
        resize-fns (make-resize-fns set-box-height)]
    (letfn [(change-conversation-click [to]
              (when-not busy?
                (if-let [pid (:pid @common-info)]
                  (get-conversation pid to)
                  (log! :error (str "change-conversation-click fails: common-info = " @common-info)))))
            (process-user-input [text]
              (when (not-empty text)
                (let [[ask-llm? question]  (re-matches #"\s*LLM:(.*)" text)
                      [surrogate? product] (re-matches #"\s*SUR:(.*)" text)
                      [sur+ map-str]       (re-matches #"\s*SUR\+:(.*)" text)
                      [sur-follow-up? q]   (re-matches #"\s*SUR\?:(.*)" text)
                      msg (cond  ask-llm?       {:dispatch-key :ask-llm :question question}
                                 surrogate?     {:dispatch-key :start-surrogate :product product}
                                 sur+           {:dispatch-key :start-surrogate+ :map-str map-str} ; like :start-surrogate, but provide a map of stuff.
                                 sur-follow-up? {:dispatch-key :surrogate-follow-up :pid (:pid @common-info) :question q}

                                 :else          {:dispatch-key :domain-expert-says :msg-text text :promise-keys @ws/pending-promise-keys})]
                  ;; ToDo: Human-interjected questions, though some of them are stored, don't store the human-interjected annotation.
                  ;;       In fixing this, keep the annotation separate from the question because if a surrogate sees it, it will be confused.
                  (when sur-follow-up?
                    (set-msg-list (conj msg-list {:message/content (str "<b>[Human-interjected question]</b><br/>" q)
                                                  :message/from :system})))
                  (when ask-llm?
                    (set-msg-list (conj msg-list {:message/content (str "<b>[Side discussion with LLM]</b><br/>" question)
                                                  :message/from :system})))
                  ;;(set-msg-list (add-msg text :human))
                  (add-msg text :human)
                  (ws/send-msg msg))))]
      ;; ------------- Talk through web socket, initiated below.
      (hooks/use-effect :once ; These are used outside the component scope.
        (register-fn :clear-msgs   (fn [] (set-cs-msg-list []) (reset! msgs-atm [])))
        (register-fn :add-tbd-text (fn [text] (set-msg-list (add-msg text :system))))
        (register-fn :add-sur-text (fn [text] (set-msg-list (add-msg text :surrogate))))
        (register-fn :set-active-conv set-active-conv)
        (register-fn :set-busy? (fn [val] (swap! common-info #(assoc % :busy? val)) (do (set-busy? val)))) ; Yes. Need to do both.
        (register-fn :get-busy? (fn [] (:busy? @common-info))) ; This is why need it in common-info. (fn [] busy?) is a clojure; not useful.
        (register-fn :get-msg-list  (fn [] @msgs-atm))                               ; These two used to update message time.
        (register-fn :set-cs-msg-list (fn [msgs] (set-cs-msg-list (msgs2cs msgs))))  ; These two used to update message time.
        (reset! update-msg-dates-process (js/window.setInterval (fn [] (update-msg-times)) 60000)))
      (hooks/use-effect [msg-list]
        (reset! msgs-atm msg-list)
        (set-cs-msg-list (msgs2cs msg-list)))
      ;; ----------------- component UI structure.
      ($ ShareUpDown
         {:init-height chat-height
          :up-portion 0.8
          :share-fns resize-fns
          :up ($ Box {:sx ; This work!
                      #js {:overflowY "auto"  ; Creates a scroll bar
                           :display "flex"    ; So that child can be 100% of height. See https://www.geeksforgeeks.org/how-to-make-flexbox-children-100-height-of-their-parent-using-css/
                           :height box-height ; When set small enough, scroll bars appear.
                           :flexDirection "column"
                           :bgcolor "#f0e699"}} ; "#f0e699" is the yellow color used in MessageList. (see style in home.html).
                 ;; https://github.com/chatscope/use-chat-example/blob/main/src/components/Chat.tsx (See expecially :onChange and :onSend.)
                 ($ MainContainer {:sx {:display "flex" :height box-height}}
                    ($ Sidebar {:position "left" :sx #js {:maxWidth "100px"}}
                       ($ ConversationList
                          ($ Conversation {:name "Process"
                                           :active (= active-conv :process)
                                           :onClick (fn [_] (change-conversation-click :process))})
                          ($ Conversation {:name "Data"
                                           :active (= active-conv :data)
                                           :onClick (fn [_] (change-conversation-click :data))})
                          ($ Conversation {:name "Resources"
                                           :active (= active-conv :resources)
                                           :onClick (fn [_] (change-conversation-click :resources))})
                          ($ Conversation {:name "Optimality"
                                           :active (= active-conv :optimality)
                                           :onClick (fn [_] (change-conversation-click :optimality))})))

                    ($ ChatContainer
                       ($ MessageList
                          {:typingIndicator (when busy? ($ TypingIndicator {:content "Interviewer is typing"}))
                          #_#_ :style #js {:height "500px"}}
                          cs-msg-list))))
          :dn ($ Box {:sx #js {:width "95%"}} ; This fixes a sizing bug!
                 ($ Stack {:direction "row" :spacing "0px"}
                    ($ ButtonGroup
                       ($ AttachmentModal {:post-attach-fn #(log! :info (str "attach-fn: args = " %))})) ; This has the attachment modal
                    ($ MessageInput {:placeholder "Type message here...."
                                     :onSend #(do (log! :info (str "onSend: " %))
                                                  (process-user-input %))
                                     :attachButton false
                                     :fancyScroll false
                                     ;; It sets height to whatever you'd like with px, but doesn't expand scroll bars. It doesn't respond to :height <percent> either.
                                     :style #js {#_#_:height "200px" :width "90%"}})))}))))
