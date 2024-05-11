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
   [stbd-app.util       :refer [register-fn lookup-fn]]
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
          (<= 3600000 diff 72000000) "1 hour ago"
          (< start-of-day-millis msg-epoch-millis) (str (quot diff 3600000) " hours ago")
          :else (-> (inst2date instant) (subs 0 15)))))

(def key-atm (atom 0))
(defn new-key [] (swap! key-atm inc) (str "msg-" @key-atm))

(defn msgs2cs ; cs = ChatScope, https://chatscope.io/
  "Create ChatScope structures (Message, MessageHeader, MessageSeparator, etc.) for a collection of messages."
  [msgs]
  (let [new-date (atom today)]
    (reduce (fn [r msg]
              (let [{:message/keys [content from time] :or {time (js/Date. (now))}} msg
                    content (if (= from :surrogate) (str "<b>Surrogate Expert</b><br/> " content) content)
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
  ((lookup-fn :set-cs-msg-list) (msgs2cs ((lookup-fn :get-msg-list)))))

(defnc Chat [{:keys [chat-height conv-map proj-info]}]
  (let [[msg-list set-msg-list]         (hooks/use-state (:conv conv-map))
        [user-text set-user-text]       (hooks/use-state "")                         ; Something the user said, plain text.
        [sur-text set-sur-text]         (hooks/use-state "")                         ; Something said by a surrogate, different path of execution than user-text.
        [tbd-text set-tbd-text]         (hooks/use-state "")                         ; Something the system said, a dispatch-obj with :msg.
        [box-height set-box-height]     (hooks/use-state (int (/ chat-height 2.0)))
        [cs-msg-list set-cs-msg-list]   (hooks/use-state nil)
        resize-fns (make-resize-fns set-box-height)]
    ;; ------------- Talk through web socket, initiated below.
    (hooks/use-effect :once ; These are used outside the component scope.
      (register-fn :set-tbd-text set-tbd-text)
      (register-fn :set-sur-text set-sur-text)
      (register-fn :get-msg-list  (fn [] @msgs-atm))  ; These two used to update message time.
      (register-fn :set-cs-msg-list set-cs-msg-list)  ; These two used to update message time.
      (reset! update-msg-dates-process (js/window.setInterval (fn [] (update-msg-list)) 60000)))
    ;; These are interrelated. For example, ((lookup-fn :set-sur-text) "foo") --> set-msg-list --> set-cs-msg-list --> UI update.
    (hooks/use-effect [conv-map]
      (-> conv-map :conv set-msg-list))
    (hooks/use-effect [msg-list]
      (reset! msgs-atm msg-list)
      (set-cs-msg-list (msgs2cs msg-list)))
    (hooks/use-effect [tbd-text]  ; Put TBD's (server's) message into the chat.
      (when (not-empty tbd-text)
        (set-msg-list (conj msg-list {:message/content tbd-text :message/from :system}))))
    (hooks/use-effect [sur-text]  ; Put TBD's (server's) message into the chat.
      (when (not-empty sur-text)
        (set-msg-list (conj msg-list {:message/content sur-text :message/from :surrogate}))))
    (hooks/use-effect [user-text] ; Entered by user with send button. Send user-text to the server, and put it in the chat.
       (when (not-empty user-text)
         (let [[ask-llm? question]  (re-matches #"\s*LLM:(.*)" user-text)
               [surrogate? product] (re-matches #"\s*SUR:(.*)" user-text)
               [sur-follow-up? q]   (re-matches #"\s*SUR\?:(.*)" user-text)
               msg (cond  ask-llm?       {:dispatch-key :ask-llm :question question}
                          surrogate?     {:dispatch-key :start-surrogate :product product}
                          sur-follow-up? {:dispatch-key :surrogate-follow-up :pid (:project/id proj-info) :question q}
                          :else          {:dispatch-key :domain-expert-says :msg-text user-text :promise-keys @ws/pending-promise-keys})]
           ;; ToDo: Human-injected questions, though some of them are stored, don't store the human-injected annotation.
           ;;       In fixing this, keep the annotation separate from the question because if a surrogate sees it, it will be confused.
           (set-msg-list (conj msg-list {:message/content (str "<b>[Human-injected question]</b><br/>"(or question q))
                                         :message/from :system}))
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
                        {#_#_:typingIndicator ($ TypingIndicator "Interviewer is typing") ; ToDo: insert this when it is useful.
                         :style #js {:height "500px"}}
                        cs-msg-list))))
        :dn ($ Stack {:direction "row" :spacing "0px"}
               ($ MessageInput {:placeholder "Type message here...."
                                :onSend #(do (log/info "onSend:" %)
                                             (set-user-text %))
                                :fancyScroll false
                                ;;:autoFocus false ; ToDo: Needs investigation. I don't know what it does.
                                ;; It sets height to whatever you'd like with px, but doesn't expand scroll bars. It doesn't respond to :height <percent> either.
                                :style #js {#_#_:height "200px" :width "90%"}}))})))
