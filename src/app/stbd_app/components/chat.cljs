(ns stbd-app.components.chat
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [applied-science.js-interop :as j]
   [clojure.edn                :as edn]
   [helix.core                 :refer [defnc $]]
   [helix.hooks                :as hooks]
   [promesa.core               :as p]
   ["@mui/icons-material/Send$default" :as Send]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/material/LinearProgress$default" :as LinearProgress]
   ["@mui/material/Link$default" :as Link]
   ["@mui/material/Stack$default" :as Stack]
   ["react-chat-elements/dist/main"    :as rce]
   [stbd-app.components.share :as share :refer [ShareUpDown]]
   [scheduling-tbd.util :as sutil :refer [timeout-info #_invalidate-timeout-info]]
   [stbd-app.db-access  :as dba]
   [stbd-app.util       :as util]
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

(defn rce-msg
  "Return a React Chat Elements (RCE) message for simple text content.
   Example usage: (rce-msg :system :text 'Hello, World!')."
  [speaker text]
  (let [{:keys [name color position]} (get msg-style speaker)]
    (clj->js {:id (swap! msg-index inc)
              :type "text"
              :title name
              :titleColor color
              :position position
              :text (vector text)})))

(defn msg2rce
  "Rewrite the conversation DB-style messages to objects acceptable to the RCE component.
   Does not do clj->js on it, however."
  [msg]
  (let [{:keys [name color position]} (get msg-style (:message/from msg))]
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
                             (:message/content msg))))))

(defn add-msg [msg-list msg]
  (-> msg-list js->clj (conj msg) clj->js))

;;; ------------------- web-socket ------------------------------
(def client-id "A random uuid naming this client. It changes on disconnect." (str (random-uuid)))
(def ws-url (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))
(def channel (atom nil))
(def connected? (atom false))
(def pending-promise-keys "These are created by the server to track what is being responded to." (atom #{}))

;;; ToDo: A problem scenario is that the server has put up multiple messages and the user-says just once.
;;;       I think I'm going to have to have a ws message from the server just about clearing a key.
;;;       Don't have the client clear it; only have it pick one that it is responding to.
(defn add-promise-key   [k] (when k (swap! pending-promise-keys conj k)))
(defn clear-promise-key [k] (when k (swap! pending-promise-keys disj k)))

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

(defn send-message
  "Send the message to the server. msg can be any Clojure object but if it is a map we add the :client-id.
   Example usage: (send-message! {:dispatch-key :ping})"
  [msg]
  (let [msg (if (map? msg) (assoc msg :client-id client-id) msg)]
    (if-let [chan @channel]
      (.send chan (pr-str msg))
      (throw (ex-info "Couldn't send message; no channel." {:message msg})))))

;;; :tbd-says isn't in this because it sets the set-system-text hook.
(defn dispatch-msg
  "Call a function depending on the value of :dispatch-key in the message."
  [{:keys [dispatch-key promise-key]}]
  (cond (= dispatch-key :clear-promise-key) (clear-promise-key promise-key)
        (= dispatch-key :alive?)            (send-message {:alive? true})))

;;; ========================= Component ===============================
(defn make-resize-fns
  "These are used by ShareUpDown. The argument is a Hook state variable set- function."
  [set-height-fn]
  {:on-resize-up (fn [_parent _width height] (when height (set-height-fn height)))})

(defnc Chat [{:keys [chat-height conv-map]}]
  (let [[msg-list set-msg-list] (hooks/use-state (->> conv-map :conv (mapv msg2rce) clj->js))
        [progress set-progress] (hooks/use-state 0)
        [progressing? _set-progressing] (hooks/use-state false)
        [user-text     set-user-text]   (hooks/use-state "")
        [system-text set-system-text]   (hooks/use-state "")
        [box-height set-box-height]     (hooks/use-state (int (/ chat-height 2.0)))
        input-ref (hooks/use-ref nil)
        resize-fns (make-resize-fns set-box-height)]
    (hooks/use-effect [conv-map] ;... and this is necessary.
      (set-msg-list (->> conv-map :conv (mapv msg2rce) clj->js)))
    (letfn [(connect! []  ; I think this has to be here owing to scoping restrictions on hooks functions,... ToDo: Can't I pass it in?
              (if-let [chan (js/WebSocket. ws-url)]
                (do (log/info "Websocket Connected!")
                    (reset! channel chan)
                    (reset! connected? true)
                    (set! (.-onmessage chan)
                          (fn [event]
                            (let [msg (-> event .-data edn/read-string)]
                              (dispatch-msg msg) ; Just for :clear-promise-key messages currently.
                              (when (= :tbd-says (:dispatch-key msg))
                                (log/info "promise-key = " (:promise-key msg) "msg =" (:msg msg))
                                (add-promise-key (:promise-key msg))
                                (set-system-text (:msg msg)))))) ; ...namely, this function.
                    (set! (.-onerror chan) (fn [& arg] (log/error "Error on socket: arg=" arg))))
                (throw (ex-info "Websocket Connection Failed:" {:url ws-url}))))]
      ;; ------------- talk through web socket; server-initiated.
      (when-not @connected? (connect!)) ; Start the web socket.
      (hooks/use-effect [system-text]
        (when (not-empty system-text)
          (let [new-msg (rce-msg :system system-text)]
            (set-msg-list (add-msg msg-list new-msg)))))
      ;; ------------- Send user-text through REST API; wait on promise for response.
      (hooks/use-effect [user-text]
        (when (not-empty user-text)
          (-> (dba/user-says user-text @pending-promise-keys)
              (p/then  #(set-msg-list (->> % msg2rce (add-msg msg-list))))
              (p/catch #(log/info (str "CLJS-AJAX user-says error: status = " %))))))
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
                                                 (set-msg-list (add-msg msg-list (rce-msg :user text)))
                                         (j/assoc! iref :value "")
                                         (set-user-text text)))}
                       ($ Send))))}))))
