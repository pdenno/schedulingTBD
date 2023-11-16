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
   [scheduling-tbd.util :as sutil :refer [timeout-info #_invalidate-timeout-info]]
   [stbd-app.db-access  :as dba]
   [stbd-app.util       :as util]
   [taoensso.timbre     :as log :refer-macros [info debug log]]))

(def diag (atom nil))

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

(def intro-message
  "The first message of a conversation."
  [{:msg-text/string "Describe your scheduling problem in a few sentences or "}
   {:msg-link/uri "http://localhost:3300/learn-more"
    :msg-link/text "learn more about how this works"}
   {:msg-text/string "."}])

;;; ------------------- web-socket ------------------------------

(def client-id "A random uuid naming this client. It changes on disconnect." (str (random-uuid)))
(def ws-url (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))
(def channel (atom nil))
(def connected? (atom false))

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

;;; ------------------- Component ------------------------------
(defnc Chat [{:keys [height conv-map]}]
  (let [[msg-list set-msg-list] (hooks/use-state (->> conv-map :conv (mapv msg2rce) clj->js)) ; ToDo: Not clear why this doesn't set msg-list...
        [progress set-progress] (hooks/use-state 0)
        [progressing? _set-progressing] (hooks/use-state false)
        [user-text     set-user-text]   (hooks/use-state "")
        [system-text set-system-text]   (hooks/use-state "")
        input-ref (hooks/use-ref nil)]
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
                              (when (= :tbd-says (:dispatch-key msg))
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
          (-> (dba/user-says user-text)
              (p/then #(set-msg-list (->> % msg2rce (add-msg msg-list))))
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
      (reset! diag {:msg-list msg-list})
      ;; ----------------- component UI structure.
      ($ Stack {:direction "column" :spacing "0px"}
         ($ Box {:sx (clj->js {:overflowY "auto" ; :sx was :style
                               :display "flex"
                               :flexGrow 1
                               :maxHeight 300 ; (- height 100)
                               :flexDirection "column"})}
            ($ rce/MessageList {:dataSource msg-list}))
         ($ LinearProgress {:variant "determinate" :value progress})
         ($ Stack {:direction "row" :spacing "0px"}
            ($ rce/Input {:referance input-ref ; <==== Yes, rilly!
                          :value user-text
                          :placeholder "Type here..."
                          :multiline true})
            ($ IconButton {:onClick #(when-let [iref (j/get input-ref :current)]
                                       (when-let [text (not-empty (j/get iref :value))]
                                         (set-msg-list (add-msg msg-list (rce-msg :user text)))
                                         (j/assoc! iref :value "")
                                         (set-user-text text)))}
               ($ Send)))))))
