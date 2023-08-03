(ns stbd-app.components.chat
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [ajax.core :refer [GET POST]]
   ;;[applied-science.js-interop :as j]
   [helix.core :refer [defnc $]]
   [helix.hooks :as hooks]
   ;;[stbd-app.util :as util]
   ["@mui/icons-material/Send$default" :as Send]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/material/LinearProgress$default" :as LinearProgress]
   ["@mui/material/Stack$default" :as Stack]
   ["react-chat-elements/dist/main"    :as rce]
   [scheduling-tbd.util :as sutil :refer [timeout-info #_invalidate-timeout-info]]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def diag (atom nil))

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

(def white-style (clj->js {:color "background.paper"}))
(def blue-style (clj->js {:color "primary"}))

(defnc Chat [{:keys [height]}]
  (let [[progress set-progress] (hooks/use-state 0)
        [progressing? _set-progressing] (hooks/use-state false)]
    (hooks/use-effect [progressing?]
      (log/info "In use-effect: progressing? = " progressing?)
      (reset! progress-atm 0)
      (reset! progress-handle
              (js/window.setInterval
               (fn []
                 (let [percent (compute-progress)]
                   (if (or (>= progress 100) (not progressing?))
                     (do (set-progress 0) (js/window.clearInterval @progress-handle))
                     (set-progress (reset! progress-atm percent)))))
               200)))
    (letfn [(send-success [{:keys [save-id]}]
              (log/info "Saved user input:" save-id))
            (send-failure [status status-text]
                (log/error "Saving example failed: status = " status " status text = " status-text))
            (handle-send []
              (POST "/api/user-says"
                    {:params {:user-text "hello"} ; <====================================
                     :timeout 3000
                     :handler       send-success
                     :error-handler send-failure}))]
      (log/info "chat height = " height)
      (reset! diag {:height height})
      ($ Stack {:direction "column" :spacing "0px"}
         ($ Box {:sx (clj->js {:overflowY "auto" ; :sx was :style
                               :display "flex"
                               :flexGrow 1
                               :maxHeight 300 ; (- height 100)
                               :flexDirection "column"})}
            ($ rce/MessageList {#_#_:toBottomHeight 300
                                :dataSource (clj->js [{:type "text" :text "hello!" :title "TBD" :titleColor "Red"} ; Default is :position "left"
                                                      {:position "right" :type "text" :text "hello, TBD!"  :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:position "right" :type "text" :text "more text..." :title "You" :titleColor "Green"}
                                                      {:type "SystemMessage" :text "End of conversation, 2023-08-01"}])}))
         ($ LinearProgress {:variant "determinate" :value progress})
         ($ Stack {:direction "row" :spacing "0px"} ; ToDo: Maybe look into prop :rightButtons. Didn't work first time.
            ($ rce/Input {:placeholder "Type here..."
                          :style {:min-width "400px"}
                          :multiline true})
            ($ IconButton {:onClick handle-send}
               ($ Box {:sx (clj->js {:background "primary"})}
                  ($ Send  #_{:sx white-style}))))))))
