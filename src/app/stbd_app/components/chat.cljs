(ns stbd-app.components.chat
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [ajax.core :refer [GET POST]]
   [applied-science.js-interop :as j]
   [helix.core                 :refer [defnc $]]
   [helix.hooks                :as hooks]
   [promesa.core               :as p]
   ;;[stbd-app.util :as util]
   ["@mui/icons-material/Send$default" :as Send]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/material/LinearProgress$default" :as LinearProgress]
   ["@mui/material/Link$default" :as Link]
   ["@mui/material/Stack$default" :as Stack]
   ["react-chat-elements/dist/main"    :as rce]
   [scheduling-tbd.util :as sutil :refer [timeout-info #_invalidate-timeout-info]]
   [stbd-app.util :as util]
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

(defn register
  [name elem]
  (swap! util/component-refs #(assoc % name elem))
  elem)

(defn add-msg [msg-list msg]
  (-> msg-list
      js->clj
      (conj msg)
      clj->js))

(defnc Chat [{:keys [height]}]
  (let [[msg-list set-msg-list] (hooks/use-state (clj->js [{:type "text"
                                                            :text (clj->js ["Describe your scheduling problem in a few sentences or "
                                                                            ($ Link {:key 1 :href "http://localhost:3300/learn-more"}
                                                                               "learn more about how this works")
                                                                            "."])
                                                            :title "TBD"
                                                            :titleColor "Red"}]))
        [progress set-progress] (hooks/use-state 0)
        [progressing? _set-progressing] (hooks/use-state false)
        [user-text set-user-text] (hooks/use-state "")
        input-ref (hooks/use-ref nil)]
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
    (hooks/use-effect [user-text]
       (when (not-empty user-text)
         (let [prom (p/deferred)]
           (POST "/api/user-says"
                 {:params {:user-text user-text}
                  :timeout 30000
                  :handler (fn [resp] (p/resolve! prom resp))
                  :error-handler (fn [{:keys [status status-text]}]
                                   (p/reject! prom (ex-info "CLJS-AJAX error on /api/user-says" {:status status :status-text status-text})))})
           (-> prom
               (p/then #(set-msg-list (add-msg msg-list {:type "text" :text (:msg %) :title "TDB" :titleColor "Red"})))
               (p/catch #(log/info (str "CLJS-AJAX user-says error: status = " %)))))))
    ;;(log/info "chat height = " height)
    ;;(reset! diag {:height height})
    ($ Stack {:direction "column" :spacing "0px"}
       ($ Box {:sx (clj->js {:overflowY "auto" ; :sx was :style
                             :display "flex"
                             :flexGrow 1
                             :maxHeight 300 ; (- height 100)
                             :flexDirection "column"})}
          ($ rce/MessageList {:dataSource msg-list}))
       ($ LinearProgress {:variant "determinate" :value progress})
       ($ Stack {:direction "row" :spacing "0px"}
          ($ rce/Input {:referance input-ref ; <==== Yes, rilly!  Also, need a way to clear this after Send (there was code for clear)
                        :value user-text
                        :placeholder "Type here..."
                        :multiline true})
          ($ IconButton {:onClick #(when-let [iref (j/get input-ref :current)]
                                     (when-let [user-text (not-empty (j/get iref :value))]
                                       (set-msg-list (add-msg msg-list  {:type "text" :text user-text :title "You" :color "Green" :position "right"}))
                                       (j/assoc! iref :value "")
                                       (set-user-text user-text)))}
             ($ Send))))))


(defn get-children
  "Return a vector of the children of an HTMLCollection."
  [obj]
  (when (= "HTMLCollection" (j/get-in obj [:constructor :name]))  ;; HANDY! Get the string naming the type.
    (let [res (atom [])]
      (doseq [ix (range (j/get obj :length))]
        (swap! res conj (j/get obj ix)))
      @res)))

(defn find-elem
  "Search the DOM for a node passing the argument test."
  [node test]
  (let [found? (atom false)
        cnt (atom 0)]
    (letfn [(fe [obj]
              (swap! cnt inc)
              (let [typ (j/get-in obj [:constructor :name])
                    chiln?  (j/get obj :children)]
                (log/info "typ = " typ " obj = " obj)
                (cond @found?                  found?
                      (> @cnt 50)              :failure
                      (test obj)               (reset! found? obj)
                      (vector? obj)            (map fe obj)
                      (nil? obj)               found?
                      chiln?                   (fe chiln?)
                      (= "HTMLCollection" typ) (->> obj get-children fe))))]
      (fe node)
      @found?)))
