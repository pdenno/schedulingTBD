(ns stbd-app.core
  (:require
   [applied-science.js-interop :as j]
   [helix.core :as helix :refer [defnc $ <>]]
   [helix.hooks :as hooks]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/colors" :as colors]
   ["@mui/material/CssBaseline$default" :as CssBaseline]
   ["@mui/material/Stack$default"      :as Stack]
   ;; Example: https://gist.github.com/geraldodev/a9b60dd611d1628f9413dd6de6c3c974#file-material_ui_helix-cljs-L14
   ["@mui/material/styles" :as styles]
   ["@mui/material/Typography$default" :as Typography]
   [promesa.core :as p]
   ["react-dom/client"          :as react-dom]
   [scheduling-tbd.util         :refer [config-log!]]
   [stbd-app.components.chat    :as chat]
   [stbd-app.components.editor  :as editor :refer [Editor]]
   [stbd-app.components.project :as proj :refer [SelectProject]]
   [stbd-app.components.share   :as share :refer [ShareLeftRight]]
   [stbd-app.db-access :as dba]
   [stbd-app.util      :as util :refer [register-fn lookup-fn update-common-info!]]
   [stbd-app.ws        :as ws]
   [taoensso.telemere  :refer [log!]]))

(def ^:diag diag (atom {}))


;;; ToDo: So many problems with this. (Does ANY of it work?).
;;; Example of using style in helix (a gist): https://gist.github.com/geraldodev/a9b60dd611d1628f9413dd6de6c3c974#file-material_ui_helix-cljs-L14
(def app-theme
  (styles/createTheme
   (j/lit {:palette {:background {:paper "#F0F0F0"}
                     :primary   colors/blue
                     :secondary colors/grey} ; Secondary here doesn't change icon color. Needs 'main' or 'A400' property.
           :typography {:subtitle1 {:fontSize 5}}

           #_#_:text {:primary "#173A5E"
                  :secondary "#46505A"}

           :components {:MuiCssBaseline {:text {:primary "#173A5E" ; This is being ignored.
                                                :secondary "#46505A"}}
                        #_#_:MuiIconButton
                        {:variants [{:props {:variant "myWhite"}
                                     :style {:color "secondary"}}]}
                        :MuiDivider
                        {:variants [{:props {:variant "activeVert"}  ; vertical divider of horizontal layout
                                     :style {:cursor "ew-resize"
                                             :color "black" ; Invisible on firefox.
                                             :width 4}}
                                    {:props {:variant "activeHoriz"} ; horizontal divider of vertical layout
                                     :style {:cursor "ns-resize"
                                             :color "black"  ; Invisible on firefox.
                                             :height 3}}]} ; For some reason looks better with 3, not 4.
                        :MuiTextField
                        {:variants [{:props {:variant "dataEditor"}
                                     :style {:multiline true}}]}}})))

#_(defn run-code
  "ev/processRM the source, returning a string that is either the result of processing
   or the error string that processing produced."
  [source]
  (log! :info "run-code: running some code")
  (when-some [code (not-empty (str/trim source))]
    (let [user-data (get-editor-text "data-editor")
          result (try (ev/processRM :ptag/exp code  {:pprint? true :execute? true :sci? true :user-data user-data})
                      (catch js/Error e {:failure (str "Error: " (.-message e))}))]
      result)))

#_(j/defn eval-cell
  "Run <whatever process> on the string retrieved from the editor's state.
   Apply the result to the argument function on-result, which was is the set-result function set up by hooks/use-state.
   Similarly, on-progress is the progress bar completion value set up by hooks/use-state [progress set-progress].
   Note that the actual change to the output editor is done in a use-effect [state] in the toplevel component.
   Returns nil."
  [on-result-fn progress-bool-fn ^:js {:keys [state]}]
  (progress-bool-fn true)
  (as-> state ?res
    (.-doc ?res)
    (str ?res)
    #_(run-code ?res)
    (-> ?res
        (p/then #(-> % str on-result-fn)))) ; for side-effect
  nil)

(def top-share-fns
  "These, for convenience, keep track of what methods need be called on resizing. The are only needed for CodeMirror things, I think."
  {:right-share  {:on-resize-up    (partial editor/resize "code-editor")
                  :on-stop-drag-up (partial editor/resize-finish "code-editor")}})

(defnc Top [{:keys [width height]}]
  (let [banner-height 58 ; was 42 hmmm...
        [proj-infos set-proj-infos]           (hooks/use-state nil) ; Has keys :project/id and :project/name
        [proj set-proj]                       (hooks/use-state nil) ; Same structure as a proj-info element.
        [code set-code]                       (hooks/use-state "")
        useful-height (int (- height banner-height))
        chat-side-height useful-height
        code-side-height useful-height]
    (hooks/use-effect :once
      (log! :debug "ONCE")
      (editor/resize-finish "code-editor" nil code-side-height) ; Need to set :max-height of resizable editors after everything is created.
      (register-fn :set-code set-code)
    (-> (dba/get-project-list) ;  Resolves to map with client's :current-project :others, and cid the first two are maps of :projec/id :project/name.
        (p/then (fn [{:keys [current-project others cid] :as _resp}]
                  (set-proj-infos (conj others current-project))
                  (set-proj current-project)
                  (update-common-info! current-project)
                  ((lookup-fn :get-conversation) (:project/id current-project) cid)))))
    ;; ------- component (end of use-effect :once)
    ($ Stack {:direction "column" :height useful-height}
       ($ Typography
          {:variant "h4"
           :color "white"
           :backgroundColor "primary.main"
           :padding "2px 2px 2px 20px"
           :noWrap false
           :height banner-height}
          ($ Stack {:direction "row"}
             "schedulingTBD"
             ($ Box
                ($ SelectProject {:current-proj proj :proj-infos proj-infos}))))
       ($ ShareLeftRight
          {:left  ($ Stack {:direction "column"} ; I used to put the SelectProject in this Stack. Change :chat-height if you put it back.
                     ($ chat/Chat {:chat-height chat-side-height :proj-info proj}))
           :right ($ Editor {:text code
                             :name "code-editor"
                             :height code-side-height})
           :lf-pct 0.50
           :init-width width}))))

(defnc app []
  {:helix/features {:check-invalid-hooks-usage true}}
  (let  [[width  set-width]  (hooks/use-state (j/get js/window :innerWidth))
         [height set-height] (hooks/use-state (j/get js/window :innerHeight))
         ;; https://reactrouter.com/en/6.8.2/hooks/use-search-params
         #_#_[search-params set-search-params] (useSearchParams)
         carry-dims-atm (atom {:width width :height height})]
    ;; Uncaught Error: useLocation() may be used only in the context of a <Router> component.
    ;; (js/console.log "search params = " search-params)
    (letfn [(handle-resize [& _args]
              (let [new-width  (j/get js/window :innerWidth)
                    new-height (j/get js/window :innerHeight)]
                (set-width  new-width)
                (set-height new-height)
                (reset! carry-dims-atm {:width new-width :height new-height})))]
      ;; https://www.pluralsight.com/guides/re-render-react-component-on-window-resize
      ;; React gives us a way to do this [cleanup handler] with useEffect. When passing a function to useEffect,
      ;; if that function also returns a function, that returned function will be called to perform any needed cleanup.
      ;; We can put our removeEventListener code there:
      (hooks/use-effect [width height]
        (fn [& _] (js/window.removeEventListener "resize" handle-resize)))
      (js/window.addEventListener "resize" handle-resize)
      (<> ; https://reactjs.org/docs/react-api.html#reactfragment
       ;; https://mui.com/material-ui/react-css-baseline/
       ;; ToDo: See for example https://mui.com/material-ui/customization/typography/ search for MuiCssBaseline
       ;; Use of CssBaseline removes padding/margin around application, if nothing else.
       (CssBaseline {:children #js []}) ; https://v4.mui.com/components/css-baseline/
       ($ styles/ThemeProvider
          {:theme app-theme}
          ($ Top {:width  (:width  @carry-dims-atm)
                  :height (:height @carry-dims-atm)})))))) ; ToDo: Work required here to check whether it is called with an example UUID.

(defonce root (react-dom/createRoot (js/document.getElementById "app")))

;;; --------------- https://code.thheller.com/blog/shadow-cljs/2019/08/25/hot-reload-in-clojurescript.html ----------------------
(defn ^{:after-load true, :dev/after-load true} start []
  (config-log!)
  (ws/connect!)
  (.render root ($ app)))

(defn ^:export init []
  (start))

;;; ToDo: This only executes on recompile; I'd like it to run on reload too.
;;; Note that start/stop above are shadow-cljs things and reload isn't.
;;; For a possible solution See "performance.getEntriesByType("navigation")" at
;;; https://stackoverflow.com/questions/5004978/check-if-page-gets-reloaded-or-refreshed-in-javascript
(defn ^{:before-load true, :dev/before-load true #_#_:dev/before-load-async true} stop []
  (log! :info "STOP")
  (when-let [proc @ws/ping-process]  (js/window.clearInterval proc)) ; clear old ping-process, if any.
  (when-let [proc @ws/check-process] (js/window.clearInterval proc))
  (when-let [proc @chat/update-msg-dates-process] (js/window.clearInterval proc))
  (when (ws/channel-ready?)
    (log! :info (str "Telling server to close channel for client-id = " @ws/client-id))
    (ws/send-msg {:dispatch-key :close-channel}) ; The client-id is appended by send-message!.
    (log! :info (str "Message sent for client-id = " @ws/client-id))))
