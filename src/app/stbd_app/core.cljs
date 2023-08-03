(ns stbd-app.core
  (:require
   [applied-science.js-interop :as j]
   [clojure.string :as str]
   ["@codemirror/view" :as view]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/ButtonGroup$default" :as ButtonGroup]
   ["@mui/material/colors" :as colors]
   ["@mui/material/CssBaseline$default" :as CssBaseline]
   ["@mui/material/LinearProgress$default" :as LinearProgress]
   ["@mui/material/Stack$default" :as Stack]
   ["@mui/material/styles" :as styles] ; See it used here: https://gist.github.com/geraldodev/a9b60dd611d1628f9413dd6de6c3c974#file-material_ui_helix-cljs-L14
   ["@mui/material/Typography$default" :as Typography]
   [promesa.core :as p]
   [scheduling-tbd.util :as sutil]
   [stbd-app.util :as util]
   [stbd-app.components.chat :as chat :refer [Chat]]
   [stbd-app.components.editor :as editor :refer [Editor set-editor-text get-editor-text SelectExample]]
   [stbd-app.components.examples :as examples :refer [rm-examples]]
   [stbd-app.components.share :as share :refer [ShareUpDown ShareLeftRight]]
   [stbd-app.components.save-modal :refer [SaveModal]]
   [helix.core :as helix :refer [defnc $ <>]]
   [helix.hooks :as hooks]
   ["react-dom/client" :as react-dom]
   ["react-router-dom" :as router :refer [useSearchParams]]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def svr-prefix "http://localhost:3000")
(declare get-user-data get-user-code)

(def progress-handle
  "The thing that can be called by js/window.clearInterval to stop incrementing progress under js/window.setInterval."
  (atom nil))
(def progress-atm "Percent allowed duration for eval-cell. 100% is a timeout." (atom 0))

(def diag (atom {}))

(def exerciser-theme
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
  (log/info "run-code: running some code")
  (when-some [code (not-empty (str/trim source))]
    (let [user-data (get-user-data)
          ;_zippy (log/info "******* For RM eval: CODE = \n" code)
          ;_zippy (log/info "******* For RM eval: DATA = \n" user-data)
          result (try (ev/processRM :ptag/exp code  {:pprint? true :execute? true :sci? true :user-data user-data})
                      (catch js/Error e {:failure (str "Error: " (.-message e))}))]
      result)))

(j/defn eval-cell
  "Run RADmapper on the string retrieved from the editor's state.
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
        (p/then #(-> % str on-result-fn)) ; for side-effect
        (p/catch (fn [e]
                   (js/window.clearInterval @progress-handle)
                   (log/info "Error in eval-cell")
                   (-> e str on-result-fn)))
        (p/finally (fn [_]
                     (progress-bool-fn false)
                     (log/info "clear handler")
                     (js/window.clearInterval @progress-handle)))))
  nil)

(defn add-result-action
  "Return the keymap updated with the partial for :on-result, I think!" ;<===
  [{:keys [on-result progress-bool]}]
  (.of view/keymap
       (j/lit
        [{:key "Mod-Enter"
          :run (partial eval-cell on-result progress-bool)}])))

(defn get-props [obj]
  (when (map? (js->clj obj))
    (js->clj (or (j/get obj :props) (get obj "props")))))

(defn get-user-data
  "Return the string content of the data editor."
  []
  (if-let [s (j/get-in (get-in @util/component-refs ["data-editor" :view]) [:state :doc])]
    (.toString s)
    ""))

(defn get-user-code
  "Return the string content of the data editor."
  []
  (if-let [s (j/get-in (get-in @util/component-refs ["code-editor" :view]) [:state :doc])]
    (.toString s)
    ""))

(defn search-props
  "Return the object that has a prop that passes the test."
  [obj test]
  (let [found? (atom nil)
        cnt (atom 0)]
    (letfn [(sp [obj]
              (swap! cnt inc)
              (cond @found?                 @found?,
                    (> @cnt 500)            nil,    ; ToDo: Remove this.
                    (test obj)              (reset! found? obj),
                    (get-props obj)         (doseq [p (-> obj get-props vals)]
                                              (sp p)),
                    (vector? (js->clj obj)) (doseq [p (js->clj obj)] (sp p))))]
      (sp obj)
      @found?)))

(def top-share-fns
  "These, for convenience, keep track of what methods need be called on resizing."
  {:left-share   {:on-stop-drag-lf (partial editor/resize-finish "data-editor")}
   :right-share  {:on-resize-up    (partial editor/resize "code-editor")
                  :on-resize-dn    (partial editor/resize "result")
                  :on-stop-drag-up (partial editor/resize-finish "code-editor")
                  :on-stop-drag-dn (partial editor/resize-finish "result")}})

;;; ToDo: Needs work.
(defn compute-progress
  "Use either progress-atm or timeout-info to return a percent done."
  []
  (let [#_#_now (.getTime (js/Date.))]
    (+ @progress-atm 2)))

(defnc Top [{:keys [rm-example width height]}]
  (let [banner-height 42
        useful-height (- height banner-height)
        chat-height (- useful-height banner-height 20) ; ToDo: 20 (a gap before the editor starts)
        code-editor-height   (int (* useful-height 0.5))]    ; <================================== Ignored?
    ;; setInterval runs its argument function again and again at the argument time interval (milliseconds).
    ;; setInterval returns a handle that can be used by clearInterval to stop the running.
    (hooks/use-effect :once ; Need to set :max-height of resizable editors after everything is created.
      (editor/resize-finish "code-editor" nil code-editor-height))
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
             ($ Box {:minWidth (- width 320)}))) ; I'm amazed this sorta works! The 320 depends on the width of "RADmapper".
       ($ ShareLeftRight
          {:left  ($ Stack {:direction "column"}
                     ($ SelectExample {:init-example (:name rm-example)})
                     ;; https://detaysoft.github.io/docs-react-chat-elements/docs/messagelist
                     ($ chat/Chat :height chat-height))
           :right ($ ShareUpDown
                     {:init-height (- useful-height 20) ; ToDo: Not sure why the 20 is needed.
                      :up ($ Editor {:name "code-editor"
                                     :height code-editor-height
                                     :text (:code rm-example)})
                      :dn ($ Box)
                      :share-fns (:right-share top-share-fns)})
           :share-fns (:left-share top-share-fns)
           :lf-pct 0.50 ; <=================================
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
          {:theme exerciser-theme}
          ($ Top {:width  (:width  @carry-dims-atm)
                  :height (:height @carry-dims-atm)
                  :rm-example (get rm-examples 0)})))))) ; ToDo: Work required here to check whether it is called with an example UUID.

(defonce root (react-dom/createRoot (js/document.getElementById "app")))

(defn ^{:after-load true, :dev/after-load true} mount-root []
  (sutil/config-log :info)
  (log/info "Logging level for the client:"
            (->> log/*config*
                 :min-level
                 (filter #(-> % first (contains? "stbd-app.*")))
                 first second))
  (.render root ($ app)))

(defn ^:export init []
  (mount-root))
