(ns stbd-app.components.graph
  (:require
   [applied-science.js-interop :as j]
   [helix.core         :as helix :refer [defnc $]]
   [helix.dom          :as dom]
   [helix.hooks        :as hooks]
   ["mermaid/dist/mermaid.esm.mjs$default" :as Mermaid :refer [initialize render]]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default"   :as Button]
   ["@mui/material/Dialog$default"   :as Dialog]
   [promesa.core        :as p]
   [stbd-app.util       :as util :refer [register-fn]]
   [taoensso.telemere  :refer [log!]]))


;;; (str #_"%%{init: {\"scale\": 3, \"width\": 1500, \"height\": 1000 } }%%\n" graph-cmd) ; no help
(defnc GraphPane [{:keys [init-graph]}]
  (let [graph-ref (hooks/use-ref nil)
        [graph-cmd set-graph-cmd] (hooks/use-state init-graph)]
    (hooks/use-effect :once
      (register-fn :set-graph-pane set-graph-cmd)
      ;; See https://mermaid.js.org/schemas/config.schema.json. Theme works (try "forest"); sizing does not work!
      (initialize (clj->js {:startOnLoad false, :securityLevel "loose" :theme "dark"})))
    (hooks/use-effect [graph-cmd]
      (try (when-let [gdiv (j/get graph-ref :current)]
             (-> (render "mermaid-id-" graph-cmd) ; "graph-svg-id" is the svg id, not the dom/div id. It isn't used.
                 (p/then (fn [svg] (j/assoc! gdiv :innerHTML #_this-svg (j/get svg :svg))))
                 (p/catch (fn [error] (log! :error (str "Error in Mermaid: " error))))))
           (catch :default _e nil)))
    ;; :minWidth and :minHeight matter here!
    ($ Box {:sx #js {:display "flex", :flexDirection "column" :height "100%" :width "100%" :minHeight "1400px" :minWidth "800px" :alignItems "stretch" :overflowY "auto"}}
       (dom/div {:ref graph-ref :id "graphDiv"}))))


;;; https://mui.com/material-ui/react-modal/
(defnc GraphModal [{:keys [graph]}]
  (let [modal           (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]
    (letfn [(handle-open [] (when (j/get modal :current) (set-open true)))
            (handle-close [] (set-open false))]
      (dom/div {:ref modal}
               ($ Button {:color "warning" :onClick handle-open} "Graph")
               ($ Dialog  {:open open :onClose handle-close :fullScreen true :maxWidth "lg"}
                  ($ GraphPane {:init-graph graph}))))))
