(ns stbd-app.components.graph-modal
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [applied-science.js-interop :as j]
   [helix.core                 :refer [defnc $]]
   [helix.dom                  :as dom]
   [helix.hooks                :as hooks]
   [stbd-app.components.graph  :refer [GraphPane]]
   ["@mui/material/Button$default"   :as Button]
   ["@mui/material/Dialog$default"   :as Dialog]))

;;; https://mui.com/material-ui/react-modal/
(defnc GraphModal [{:keys [graph]}]
  (let [modal           (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]
    (letfn [(handle-open [] (when (j/get modal :current) (set-open true)))
            (handle-close [] (set-open false))]
      (dom/div {:ref modal :minWidth 1000}
               ($ Button {:color "warning" :onClick handle-open} "Graph")
               ($ Dialog  {:open open :onClose handle-close :minWidth 1000}
                  ($ GraphPane {:init-graph graph :minWidth 1000})))))) ; minWidth does nothing.
