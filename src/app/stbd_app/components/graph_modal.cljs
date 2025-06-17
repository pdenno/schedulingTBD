(ns stbd-app.components.graph-modal
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [applied-science.js-interop :as j]
   [helix.core                 :refer [defnc $]]
   [helix.dom                  :as dom]
   [helix.hooks                :as hooks]
   [stbd-app.components.graph  :refer [GraphPane]]
   ["@mui/icons-material/Schema$default" :as SchemaIcon]
   ["@mui/material/Dialog$default"       :as Dialog]
   ["@mui/material/IconButton$default"   :as IconButton]))

(def example-graph nil)

;;; https://mui.com/material-ui/react-modal/
(defnc GraphModal [{:keys [graph]}]
  (let [modal           (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]
    (letfn [(handle-open [] (when (j/get modal :current) (set-open true)))
            (handle-close [] (set-open false))]
      (dom/div {:ref modal}
               ($ IconButton {:color "warning" :onClick handle-open}
                  ($ SchemaIcon))
               ($ Dialog  {:open open :onClose handle-close}
                  ($ GraphPane {:init-graph example-graph}))))))
