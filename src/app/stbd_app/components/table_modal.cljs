(ns stbd-app.components.table-modal
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [applied-science.js-interop :as j]
   [helix.core :refer [defnc $]]
   [helix.dom :as dom]
   [helix.hooks :as hooks]
   [stbd-app.components.table :refer [TablePane]]
   ["@mui/icons-material/AppRegistration$default" :as AppRegIcon]
   ["@mui/material/Dialog$default"          :as Dialog]
   ["@mui/material/IconButton$default" :as IconButton]))

(def example-table
  {:table-headings [{:title "Dessert (100g serving)" :key :name},
                    {:title "Calories" :key :calories},
                    {:title "Fat (g)" :key :fat}
                    {:title "Carbs (g)" :key :carbs},
                    {:title "Protein (g)" :key :protein}]
   :table-body [{:name "Frozen yogurt", :calories 159, :fat 6.0, :carbs 24, :protein 4.0}
                {:name "Ice cream sandwich" :calories 237, :fat  9.0, :carbs 37, :protein 4.3}
                {:name "Eclair", :calories 262, :fat 16.0, :carbs 24, :protein 6.0}
                {:name "Cupcake", :calories 305, :fat 3.7, :carbs 67, :protein 4.3}
                {:name "Gingerbread", :calories 356, :fat 16.0, :carbs 49, :protein 3.9}]})

;;; https://mui.com/material-ui/react-modal/
(defnc TableModal [{:keys [table post-table-fn]}]
  (let [modal           (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]
    (letfn [(handle-open [] (when (j/get modal :current) (set-open true)))
            (handle-close [] (set-open false))]
      (dom/div {:ref modal}
               ($ IconButton {:color "warning" :onClick handle-open}
                  ($ AppRegIcon))
               ($ Dialog  {:open open :onClose handle-close}
                  ($ TablePane {:init-table example-table}))))))
