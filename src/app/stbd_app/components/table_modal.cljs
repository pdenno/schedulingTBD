(ns stbd-app.components.table-modal
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [applied-science.js-interop :as j]
   [clojure.edn                :as edn]
   [helix.core :refer [defnc $]]
   [helix.dom :as dom]
   [helix.hooks :as hooks]
   [stbd-app.components.table :refer [TablePane]]
   ["@mui/material/Button$default"                :as Button]
   ["@mui/material/Dialog$default"                :as Dialog]))

#_(def example-table
  {:table-headings [{:title "Stage", :key :stage} {:title "Duration", :key :duration}],
   :table-body
   [{:stage "Milling", :duration "30 minutes"}
    {:stage "Mashing", :duration "1.5 to 2 hours"}
    {:stage "Boiling", :duration "1 to 1.5 hours"}
    {:stage "Cooling", :duration "30 to 60 minutes"}
    {:stage "Fermentation", :duration "7 to 14 days (varies by beer type)"}
    {:stage "Conditioning", :duration "7 to 30 days (varies by beer type)"}
    {:stage "Bottling/Kegging", :duration "2 to 4 hours per batch"}
    {:stage "Distribution", :duration "Varies by destination, typically 1 to 2 days locally"}]})

;;; https://mui.com/material-ui/react-modal/
(defnc TableModal [{:keys [table]}]
  (let [modal           (hooks/use-ref nil)
        table     (when (not-empty table) (edn/read-string table))
        [open set-open] (hooks/use-state false)]
    (letfn [(handle-open [] (when (j/get modal :current) (set-open true)))
            (handle-close [] (set-open false))]
      (dom/div {:ref modal}
               ($ Button {:onClick handle-open :color "warning"} "Table")
               ($ Dialog  {:open open :onClose handle-close}
                  ($ TablePane {:init-table table}))))))
