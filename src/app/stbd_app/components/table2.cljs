(ns stbd-app.components.table2
  "A React component to display tables using MUI DataGrid."
  (:require
   [applied-science.js-interop :as j]
   [clojure.edn :as edn]
   [helix.core :as helix :refer [defnc $]]
   [helix.hooks :as hooks]
   ["@mui/icons-material/Add$default" :as Add]
   ["@mui/icons-material/Remove$default" :as Remove]
   ["@mui/icons-material/KeyboardArrowUp$default" :as ArrowUp]
   ["@mui/icons-material/KeyboardArrowDown$default" :as ArrowDown]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/ButtonGroup$default" :as ButtonGroup]
   ["@mui/material/Dialog$default" :as Dialog]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/material/Stack$default" :as Stack]
   ["@mui/x-data-grid" :refer [DataGrid]]
   [stbd-app.util :as util :refer [register-fn]]
   [stbd-app.ws :as ws]
   [taoensso.telemere :refer [log!]]))

(def ^:diag diag (atom nil))

(defn table-data->datagrid-format
  "Convert our table format to DataGrid format.
   Input: {:table-headings [{:title 'Stage' :key :stage}...]
           :table-body [{:stage 'Milling' :duration '30 minutes'}...]}
   Output: {:columns [...] :rows [...]}"
  [table move-row-up-fn move-row-down-fn]
  (let [headings (:table-headings table)
        body (:table-body table)
        row-count (count body)
        data-columns (mapv (fn [{:keys [key title]}]
                             {:field (name key)
                              :headerName title
                              :editable true
                              :flex 1
                              :minWidth 150})
                           headings)
        ;; Add actions column for row reordering
        actions-column {:field "actions"
                        :headerName "Actions"
                        :width 120
                        :sortable false
                        :filterable false
                        :disableColumnMenu true
                        :renderCell (fn [params]
                                      (let [row-id (j/get params :id)]
                                        ($ Box {:sx #js {:display "flex" :gap 0.5}}
                                           ($ IconButton {:size "small"
                                                          :onClick #(move-row-up-fn row-id)
                                                          :disabled (= row-id 0)}
                                              ($ ArrowUp))
                                           ($ IconButton {:size "small"
                                                          :onClick #(move-row-down-fn row-id)
                                                          :disabled (>= row-id (dec row-count))}
                                              ($ ArrowDown)))))}
        columns (clj->js (conj data-columns actions-column))
        rows (clj->js (mapv (fn [row index]
                              (-> (reduce-kv (fn [acc k v]
                                               (assoc acc (name k) (str v))) ; Ensure all values are strings
                                             {}
                                             row)
                                  (assoc "id" index)))
                            body
                            (range)))]
    {:columns columns :rows rows}))

(defn datagrid-format->table-data
  "Convert DataGrid format back to our table format."
  [rows headings]
  (let [table-body (mapv (fn [row]
                           (reduce (fn [acc {:keys [key]}]
                                     (let [field (name key)
                                           value (j/get row field "")]
                                       (assoc acc key value)))
                                   {}
                                   headings))
                         rows)]
    {:table-headings headings
     :table-body table-body}))

(def original-table "Used by the 'Reset' button." (atom nil))

;;; table is a map with keys :table-headings and :table-body.
(defnc Table2Pane [{:keys [init-table]}]
  (let [[table set-table] (hooks/use-state init-table)
        table-headings (:table-headings table)
        [rows set-rows] (hooks/use-state #js [])
        [next-id set-next-id] (hooks/use-state 0)
        [selected-rows set-selected-rows] (hooks/use-state #js [])

        ;; Move row functions
        move-row-up (fn [row-id]
                      (let [rows-array (js->clj rows)
                            current-index (first (keep-indexed #(when (= (get %2 "id") row-id) %1) rows-array))]
                        (when (and current-index (> current-index 0))
                          (let [moved-row (nth rows-array current-index)
                                target-index (dec current-index)
                                reordered-rows (-> rows-array
                                                   (vec)
                                                   (#(assoc % current-index (nth % target-index)))
                                                   (#(assoc % target-index moved-row)))
                                new-rows (clj->js reordered-rows)]
                            (set-rows new-rows)))))

        move-row-down (fn [row-id]
                        (let [rows-array (js->clj rows)
                              current-index (first (keep-indexed #(when (= (get %2 "id") row-id) %1) rows-array))
                              max-index (dec (count rows-array))]
                          (when (and current-index (< current-index max-index))
                            (let [moved-row (nth rows-array current-index)
                                  target-index (inc current-index)
                                  reordered-rows (-> rows-array
                                                     (vec)
                                                     (#(assoc % current-index (nth % target-index)))
                                                     (#(assoc % target-index moved-row)))
                                  new-rows (clj->js reordered-rows)]
                              (set-rows new-rows)))))

        {grid-columns :columns initial-rows :rows} (when table (table-data->datagrid-format table move-row-up move-row-down))]
    (letfn [(submit-table [_]
              (let [updated-table (datagrid-format->table-data rows table-headings)]
                (ws/send-msg {:dispatch-key :domain-expert-says
                              :table updated-table
                              :promise-keys @ws/pending-promise-keys})))
            (reset-table [_]
              (when @original-table
                (set-table @original-table)
                (let [{new-rows :rows} (table-data->datagrid-format @original-table move-row-up move-row-down)]
                  (set-rows new-rows)
                  (set-next-id (count new-rows)))))
            (add-row []
              (let [new-row (reduce (fn [acc {:keys [key]}]
                                      (assoc acc (name key) ""))
                                    {"id" next-id}
                                    table-headings)
                    new-rows (.concat rows #js [(clj->js new-row)])]
                (set-rows new-rows)
                (set-next-id (inc next-id))))
            (delete-selected-rows []
              (log! :info (str "Delete called - Selected rows type: " (type selected-rows)))
              (log! :info (str "Delete called - Selected rows js->clj: " (js->clj selected-rows)))
              (when (and selected-rows
                         (let [selection-model (js->clj selected-rows)
                               ids-set (get selection-model "ids")]
                           (and ids-set (> (.-size ids-set) 0))))
                (let [selection-model (js->clj selected-rows)
                      ids-set (get selection-model "ids")
                      selected-ids (set (js->clj (js/Array.from ids-set)))
                      current-rows (js->clj rows)
                      _ (log! :info (str "Selected IDs: " selected-ids))
                      _ (log! :info (str "Sample row: " (first current-rows)))
                      filtered-rows (->> current-rows
                                         (remove (fn [row]
                                                   (contains? selected-ids (get row "id"))))
                                         (vec)
                                         (clj->js))]
                  (log! :info (str "Filtered rows count: " (.-length filtered-rows)))
                  (set-rows filtered-rows)
                  (set-selected-rows #js []))))
            (_handle-rows-update [new-rows]  ; ToDo: Not used yet! Is it needed?
              ;; This is called when DataGrid processes row updates
              (set-rows new-rows)
              new-rows)
            (handle-process-row-update [new-row _old-row]
              ;; Handle individual row updates (for editing)
              new-row)]
      (hooks/use-effect :once
                        (register-fn :set-table2-pane set-table))
      (hooks/use-effect [init-table]
                        (when init-table
                          (set-table init-table)
                          (when (and (not @original-table) (-> init-table :table-body not-empty))
                            (reset! original-table init-table))
                          (let [{new-rows :rows} (table-data->datagrid-format init-table move-row-up move-row-down)]
                            (set-rows new-rows)
                            (set-next-id (count new-rows)))))
      (when table-headings
        ($ Stack {:sx #js {:height "100%" :width "100%" :minWidth "1200px"}
                  :direction "column" :spacing 2}
           ;; Action buttons
           ($ Box {:sx #js {:display "flex" :gap 1 :mb 1}}
              ($ Button {:onClick add-row
                         :startIcon ($ Add)
                         :variant "outlined"
                         :size "small"}
                 "Add Row")
              ($ Button {:onClick delete-selected-rows
                         :startIcon ($ Remove)
                         :variant "outlined"
                         :size "small"
                         :disabled (or (not selected-rows)
                                       (let [selection-model (js->clj selected-rows)
                                             ids-set (get selection-model "ids")]
                                         (or (not ids-set) (zero? (.-size ids-set)))))}
                 "Delete Selected"))
           ;; DataGrid
           ($ Box {:sx #js {:height "500px" :width "100%"}}
              ($ DataGrid {:rows rows
                           :columns grid-columns
                           :pageSize 25
                           :rowsPerPageOptions #js [10 25 50]
                           :disableSelectionOnClick false
                           :checkboxSelection true
                           :disableRowSelectionOnClick false
                           :processRowUpdate handle-process-row-update
                           :onProcessRowUpdateError #(log! :error (str "Row update error: " %))
                           :onRowSelectionModelChange set-selected-rows
                           :sx #js {:bgcolor "#fff"
                                    "& .MuiDataGrid-cell--editable" #js {:bgcolor "#fff6d9"}
                                    "& .MuiDataGrid-cell:focus" #js {:outline "none"}
                                    "& .MuiDataGrid-cell:focus-within" #js {:outline "solid #1976d2 1px"}}}))
           ;; Submit/Reset buttons
           ($ Box {:sx #js {:display "flex" :justifyContent "center" :mt 2}}
              ($ ButtonGroup {:variant "contained" :size "small"}
                 ($ Button {:onClick submit-table} "Submit")
                 ($ Button {:onClick reset-table} "Reset"))))))))

(def ^:diag example-table
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
(defnc Table2Modal [{:keys [table]}]
  (let [modal (hooks/use-ref nil)
        table (when (not-empty table) (edn/read-string table))
        [open set-open] (hooks/use-state false)]
    (letfn [(handle-open [] (when (j/get modal :current) (set-open true)))
            (handle-close [] (set-open false))]
      ($ Box {:ref modal}
         ($ Button {:onClick handle-open :color "warning"} "Table")
         ($ Dialog {:open open :onClose handle-close :fullScreen true}
            ($ Table2Pane {:init-table table}))))))
