(ns stbd-app.components.tables
  "A React component to display tables."
  (:require
   [applied-science.js-interop :as j]
   [helix.core         :as helix :refer [defnc $]]
   [helix.hooks                :as hooks]
   ["@mui/icons-material/Add$default" :as Add]
   ["@mui/icons-material/Remove$default" :as Remove]
   ["@mui/icons-material/ArrowUpward$default" :as Up]
   ["@mui/icons-material/ArrowDownward$default" :as Down]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/ButtonGroup$default"  :as ButtonGroup]
   ["@mui/material/IconButton$default"  :as IconButton]
   ["@mui/material/Paper$default" :as Paper]
   ["@mui/material/Stack$default" :as Stack]
   ["@mui/material/Table$default" :as Table]
   ["@mui/material/TableBody$default" :as TableBody]
   ["@mui/material/TableCell$default" :as TableCell]
   ["@mui/material/TableContainer$default" :as TableContainer]
   ["@mui/material/TableHead$default" :as TableHead]
   ["@mui/material/TableRow$default" :as TableRow]
   ["@mui/material/TextField$default" :as TextField]
   [stbd-app.util       :as util :refer [lookup-fn register-fn update-common-info!]]
   [stbd-app.ws         :as ws]
   [taoensso.telemere  :refer [log!]]))

(def diag (atom nil))

(defnc RowButtons [{:keys [add-fn remove-fn up-fn down-fn]}]
  ($ ButtonGroup {}
     ($ IconButton {:onClick (fn [_] (add-fn)) :key "add"}
        ($ Add))
     ($ IconButton {:onClick (fn [_] (remove-fn)) :key "remove"}
        ($ Remove))
     ($ IconButton {:onClick (fn [_] (up-fn)) :key "up" }
        ($ Up))
     ($ IconButton {:onClick (fn [_] (down-fn)) :key "down" }
        ($ Down))))

(defn set-row-ids [table]
  (when-not table (log! :error "Null table in set-row-ids."))
  (let [row-cnt (atom -1)]
    (update table :table-body
            #(-> (for [row %]
                   (assoc row :internal/id (swap! row-cnt inc)))
                 vec))))

;;; BTW, :key values are essential to making edits work in this application.
(defnc EditCell [{:keys [cell-id row-id text table set-table-fn]}]
  ($ TableCell {:key cell-id :sx #js {:padding "0"}}
     ($ TextField {:key text ; :key must be text, nothing else seems to work!
                   :size "small"
                   :sx #js {:bgcolor "#fff6d9"} ; "#f0e699" is the yellow color used in MessageList; this is lighter.
                   :defaultValue text
                   :autoFocus true
                   :onChange (fn [event]
                               (let [val (j/get-in event [:target :value])]
                                  (set-table-fn ; ToDo: This makes things sluggish. Is there a better way?
                                   (assoc-in table [:table-body row-id cell-id] val))))})))

(defnc BodyRows [{:keys [table set-table-fn]}]
  (let [table (set-row-ids table)]
    (for [table-row (:table-body table)]
      (let [row-id (:internal/id table-row)
            column-keys  (->> table :table-headings (mapv :key))]
        ($ TableRow {:key row-id :editMode "row" :editable true :sx #js {:padding "0 0 0 0"}}
           (-> (for [k column-keys]
                 ($ EditCell {:key (name k) :text (get table-row k) :row-id row-id :cell-id k :table table :set-table-fn set-table-fn}))
               vec
               (conj
                ($ RowButtons {:add-fn    (fn [] (set-table-fn
                                                  (-> (update table :table-body
                                                              #(-> (subvec % 0 (inc row-id))
                                                                   (conj (reduce (fn [res attr] (assoc res attr "")) {} column-keys))
                                                                   (into (subvec % (inc row-id)))))
                                                      set-row-ids)))

                               :remove-fn (fn [] (set-table-fn
                                                  (-> (update table :table-body
                                                              #(-> (subvec % 0 row-id)
                                                                   (into (subvec % (inc row-id)))))
                                                      set-row-ids)))

                               :up-fn     (fn [] (when-not (zero? row-id)
                                                   (set-table-fn
                                                    (-> (update table :table-body
                                                                #(-> (subvec % 0 (dec row-id))
                                                                     (conj (nth % row-id))
                                                                     (conj (nth % (dec row-id)))
                                                                     (into (subvec % (inc row-id)))))
                                                        set-row-ids))))

                               :down-fn   (fn [] (when-not (= row-id (-> table :table-body count dec))
                                                   (set-table-fn
                                                    (->
                                                     (update table :table-body
                                                             #(-> (subvec % 0 row-id)
                                                                  (conj (nth % (inc row-id)))
                                                                  (conj (nth % row-id))
                                                                  (into (subvec % (+ 2 row-id)))))
                                                     set-row-ids))))}))))))))

(def original-table "Used by the 'Reset' button." (atom nil))

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

;;; table is a map with keys :table-headings and :table-body.
(defnc DataArea [{:keys [table-in]}]
  (let [[table set-table] (hooks/use-state (set-row-ids table-in))
        table-headings (:table-headings table)]
    (register-fn :set-table (fn [x] (set-table x)))
    (letfn [(submit-table [_] (ws/send-msg {:dispatch-key :domain-expert-says
                                            :table table
                                            :promise-keys @ws/pending-promise-keys})) ; ToDo: investigate pending-promise-keys.
            (reset-table [_] (set-table @original-table))]
      ;; :once won't work here since table-in is usually nil and I make things happen with the registered :set-table function.
      (hooks/use-effect [table]
        (when (and (not @original-table)
                   (-> table :table-body not-empty))
          (reset! original-table (set-row-ids table))))
      (if table-headings
        ($ Stack {:id "data-area" :direction "column" :spacing "0px"}
           ($ TableContainer {:component Paper}
              ($ Table {:size "small"}
                 ($ TableHead {}
                    ($ TableRow {}
                       (for [{:keys [key title]} table-headings] ($ TableCell {:key key} title))))
                 ($ TableBody {}
                    ($ BodyRows {:table table :set-table-fn set-table}))))
           ($ ButtonGroup {:variant "contained" :size "small" :align "center"}
              ($ Button {:onClick submit-table  :width "50px"} "Submit")
              ($ Button {:onClick reset-table  :width "50px"} "Reset")))
        ($ Box {:id "data-area"})))))
