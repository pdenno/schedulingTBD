(ns stbd-app.components.tables
  "A React component to display tables."
  (:require
   [applied-science.js-interop :as j]
   [clojure.string :as str]
   [helix.core         :as helix :refer [defnc $]]
   [helix.hooks                :as hooks]

   ["@mui/icons-material/Add$default" :as Add]
   ["@mui/icons-material/Remove$default" :as Remove]
   ["@mui/icons-material/ArrowUpward$default" :as Up]
   ["@mui/icons-material/ArrowDownward$default" :as Down]

   ["@mui/material/IconButton$default"  :as IconButton]
   ["@mui/material/ButtonGroup$default"  :as ButtonGroup]

   ["@mui/material/Table$default" :as Table]
   ["@mui/material/TableBody$default" :as TableBody]
   ["@mui/material/TableCell$default" :as TableCell]
   ["@mui/material/TableContainer$default" :as TableContainer]
   ["@mui/material/TableHead$default" :as TableHead]
   ["@mui/material/TableRow$default" :as TableRow]
   ["@mui/material/Paper$default" :as Paper]
   [taoensso.telemere  :refer [log!]]))

(def example-table-data [{:internal/id 1 :name "Frozen yogurt", :calories 159, :fat 6.0, :carbs 24, :protein 4.0}
                         {:internal/id 2 :name "Ice cream sandwich" :calories 237, :fat  9.0, :carbs 37, :protein 4.3}
                         {:internal/id 3 :name "Eclair", :calories 262, :fat 16.0, :carbs 24, :protein 6.0}
                         {:internal/id 4 :name "Cupcake", :calories 305, :fat 3.7, :carbs 67, :protein 4.3}
                         {:internal/id 5 :name "Gingerbread", :calories 356, :fat 16.0, :carbs 49, :protein 3.9}])

(def updated-table-data [{:internal/id 1 :name "NOT Frozen yogurt", :calories 159, :fat 6.0, :carbs 24, :protein 4.0}
                         {:internal/id 2 :name "NOT Ice cream sandwich" :calories 237, :fat  9.0, :carbs 37, :protein 4.3}])


(def example-table-headings ["Dessert (100g serving)",  "Calories", "Fat (g)", "Carbs (g)", "Protein (g)"])

(def diag (atom nil))

(defnc RowButtons [{:keys [add-fn remove-fn up-fn down-fn]}]
  ($ ButtonGroup {}
     ($ IconButton {:key "add" :onClick (fn [_]  (log! :info "Click Add")    (add-fn))}
        ($ Add))
     ($ IconButton {:key "remove" :onClick (fn [_]  (log! :info "Click Remove") (remove-fn))}
        ($ Remove))
     ($ IconButton {:key "up" :onClick (fn [_]  (log! :info "Click Up")     (up-fn))}
        ($ Up))
     ($ IconButton {:key "down" :onClick (fn [_]  (log! :info "Click Down")   (down-fn))}
        ($ Down))))

(defn set-internal-ids [table]
  (let [row-cnt (atom -1)]
    (-> (for [row table] (assoc row :internal/id (swap! row-cnt inc))) vec)))

(defnc BodyRows [{:keys [body-rows columns-vec set-table-fn]}]
  (let [[key-column & other-columns] columns-vec]
    (for [table-row body-rows]
      (let [id (:internal/id table-row)]
        ($ TableRow {:key id}
           (-> (into [($ TableCell (get table-row key-column))]
                     (for [k other-columns] ($ TableCell {:align "right"} (get table-row k))))
               vec
               (conj ($ RowButtons {:add-fn    (fn [] (set-table-fn
                                                        (-> (subvec body-rows 0 (inc id))
                                                            (conj (reduce (fn [res attr] (assoc res attr "")) {} columns-vec))
                                                            (into (subvec body-rows (inc id)))
                                                            set-internal-ids)))

                                    :remove-fn (fn [] (set-table-fn
                                                        (-> (subvec body-rows 0 id)
                                                            (into (subvec body-rows (inc id)))
                                                            set-internal-ids)))

                                    :up-fn     (fn [] (when-not (zero? id)
                                                        (set-table-fn
                                                         (-> (subvec body-rows 0 (dec id))
                                                             (conj (nth body-rows id))
                                                             (conj (nth body-rows (dec id)))
                                                             (into (subvec body-rows (inc id)))
                                                             set-internal-ids))))

                                    :down-fn   (fn [] (when-not (= id (-> body-rows count dec))
                                                        (set-table-fn
                                                         (-> (subvec body-rows 0 id)
                                                             (conj (nth body-rows (inc id)))
                                                             (conj (nth body-rows id))
                                                             (into (subvec body-rows (+ 2 id)))))))}))))))))

(def original-table-data (atom nil))

(defnc BasicTable [{:keys [table-headings table-data] :or {table-headings example-table-headings
                                                           table-data example-table-data}}]
  (let [[table-data set-table-data] (hooks/use-state (set-internal-ids table-data))]
    (hooks/use-effect :once (reset! original-table-data table-data))
    (reset! diag table-data)
    ($ TableContainer {:component Paper}
       ($ Table {:sx {:minWidth 650}
                 :size "small"                      ; Works
                 :aria-label "a dense table"}
          ($ TableHead {}
             ($ TableRow {}
                (into [($ TableCell {:key (first table-headings)} (first table-headings))]
                      (for [head (rest table-headings)]
                        ($ TableCell {:align "right" :key head} head)))))
          ($ TableBody {}
             ($ BodyRows {:set-table-fn set-table-data
                          :body-rows table-data
                          :columns-vec [:name :calories :fat :carbs :protein]}))))))
