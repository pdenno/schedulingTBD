(ns stbd-app.components.table
  "A React component to display tables."
  (:require
   [applied-science.js-interop :as j]
   [clojure.edn                :as edn]
   [helix.core         :as helix :refer [defnc $]]
   [helix.hooks                :as hooks]
   ["@mui/icons-material/Add$default" :as Add]
   ["@mui/icons-material/Remove$default" :as Remove]
   ["@mui/icons-material/ArrowUpward$default" :as Up]
   ["@mui/icons-material/ArrowDownward$default" :as Down]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/ButtonGroup$default"  :as ButtonGroup]
   ["@mui/material/Dialog$default"  :as Dialog]
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
   [stbd-app.util       :as util :refer [register-fn]]
   [stbd-app.ws         :as ws]
   [taoensso.telemere  :refer [log!]]))

(def ^:diag diag (atom nil))

(defnc RowButtons [{:keys [add-fn remove-fn up-fn down-fn index-id]}]
 ($ TableCell {:key (str "tc-" index-id) :sx #js {:padding "0"}}  ; Otherwise gets an error: <div> cannot appear as a child of <tr>.
    ($ ButtonGroup {:key (str "bg-" index-id)}
       ($ IconButton {:onClick (fn [_] (add-fn))    :key (str "add-" index-id)}
          ($ Add))
       ($ IconButton {:onClick (fn [_] (remove-fn)) :key (str "remove-" index-id)}
          ($ Remove))
       ($ IconButton {:onClick (fn [_] (up-fn))     :key (str "up-" index-id)}
          ($ Up))
       ($ IconButton {:onClick (fn [_] (down-fn))   :key (str "down-" index-id)}
          ($ Down)))))

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
                   #_#_:size "small"
                   :sx #js {:bgcolor "#fff6d9" :alignItems "stretch" :width "100%"}  ; "#f0e699" is the yellow color used in MessageList; this is lighter.
                   :defaultValue text
                   :autoFocus true
                   :onChange (fn [event]
                               (let [val (j/get-in event [:target :value])]
                                  (set-table-fn ; ToDo: This makes things sluggish. Is there a better way?
                                   (assoc-in table [:table-body row-id cell-id] val))))})))

(defnc BodyRows [{:keys [table set-table-fn]}]
  (let [index-id (atom 0)
        table (set-row-ids table)]
    (for [table-row (:table-body table)]
      (let [ix (swap! index-id inc)
            row-id (:internal/id table-row)
            column-keys  (->> table :table-headings (mapv :key))]
        ($ TableRow {:key row-id #_#_:editMode "row" :editable true :sx #js {:width "100%" :minWidth "1300px" :alignItems "stretch" :padding "0 0 0 0"}}
           (-> (for [k column-keys]
                 ($ EditCell {:sx #js {:width "100%" :minWidth "200px" :alignItems "stretch"}
                              :key (name k) :text (get table-row k) :row-id row-id :cell-id k :table table :set-table-fn set-table-fn}))
               vec
               (conj
                ($ RowButtons {:key (str "rb-" ix)
                               :sx #js {:width "100%" :alignItems "stretch"}
                               :index-id ix
                               :add-fn    (fn [] (set-table-fn
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

;;; table is a map with keys :table-headings and :table-body.
(defnc TablePane [{:keys [init-table]}]
  (let [[table set-table] (hooks/use-state (set-row-ids init-table))
        table-headings (:table-headings table)]
    (letfn [(submit-table [_] (ws/send-msg {:dispatch-key :domain-expert-says
                                            :table table
                                            :promise-keys @ws/pending-promise-keys})) ; ToDo: investigate pending-promise-keys.
            (reset-table [_] (set-table @original-table))]
      (hooks/use-effect :once
        (register-fn :set-table-pane set-table))
        ;; :once won't work here since init-table is usually nil and I make things happen with the registered :set-table-pane function.
        (hooks/use-effect [table]
          (when (and (not @original-table) (-> table :table-body not-empty))
             (reset! original-table (set-row-ids table))))
      (when table-headings ; Changing :minHeight and :minWidth here will change things, but others won't fill the space.
        ($ Stack {:sx #js {#_#_:display "flex", :height "100%" :width "100%" :minWidth "1200px" :alignItems "stretch" :overflowY "auto" #_#_:overflowX "auto"}
                  :id "data-area" :direction "column" :spacing "0px"}
           ($ TableContainer {:component Paper
                              :sx #js {#_#_:display "flex", :height "100%" :width "100%" :minWidth "1200px" :alignItems "stretch" :overflowY "auto" #_#_:overflowX "auto"}} ; does nothing.
              ($ Table {:sx #js {:width "100%" :minWidth "1200px"}} #_{:sx #js {:minWidth 900}} ; DOES MATTER, but I can't make other things match!
                 ($ TableHead {:sx #js {:width "100%"}} ; does nothing
                    ($ TableRow {:sx #js {:width "100%"}} ; does nothing
                       (for [{:keys [key title]} table-headings] ($ TableCell {:key key} title))))
                 ($ TableBody {:sx #js {:width "100%"}} ; Does nothing
                    ($ BodyRows {:table table :set-table-fn set-table}))))
           ($ Box {:align "center"}
              ($ ButtonGroup {:variant "contained" :size "small"}
                 ($ Button {:onClick submit-table  :width "50px"} "Submit")
                 ($ Button {:onClick reset-table  :width "50px"} "Reset"))))))))


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
      ($ Box {:ref modal
              #_#_:sx #js {:minWidth "1200px"}}
         ($ Button {:onClick handle-open :color "warning"} "Table")
         ($ Dialog  {:open open :onClose handle-close
                     :sx #js {:width "fitContent" :maxWidth "xl" :fullWidth true :minWidth "1400px"}}
            ($ TablePane {:init-table table}))))))
