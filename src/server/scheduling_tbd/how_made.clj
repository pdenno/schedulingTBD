(ns scheduling-tbd.how-made
  "Create a DB of 'How It's Made' (him) episodes. Store LLM-generated scheduling challenges for testing."
  (:require
   [clojure.pprint          :refer [pprint]]
   [clojure.string          :as str]
   [datahike.api            :as d]
   [datahike.pull-api       :as dp]
   [hickory.core            :as hick]
   [mount.core              :as mount :refer [defstate]] ; I suppose there are easier ways, but I'd like to learn this tool.
   [scheduling-tbd.util     :as util :refer [connect-atm]]
   [taoensso.timbre :as log]))

(def him-schema+
  "Defines content of the How It's Made DB. In convenient format, not Datahike format."
  {;; ---------------------- episode
   :episode/date
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string,
        :doc "the date the episode first aired as a string."}
   :episode/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "an unqualified keyword :<season>-<seq> uniquely identifying the episode."}
   :episode/sequence-number
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long, :unique :db.unique/identity
        :doc "an integer uniquely identifying the episode."}
   :episode/segments
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "a collection of map objects each describing a segment that was part of this episode."}
   ;; ---------------------- segment
   :segment/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "a string that uniquely identifies the stement."}
   :segment/link
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to an optional segment link object for the segment."}
   :segment/useless?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "an optional value for segments for segments that are unlikely to be useful (e.g. 'sardines')
 This might be set after analyzing the DB a bit."}
   ;; ---------------------- segment-link
   :segment-link/href
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "the href of the optional segment-link"}
   :segment-link/text
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a text of the link."}})

(def him-schema
  "Create a Datahike-compatible schema from the above."
  (reduce-kv (fn [r k v]
               (conj r (-> v
                           (dissoc :mm/info)
                           (assoc :db/ident k))))
             []
             him-schema+))

(defn hick-tables
  "Walk the hickory, collecting 'wikitables'."
  [hick-data]
  (let [tables (atom [])]
    (letfn [(wh! [x]
              (cond (and (map? x)
                         (= (:type x) :element)
                         (= "wikitable" (-> x :attrs :class)))    (swap! tables #(conj % x))
                    (map? x)                                      (doseq [[_k v] (seq x)] (wh! v))
                    (vector? x)                                   (doseq [v x] (wh! v))))]
      (wh! hick-data)
      @tables)))

#_{:type :element,
 :attrs nil,
 :tag :tr,
 :content
 ["\n"
  {:type :element, :attrs {:style "width:5%;"}, :tag :th, :content ["Series Ep.\n"]}
  "\n"
  {:type :element, :attrs {:style "width:5%;"}, :tag :th, :content ["Episode\n"]}
  "\n",,,]}

;;; There are 32 seasons; some have the same year.
(defn season?
  "Return true if the table concerns episodes in a season."
  [table]
  (let [season? (atom false)]
    (letfn [(series-map?  [x] (and (map? x) (= "Series Ep.\n" (-> x :content (nth 0)))))
            (episode-map? [x] (and (map? x) (= "Episode\n"    (-> x :content (nth 0)))))
            (sea? [x]
              (cond @season?                                 true
                    (and (map? x)
                         (= :element (:type x))
                         (some series-map? (:content x))
                         (some episode-map? (:content x)))  (reset! season? true)
                    (map? x)                                (doseq [[_k v] (seq x)] (sea? v))
                    (vector? x)                             (doseq [v x] (sea? v))))]
      (sea? table)
      @season?)))

(defn table-episodes
  "Return a vector of maps about the episodes of the table."
  [table]
  (let [rows (atom [])]
    (letfn [(get-rows! [x] (cond (and (map? x) (= :tr (:tag x))) (swap! rows #(conj % x))
                                 (map? x)                        (doseq [[_k v] (seq x)] (get-rows! v))
                                 (vector? x)                     (doseq [v x] (get-rows! v))))
            (seg-data [seg] (let [seg-name (reduce (fn [res elem]
                                                     (cond (string? elem) (str res elem)
                                                           (map? elem)    (str res (-> elem :content (nth 0))))) "" seg)
                                  ref (when (some map? seg) (-> (filter map? seg) (nth 0) :attrs))
                                  {:keys [href title]} ref]
                              (cond-> {:segment/name (str/replace seg-name #"\n" "")}
                                href  (assoc :segment/link {})
                                href  (assoc-in [:segment/link :segment-link/href] href)
                                title (assoc-in [:segment/link :segment-link/text] title))))
            (row2obj [x] (let [[[ep-id] [ep-num] & segs] (->> x :content (filter map?) (mapv :content))
                               date (last segs)]
                           {:episode/id ep-id
                            :episode/sequence-number (read-string ep-num)
                            :episode/date (-> date (nth 0) (str/replace  #"\n" "")) ; ToDo: More strings need this.
                            :episode/segments (->> segs butlast (mapv seg-data))}))]

      (get-rows! table)
      (->> @rows
           rest ; drop table header
           (mapv row2obj)))))

(defn made-data
  "Parse the file into 'Hickory content', a map. For each table, parse it maps about episodes."
  []
  (->> "./data/List of How It's Made episodes - Wikipedia.html"
      slurp
      hick/parse
      hick/as-hickory
      hick-tables
      (filter season?)
      (mapcat table-episodes)
      vec))

;;; We bother with this because someday we ought to have testing about HIM things.
(def suspected-useless
  "A set of segments (their names) suspected to be poor candidates for asking about making one (e.g. sardines)."
  #{"Sardines"})

(defn mark-as-useless
  "Set :segment/useless? to true for segments in the argument set of :segment/name values.
   Returns nil
   Example useage (mark-as-useless #{\"Sardines\"})."
  [useless-set]
  (let [conn (connect-atm :him)]
    (doseq [name useless-set]
      (if (d/q '[:find ?e .
                 :in $ ?name
                 :where [?e :segment/name ?name]]
               @conn name) ; d/transact uses the atom; d/q want its value.
        (d/transact conn {:tx-data [{:segment/name name :segment/useless? true}]})
        (log/warn "There is not segment named" name)))))

(defn him-segment-names []
  (d/q '[:find [?name ...] :where [_ :segment/name ?name]]
       @(connect-atm :him)))

(defn get-him-db-content
  "Return sorted DB content, or part thereof.
    :min-seq returns no episodes with a smaller :episode/sequence-number.
    :max-seq returns no episodes with a larger  :episode/sequence-number.
    :names Returns only only segments in the argument collection (not its episode).

   Example usage: (get-him-db-content {:min-seq 1 :max-seq 5}).
   Example usage: (get-him-db-content #{\"Sardines\" \"Bread\"})."
  [& {:keys [min-seq max-seq names]}]
  (assert (or (empty? names) (set? names)))
  (let [conn-atm (connect-atm :him)]
    (cond->> (d/q '[:find [?e ...] :where [?e :episode/id]] @conn-atm)
      true     sort
      true     (mapv #(util/resolve-db-id {:db/id %} conn-atm #{:db/id}))
      min-seq  (filter #(>= (:episode/sequence-number %) min-seq))
      max-seq  (filter #(<= (:episode/sequence-number %) max-seq))
      names    (filter (fn [epi] (some #(names (:segment/name %)) (:episode/segments epi))))
      names    (mapcat :episode/segments)
      names    (filter #(names (:segment/name %)))
      true     vec)))

(defn write-db-backup
  "Write a file of current db content.
   Example-usage (write-db-backup \"data/him-db.edn\")."
  [filename]
  (spit filename (with-out-str (pprint (get-him-db-content)))))

(defn create-him-db
  "Create the How It's Made DB."
  [cfg]
  (when (d/database-exists? cfg) (d/delete-database cfg))
  (d/create-database cfg)
  (let [data (made-data)
        conn (connect-atm :him)]
    (d/transact conn him-schema)
    (d/transact conn {:tx-data data})
    (mark-as-useless suspected-useless)))

;;;--------------------- Starting and stopping ---------------------
;;; Based on ./db.clj
(defn init-him
  "Set sys-db-cfg atoms for system db and the template for the proj-base-cfg (:path-base).
   Recreate the system database if sys-db-cfg.rebuild-db? = true."
  []
  (let [base-dir (or (-> (System/getenv) (get "SCHEDULING_TBD_DB"))
                     (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases."
                                          "\nCreate directories 'projects' and 'system' under it.") {})))
        him-cfg {:store {:backend :file :path (str base-dir "/etc/other-dbs/him")}
                 :keep-history? false
                 :rebuild-db? false ; <================== ToDo: Make rebuild use data/him-db.edn if it exists.
                 :schema-flexibility :write}]
    (util/register-db :him him-cfg)
    (when (:rebuild-db? him-cfg) (create-him-db him-cfg))
    him-cfg))

(defstate him-cfg
  :start (init-him))