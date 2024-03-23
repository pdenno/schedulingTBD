(ns scheduling-tbd.how-made
  "Create a DB of 'How It's Made' (him) episodes. Store LLM-generated scheduling challenge text for testing.
   See  https://en.wikipedia.org/wiki/List_of_How_It%27s_Made_episodes
   There are about 400 episodes; each episode has 4 segments.
   The content of the DB is often the same as what is in data/him-db.edn, (ToDo:) which is used to create the base DB."
  (:require
   [clojure.edn             :as edn]
   [clojure.java.io         :as io]
   [clojure.pprint          :refer [pprint]]
   [clojure.string          :as str]
   [datahike.api            :as d]
   [hickory.core            :as hick] ; There might be easier ways, but I'd like to learn this tool.
   [mount.core              :as mount :refer [defstate]]
   [scheduling-tbd.db       :as proj-db]
   [scheduling-tbd.domain   :as dom]
   [scheduling-tbd.llm      :as llm]
   [scheduling-tbd.sutil    :as sutil :refer [connect-atm db-cfg-map]]
   [taoensso.timbre :as log]))

(def him-schema+
  "Defines content of the How It's Made DB in a convenient format, not Datahike format."
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
        :doc "a string that uniquely identifies the segment."}
   :segment/link
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to an optional segment link object for the segment."}
   :segment/useless?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "a boolean that is true if the segment is unlikely to serve as a useful testcase (e.g. 'sardines')"}
   :segment/challenge-intro
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a short paragraph describing the scheduling challenges of making the thing in :segment/name."}
   ;; ---------------------- segment-link
   :segment-link/href
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "the href of the optional :segment/link"}
   :segment-link/text
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a text of the optional :segment/link."}})

(def him-schema
  "Create a Datahike-compatible schema from the above."
  (reduce-kv (fn [r k v]
               (conj r (-> v
                           (dissoc :mm/info)
                           (assoc :db/ident k))))
             []
             him-schema+))

;;; -------------------- Processing the raw html ----------------------------
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
                            :episode/sequence-number (edn/read-string ep-num)
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
;;; -------------------- DB functions ----------------------------
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
    :min-seq - return no episodes with an :episode/sequence-number smaller than this.
    :max-seq - return no episodes with an :episode/sequence-number larger than this.
    :names - return only segments in the argument collection (not its episode).

   Example usage: (get-him-db-content {:min-seq 3 :max-seq 5}).
   Example usage: (get-him-db-content #{\"Sardines\" \"Bread\"})."
  [& {:keys [min-seq max-seq names]}]
  (assert (or (empty? names) (set? names)))
  (let [conn-atm (connect-atm :him)]
    (cond->> (d/q '[:find [?e ...] :where [?e :episode/id]] @conn-atm)
      true     sort
      true     (mapv #(sutil/resolve-db-id {:db/id %} conn-atm #{:db/id}))
      min-seq  (filter #(>= (:episode/sequence-number %) min-seq))
      max-seq  (filter #(<= (:episode/sequence-number %) max-seq))
      names    (filter (fn [epi] (some #(names (:segment/name %)) (:episode/segments epi))))
      names    (mapcat :episode/segments)
      names    (filter #(names (:segment/name %)))
      true     vec)))

(defn segment-exists?
  [name]
  (d/q '[:find  ?e .
         :in $ ?name
         :where [?e :segment/name ?name]]
       @(connect-atm :him) name))

(defn intro-exists?
  [name]
  (d/q '[:find  ?e .
         :in $ ?name
         :where
         [?e :segment/name ?name]
         [?e :segment/challenge-intro]]
       @(connect-atm :him) name))

;;;--------------------- Populating :segment/challenge-intro  ---------------------
(defn write-challenge-intro
  "Write the value for :segment/challenge-intro for the argument segment name.
   Example usage: (write-challenge-intro \"Aluminium foil\")"
  [seg-name]
  (if (segment-exists? seg-name)
    (if-let [challenge (-> (str "a company that makes " seg-name)
                             dom/pretend-you-manage-prompt
                             (llm/query-llm {:model-class :gpt-4 :raw-text? true}))]
      (do (log/info "Intro Response: " challenge)
          (d/transact (connect-atm :him) {:tx-data [{:segment/name seg-name
                                                     :segment/challenge-intro challenge}]}))
      (log/error "Problem getting challenge-intro for" seg-name))
    (log/error "No such segment:" seg-name)))

;;; You probably want to call write-db-backup after running this.
(defn write-intros
  "Write intros for segments in the argument episodes, indexed like get-him-db-content, plus the key :redo?.
   Example usage: (write-intros {:min-seq 6, :max-seq 15})."
  [& {:keys [names] :as epi-args}]
  (let [content (get-him-db-content epi-args)
        seg-names (if (not-empty names)
                    content
                    (->> content
                         (mapcat :episode/segments)
                         (map :segment/name)))]
    (doseq [seg-name seg-names]
      (if (and (intro-exists? seg-name) (not (:redo? epi-args)))
        (log/info "Intro exists and you didn't ask to :redo?, so skipping:" seg-name)
        (write-challenge-intro seg-name)))))

;;;--------------------- Starting, stopping, recreating etc.  ---------------------
(defn write-db-backup
  "Write a file of current db content.
   Example-usage (write-db-backup \"my-him-db.edn\")
   Example-usage (write-db-backup) writes to data/him-db.edn which is kept in the repository."
  ([] (write-db-backup "data/him-db.edn"))
  ([filename]
   (spit filename (with-out-str (pprint (get-him-db-content))))))

(defn create-him-db
  "Create the How It's Made DB."
  [cfg]
  (when (d/database-exists? cfg) (d/delete-database cfg))
  (d/create-database cfg)
  (let [data (if (.exists (io/file "./data/him-db.edn"))
               (-> "./data/him-db.edn" slurp edn/read-string)
               (made-data))
        conn (connect-atm :him)]
    (d/transact conn him-schema)
    (d/transact conn {:tx-data data})
    (mark-as-useless suspected-useless)))

;;; ------------------- Create projects for entries ---------------
(defn segment-intros
  "Return the a map of info for segments that have intros."
  []
  (d/q '[:find ?name ?intro
         :keys segment/name segment/intro
         :where
         [?e :segment/challenge-intro ?intro]
         [?e :segment/name ?name]]
       @(connect-atm :him)))

(defn create-project!
  "Add the project to the system and create a project DB for it.
   Example usage (create-project! 'Aluminium Foil') -- really!."
  [seg-name]
  (if-let [intro (->> (segment-intros)
                      (some #(when (= seg-name (:segment/name %)) (:segment/intro %))))]
    (let [pname (-> seg-name (str " segment scheduling"))]
      (proj-db/create-proj-db!
       {:project/name pname
        :project/id   (-> pname str/lower-case (str/replace #"\s+" "-") keyword)}
       {}
       {:force? true :make-current? false})
      (proj-db/add-msg :him (:segment/challenge-intro intro) :user))
    (log/error "HIM segment by that name not found:" seg-name)))

(def rebuild-him? "True if mount/init should rebuild the How It's Made database." false)

;;; ------------------- Starting and stopping ---------------
(defn init-him
  "Set sys-db-cfg atoms for system db and the template for the proj-base-cfg (:path-base).
   Recreate the system database if sys-db-cfg.rebuild-db? = true."
  []
  (let [him-cfg (db-cfg-map :him)]
    (sutil/register-db :him him-cfg)
    (when rebuild-him? (create-him-db him-cfg))
    him-cfg))

(defstate him-cfg
  :start (init-him))
