(ns scheduling-tbd.db
  "System and project database schemas and database initialization.
   There are other databases, see for example, shop.clj."
  (:require
   [clojure.core :as c]
   [clojure.edn  :as edn]
   [clojure.instant]
   [clojure.java.io              :as io]
   [clojure.pprint       :refer [pprint]]
   [clojure.set                  :as set]
   [clojure.spec.alpha           :as s]
   [clojure.string               :as str]
   [datahike.api                 :as d]
   ;[datahike.pull-api            :as dp]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.util  :as util :refer [now]]
   [scheduling-tbd.specs :as spec]
   [scheduling-tbd.sutil :as sutil :refer [register-db connect-atm datahike-schema db-cfg-map resolve-db-id]]
   [taoensso.timbre :as log :refer [debug]])
  (:import
   java.time.LocalDateTime))

(def db-schema-sys+
  "Defines content that manages project DBs and their analysis including:
     - The project's name and db directory
     - Planning domains, methods, operators, and axioms"
  {;; ---------------------- project
   :project/deleted?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "a boolean marking the projected as no longer existing.
              It won't be written to backup. The DB will be moved under the 'deleted' directory."}
   :project/dir ; ToDo: Fix this so that the string doesn't have the root (env var part) of the pathname.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "a string naming a subdirectory containing a project."}
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword, :unique :db.unique/identity
        :doc "a keyword matching the one in the same named property of a project database"}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string, same as the :project/name in the project's DB."}

;;; ---------------------- system
   :system/default-project-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword,
        :doc "a keyword providing a project-id clients get when starting up."}
   :system/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "the value 'SYSTEM' to represent a single object holding data such as the current project name."}})


(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db."
  {
   ;; ---------------------- message
   :message/from
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The agent, either :user or :system, issuing the message."}
   :message/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long :unique :db.unique/identity
        :doc "The unique ID of a message. These are natural numbers starting at 0, but owing to 'LLM:' prompts, which aren't stored, some values can be skipped."}
   :message/content
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "msg-text and msg-link objects."}
   :message/time
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "The time at which the message was sent."}

   ;; ---------------------- msg-link
   :msg-link/text
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The text of describing a msg-link which is itself part of :message/content."}
   :msg-link/uri
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
           :doc "The URI of a msg-link which is itself part of :message/content."}

   ;; ---------------------- msg-text
   :msg-text/string
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
           :doc "A text string as part of :message/content."}

   ;; ---------------------- project
   :project/current-domain
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "a :domain/id (keyword in domains db) indicating where this project is currently working."}
   :project/deleted?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "a boolean marking the projected as no longer existing.
              It won't be written to backup. The DB will be moved under the 'deleted' directory."}
   :project/desc ; ToDo: If we keep this at all, it would be an annotation on an ordinary :message/content.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "the original paragraph written by the user describing what she/he wants done."}
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a lowercase kebab-case keyword naming a project; unique to the project."}
   :project/industry
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a short description of the industry in which we are doing scheduling."}
   :project/messages
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "message objects of the project"}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "4 words or so describing the project; e.g. 'craft brewing production scheduling'"}
   :project/state-string
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string, that can be edn/read-string into a vector of propositions."}
   :project/surrogate
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the project's surrogate object, if any."}

   ;; ---------------------- summary
   :summary/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "the value 'SUMMARY'. This is used to keep information about the state of the conversation."}
   :summary/interview-state
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "a keyword (enum val from some set; ns=interview-state) indicating the current disposition of the interview."}
   ;; ---------------------- resource type
   :res-t/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task type; unique to the project."}
   :res-t/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task; unique to the project."}
   :res-t/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this task; unique to the project."}
   :res-t/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this type (e.g. in an ontology)."}

   ;; ---------------------- resource instance
   :res-i/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :res-i/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task"}
   :res-i/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this task"}
   :res-i/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this instance (e.g. in an ontology)."}

      ;; ---------------------- surrogate
   :surrogate/id ; Could this and :surrogate/thread be combined?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A string that uniquely identifies this surrogate, for example, 'craft beer-1'."}

   :surrogate/subject-of-expertise
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The short string identifying what this surrogate is good at minus the verb, which is in the system instruction.
              For example, this might just be 'craft beer'."}

   :surrogate/thread-str ; Could this and :surrogate/id be combined?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant thread associated with this project. This is a string which can be read-string'ed to the "}

   :surrogate/system-instruction
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The complete instruction provided in configuring an OpenAI (or similar) assistant.
              Typically this substitutes the subject-of-expertise into a template string."}

   :surrogate/assistant-obj-str
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "Stringified EDN for what OpenAI (or similar) returns when an assistant is created."}

   ;; ---------------------- task instance
   :task-i/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :task-i/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task; unique to the project."}
   :task-i/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "a description of this this task; unique to the project."}
   :task-i/pre-task
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a task/name identifying a task that must occur before this task "}
   :task-i/resource-inst
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a keyword naming a :res-t/id or :res-i/id (type or instance) used in this task"}
   :task-i/duration-est
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to a duration (dur) object"}
   :task-i/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this instance (e.g. in an ontology)."}

      ;; ---------------------- task type (Of course these are not planner tasks!)
   :task-t/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :task-t/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task; unique to the project."}
   :task-t/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this task; unique to the project."}
   :task-t/pre-task
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a task/name identifying a task that must occur before this task "}
   :task-t/resource-type
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "a keyword naming a resource (type or instance) used in this task"}
   :task-t/duration-est
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to a duration (dur) object"}
   :task-t/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this instance (e.g. in an ontology)."}

   ;; ---------------------- work
   :work/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming something to be accomplished."}
   :work/objective-sentence
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The sentence from user description best describing the scheduling objective."}})

;;; ToDo: This was used in the old get-project. I'll keep it around for a while to see if it is useful.
#_(def db-object-ids
  (reduce-kv (fn [res k v] (if (= :db.unique/identity (:db/unique v)) (conj res k) res))
             []
             db-schema-proj+))

(def ^:diag diag (atom nil))
(def db-schema-sys  (datahike-schema db-schema-sys+))
(def db-schema-proj (datahike-schema db-schema-proj+))

;;; Atom for the configuration map used for connecting to the project db.
(defonce proj-base-cfg (atom nil))

(defn project-exists?
  "If a project with argument :project/id (a keyword) exists, return the root entity ID of the project
   (the entity id of the map containing :project/id in the database named by the argumen proj-id)."
  [proj-id]
  (assert (keyword? proj-id))
  (when (d/q '[:find ?e .
               :in $ ?proj-id
               :where
               [?e :project/id ?proj-id]
               (not [?e :project/deleted? true])]
             @(connect-atm :system) proj-id)
    (d/q '[:find ?e .
           :in $ ?proj-id
           :where
           [?e :project/id ?proj-id]]
         @(connect-atm proj-id) proj-id)))

(defn get-project
  "Return the project structure."
  ([pid] (get-project pid #{:db/id}))
  ([pid filter-set]
   (let [conn (connect-atm pid)]
     (when-let [eid (project-exists? pid)]
       (resolve-db-id {:db/id eid} conn filter-set)))))

(defn default-project
  "Return a map of the  default :project/id and :project/name.
   :project/id is from the system database, and
   providing :project/name was a mistake in several situations." ; ToDo: Fix this.
  []
  (let [pid (d/q '[:find ?proj .
                   :where
                   [?entity :system/name "SYSTEM"]
                   [?entity :system/default-project-id ?proj]]
                 @(connect-atm :system))]
    (get-project pid #{:project/name})
    (when-let [eid (project-exists? pid)]
      (resolve-db-id {:db/id eid} (connect-atm pid)
                     :keep-set #{:project/name :project/id}))))

(defn list-projects
  "Return a vector of keywords maps describing each project known by the system DB."
  ([] (list-projects {:from-storage? false}))
  ([{:keys [from-storage?]}]
   (if from-storage?
     (if-let [base-dir (-> (System/getenv) (get "SCHEDULING_TBD_DB"))]
       (let [files (-> base-dir (str "/projects/") clojure.java.io/file .listFiles)]
         (mapv #(-> % .getName keyword) files))
       (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases.") {})))
     ;; Otherwise we list using system db. These are the 'legitmate' projects (they aren't :project/deleted? = true).
     (d/q '[:find [?proj-id ...]
            :where
            [?e :project/id  ?proj-id]
            (not [?e :project/deleted? true])]
          @(connect-atm :system)))))

(defn get-state
  [pid & {:keys [sort?] :or {sort? true}}]
  (if (= pid :new-project)
    []
    (let [conn (connect-atm pid)]
      (when-let [state-str (d/q '[:find ?s .
                                  :in $ ?pid
                                  :where
                                  [?e :project/id ?pid]
                                  [?e :project/state-string ?s]]
                                @conn)]
        (cond->> (edn/read-string state-str)
          sort? (sort-by first))))))

(def message-keep-set "A set of properties with root :project/messages used to retrieve typically relevant message content."
  #{:project/messages :message/id :message/from :message/content :message/time :msg-text/string :msg-link/uri})

(defn get-messages
  "For the argument project (pid) return messages sorted by their :message/id."
  [pid]
  (when-let [eid (project-exists? pid)]
    (->> (resolve-db-id {:db/id eid}
                       (connect-atm pid)
                       :keep-set message-keep-set)
        :project/messages
        (sort-by :message/id)
        vec)))

(defn put-state
  "Write an updated state to the project database."
  [pid state-vec]
  (let [eid (project-exists? pid)
        conn (connect-atm pid)]
    (d/transact conn {:tx-data [[:db/add eid :project/state-string (str state-vec)]]})))

;;; ----------------------- Backup and recover project and system DB ---------------------
(defn backup-proj-db
  [id & {:keys [target-dir] :or {target-dir "data/projects/"}}]
  (let [conn-atm (connect-atm id)
        filename (str target-dir (name id) ".edn")
        s (with-out-str
            (println "[")
            (doseq [ent-id  (sutil/root-entities conn-atm)]
              (let [obj (resolve-db-id {:db/id ent-id} conn-atm #{:db/id})]
                ;; Write content except schema elements and transaction markers.
                (when-not (and (map? obj) (or (contains? obj :db/ident) (contains? obj :db/txInstant)))
                  (pprint obj)
                  (println))))
            (println "]"))]
    (log/info "Writing project to" filename)
    (spit filename s)))

(defn backup-proj-dbs
  "Backup the project databases one each to edn files. This will overwrite same-named files in tar-dir.
   Example usage: (backup-proj-dbs)."
  [& {:keys [target-dir] :or {target-dir "data/projects/"}}]
  (doseq [{:project/keys [id]} (list-projects)]
    (backup-proj-db id {:target-dir target-dir})))

(defn backup-system-db
  "Backup the system database to an edn file."
  [& {:keys [target-dir] :or {target-dir "data/"}}]
      (let [conn-atm (connect-atm :system)
            filename (str target-dir "system-db.edn")
            s (with-out-str
                (println "[")
                (doseq [ent-id  (sutil/root-entities conn-atm)]
                  (let [obj (resolve-db-id {:db/id ent-id} conn-atm #{:db/id})]
                    ;; Write content except schema elements and transaction markers.
                    (when-not (and (map? obj) (or (contains? obj :db/ident) (contains? obj :db/txInstant)))
                      (pprint obj)
                      (println))))
                (println "]"))]
      (log/info "Writing system DB to" filename)
      (spit filename s)))

(defn recreate-system-db!
  "Recreate the system database from an EDN file."
  [& {:keys [target-dir] :or {target-dir "data/"}}]
  (if (.exists (io/file (str target-dir "system-db.edn")))
    (let [cfg (db-cfg-map :system)]
      (log/info "Recreating the system database.")
      (when (d/database-exists? cfg) (d/delete-database cfg))
      (d/create-database cfg)
      (register-db :system cfg)
      (let [conn (connect-atm :system)]
        (d/transact conn db-schema-sys)
        (d/transact conn (-> "data/system-db.edn" slurp edn/read-string)))
      cfg)
    (log/error "Not recreating system DB: No backup file.")))

(defn recreate-project-db!
  "Recreate a DB for each project using EDN files."
  [id]
  (let [backup-file (format "data/projects/%s.edn" (name id))]
    (if (.exists (io/file backup-file))
      (let [cfg (db-cfg-map :project id)]
        (when (d/database-exists? cfg) (d/delete-database cfg))
        (d/create-database cfg)
        (register-db id cfg)
        (let [conn (connect-atm id)]
          (d/transact conn db-schema-proj)
          (d/transact conn (->> backup-file slurp edn/read-string)))
        cfg)
    (log/error "Not recreating DB because backup file does not exist:" backup-file))))

(def keep-db? #{:him :planning-domains})
(defn recreate-dbs!
  "Recreate the system DB on storage from backup.
   For each project it lists, recreate it from backup if such backup exists."
  []
  (swap! sutil/databases-atm
         #(reduce-kv (fn [res k v] (if (keep-db? k) (assoc res k v) res)) {} %))
  (recreate-system-db!)
  (doseq [pid (list-projects)]
    (recreate-project-db! pid)))

(defn unknown-projects
  "Return a vector of directories that the system DB does not know."
  []
  (set/difference
   (set (list-projects {:from-storage? true}))
   (set (list-projects))))

;;; ----------------------- Creating a project DB ----------------------
(defn unique-proj
  "If necessary to ensure uniqueness, update the project name and id."
  [proj-info]
  (let [names (d/q '[:find [?name ...]
                     :where [_ :project/name ?name]] @(connect-atm :system))
        name (:project/name proj-info)]
    (if (not-any? #(= name %) names)
      proj-info
      (let [pat (re-pattern (str "^" name ".*"))
            similar-names (filter #(re-matches pat %) names)
            pat (re-pattern (str "^" name "( \\d+)?"))
            nums (map #(let [[success num] (re-matches pat %)]
                         (when success (or num "0"))) similar-names)
            num (->> nums (map read-string) (apply max) inc)
            new-name (str name " " num)
            new-id   (-> new-name (str/replace #"\s+" "-") keyword)]
        (-> proj-info
            (assoc :project/name new-name)
            (assoc :project/id new-id))))))

(defn max-msg-id
  "Return the current highest message ID used in the project."
  [pid]
  (when-let [eid (project-exists? pid)]
    (as-> (resolve-db-id {:db/id eid}
                        (connect-atm pid)
                        :keep-set #{:project/messages :message/id}) ?o
      (:project/messages ?o)
      (map :message/id ?o)
      (if (empty? ?o) 0 (apply max ?o)))))

;;; See "Map forms" at https://docs.datomic.com/pro/transactions/transactions.html
;;; This is typical: You get the eid of the thing you want to add properties to.
;;; You specify that as the :db/id and then just add whatever you want for the properties.
;;; If the property is cardinality many, it will add values, not overwrite them.
(defn add-msg
  "Create a message object and add it to the database with :project/id = id."
  [pid from msg-vec]
  (s/assert ::spec/chat-msg-vec msg-vec)
  (if-let [conn (connect-atm pid)]
    (let [msg-id (inc (max-msg-id pid))]
      (d/transact conn {:tx-data [{:db/id (project-exists? pid)
                                  :project/messages #:message{:id msg-id :from from :time (now) :content msg-vec}}]}))
    (throw (ex-info "Could not connect to DB." {:pid pid}))))

(defn add-project
  "Add the argument project (a db-cfg map) to the system database."
  ([id proj-name dir]
   (d/transact (connect-atm :system)
               {:tx-data [{:project/id id
                           :project/name proj-name
                           :project/dir dir}]})))


(s/def ::project-info (s/keys :req [:project/id :project/name]))

;;; BTW, I don't have a good way to delete the project yet from the system-db.
;;;    1) (db/backup-system-db)
;;;    2) Edit the .edn to remove the project.
;;;    3) (db/recreate-system-db!)
(defn create-proj-db!
  "Create a project database for the argument project.
   The project-info map must include :project/id and :project/name.
     proj-info  - map containing at least :project/id and :project/name.
     additional - a vector of maps to add to the database.
     opts -  {:force? - overwrite project with same name}
   Return the "
  ([proj-info] (create-proj-db! proj-info {}))
  ([proj-info additional-info] (create-proj-db! proj-info additional-info))
  ([proj-info additional-info opts]
   (s/assert ::project-info proj-info)
   (let [{:project/keys [id name]} (if (:force? opts) proj-info (unique-proj proj-info))
         cfg (db-cfg-map :project id)
         dir (-> cfg :store :path)]
     (when-not (-> dir java.io.File. .isDirectory)
       (-> cfg :store :path java.io.File. .mkdir))
     (add-project id name dir)
     (when (d/database-exists? cfg) (d/delete-database cfg))
     (d/create-database cfg)
     (register-db id cfg)
     ;; Add to project db
     (d/transact (connect-atm id) db-schema-proj)
     (d/transact (connect-atm id) {:tx-data [{:project/id id
                                              :project/name name
                                              :project/state-string "[]"}]})
     (when (not-empty additional-info)
       (d/transact (connect-atm id) additional-info))
     ;; Add knowledge of this project to the system db.
     (log/info "Created project database for" id)
     id)))

(defn delete-project
  "Remove project from the system DB and move its project directory to the the deleted directory.
   Both the :system and project databases can have a :project/deleted? attribute."
  [pid]
  (if (and (some #(= % pid) (list-projects {:from-storage? true}))
           (some #(= % pid) (list-projects)))
    (let [cfg (db-cfg-map :project pid)
          source (-> cfg :store :path)
          filename (-> source io/file .getName)
          target (str (:base-dir cfg) "/deleted/" filename)
          eid (project-exists? pid)
          sys-proj-eid (d/q `[find ?e . :where [?e :project/id ~pid]] (connect-atm :system))]
      (log/warn "Deleting project" pid (str ". (Moving it to" target ")"))
      (d/transact (connect-atm :system) [[:db/add sys-proj-eid :project/deleted? true]])
      (d/transact (connect-atm pid)     [[:db/add eid          :project/deleted? true]])
      (sutil/deregister-db pid)
      (log/warn "Moving project" pid "to " target)
      (sutil/move-file source target))
    (log/warn "Delete-project: Project not found:" pid)))

;;; -------------------- Starting and stopping -------------------------
(defn register-project-dbs
  "Make a config for each project and register it."
  []
  (doseq [id (list-projects {:from-storage? true})]
    (register-db id (db-cfg-map :project id))))

(defn init-db-cfgs
  "Register DBs using "
  []
  (register-project-dbs)
  (register-db :system (db-cfg-map :system))
  {:sys-cfg (db-cfg-map :system)})

(defstate sys&proj-database-cfgs
  :start (init-db-cfgs))
