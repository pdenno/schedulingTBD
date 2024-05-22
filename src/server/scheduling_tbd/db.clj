(ns scheduling-tbd.db
  "System and project database schemas and database initialization.
   There are other databases, see for example, him.clj."
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
   [taoensso.timbre :as log :refer [debug]]))

(def db-schema-sys+
  "Defines content that manages project DBs and their analysis including:
     - The project's name and db directory
     - Planning domains, methods, operators, and axioms"
  {;; ---------------------- agent
   :agent/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A unique ID for each agent to ensure only one of each type is available."}
   :agent/assistant-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant id (a string) associated with this surrogate."}
   :agent/thread-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant thread (a string) uniquely identifying the thread on which this surrogate operates."}

   ;; ---------------------- project
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
        :doc "the value 'SYSTEM' to represent a single object holding data such as the current project name."}
   :system/agents
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "an agent (OpenAI Assistant) that outputs a vector of clojure maps in response to queries."}})

;;;========================================================== Project DBs ==========================================
(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db."
  {;; ---------------------- box
   :box/string-val
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "boxed value"}
   :box/number-val
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number
        :doc "boxed value"}
   :box/keyword-val
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "boxed value"}
   :box/boolean-val
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "boxed value"}

   ;; ---------------------- duration
   :duration/value
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to map describing a quantity of time."}

   ;; ---------------------- message
   :message/content
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "msg-text and msg-link objects."}
   :message/from
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The agent issuing the message, #{:human :surrogate :system}."}
   :message/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long :unique :db.unique/identity
        :doc (str "The unique ID of a message. These are natural numbers starting at 0, but owing to 'LLM:' prompts, "
                  "which aren't stored, some values can be skipped.")}
   :message/tags
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "Optional keywords used to classify the message."}
   :message/time
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "The time at which the message was sent."}

   ;; ---------------------- objective (about a scheduling objective)
   :objective/code
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "MiniZinc expressing the scheduling object."}
   :objective/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming something to be accomplished."}
   :objective/text
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string
        :doc "strings expressing the scheduling objective."}

   ;; ---------------------- problem (the planning problem and current state)
   :problem/domain
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "A keyword identifying the problem domain map"}
   :problem/goal-string
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "A string that can be edn/read-string into a predicate."}
   :problem/state-string
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "A string that can be edn/read-string into a set of predicates"}

   ;; ---------------------- process (about production process types)
   :process/duration-comment
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a comment about the duration of a process."}
   :process/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this process; perhaps stated in an interview."}
   :process/duration
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to a duration (dur) object; typically an estimate"}
   :process/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc (str "A string, perhaps from an interview uniquely identifying the process."
                  "The top-level production process will have a process-type/id = :project/id.")}
   :process/pre-processes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a process/id identifying a process that must occur before this task "}
   :process/resource
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a keyword naming a resource (type or instance) used in this task"}
   :process/sub-processes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "process objects that occur within the scope of this project object"}
   :process/supply-chain?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "true if the process in a supply chain process, rather than production process"}
   :process/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this process type (e.g. in an ontology)."}
   :process/var-name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a camelCase string naming the process that can be used in MiniZinc code."}

   ;; ---------------------- process-instance (about actual process that have occurred or will occur).
   :process-instance/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :process-instance/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task; unique to the project."}
   :process-instance/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "a description of this this task; unique to the project."}
   :process-instance/pre-task
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a task/name identifying a task that must occur before this task "}
   :process-instance/resource-inst
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a keyword naming a :res-t/id or :res-i/id (type or instance) used in this task"}
   :process-instance/start
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "a instant object indicating when the process started."}
   :process-instance/end
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "a instant object indicating when the process ended."}

   ;; ---------------------- project
   :project/code
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "Code associated with the project."}

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
   :project/objective
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to the scheduling objective of the project."}
   :project/planning-problem
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "an object with keys :problem/domain, :problem/goal-string, and :problem/state-string at least."}
   :project/processes
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the project's process objects; everything about processes."}
   :project/surrogate
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the project's surrogate object, if any."}
   :project/surrogate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "true if domain expertise is provided by an artificial agent."}

   ;; ---------------------- quantity (an amount of something)
   :quantity/value-string
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string on which edn/read-string can be applied to produce a number or term like :several."}
   :quantity/units
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "a keyword identifying the units of measure, :weeks, :months, :meters, etc."}

   ;; ---------------------- quantity-range (a map with two properties low and high, the values of which are quantities.
   :quantity-range/low
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a quantity value expressing the low end of a range of values."}

   :quantity-range/high
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a quantity value expressing the high end of a range of values."}

   ;; ---------------------- resource type
   :resource-type/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the resource type; unique to the project."}
   :resource-type/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this resource; unique to the project."}
   :resource-type/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this resource; unique to the project."}
   :resource-type/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this type (e.g. in an ontology)."}

   ;; ---------------------- resource instance
   :resource/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the resource; unique to the project."}
   :resource/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this resource"}
   :resource/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this resource"}
   :resource/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this instance (e.g. in an ontology)."}

   ;; ---------------------- surrogate
   :surrogate/assistant-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant id(a string) associated with this surrogate."}
   :surrogate/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A project-oriented string that uniquely identifies this surrogate, for example, 'craft beer-1'."}
   :surrogate/subject-of-expertise
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The short string identifying what this surrogate is good at minus the verb, which is in the system instruction.
              For example, this might just be 'craft beer'."}
   :surrogate/system-instruction
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The complete instruction provided in configuring an OpenAI (or similar) assistant.
              Typically this substitutes the subject-of-expertise into a template string."}
   :surrogate/thread-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant thread (a string) uniquely identifying the thread that this surrogate uses."}})


(def ^:diag diag (atom nil))
(def db-schema-sys  (datahike-schema db-schema-sys+))
(def db-schema-proj (datahike-schema db-schema-proj+))

;;; Atom for the configuration map used for connecting to the project db.
(defonce proj-base-cfg (atom nil))

(defn project-exists?
  "If a project with argument :project/id (a keyword) exists, return the root entity ID of the project
   (the entity id of the map containing :project/id in the database named by the argumen proj-id)."
  [pid]
  (assert (keyword? pid))
  (when (d/q '[:find ?e .
               :in $ ?pid
               :where
               [?e :project/id ?pid]]
             @(connect-atm :system) pid)
    (d/q '[:find ?e .
           :in $ ?pid
           :where
           [?e :project/id ?pid]]
         @(connect-atm pid) pid)))

(defn get-project
  "Return the project structure."
  ([pid] (get-project pid #{:db/id}))
  ([pid drop-set]
   (let [conn (connect-atm pid)]
     (when-let [eid (project-exists? pid)]
       (resolve-db-id {:db/id eid} conn :drop-set drop-set)))))

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
            [?e :project/id  ?proj-id]]
          @(connect-atm :system)))))

(defn get-thread-id
  "Get the thread object of the argument PID."
  ([pid] (get-thread-id pid true))
  ([pid fail-when-missing?]
   (let [eid (project-exists? pid)
         res (when eid
               (-> (resolve-db-id {:db/id eid}
                                  (connect-atm pid)
                                  :keep-set #{:project/surrogate :surrogate/thread-id})
                   :project/surrogate
                   :surrogate/thread-id))]
     (cond res                         res
           (not fail-when-missing?)    nil
           :else                       (throw (ex-info "Did not find thread-id." {:pid pid}))))))

(defn get-assistant-id
  "Get the thread object of the argument PID."
  ([pid] (get-assistant-id pid true))
  ([pid fail-when-missing?]
   (let [eid (project-exists? pid)
         res (when eid
               (-> (resolve-db-id {:db/id eid}
                                  (connect-atm pid)
                                  :keep-set #{:project/surrogate :surrogate/assistant-id})
                   :project/surrogate
                   :surrogate/assistant-id))]
     (cond res                         res
           (not fail-when-missing?)    nil
           :else                       (throw (ex-info "Did not find assistant-id." {:pid pid}))))))

(def message-keep-set "A set of properties with root :project/messages used to retrieve typically relevant message content."
  #{:project/messages :message/id :message/from :message/content :message/time})

(defn get-messages
  "For the argument project (pid) return a vector of messages sorted by their :message/id."
  [pid]
  (when-let [eid (project-exists? pid)]
    (->> (resolve-db-id {:db/id eid}
                       (connect-atm pid)
                       :keep-set message-keep-set)
        :project/messages
        (sort-by :message/id)
        vec)))

(defn get-problem
  "Return the planning problem. We add new keys for the things that end in -string,
   that is, for :problem/goal-string and :problem/state-string :goal and :state are added respectively."
  [pid]
  (as-> (get-project pid #{:db/id :project/messages :project/surrogate :project/code}) ?x
    (:project/planning-problem ?x)
    (assoc ?x :goal (-> ?x :problem/goal-string edn/read-string))
    (assoc ?x :state (-> ?x :problem/state-string edn/read-string))))

(defn get-code
  "Return the code string for the argument project (or an empty string if it does not exist)."
  [pid]
  (or (d/q '[:find ?t .
             :in $ ?pid
             :where
             [?e :project/id ?pid]
             [?e :project/code ?t]]
           @(connect-atm pid) pid)
      ""))

;;; ToDo: For the time being, code is replaced as a whole.
;;;       For purposes of backtracking more capability might be useful.
(defn put-code
  "Save the argument MiniZinc code to the project's database.
   The code is written as a whole; editing happens elsewhere."
  [pid code-text]
  (assert (string? code-text))
  (let [conn (connect-atm pid)
        eid (project-exists? pid)]
    (d/transact conn {:tx-data [[:db/add eid :project/code code-text]]})))

(defn get-planning-state
  "Return the planning state vector (a collection of ground propositions) for the argument project, or [] if none."
  [pid]
  (if-let [state-str (d/q '[:find ?s .
                            :where
                            [_ :project/planning-problem ?pp]
                            [?pp :problem/state-string ?s]]
                          @(connect-atm pid) pid)]
    (->> state-str edn/read-string (sort-by first) vec)
    []))

(defn put-planning-state
  "Write an updated state to the project database. Argument is a vector or set. "
  [pid state]
  (assert (every? #(s/valid? ::spec/ground-positive-proposition %) state))
  (let [state-set (set state)
        conn (connect-atm pid)
        eid (d/q '[:find ?eid . :where [?eid :problem/state-string]] @conn)]
    (d/transact conn
                {:tx-data [[:db/add eid :problem/state-string (str state-set)]]})))

(defn add-planning-state
  "Add the argument vector of ground proposition to state."
  [pid more-state]
  (assert (every? #(s/valid? ::spec/ground-positive-proposition %) more-state))
  (log/info "add-planning-state: more-state = " more-state)
  (put-planning-state pid (into (get-planning-state pid) more-state)))

(defn get-process
  "Return the process structure for the argument pid and process-id."
  [pid proc-id]
  (assert (keyword? pid))
  (assert (keyword? proc-id))
  (let [conn (connect-atm pid)
        eid (d/q '[:find ?eid .
                   :in $ ?proc-id
                   :where [?eid :process/id ?proc-id]]
                 @conn
                 proc-id)]
    (resolve-db-id {:db/id eid} conn)))

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
  (doseq [id (list-projects)]
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

(def keep-db? #{:him})
(defn recreate-dbs!
  "Recreate the system DB on storage from backup.
   For each project it lists, recreate it from backup if such backup exists."
  []
  (swap! sutil/databases-atm
         #(reduce-kv (fn [res k v] (if (keep-db? k) (assoc res k v) res)) {} %))
  (recreate-system-db!)
  (log/info "Recreating these projects:" (list-projects))
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
  ([pid from msg] (add-msg pid from msg []))
  ([pid from msg tags]
   (reset! diag [pid from msg tags])
   (assert (#{:system :human :surrogate :developer-injected} from))
   (assert (string? msg))
   ;;(log/info "add-msg: pid =" pid "msg =" msg)
   (if-let [conn (connect-atm pid)]
     (let [msg-id (inc (max-msg-id pid))]
       (d/transact conn {:tx-data [{:db/id (project-exists? pid)
                                    :project/messages (cond-> #:message{:id msg-id :from from :time (now) :content msg}
                                                        (not-empty tags) (assoc :message/tags tags))}]}))
     (throw (ex-info "Could not connect to DB." {:pid pid})))))

(defn add-project
  "Add the argument project (a db-cfg map) to the system database."
  ([id proj-name dir]
   (d/transact (connect-atm :system)
               {:tx-data [{:project/id id
                           :project/name proj-name
                           :project/dir dir}]})))

(s/def ::project-info (s/keys :req [:project/id :project/name]))

(defn create-proj-db!
  "Create a project database for the argument project.
   The project-info map must include :project/id and :project/name.
     proj-info  - map containing at least :project/id and :project/name.
     additional - a vector of maps to add to the database.
     opts -  {:force? - overwrite project with same name}
   Return the PID of the project."
  ([proj-info] (create-proj-db! proj-info {} {}))
  ([proj-info additional-info] (create-proj-db! proj-info additional-info {}))
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
                                              :project/name name}]})
     (when (not-empty additional-info)
       (d/transact (connect-atm id) additional-info))
     ;; Add knowledge of this project to the system db.
     (log/info "Created project database for" id)
     id)))

(defn delete-project
  "Remove project from the system."
  [pid]
  (if (some #(= % pid) (list-projects))
    (let [conn-atm (connect-atm :system)]
      (when-let [s-eid (d/q '[:find ?e . :in $ ?pid :where [?e :project/id ?pid]] @conn-atm pid)]
        (let [obj (resolve-db-id {:db/id s-eid} conn-atm)]
          (d/transact (connect-atm :system) {:tx-data (for [[k v] obj] [:db/retract s-eid k v])})
          (sutil/deregister-db pid)
          nil)))
    (log/warn "Delete-project: Project not found:" pid)))

(defn get-agent
  "Return a map of {:aid <string> and :tid <string> for the argument agent-id (a keyword)."
  [agent-id]
  (assert (#{:process-agent :table-agent} agent-id))
  (-> (d/q '[:find ?aid ?tid
             :keys aid tid
             :in $ ?agent-id
             :where
             [?e :agent/id ?agent-id]
             [?e :agent/assistant-id ?aid]
             [?e :agent/thread-id ?tid]]
           @(connect-atm :system) agent-id)
      first))

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
