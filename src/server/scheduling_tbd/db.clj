(ns scheduling-tbd.db
  "Database schema, serialization, and connection implementation."
  (:require
   [clojure.core :as c]
   [clojure.spec.alpha           :as s]
   [clojure.string               :as str]
   [datahike.api                 :as d]
   ;[datahike.pull-api            :as dp]
   [mount.core :as mount :refer [defstate]]
   [taoensso.timbre :as log]))

(def db-schema-sys+
  "Associates a database directory with each project"
  {:project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword, :unique :db.unique/identity
        :doc "a keyword matching the one in the same named property of a project database"}
   :project/dir
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "a string naming a subdirectory containing a project."}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string, same as the :project/name in the project's DB.."}
   :system/current-project
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword, :unique :db.unique/identity
        :doc "a keyword naming the current project; set by user using UI."}})

(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db. "
  {;; ---------------------- project
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "4 words or less describing the project; e.g. 'craft brewing'"}
   :project/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "the original paragraph written by the user describing what she/he wants done."}
   :project/industry
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a short description of the industry in which we are doing scheduling."}
   :project/dir
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "the original paragraph written by the user describing what she/he wants done."}

   ;; ---------------------- task type
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

   ;; ---------------------- work
   :work/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming something to be accomplished."}
   :work/objective-sentence
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The sentence from user description best describing the scheduling objective."}})


(defn datahike-schema
  "Create a Datahike-compatible schema from the above."
  [schema]
  (reduce-kv (fn [r k v]
               (conj r (-> v
                           (dissoc :mm/info)
                           (assoc :db/ident k))))
             []
             schema))

(def db-schema-sys  (datahike-schema db-schema-sys+))
(def db-schema-proj (datahike-schema db-schema-proj+))

;;; Atom for configuration map used for connecting to the system db.
;;; It is set by alter-var-root code in this namespace."
(defonce sys-db-cfg (atom nil))

(defn create-sys-db!
  "Create the system database."
  []
  (when (:rebuild-db? @sys-db-cfg)
    (when (d/database-exists? @sys-db-cfg) (d/delete-database @sys-db-cfg))
    (d/create-database @sys-db-cfg)
    (let [conn (d/connect @sys-db-cfg)]
      (d/transact conn db-schema-sys)
      (log/info "Created system schema" conn)
      (d/transact conn [{:system/current-project :system/undefined-project}])
      conn)))

;;; Atom for the configuration map used for connecting to the project db.
(defonce proj-base-cfg (atom nil))

(defn current-proj
  "Get the current project from the system database."
  []
  (d/q '[find ?proj :where [_ :system/current-project ?proj]]))

(defn connect-sys
  "Set the var rad-mapper.db-util/conn by doing a d/connect.
   Return a connection atom."
  []
  (if (d/database-exists? @sys-db-cfg)
    (d/connect @sys-db-cfg)
    (log/warn "There is no DB to connect to.")))

(defn connect-proj
  "Set the var rad-mapper.db-util/conn by doing a d/connect.
   Return a connection atom."
  []
  (if (d/database-exists? @proj-base-cfg)
    (d/connect @proj-base-cfg)
    (log/warn "There is no DB to connect to.")))

(s/def ::project-db (s/keys :req [:project/name]))
(defn create-proj-db!
  "Create a project database for the argument project."
  [{:project/keys [name id] :as trans-map}]
  (if (s/valid? ::project-db trans-map)
     (let [new-proj-data (-> trans-map (assoc :project/id id) vector)
           dir (str (-> @proj-base-cfg :store :path-base) (name id))]
       (.mkdir (java.io.File. dir))
       ;; ToDo: :project/id is unique. Don't wipe out an existing project. User could be starting over. Maybe add a number.
       (let [db-cfg (assoc @proj-base-cfg :path dir)]
         (d/create-database db-cfg)
         (sutil/register-db id db-cfg))
       (let [conn (connect-proj)]
         (d/transact conn db-schema-proj)
         (d/transact conn {:tx-data new-proj-data})
         ;; Add knowledge of this project to the system db.
         (d/transact (connect-sys) {:tx-data [{:project/id id,
                                               :project/name (-> new-proj-data first :project/name),
                                               :project/dir dir}]})
         (log/info "Created project database" dir)))
     (throw (ex-info "Project database must provide :proj/name" {:trans-map trans-map}))))

(defn init-db-cfgs
  "Set sys-db-cfg atoms for system db and the template for the proj-base-cfg (:path-base).
   Recreate the system database if sys-db-cfg.rebuild-db? = true."
  []
  (let [base-dir (or (-> (System/getenv) (get "SCHEDULING_TBD_DB"))
                     (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases."
                                          "\nCreate directories 'projects' and 'system' under it.") {})))]
    (reset! proj-base-cfg {:store {:backend :file :path-base (str base-dir "/projects/")}
                           :schema-flexibility :write})

    (reset! sys-db-cfg {:store {:backend :file :path (str base-dir "/system")}
                        :rebuild-db? true ; <==================
                        :schema-flexibility :write})
    (when (-> sys-db-cfg deref :rebuild-db?) (create-sys-db!))
    {:sys-cfg @sys-db-cfg
     :proj-base @proj-base-cfg}))


(defstate database-cfgs
  :start (init-db-cfgs))
