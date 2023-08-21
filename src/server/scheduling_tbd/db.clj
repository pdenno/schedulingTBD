(ns scheduling-tbd.db
  "System and project database schemas and database initialization."
  (:require
   [clojure.core :as c]
   [clojure.instant]
   [clojure.spec.alpha           :as s]
   [clojure.string               :as str]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.shop :as shop :refer [db-schema-shop2+]]
   [scheduling-tbd.sutil :as sutil :refer [register-db connect-atm]]
   [taoensso.timbre :as log])
  (:import
   java.time.LocalDateTime))

(def db-schema-sys+
  "Defines content that manages project DBs and their analysis including:
     - The project's name and db directory
     - Planning domains, methods, operators, and axioms"
  {;; ---------------------- project
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword, :unique :db.unique/identity
        :doc "a keyword matching the one in the same named property of a project database"}
   :project/dir
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "a string naming a subdirectory containing a project."}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string, same as the :project/name in the project's DB.."}
;;; ---------------------- system
   :system/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "the value 'SYSTEM' to represent a single objectin holding data such as the current project name."}
   :system/current-project-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword,
        :doc "a keyword naming the current project; set by user using UI, it is one of the :project/id values in this DB."}
   :system/initial-prompt
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string,
        :doc "The prompt that starts a conversation."}})

(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db."
  {;; ---------------------- message
   :message/from
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The agent, either :user or :system, issuing the message."}
   :message/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long :unique :db.unique/identity
        :doc "The unique ID of a message. These are sequential natural numbers starting at 0."}
   :message/text
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The text of the message."}
   :message/time
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "The time at which the message was sent."}

   ;; ---------------------- project
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

   ;; ---------------------- summary
   :summary/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "the value 'SUMMARY'. This is used to keep information about the state of the conversation."}
   :summary/next-msg-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc "The ID (a natural number) to be assigned to the next message written (either side of conversation)."}

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

(def db-schema-sys  (datahike-schema (merge  db-schema-shop2+ db-schema-sys+)))
(def db-schema-proj (datahike-schema db-schema-proj+))

;;; Atom for configuration map used for connecting to the system db.
;;; It is set by alter-var-root code in this namespace."
(defonce sys-db-cfg (atom nil))

;;; The Datahike datatypes are as follows:
;;; #{:db.type/instant :db.type/boolean :db.type/uuid _:db.type/value_ :db.type/string :db.type/keyword :db.type/number
;;;   :db.type/ref :db.type/bigdec :db.type/float :db.type/bigint :db.type/double :db.type/long :db.type/symbol}
(def initial-prompt
  "The prompt that starts a conversation. This variable might go away as we add authoring capability."
  "[\"Describe your scheduling problem in a few sentences or \"
    {:link-info {:href \"http://localhost:3300/learn-more\"
                 :text \"learn more about how this works\"}}
   \".\"]")

(defn create-sys-db!
  "Create the system database."
  []
  (when (:rebuild-db? @sys-db-cfg)
    (log/info "Recreating the system database.")
    (when (d/database-exists? @sys-db-cfg) (d/delete-database @sys-db-cfg))
    (d/create-database @sys-db-cfg)
    (register-db :system @sys-db-cfg)
    (let [conn (connect-atm :system)]
      (d/transact conn db-schema-sys)
      (d/transact conn [{:system/name "SYSTEM" ; Singleton
                         :system/initial-prompt initial-prompt}])
      conn)))

;;; Atom for the configuration map used for connecting to the project db.
(defonce proj-base-cfg (atom nil))

(defn current-project-id
  "Get the current project from the system database."
  []
  (d/q '[:find ?proj .
         :where
         [?entity :system/name "SYSTEM"]
         [?entity :system/current-project-id ?proj]]
       @(connect-atm :system)))

(defn set-current-project
  "Get the current project from the system database."
  [id]
  (d/transact (connect-atm :system)
              [{:system/name "SYSTEM" ; Singleton
                :system/current-project-id id}]))

(defn list-projects
  "List all the projects known by the system DB."
  []
  (d/q '[:find [?proj-id ...]
         :where [_ :project/id ?proj-id]]
       @(connect-atm :system)))

(def diag (atom nil))

(defn unique-proj
  "If necessary to ensure uniqueness, update the project name and id."
  [proj-info]
  (let [names (d/q '[:find [?name ...]
                      :where [_ :project/name ?name]] @(connect-atm :system))
        name (:project/name proj-info)]
    (if (not-any? #(= name %) names)
      proj-info
      (let [pat (re-pattern (str "^" name "( \\d+)?"))
            nums (map #(let [[success num] (re-matches pat %)]
                         (when success (or num "0"))) names)
            num (->> nums (map read-string) (apply max) inc)
            new-name (str name " " num)
            new-id   (-> new-name (str/replace #"\s+" "-") keyword)]
        (-> proj-info
            (assoc :project/name new-name)
            (assoc :project/id new-id))))))

(s/def ::project-db (s/keys :req [:project/id :project/name]))
(defn create-proj-db!
  "Create a project database for the argument project."
  [proj-info]
  (reset! diag proj-info)
  (if (s/valid? ::project-db proj-info)
    (let [proj-info (unique-proj proj-info)
          id (:project/id proj-info)
          dir (str (-> @proj-base-cfg :store :path-base) (name id))]
       (.mkdir (java.io.File. dir))
       ;; ToDo: :project/id is unique. Don't wipe out an existing project. User could be starting over. Maybe add a number.
       (let [db-cfg (assoc-in @proj-base-cfg [:store :path] dir)]
         (d/create-database db-cfg)
         (register-db id db-cfg))
       (let [conn (connect-atm id)]
         (d/transact conn db-schema-proj)
         (d/transact conn {:tx-data [proj-info
                                     {:summary/name "SUMMARY"
                                      :summary/next-msg-id 0}]})
         ;; Add knowledge of this project to the system db.
         (d/transact (connect-atm :system)
                     {:tx-data [{:system/name "SYSTEM"
                                 :system/current-project-id id}
                                {:project/id   (:project/id   proj-info)
                                 :project/name (:project/name proj-info)
                                 :project/dir dir}]})
         (log/info "Created project database" dir "for" (:project/name proj-info))
       proj-info))
     (throw (ex-info "Project database must provide :proj/name and :proj/id"
                     {:proj-info proj-info}))))

(defn add-msg
  "Create a message object and add it to the database with :project/id = id."
  [id msg-text from]
  (if-let [conn (connect-atm id)] ; ToDo the then part (and thus the if) should not be necessary.
    (let [msg-id (d/q '[:find ?next-id .
                        :where
                        [?e :summary/name "SUMMARY"]
                        [?e :summary/next-msg-id ?next-id]]
                      @conn)
          msg (-> {}
                  (assoc :message/id msg-id)
                  (assoc :message/from from)
                  (assoc :message/time (-> (LocalDateTime/now)
                                           str
                                           clojure.instant/read-instant-date ))
                  (assoc :message/text msg-text))]
      (d/transact conn
                  [msg
                   {:summary/name "SUMMARY"
                    :summary/next-msg-id (inc msg-id)}])
      msg)
    (log/info "Project does not exist" {:id id})))

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
                        :rebuild-db? true ; <================== Currently, if you rebuild, you need to get rid of project directories.
                        :schema-flexibility :write})
    (when (-> sys-db-cfg deref :rebuild-db?) (create-sys-db!))
    (log/info "Existing projects:" (list-projects))
    {:sys-cfg @sys-db-cfg
     :proj-base @proj-base-cfg}))

(defstate database-cfgs
  :start (init-db-cfgs))
