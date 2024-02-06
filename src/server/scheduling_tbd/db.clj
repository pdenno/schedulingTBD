(ns scheduling-tbd.db
  "System and project database schemas and database initialization."
  (:require
   [clojure.core :as c]
   [clojure.edn  :as edn]
   [clojure.instant]
   [clojure.java.io              :as io]
   [clojure.pprint       :refer [pprint]]
   [clojure.spec.alpha           :as s]
   [clojure.string               :as str]
   [datahike.api                 :as d]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.shop :as shop :refer [db-schema-shop2+]]
   [scheduling-tbd.util  :as util :refer [now]]
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
        :doc "the value 'SYSTEM' to represent a single object holding data such as the current project name."}
   :system/current-project-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword,
        :doc "a keyword naming the current project; set by user using UI, it is one of the :project/id values in this DB."}})

(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db."
  {;; ---------------------- message
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
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a lowercase kebab-case keyword naming a project; unique to the project."}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "4 words or so describing the project; e.g. 'craft brewing production scheduling'"}
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
   :summary/next-msg-id ; ToDo: Is this worthwhile? datalog can do this!
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

(def diag (atom nil))

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

(defn project-exists?
  [proj-id]
  (assert (keyword? proj-id))
  (d/q '[:find ?e .
         :in $ ?proj-id
         :where [?e :project/id ?proj-id]]
       @(connect-atm :system) proj-id))

(defn set-current-project
  "Get the current project from the system database."
  [proj-id & {:keys [check-exists?] :or {check-exists? true}}]
  (when check-exists? (project-exists? proj-id))
  (d/transact (connect-atm :system)
              [{:system/name "SYSTEM" ; Singleton
                :system/current-project-id proj-id}]))

(defn list-projects
  "Return a vector of maps describing each project known by the system DB."
  []
  (d/q '[:find ?proj-id ?proj-dir
         :keys project/id :project/dir
         :where
         [?e :project/id  ?proj-id]
         [?e :project/dir ?proj-dir]]
       @(connect-atm :system)))

;;; ----------------------- Backup and recover project and system DB ---------------------
(defn backup-proj-db
  [id & {:keys [target-dir] :or {target-dir "data/projects/"}}]
  (let [conn-atm (connect-atm id)
        filename (str target-dir (name id) ".edn")
        s (with-out-str
            (println "[")
            (doseq [ent-id  (sutil/root-entities conn-atm)]
              (let [obj (sutil/resolve-db-id {:db/id ent-id} conn-atm #{:db/id})]
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
                  (let [obj (sutil/resolve-db-id {:db/id ent-id} conn-atm #{:db/id})]
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
    (do (log/info "Recreating the system database.")
        (when (d/database-exists? @sys-db-cfg) (d/delete-database @sys-db-cfg))
        (d/create-database @sys-db-cfg)
        (register-db :system @sys-db-cfg)
        (let [conn (connect-atm :system)]
          (d/transact conn db-schema-sys)
          (d/transact conn (-> "data/system-db.edn" slurp edn/read-string)))
        true)
    (log/error "Not recreating system DB: No backup file.")))

(defn recreate-project-db!
  "Recreate a DB for each project using EDN files."
  [id]
  (let [backup-file (format "data/projects/%s.edn" (name id))]
    (if (.exists (io/file backup-file))
      (let [cfg (-> @proj-base-cfg (assoc-in [:store :path]
                                             (str (-> @proj-base-cfg :store :base-path)
                                                  (name id))))]
        (when (d/database-exists? cfg) (d/delete-database cfg))
        (d/create-database cfg)
        (register-db id cfg)
        (let [conn (connect-atm id)]
          (d/transact conn db-schema-proj)
          (d/transact conn (->> backup-file slurp edn/read-string)))
        )
        (log/error "Not recreating DB because backup file does not exist:" backup-file)))
     )
    
(defn recreate-project-dbs!
  "Recreate a DB for each project using EDN files."
  []
  (log/info "Recreating project databases.")
  (doseq [proj (list-projects)]
    (recreate-project-db! (:project/id proj))))

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

(defn next-msg-id
  "Return the next message ID in the project's DB."
  [project-id]
  (if-let [conn (connect-atm project-id)]
    (d/q '[:find ?next-id .
           :where
           [?e :summary/name "SUMMARY"]
           [?e :summary/next-msg-id ?next-id]]
         @conn)
    (log/info "Project does not exist" {:id project-id})))

(defn inc-msg-id!
  "Increment the next message ID in the project's DB."
  [project-id]
  (let [id (next-msg-id project-id)
        conn (connect-atm project-id)]
    (d/transact conn [{:summary/name "SUMMARY"
                       :summary/next-msg-id (inc id)}])))

(defn message-form
  "Create a map that looks like the messages that are stored in the DB and communicated to the web app.
   Assumes just one msg-text."
  [msg-id from msg-text]
  {:message/id msg-id
   :message/from from
   :message/content [{:msg-text/string msg-text}]
   :message/time (-> (LocalDateTime/now)
                     str
                     clojure.instant/read-instant-date)})
(defn add-msg
  "Create a message object and add it to the database with :project/id = id."
  [project-id msg-text from]
  (if-let [conn (connect-atm project-id)] ; ToDo the then part (and thus the if) should not be necessary.
    (let [msg-id (d/q '[:find ?next-id .
                        :where
                        [?e :summary/name "SUMMARY"]
                        [?e :summary/next-msg-id ?next-id]]
                      @conn)
          msg (message-form msg-id from msg-text)]
      (d/transact conn
                  [msg
                   {:summary/name "SUMMARY"
                    :summary/next-msg-id (inc msg-id)}])
      msg)
    (log/info "Project does not exist" {:id project-id})))

(def intro-prompt
  "This is the DB form of the first message of a conversation."
  #:message{:from :system,
            :id 0,
            :content [{:msg-text/string "Describe your scheduling problem in a few sentences or "}
                      {:msg-link/uri "http://localhost:3300/learn-more"
                       :msg-link/text "learn more about how this works"}
                      {:msg-text/string "."}]})

(s/def ::project-info (s/keys :req [:project/id :project/name]
                              :opt [:segment/challenge-intro])) ; Used for HIM only.

;;; BTW, I don't have a good way to delete the project yet from the system-db.
;;;    1) (db/backup-system-db)
;;;    2) Edit the .edn to remove the project.
;;;    3) (db/recreate-system-db!)
(defn create-proj-db!
  "Create a project database for the argument project."
  [proj-info]
  (if (s/valid? ::project-info proj-info)
    (let [challenge-intro (:segment/challenge-intro proj-info) ; This is a HIM thing.
          {:project/keys [id name] :as new-proj-info}
          (if-not challenge-intro (unique-proj proj-info) proj-info) ; we'll force overwrite of HIM projects.
          dir (str (-> @proj-base-cfg :store :base-path) (clojure.core/name id))]
      (when-not (-> dir java.io.File. .isDirectory) (-> dir .java.ioFile. .mkdir))
      ;; ToDo: :project/id is unique. Don't wipe out an existing project. User could be starting over. Maybe add a number.
      (let [proj-cfg (assoc @proj-base-cfg :store {:backend :file :path dir})] ; drops :base-path too.
        (when (d/database-exists? proj-cfg) (d/delete-database proj-cfg))
        (d/create-database proj-cfg)
        (register-db id proj-cfg))
      ;; Add to project db
      (let [conn (connect-atm id)]
        (d/transact conn db-schema-proj)
        (d/transact conn {:tx-data [{:summary/name "SUMMARY"
                                     :summary/next-msg-id 2} ; 2 if challenge-intro, no problem if not.
                                    {:project/id id
                                     :project/name name}
                                    (-> intro-prompt
                                        (assoc :message/time (now))
                                        (assoc :message/id 1))]})
        (when challenge-intro (add-msg id challenge-intro :user))
        (add-msg id (format "Great! We'll call your project '%s'." name) :system)
        ;; Add knowledge of this project to the system db.
        (d/transact (connect-atm :system)
                    {:tx-data [{:system/name "SYSTEM"
                                :system/current-project-id id}
                               {:project/id   id
                                :project/name name
                                :project/dir dir}]})
        (log/info "Created project database for" (:project/name new-proj-info))
        true))
    (throw (ex-info "Project database must provide :proj/name and :proj/id"
                    {:proj-info proj-info}))))

;;; -------------------- Starting and stopping -------------------------
(defn register-project-dbs
  "Make a config for each project and register it."
  []
  (doseq [{:project/keys [id dir]} (list-projects)]
    (log/info id dir)
    (register-db id {:store {:backend :file :path dir
                             :keep-history? false
                             :schema-flexibility :write}})))

(defn init-db-cfgs
  "Set sys-db-cfg atoms for system db and the template for the proj-base-cfg (:base-path).
   Recreate the system database if sys-db-cfg.recreate-db? = true."
  []
  (let [base-dir (or (-> (System/getenv) (get "SCHEDULING_TBD_DB")) ; "/opt/scheduling" typically.
                     (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases."
                                          "\nCreate directories 'projects' and 'system' under it.") {})))]
    ;; https://cljdoc.org/d/io.replikativ/datahike/0.6.1545/doc/datahike-database-configuration
    (reset! proj-base-cfg {:store {:backend :file :base-path (str base-dir "/projects/")}
                           :schema-flexibility :write})
    (reset! sys-db-cfg {:store {:backend :file :path (str base-dir "/system")}
                        :keep-history? false
                        ;:attribute-refs? true ; With this I can't transact lookup-refs!
                        :recreate-dbs? false ; <=== If true, it will recreate the system DB and project directories too. This SHOULD be true the first time you run this, or it won't run!
                        :schema-flexibility :write})
    (register-db :system @sys-db-cfg)
    (when (-> sys-db-cfg deref :recreate-dbs?)
      (recreate-system-db!)
      (recreate-project-dbs!))
    (register-project-dbs)
    {:sys-cfg @sys-db-cfg
     :proj-base @proj-base-cfg}))

(defstate database-cfgs
  :start (init-db-cfgs))
