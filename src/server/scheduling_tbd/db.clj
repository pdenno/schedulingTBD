(ns scheduling-tbd.db
  "System and project database schemas and database initialization.
   There are other databases, see for example, him.clj."
  (:require
   [clojure.core :as c]
   [clojure.edn :as edn]
   [clojure.instant]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [datahike.api :as d]
   [datahike.pull-api :as dp]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.specs :as specs]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm datahike-schema db-cfg-map register-db resolve-db-id]]
   [scheduling-tbd.util :as util :refer [now]]
   [taoensso.telemere :refer [log!]]))

(def db-schema-agent+
  "Defines properties that can be used for agents, which can be stored either in the system DB or a project DB.
   This common information is merged into other system and project schema."
  {:agent/agent-id ; Yeah a redundant name, but I think it makes things easier in agent_db.clj, where I strip off the ns.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A unique ID for each agent to ensure only one of each type is available."}
   :agent/agent-type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "Currently one of #{:system :project :shared-assistant}."}
   :agent/assistant-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant id (a string) associated with this surrogate."}
   :agent/base-type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "Indicates the purpose and instructions given."}
   :agent/expertise
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of what the agent is good at (used in surrogates, at least)."}
   :agent/instruction-path
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "as it is used by OpenAI circa 2025."}
   :agent/instruction-string
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "This is only used with surrogates, it is the system instruction verbatim. Idea is I can't run SUR: for things like a music school."}
   :agent/llm-provider
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "Currently either :openai or :azure."}
   :agent/model-class
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "One of the classes of LLM model defined in the system. See llm.clj."}
   :agent/pid
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "This is used to distinguish runnable agents affiliated with projects from their templates in the system DB."}
   :agent/response-format-path
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "as it is used by OpenAI circa 2025."}
   :agent/surrogate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "True if the agent is a human surrogate."}
   :agent/thread-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant thread (a string) uniquely identifying the thread on which this surrogate operates."}
   :agent/timestamp
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "The time at which the agent was created."}
   :agent/tools
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "as it is used by OpenAI circa 2025."}
   :agent/vector-store-paths
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string
        :doc "as it is used by OpenAI circa 2025."}})

(def db-schema-sys+
  "Defines content that manages project DBs and their analysis including:
     - The project's name and db directory
     - system-level agents
     - See also db-schema-agent+ which gets merged into this."
  {;; ------------------------------- EADS
   :EADS/budget-decrement
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/double
        :doc "The cost of each question, decrementing againt a budget for the entire conversation."}
   :EADS/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A unique ID for each EADS. The namespace of the keyword is the cid, e.g. :process/flow-shop."}
   :EADS/msg-str
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The stringified message object, it can be edn/read-string. It is the EDN version of the JSON in resources used by ork."}
   :EADS/can-produce-visuals
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "Indicates the purpose and instructions given."}
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
   :system/agents
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "an agent (OpenAI Assistant) that outputs a vector of clojure maps in response to queries."}
   :system/default-project-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword,
        :doc "a keyword providing a project-id clients get when starting up."}
   :system/EADS
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "EADS objects"}
   :system/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "the value 'SYSTEM' to represent a single object holding data such as the current project name."}
   :system/projects
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "the projects known by the system."}
   :system/specs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "spec objects used for checking completion of EADS, etc."}})

;;;========================================================== Project DBs ==========================================
(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db.
   See also db-schema-agent+ which gets merged into this."
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

   ;; ---------------------- claim (something believed owing to what users said in interviews)
   :claim/string
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc (str "a stringified fact (in predicate calculus) about the project, similar in concept to planning state fact in the earlier design.\n"
                  "For example, (:process/production-motivation make-to-stock).")}
   :claim/conversation-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The conversation from which this claim is founded. Currently a cid."}
   :claim/question-type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The question-type (if any) on which this claim is founded."}
   :claim/confidence
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc "a number [0,1] expressing how strongly we believe the proposition ."}

   ;; ---------------------- conversation
   :conversation/active-EADS-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc (str "The id of the EADS instructions which is currently being pursued in this conversation.\n"
                  "If the conversation doesn't have one, functions such as ork/get-EADS-id can determine it using the project's orchestrator agent."
                  "Though only one EADS can truely be active at a time, we index the active-EADS-id by [pid, cid] because other conversations "
                  "could still need work. See also :project/active-EADS-id")}
   :conversation/status
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc (str "A keyword that specifies the status of the conversation. Thus far, the only values are :eads-exhausted :in-progress, and "
                  ":not-started. :in-progress only means that this conversation can be pursued further. For this conversation to be the one "
                  "currently being pursued, it must also be the case that :project/active-conversation is this conversation.")}
   :conversation/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword uniquely identifying the kind of conversation; so far just #{:process :data :resources :optimality}."}
   :conversation/messages
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the messages between the interviewer and interviewees of the conversation."}

   ;; ---------------------- summary data structures
   :dstruct/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "the EADS-id uniquely identifying the summary data structure."}
   :dstruct/str
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string that can be edn/read-string to the data structure."}
   :dstruct/budget-left
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/double
        :doc "the amount of budget left for questioning."}

   ;; ---------------------- message
   :message/answers-question
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc "an optional property that refers to a :message/id of a question for which this response is deemed to be a answer."}
   :message/code
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "Code produced at this point in the conversation."}
   :message/code-execution
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "Result of running code produced at this point in the conversation."}
   :message/content
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string with optional html links."}
   :message/EADS-data-structure
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string that can be edn/read-string into an EADS data structure inferred from conversation so far."}
   :message/from
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The agent issuing the message, #{:human :surrogate :system}."}
   :message/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc (str "The unique ID of a message. These are natural numbers starting at 0, but owing to 'LLM:' prompts,\n"
                  "which aren't stored, some values can be skipped. Because these are not unique to the DB, they are not :db.unique/identity.")}
   :message/pursuing-EADS
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The EADS being pursued by this question or answer, if any."}
   :message/question-type
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "A label (from interview instructions) associated with this question or answer."}
   :message/tags
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "Optional keywords used to classify the message."}
   :message/table
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An optional table that that is the response, or part of the response of a user."}
   :message/time
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "The time at which the message was sent."}

   ;; ---------------------- project -- the top-level object in the db/file.
   :project/active-conversation
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc (str "The conversation most recently busy, #{:process...}. Note that several conversations can still need work, and there can "
                  "be an :conversation/active-EADS-id on several, however, this is the conversation to start if coming back to the project.")}
   :project/active-EADS-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The most recently purused EADS in the project. See also :conversation/active-EADS-id."}
   :project/agents
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "an agent (OpenAI Assistant, etc.) that outputs a vector of clojure maps in response to queries."}
   :project/claims
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the collection of things we believe about the project as logical statements."}
   :project/conversations
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "The conversations of this project."}

   :project/desc ; ToDo: If we keep this at all, it would be an annotation on an ordinary :message/content.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "the original paragraph written by the user describing what she/he wants done."}
   :project/summary-dstructs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "'summary' data structures indexed by their EADS-id, :dstruct/id."}
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a lowercase kebab-case keyword naming a project; unique to the project."}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "4 words or so describing the project; e.g. 'craft brewing production scheduling'"}
   :project/ork-aid
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The orchestrator agent id (OpenAI notion)."}
   :project/ork-tid
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc (str "The thread-id of orchestrator agent.\n"
                  "When this does not match the current ork-agent, "
                  "the agent needs a more expansive CONVERSATION-HISTORY message for the conversation.")}
   :project/interviewer-tid ; ToDo: This hasn't been implemented yet, I think. The ork one has. See ork/get-new-EADS-id.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc (str "The thread-id of the current interviewer agent.\n"
                  "When this does not match the current intervierwer agent tid, "
                  "the agent needs a more expansive CONVERSATION-HISTORY message for the conversation.")}
   :project/surrogate
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the project's surrogate object, if any."}
   :project/surrogate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "true if domain expertise is provided by an artificial agent."}
   :project/tables
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "true if domain expertise is provided by an artificial agent."}})

(def ^:diag diag (atom nil))
(def db-schema-sys (-> db-schema-sys+ (merge db-schema-agent+) datahike-schema))
(def db-schema-proj (-> db-schema-proj+ (merge db-schema-agent+) datahike-schema))
(def project-schema-key? (-> db-schema-proj+ (merge db-schema-agent+) keys set))

;;; ------------------------------------------------- projects ----------------------------------------------------
(defn project-exists?
  "If a project with argument :project/id (a keyword) exists, return the root entity ID of the project
   (the entity id of the map containing :project/id in the database named by the argumen project-id)."
  [pid]
  (assert (keyword? pid))
  (when (some #(= % pid) (sutil/db-ids)) ; (sutil/db-ids) includes non-project IDs.
    (d/q '[:find ?e .
           :in $ ?pid
           :where
           [?e :project/id ?pid]]
         @(connect-atm pid) pid)))

(defn get-project-name [pid]
  (when-let [eid (project-exists? pid)]
    (-> (dp/pull @(connect-atm pid) '[:project/name] eid) :project/name)))

(defn ^:admin get-project
  "Return the project structure.
   Throw an error if :error is true (default) and project does not exist."
  [pid & {:keys [drop-set error?]
          :or {drop-set #{:db/id} error? true}}]
  (let [conn (connect-atm pid :error? error?)]
    (when-let [eid (project-exists? pid)]
      (resolve-db-id {:db/id eid} conn :drop-set drop-set))))

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
       (throw (ex-info "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases." {})))
     ;; Otherwise we list using system db. These are the 'legitmate' projects in the project datebase
     (-> (d/q '[:find [?proj-id ...]
                :where
                [?e :project/id ?proj-id]]
              @(connect-atm :system))
         sort
         vec))))

(defn clean-project-for-schema
  "Remove attributes that are no longer in the schema.
   Remove nil values, these can show up after doing a :db/retract."
  [proj]
  (letfn [(cpfs [x]
            (cond (map? x) (reduce-kv (fn [m k v]
                                        (if (project-schema-key? k)
                                          (if (nil? v) m (assoc m k (cpfs v)))
                                          (do (log! :warn (str "Dropping obsolete attr: " k)) m)))
                                      {} x)
                  (vector? x) (->> (mapv cpfs x) (remove nil?) vec)
                  :else x))]
    (cpfs proj)))

(defn backup-project-db
  [id & {:keys [target-dir clean?] :or {target-dir "data/projects/" clean? true}}]
  (let [filename (str target-dir (name id) ".edn")
        proj (cond-> (get-project id)
               clean? clean-project-for-schema)
        s (with-out-str
            (println "[")
            (pprint proj)
            (println "]"))]
    (log! :info (str "Writing project to " filename))
    (spit filename s)))

(defn ^:admin backup-project-dbs
  "Backup the project databases one each to edn files. This will overwrite same-named files in tar-dir.
   Example usage: (backup-proj-dbs)."
  [& {:keys [target-dir] :or {target-dir "data/projects/"}}]
  (doseq [id (list-projects)]
    (backup-project-db id {:target-dir target-dir})))

(defn ^:admin unknown-projects
  "Return a vector of directories that the system DB does not know."
  []
  (set/difference
   (set (list-projects {:from-storage? true}))
   (set (list-projects))))

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
            new-id (-> new-name str/lower-case (str/replace #"\s+" "-") keyword)]
        (-> proj-info
            (assoc :project/name new-name)
            (assoc :project/id new-id))))))

(defn add-project-to-system
  "Add the argument project (a db-cfg map) to the system database."
  [id project-name dir]
  (let [conn-atm (connect-atm :system)
        eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn-atm)]
    (d/transact conn-atm {:tx-data [{:db/id eid
                                     :system/projects {:project/id id
                                                       :project/name project-name
                                                       :project/dir dir}}]})))

(s/def ::project-info (s/keys :req [:project/id :project/name]))

(def conversation-defaults
  [{:conversation/id :process
    :conversation/status :in-progress} ; We assume things start here.
   {:conversation/id :data
    :conversation/status :not-started}
   {:conversation/id :resources
    :conversation/status :not-started}
   {:conversation/id :optimality
    :conversation/status :not-started}])

(declare add-conversation-intros get-system)

;;; (db/create-proj-db! {:project/id :test :project/name "Test Project"} {} {:force-this-name? true})
(defn create-proj-db!
  "Create a project database for the argument project.
   The project-info map must include :project/id and :project/name.
     proj-info  - map containing at least :project/id and :project/name.
     additional - a vector of maps to add to the database.
     opts -  {:force-this-name? - overwrite project with same name}
   This always destroys DBs with the same name as that calculated here.
   Sets claims for project-id  to calculated PID and project-name.
   Return the PID of the project, which may be different than the project/id argument
   owing to the need for PIDs to be unique in the context of all projects managed by the system DB."
  ([proj-info] (create-proj-db! proj-info {} {}))
  ([proj-info additional-info] (create-proj-db! proj-info additional-info {}))
  ([proj-info additional-info {:keys [in-mem? force-this-name?] :as _opts}]
   (s/assert ::project-info proj-info)
   (let [{id :project/id pname :project/name} (if force-this-name? proj-info (unique-proj proj-info))
         cfg (db-cfg-map {:type :project :id id :in-mem? in-mem?})
         dir (-> cfg :store :path)
         files-dir (-> cfg :base-dir (str "/projects/" pname "/files"))]
     (when-not in-mem?
       (when-not (-> dir io/as-file .isDirectory)
         (-> cfg :store :path io/make-parents)
         (-> cfg :store :path io/as-file .mkdir))
       (when-not (-> files-dir io/as-file .isDirectory)
         (io/make-parents files-dir)
         (-> files-dir io/as-file .mkdir))
       (add-project-to-system id pname dir))
     (when (d/database-exists? cfg) (d/delete-database cfg))
     (d/create-database cfg)
     (register-db id cfg)
     ;; Add to project db
     (d/transact (connect-atm id) db-schema-proj)
     (d/transact (connect-atm id) {:tx-data [{:project/id id
                                              :project/name pname
                                              :project/agents (->> (get-system)
                                                                   :system/agents
                                                                   (filterv #(= (:agent/agent-type %) :shared-assistant))
                                                                   (mapv #(assoc % :agent/pid id)))
                                              :project/active-conversation :process
                                              :project/claims [{:claim/string (str `(~'project-id ~id))}
                                                               {:claim/string (str `(~'project-name ~id ~pname))}]
                                              :project/conversations conversation-defaults}]})
     (add-conversation-intros id)
     (when (not-empty additional-info)
       (d/transact (connect-atm id) additional-info))
     ;; Add knowledge of this project to the system db.
     (log! :info (str "Created project database for " id))
     id)))

(defn ^:admin delete-project!
  "Remove project from the system."
  [pid]
  (if (some #(= % pid) (list-projects))
    (let [conn-atm (connect-atm :system)]
      (when-let [s-eid (d/q '[:find ?e . :in $ ?pid :where [?e :project/id ?pid]] @conn-atm pid)]
        (let [obj (resolve-db-id {:db/id s-eid} conn-atm)]
          (d/transact (connect-atm :system) {:tx-data (for [[k v] obj] [:db/retract s-eid k v])})
          (sutil/deregister-db pid)
          nil)))
    (log! :warn (str "Delete-project: Project not found: " pid))))

(defn recreate-project-db!
  "Recreate a DB for each project using EDN files."
  ([id] (recreate-project-db! id nil))
  ([id content]
   (let [backup-file (format "data/projects/%s.edn" (name id))]
     (if (or content (.exists (io/file backup-file)))
       (let [cfg (db-cfg-map {:type :project :id id})
             files-dir (-> cfg :base-dir (str "/projects/" (name id) "/files"))]
         (when (and (-> cfg :store :path io/as-file .isDirectory) (d/database-exists? cfg))
           (d/delete-database cfg))
         (when-not (-> cfg :store :path io/as-file .isDirectory)
           (-> cfg :store :path io/make-parents)
           (-> cfg :store :path io/as-file .mkdir))
         (d/create-database cfg)
         (register-db id cfg)
         (let [conn (connect-atm id)
               content (if content
                         (vector content)
                         (->> backup-file slurp edn/read-string))]
           (d/transact conn db-schema-proj)
           (d/transact conn content))
         (when-not (-> files-dir io/as-file .isDirectory)
           (-> files-dir io/as-file .mkdir))
         cfg)
       (log! :error (str "Not recreating DB because backup file does not exist: " backup-file))))))

(defn ^:admin update-project-for-schema!
  "This has the ADDITIONAL side-effect of writing a backup file."
  [pid]
  (backup-project-db pid)
  (recreate-project-db! pid))

(defn ^:admin update-all-projects-for-schema!
  []
  (doseq [p (list-projects)]
    (update-project-for-schema! p)))

(defn get-active-cid
  "Get what the DB asserts is the project's current conversation CID, or :process if it doesn't have a value."
  [pid]
  (or (d/q '[:find ?cid . :where [_ :project/active-conversation ?cid]] @(connect-atm pid))
      :process))

(defn put-active-cid!
  [pid cid]
  (assert (#{:process :data :resources :optimality} cid))
  (if-let [eid (project-exists? pid)]
    (d/transact (connect-atm pid) {:tx-data [{:db/id eid
                                              :project/active-conversation cid}]})
    (log! :error "Could not put-active-cid!")))

(defn get-project-active-EADS-id
  [pid]
  (d/q '[:find ?eads-id .
         :where
         [_ :project/active-EADS-id ?eads-id]]
       @(connect-atm pid)))

(declare system-EADS?)

(defn put-project-active-EADS-id!
  [pid eads-id]
  (assert ((system-EADS?) eads-id))
  (if-let [eid (project-exists? pid)]
    (d/transact (connect-atm pid) {:tx-data [{:db/id eid
                                              :project/active-EADS-id eads-id}]})
    (log! :error "Project does not exist.")))

;;; --------------------------------------- System db -----------------------------------------------------------
(defn get-system
  "Return the project structure.
   Throw an error if :error is true (default) and project does not exist."
  []
  (let [conn-atm (connect-atm :system)]
    (when-let [eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn-atm)]
      (resolve-db-id {:db/id eid} conn-atm))))

(defn ^:admin backup-system-db
  "Backup the system database to an edn file."
  [& {:keys [target-dir] :or {target-dir "data/"}}]
  (let [conn-atm (connect-atm :system)
        filename (str target-dir "system-db.edn")
        s (with-out-str
            (println "[")
            (doseq [ent-id (sutil/root-entities conn-atm)]
              (let [obj (resolve-db-id {:db/id ent-id} conn-atm #{:db/id})]
                    ;; Write content except schema elements and transaction markers.
                (when-not (and (map? obj) (or (contains? obj :db/ident) (contains? obj :db/txInstant)))
                  (pprint obj)
                  (println))))
            (println "]"))]
    (log! :info (str "Writing system DB to " filename))
    (spit filename s)))

(defn recreate-system-db!
  "Recreate the system database from an EDN file
   By default this creates new agents."
  [& {:keys [target-dir]
      :or {target-dir "data/"}}]
  (if (.exists (io/file (str target-dir "system-db.edn")))
    (let [cfg (db-cfg-map {:type :system})]
      (log! :info "Recreating the system database.")
      (when (d/database-exists? cfg) (d/delete-database cfg))
      (d/create-database cfg)
      (register-db :system cfg)
      (let [conn (connect-atm :system)]
        (d/transact conn db-schema-sys)
        (d/transact conn (-> "data/system-db.edn" slurp edn/read-string))
        cfg))
    (log! :error "Not recreating system DB: No backup file.")))

(def keep-db? #{:him})
(defn ^:admin recreate-dbs!
  "Recreate the system DB on storage from backup.
   For each project it lists, recreate it from backup if such backup exists."
  []
  (swap! sutil/databases-atm
         #(reduce-kv (fn [res k v] (if (keep-db? k) (assoc res k v) res)) {} %))
  (recreate-system-db!)
  (log! :info (str "Recreating these projects: " (list-projects)))
  (doseq [pid (list-projects)]
    (recreate-project-db! pid)))

;;; ------------------ agents -----------------------------------------------------------------
;;; Agent-id is always unique, even when have multiple llm-providers, but usually you want to refer
;;; to it 'conceptually' as a system agent or project agent by the default llm provider.
(s/def ::agent-id (s/or :system-agent keyword? :project-agent ::agent-id-map))
(s/def ::agent-id-map (s/keys :req-un [::pid ::base-type]))

(defn agent-exists?
  "Return the eid of the argument agent-id (a keyword) if the agent exists, nil otherwise.
   If the id argument is a keyword it names the :agent/base-type. If id is a map, the map provides :base-type.
   Since the function doesn't actually search for :agent/agent-id, use the optional second argument, llm-provider,
   (a keyword) if you want the agent from some llm other than the sutil/default-llm-provider."
  ([agent-id] (agent-exists? agent-id @sutil/default-llm-provider))
  ([agent-id llm-provider]
   (s/valid? ::agent-id agent-id)
   (let [db-id (if (keyword? agent-id) :system (:pid agent-id))
         base-type (if (keyword? agent-id) agent-id (:base-type agent-id))]
     (d/q '[:find ?eid .
            :in $ ?base-type ?provider
            :where
            [?eid :agent/base-type ?base-type]
            [?eid :agent/llm-provider ?provider]]
          @(connect-atm db-id) base-type llm-provider))))

(defn get-agent
  "Get the argument agent, rehydrating :agent/tools.
   If error? = true and there is no such agent, generate an error message (but do not throw).
   If error? = false and there is no such agent, return {}."
  ([agent-id] (get-agent agent-id true))
  ([agent-id error?]
   (s/valid? ::agent-id agent-id)
   (let [db-id (if (keyword? agent-id) :system (:pid agent-id))]
     (if-let [eid (agent-exists? agent-id)]
       (as-> (resolve-db-id {:db/id eid} (connect-atm db-id)) ?x
         (if (contains? ?x :agent/tools)
           (update ?x :agent/tools edn/read-string)
           ?x))
       (if error?
         (log! :error (str "Agent not found: " agent-id))
         {})))))

(defn put-agent!
  "Add the argument agent object to database corresponding to the agent-id.
   Set the :agent/timestamp and llm-provider.
   This will do an update-agent of the agent already exists."
  [agent-id agent-map]
  (reset! diag agent-map)
  (s/assert ::agent-id agent-id)
  (s/assert ::specs/db-agent agent-map)
  (let [agent-map (cond-> agent-map
                    (not (contains? agent-map :agent/llm-provider)) (assoc :agent/llm-provider @sutil/default-llm-provider)
                    true (assoc :agent/timestamp (now))
                    (contains? agent-map :agent/tools) (update :agent/tools str))
        conn-atm (connect-atm (if (keyword? agent-id) :system (:pid agent-id)))
        top-level-rel (if (keyword? agent-id) :system/name :project/id)
        target-rel (if (keyword? agent-id) :system/agents :project/agents)
        eid (d/q '[:find ?eid .
                   :in $ ?rel
                   :where [?eid ?rel]]
                 @conn-atm top-level-rel)]
    (if eid
      (d/transact conn-atm {:tx-data [{:db/id eid target-rel agent-map}]})
      (log! :error (str "No such DB. agent-id = " agent-id)))))

;;; ----------------------------------- Claims -------------------------------------------------------------
(defn get-claims
  "Return the planning state set (a collection of ground propositions) for the argument project, or #{} if none."
  [pid & {:keys [objects?]}]
  (let [conn @(connect-atm pid)]
    (if objects?
      (let [eids (d/q '[:find [?eid ...] :where [?eid :claim/string]] conn)]
        (for [eid eids]
          (let [{:claim/keys [string conversation-id question-type confidence]}
                (dp/pull conn '[*] eid)]
            (cond-> {:claim (edn/read-string string)}
              conversation-id (assoc :conversation-id conversation-id)
              question-type (assoc :question-type question-type)
              confidence (assoc :confidence confidence)))))
      ;; Otherwise just return predicate forms
      (if-let [facts (d/q '[:find [?s ...]
                            :where
                            [_ :project/claims ?pp]
                            [?pp :claim/string ?s]]
                          conn)]
        (-> (mapv edn/read-string facts) set)
        #{}))))

(s/def ::claim (s/keys :req-un [:claim/string :claim/cid] :opt-un [:claim/q-type :claim/confidence]))
(s/def :claim/string string?)
(s/def :claim/cid #({:process :data :resources :optimality} %))
(s/def :claim/q-type keyword?)
(s/def :claim/confidence (s/and number? (s/or :hopeless zero? :hopeful pos?)))

(defn add-claim!
  "Add the argument vector of ground proposition to state, a set. Returns true."
  [pid {:keys [string cid q-type confidence] :as claim}]
  (s/assert ::claim claim)
  (assert (s/valid? ::claim claim))
  (let [conn (connect-atm pid)
        eid (d/q '[:find ?eid . :where [?eid :project/claims]] @conn)]
    (d/transact conn {:tx-data [{:db/id eid
                                 :project/claims
                                 (cond-> {:claim/string string}
                                   cid (assoc :claim/conversation-id cid)
                                   q-type (assoc :claim/question-type q-type)
                                   confidence (assoc :claim/confidence confidence))}]}))
  true)

;;; --------------------------------------- Conversations ---------------------------------------------------------------------
(def conversation-intros
  {:process
   (str "This is where we discuss how product gets made, or in the cases of services, how the service gets delivered. "
        "It is also where we introduce MiniZinc, the <a href=\"terms/dsl\">domain specific language</a> (DSL) "
        "through which together we design a solution to your scheduling problem. "
        "You can read more about <a href=\"about/process-conversation\">how this works</a>.")
   :data
   (str "This is where we ask you to talk about the data that drives your decisions (customer orders, due dates, worker schedules,... whatever). "
        "Here you can either upload actual data as spreadsheets, or we can talk about the kinds of information you use in general terms and "
        "we can invent some similar data to run demonstrations. "
        "Whenever someone suggests that you upload information to them, you should be cautious. "
        "Read more about the intent of this conversation and the risks of uploading data <a href=\"about/uploading-data\">here</a>.")
   :resources
   (str "This is typically the third conversation we'll have, after discussing process and data. "
        "(By the way, you can always go back to a conversation and add to it.) "
        "You might have already mentioned the resources (people, machines) by which you make product or deliver services. "
        "Here we try to integrate this into the MiniZinc solution. Until we do that, we won't be able to generate realistic schedules.")
   :optimality
   (str "This is where we discuss what you intend by 'good' and 'ideal' schedules. "
        "With these we formulate an objective and model it in MiniZinc. "
        "The MiniZinc solution can change substantially owing to this discussion, but owing to all the work we did "
        "to define requirements, we think it will be successful.")})

(declare add-msg get-active-EADS-id)

(defn add-conversation-intros
  "Add an intro describing the topic and rationale of the conversation."
  [pid]
  (doseq [cid [:process :data :resources :optimality]]
    (add-msg {:pid pid :cid cid :from :system :text (get conversation-intros cid) :tags [:conversation-intro]})))

(defn conversation-exists?
  "Return the eid of the conversation if it exists."
  [pid cid]
  (assert (#{:process :data :resources :optimality} cid))
  (d/q '[:find ?eid .
         :in $ ?cid
         :where [?eid :conversation/id ?cid]]
       @(connect-atm pid) cid))

(defn get-conversation
  "For the argument project (pid) return a vector of messages sorted by their :message/id."
  [pid cid]
  (assert (#{:process :data :resources :optimality} cid))
  (if-let [eid (conversation-exists? pid cid)]
    (-> (resolve-db-id {:db/id eid} (connect-atm pid))
        (update :conversation/messages #(->> % (sort-by :message/id) vec)))
    {}))

(defn get-conversation-status
  "Returns a keyword indicating the status of the argument conversation.
   If the conversation does not have a value for :conversation/status it returns nil."
  [pid cid]
  (if-let [eid (conversation-exists? pid cid)]
    (if-let [status (d/q '[:find ?status .
                           :in $ ?eid
                           :where [?eid :conversation/status ?status]]
                         @(connect-atm pid) eid)]
      status
      ;; ToDo: This can go away once old projects go away.
      (do (log! :warn "Conversation status not set. Returning :not-started")
          :not-started))
    (log! :error (str "No such conversation: pid = " pid " cid = " cid))))

(defn put-conversation-status!
  "Set the project' s:converation/status attribute to true."
  [pid cid status]
  (assert (#{:eads-exhausted :not-started :in-progress} status))
  (if-let [eid (conversation-exists? pid cid)]
    (d/transact (connect-atm pid) {:tx-data [[:db/add eid :conversation/status status]]})
    (log! :error (str "No such conversation: pid = " pid " cid = " cid))))

;;; --------------------------------------- Messages ---------------------------------------------------------------------
(defn message-exists?
  "Return the :db/id if a message with the argument coordinates is found."
  [pid cid mid]
  (d/q '[:find ?eid .
         :in $ ?cid ?mid
         :where
         [?c :conversation/id ?cid]
         [?c :conversation/messages ?eid]
         [?eid :message/id ?mid]]
       @(connect-atm pid) cid mid))

(defn max-msg-id
  "Return the current highest message ID used in the project."
  [pid cid]
  (let [ids (->> (get-conversation pid cid) :conversation/messages (mapv :message/id))]
    (if (empty? ids) 0 (apply max ids))))

;;; See "Map forms" at https://docs.datomic.com/pro/transactions/transactions.html
;;; This is typical: You get the eid of the thing you want to add properties to.
;;; You specify that as the :db/id and then just add whatever you want for the properties.
;;; If the property is cardinality many, it will add values, not overwrite them.
(defn add-msg
  "Create a message object and add it to current conversation of the database with :project/id = id.
   Return the :message/id.
   Note that this doesn't handle :message/answers-question. That is typically done with update-msg."
  [{:keys [pid cid from text table tags question-type pursuing-EADS]}]
  (assert (keyword? cid))
  (assert (#{:system :human :surrogate :developer-injected} from))
  (assert (string? text))
  (if-let [conn (connect-atm pid)]
    (let [msg-id (inc (max-msg-id pid cid))
          pursuing-EADS (or pursuing-EADS (get-active-EADS-id pid cid))]
      (d/transact conn {:tx-data [{:db/id (conversation-exists? pid cid)
                                   :conversation/messages (cond-> #:message{:id msg-id :from from :time (now) :content text}
                                                            table (assoc :message/table table)
                                                            (not-empty tags) (assoc :message/tags tags)
                                                            question-type (assoc :message/question-type question-type)
                                                            pursuing-EADS (assoc :message/pursuing-EADS pursuing-EADS))}]})
      msg-id)
    (throw (ex-info "Could not connect to DB." {:pid pid}))))

(defn get-msg
  "Return the complete message specified by the arguments."
  [pid cid mid]
  (let [conn @(connect-atm pid)
        eid (d/q '[:find ?m-ent .
                   :in $ ?cid ?mid
                   :where
                   [?c-ent :conversation/id ?cid]
                   [?c-ent :conversation/messages ?m-ent]
                   [?m-ent :message/id ?mid]]
                 conn cid mid)]
    (dp/pull conn '[*] eid)))

(defn update-msg
  "Update the message with given info (a merge)."
  [pid cid mid {:message/keys [answers-question] :as info}]
  (let [eid (message-exists? pid cid mid)]
    (if (= mid answers-question)
      (throw (ex-info "Attempting to mark a message as answer the question it raises." {:pid pid :cid cid :mid mid}))
      (if eid
        (d/transact (connect-atm pid) {:tx-data [(merge {:db/id eid} info)]})
        (log! :warn (str "Could not find msg for update-msg: pid = " pid " cid = " cid " mid = " mid))))))

;;; ----------------------------------------- Budget ---------------------------------------------
#_(defn get-budget
    "Return the :conversation/interviewer-budget."
    [pid cid]
    (d/q '[:find ?budget .
           :in $ ?cid
           :where
           [?e :conversation/id ?cid]
           [?e :conversation/interviewer-budget ?budget]]
         @(connect-atm pid) cid))

#_(defn put-budget!
    [pid cid val]
    (let [conn-atm (connect-atm pid)
          eid (d/q '[:find ?eid .
                     :in $ ?cid
                     :where [?eid :conversation/id ?cid]] @conn-atm cid)]
      (if eid
        (d/transact conn-atm {:tx-data [{:db/id eid :conversation/interviewer-budget val}]})
        (log! :error (str "No such conversation: " cid)))))

;;; ----------------------------------------- EADS ---------------------------------------------
(defn system-EADS?
  "Return a set of EADS-ids (keywords) known to the system db. (Often used as a predicate.)"
  []
  (-> (d/q '[:find [?eads-id ...]
             :where [_ :EADS/id ?eads-id]]
           @(sutil/connect-atm :system))
      set))

(defn get-msg-dstructs
  "Return a vector of EADS data structure maps matching for the given project id and EADS-id.
   The vector returned is sorted by msg-id, so it is chronological (most recent last).
   Included in the maps are :msg-id where it was found and EADS-ref that it is about,
   which is the same as the argument eads-id."
  [pid eads-id]
  (let [db-res (d/q '[:find ?str ?msg-id
                      :keys s msg-id
                      :in $ ?eads-id
                      :where
                      [?e :message/pursuing-EADS ?eads-id]
                      [?e :message/EADS-data-structure ?str]
                      [?e :message/id ?msg-id]]
                    @(connect-atm pid) eads-id)
        dstructs (reduce (fn [r {:keys [s msg-id]}]
                           (let [{:keys [data-structure]} (edn/read-string s)]
                             (conj r (-> data-structure
                                         (assoc :msg-id msg-id)
                                         (assoc :EADS-ref eads-id)))))
                         []
                         db-res)]
    (->> dstructs (sort-by :msg-id) vec)))

(defn put-EADS-ds!
  "Attach a stringified representation of the data structure (edn) the interviewer is building to the latest message.
   The data structure should have keywords for keys at this point (not checked)."
  [pid cid ds]
  (let [max-id (max-msg-id pid cid)
        conn-atm (connect-atm pid)
        eid (d/q '[:find ?eid .
                   :in $ ?cid ?max-id
                   :where
                   [?conv :conversation/id ?cid]
                   [?conv :conversation/messages ?eid]
                   [?eid :message/id ?max-id]] @conn-atm cid max-id)]
    (if eid
      (d/transact conn-atm {:tx-data [{:db/id eid :message/EADS-data-structure (str ds)}]})
      (log! :error (str "No such conversation: " cid)))))

(defn get-active-EADS-id
  "Return the EADS-id (keyword) for whatever EADS is active in the given project and conversation.
   See also get-project-active-EADS-id."
  [pid cid]
  (d/q '[:find ?eads-id .
         :in $ ?cid
         :where
         [?e :conversation/id ?cid]
         [?e :conversation/active-EADS-id ?eads-id]]
       @(connect-atm pid) cid))

(defn put-active-EADS-id
  "Set :conversation/active-EADS. See also put-project-active-EADS-id"
  [pid cid eads-id]
  (let [eid (conversation-exists? pid cid)]
    (d/transact (connect-atm pid)
                {:tx-data [{:db/id eid :conversation/active-EADS-id eads-id}]})))

(defn put-EADS-instructions!
  "Update the system DB with a (presumably) new version of the argument EADS instructions.
   Of course, this is a development-time activity."
  [{:keys [EADS budget-decrement] :as eads-instructions}]
  (let [id (:EADS-id EADS)
        db-obj {:EADS/id id
                :EADS/budget-decrement (or budget-decrement 0.05)
                :EADS/msg-str (str eads-instructions)}
        conn (connect-atm :system)
        eid (d/q '[:find ?e . :where [?e :system/name "SYSTEM"]] @conn)]
    (log! :info (str "Writing EADS instructions to system DB: " id))
    (d/transact conn {:tx-data [{:db/id eid :system/EADS db-obj}]}))
  nil)

(defn get-EADS-instructions
  "Return the full EADS instructions object maintained in the system DB
   (the EDN structure from edn/read-string of :EADS/msg-str).
   Returns the empty string when the EADS ID is not known."
  [eads-id]
  (assert (keyword? eads-id))
  (if-let [msg-str (d/q '[:find ?msg-str .
                          :in $ ?eads-id
                          :where
                          [?e :EADS/id ?eads-id]
                          [?e :EADS/msg-str ?msg-str]]
                        @(connect-atm :system) eads-id)]
    (edn/read-string msg-str)
    ""))

(defn same-EADS-instructions?
  "Return true if the argument eads-instructions (an EDN object) is exactly what the system already maintains."
  [eads-instructions]
  (let [id (-> eads-instructions :EADS :EADS-id)]
    (= eads-instructions (get-EADS-instructions id))))

(defn ^:admin update-all-EADS-json!
  "Copy JSON versions of the system DB's EADS instructions to the files in resources/agents/iviewrs/EADS."
  []
  (doseq [eads-id (system-EADS?)]
    (if-let [eads-instructions (-> eads-id get-EADS-instructions not-empty)]
      (sutil/update-resources-EADS-json! eads-instructions)
      (log! :error (str "No such EADS instructions " eads-id)))))

;;; -------------------- Summary data structures -----------------------
(defn summary-ds-exists?
  [pid eads-id]
  (d/q '[:find ?eid .
         :in $ ?eads-id
         :where [?eid :dstruct/id ?eads-id]]
       @(connect-atm pid) eads-id))

(defn put-new-summary-ds!
  "Add a summary data-structure with the given eads-id an budget = 1.0 to the argument project."
  [pid eads-id]
  (if-let [eid (project-exists? pid)]
    (d/transact (connect-atm pid) {:tx-data [{:db/id eid
                                              :project/summary-dstructs
                                              {:dstruct/id eads-id
                                               :dstruct/budget-left 1.0}}]})
    (log! :error (str "Project not found " pid))))

(defn get-questioning-budget-left!
  "The project contains dstruct objects for everything that has been started.
   If one can't be found (it hasn't started) this returns 1.0, otherwise it
   returns the value of :dstruct/budget-left for the given eads-id.
   This will create the summary data structure if it doesn't exist."
  [pid eads-id]
  (assert (or (nil? eads-id) ((system-EADS?) eads-id)))
  (if-let [eid (summary-ds-exists? pid eads-id)]
    (d/q '[:find ?left . :in $ ?eid :where [?eid :dstruct/budget-left ?left]] @(connect-atm pid) eid)
    (do (when eads-id (put-new-summary-ds! pid eads-id))
        1.0)))

(def default-EADS-budget-decrement 0.05)

(defn reduce-questioning-budget!
  "The system stores EADS-instructions that may or may not have an :EADS/budget-decrement.
   If it does, decrement the same-named summary structure objec by that value.
   Otherwise, decrement the same-named object by the db/default-EADS-budget-decrement."
  [pid eads-id]
  (assert ((system-EADS?) eads-id))
  (let [val (get-questioning-budget-left! pid eads-id)
        dec-val (or (d/q '[:find ?dec-val .
                           :in $ ?eads-id
                           :where
                           [?e :EADS/id ?eads-id]
                           [?e :EADS/budget-decrement ?dec-val]]
                         @(connect-atm :system) eads-id)
                    default-EADS-budget-decrement)
        eid (d/q '[:find ?eid .
                   :in $ ?eads-id
                   :where
                   [?eid :dstruct/id ?eads-id]]
                 @(connect-atm pid) eads-id)]
    (if eid
      (d/transact (connect-atm pid) {:tx-data [{:db/id eid :dstruct/budget-left (- val dec-val)}]})
      (log! :error (str "No summary structure for EADS id " eads-id)))))

(defn get-summary-ds
  "Return the summary EADS data structure give the IDs for the project and EADS-id.
   If no such data structure yet, returns {}."
  [pid eads-id]
  (if-let [str (d/q '[:find ?str .
                      :in $ ?eads-id
                      :where
                      [?e :dstruct/id ?eads-id]
                      [?e :dstruct/str ?str]]
                    @(connect-atm pid) eads-id)]
    (edn/read-string str)
    (do (log! :warn (str "No summary DS for " eads-id))
        {})))

(defn ^:diag list-summary-ds
  "Return a vector of eads-id for summary-ds of the given project."
  [pid]
  (d/q '[:find [?eads-id ...]
         :where
         [_ :dstruct/id ?eads-id]]
       @(connect-atm pid)))

(defn put-summary-ds!
  "Dehydrate the given summary data structure and write it to the project DB."
  [pid eads-id dstruct]
  (let [dstruct (dissoc dstruct :msg-id)
        eid (d/q '[:find ?eid . :where [?eid :project/id]] @(connect-atm pid))]
    (if eid
      (d/transact (connect-atm pid)
                  {:tx-data [{:db/id eid
                              :project/summary-dstructs {:dstruct/id eads-id
                                                         :dstruct/str (str dstruct)}}]})
      (throw (ex-info "No eid" {:pid pid :eid eid})))))

;;; -------------------- Starting and stopping -------------------------
(defn register-project-dbs
  "Make a config for each project and register it."
  []
  (doseq [id (list-projects {:from-storage? true})]
    (register-db id (db-cfg-map {:type :project :id id}))))

(defn init-dbs
  "Register DBs using "
  []
  (register-project-dbs)
  (register-db :system (db-cfg-map {:type :system}))
  {:sys-cfg (db-cfg-map {:type :system})})

(defstate sys&proj-database-cfgs
  :start (init-dbs))
