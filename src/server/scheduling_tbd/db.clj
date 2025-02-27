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
   [datahike.pull-api            :as dp]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.sutil    :as sutil :refer [connect-atm datahike-schema db-cfg-map register-db resolve-db-id]]
   [scheduling-tbd.util     :as util :refer [now]]
   [taoensso.telemere       :refer [log!]]))

(def db-schema-sys+
  "Defines content that manages project DBs and their analysis including:
     - The project's name and db directory
     - system-level agents"
  {;; ---------------------- agent (system agents shared by projects)
   :agent/id
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
   :agent/llm-provider
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "Currently either :openai or :azure."}
   :agent/model-class
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "One of the classes of LLM model defined in the system. See llm.clj."}
   :agent/thread-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant thread (a string) uniquely identifying the thread on which this surrogate operates."}
   :agent/timestamp
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "The time at which the agent was created."}

   ;; ---------------------- agent-files files that assistants can use
   :agent-file/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A unique ID for each agent to ensure only one of each type is available."}
   :agent-file/file-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An ID to the file provided by the LLM provider."}
   :agent-file/doc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "Documentation about the file."}

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
   :system/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "the value 'SYSTEM' to represent a single object holding data such as the current project name."}
   :system/projects
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "the projects known by the system."}
})


;;;========================================================== Project DBs ==========================================
(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db."
  {;; ---------------------- agent
   :agent/id
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
   :agent/llm-provider
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "Currently either :openai or :azure."}
   :agent/model-class
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "One of the classes of LLM model defined in the system. See llm.clj."}
   :agent/surrogate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "True if the agent is a human surrogate."}
   :agent/system-instruction
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "This is only used with surrogates, it is the system instruction verbatim."}
   :agent/thread-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An OpenAI assistant thread (a string) uniquely identifying the thread on which this surrogate operates."}
   :agent/timestamp
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "The time at which the agent was created."}

   ;; ---------------------- box
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
        :doc "a stringified fact (in predicate calculus) about the project, similar in concept to planning state fact in the earlier design.
              For example, (:process/production-motivation make-to-stock)."}
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
   :conversation/done?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "true if we don't have more to add (though user might)."}
   :conversation/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword identifying the kind of conversation; so far just #{:process :data :resources :optimality}."}
   :conversation/interviewer-budget
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/double
        :doc "the budget a number (0 <= budget <= 1) indicating how much more resources the interviewer is allowed to expend."}
   :conversation/messages
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the messages of the conversation."}

   ;; ---------------------- duration
   :duration/value
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to map describing a quantity of time."}

   ;; ---------------------- message
   :message/content
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string with optional html links."}
   :message/from
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The agent issuing the message, #{:human :surrogate :system}."}
   :message/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long :unique :db.unique/identity
        :doc (str "The unique ID of a message. These are natural numbers starting at 0, but owing to 'LLM:' prompts, "
                  "which aren't stored, some values can be skipped.")}
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

   ;; ---------------------- process (about production process types)
   :process/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this process; perhaps stated in an interview."}
   :process/duration
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to a duration (dur) object; typically an estimate"}
   :process/duration-comment
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a comment about the duration of a process."}
   :process/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc (str "A string, perhaps from an interview uniquely identifying the process."
                  "The top-level production process will have a process-type/id = :project/id.")}
   :process/interview-class
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "a keyword identifying what sort of conversation lead to this process description, for example :initial-unordered for early in the interview."}
   :process/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "original text naming the process."}
   :process/pre-processes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a process/id identifying a process that must occur before this task "}
   :process/resource
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a keyword naming a resource (type or instance) used in this task"}
   :process/step-number
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc "a positive integer indicating the order of the process step in the :process/sub-processes vector."}
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
   :project/agents
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "an agent (OpenAI Assistant, etc.) that outputs a vector of clojure maps in response to queries."}
   :project/claims
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the collection of things we believe about the project as logical statements."}
   :project/code
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "Code associated with the project."}
   :project/conversations
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "The conversations of this project."}
   :project/current-conversation
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The conversation most recently busy."}
   :project/desc ; ToDo: If we keep this at all, it would be an annotation on an ordinary :message/content.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "the original paragraph written by the user describing what she/he wants done."}
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a lowercase kebab-case keyword naming a project; unique to the project."}
   :project/industry
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a short description of the industry in which we are doing scheduling."}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "4 words or so describing the project; e.g. 'craft brewing production scheduling'"}
   :project/objective
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to the scheduling objective of the project."}
   :project/processes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the project's process objects; everything about processes in a complex structure."}
   :project/surrogate
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the project's surrogate object, if any."}
   :project/surrogate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "true if domain expertise is provided by an artificial agent."}
   :project/tables
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
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

   ;; ------------------------ table
   :table/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "The ID of a table, unique in the context of this project DB."}

   :table/purpose
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The purpose of this table, on of the enumeration #{:customer orders}"}

   :table/filename
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The filename as it appears in the project directory}"}

   :table/identity-condition ; ToDo: we expect more cardinality :many, but not yet.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "The subset of column names (:object/attribute-name) that identify objects."}

   :table/attributes
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "Attribute objects for the tableThe subset of column names (:object/attribute-name) that identify objects."}

   :table/data
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "A vector of :object objects."}

   :attribute/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The name of the attribute, often based on the column name."}

   :attribute/description
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "A description of the purpose of the attribute."}

   :attribute/cardinality
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "#{:one :many}"}

   :attribute/datatype
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "#{:string :number, etc.}"}

   :object/row
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc "Row that realized this object."}

   :object/attribute-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "Name of the column (or generated name) for tRow that realized this object."}

   :object/attribute-value
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "Name of the column (or generated name) for tRow that realized this object."}

   :object/attribute-value-pairs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref}

   :column/tuple
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword}})


(def ^:diag diag (atom nil))
(def db-schema-sys  (datahike-schema db-schema-sys+))
(def db-schema-proj (datahike-schema db-schema-proj+))
(def project-schema-key? (-> db-schema-proj+ keys set))

;;; ------------------------------------------------- projects and system db generally ----------------------
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

(defn get-current-cid
  "Get what the DB asserts is the project's current conversation CID, or :process if it doesn't have a value."
  [pid]
  (or (d/q '[:find ?cid . :where [_ :project/current-conversation ?cid]] @(connect-atm pid))
      :process))

(defn conversation-exists?
  "Return the eid of the conversation if it exists."
  [pid cid]
  (assert (#{:process :data :resource :resources :optimality} cid))
  (d/q '[:find ?eid .
         :in $ ?cid
         :where [?eid :conversation/id ?cid]]
       @(connect-atm pid) cid))

(defn get-conversation-eids
  "Return a vector of the EIDs of the argument conversation's messages."
  [pid cid]
  (when-let [eid (conversation-exists? pid cid)]
    (->> (dp/pull @(connect-atm pid) '[*] eid)
         :conversation/messages
         (mapv :db/id))))

(defn ^:diag get-project
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
       (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases.") {})))
     ;; Otherwise we list using system db. These are the 'legitmate' projects in the project datebase
     (-> (d/q '[:find [?proj-id ...]
                :where
                [?e :project/id  ?proj-id]]
              @(connect-atm :system))
         sort
         vec))))

(def message-keep-set "A set of properties with root :conversation/messages used to retrieve typically relevant message content."
  #{:conversation/id :conversation/interviewer-budget :conversation/messages :message/id :message/from :message/content :message/time :message/tags :message/question-type})

(defn get-conversation
  "For the argument project (pid) return a vector of messages sorted by their :message/id."
  [pid cid]
  (assert (#{:process :data :resources :optimality} cid))
  (if-let [eid (conversation-exists? pid cid)]
    (->> (resolve-db-id {:db/id eid}
                        (connect-atm pid)
                        :keep-set message-keep-set)
         :conversation/messages
         (sort-by :message/id)
         vec)
    []))

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
              question-type   (assoc :question-type question-type)
              confidence      (assoc :confidence confidence)))))
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
                                   cid        (assoc :claim/conversation-id cid)
                                   q-type     (assoc :claim/question-type q-type)
                                   confidence (assoc :claim/confidence confidence))}]}))
  true)

(defn get-process
  "Return the process structure for the argument pid and interview-class."
  [pid interview-class]
  (assert (keyword? pid))
  (assert (keyword? interview-class))
  (let [conn (connect-atm pid)
        eid (d/q '[:find ?eid .
                   :in $ ?class
                   :where [?eid :process/interview-class ?class]]
                 @conn
                 interview-class)]
    (resolve-db-id {:db/id eid} conn)))

(defn put-process-sequence!
  "Write project/process-sequence to the project's database.
   The 'infos' argument is a vector of maps such as produced by analyze-process-durs-response."
  [pid full-obj]
  (let [conn (connect-atm pid)
        eid (project-exists? pid)]
    (d/transact conn {:tx-data [{:db/id eid :project/processes full-obj}]})))

(defn clean-project-for-schema
  "Remove attributes that are no longer in the schema.
   Remove nil values, these can show up after doing a :db/retract."
  [proj]
  (letfn [(cpfs [x]
            (cond (map? x)      (reduce-kv (fn [m k v]
                                             (if (project-schema-key? k)
                                               (if (nil? v) m (assoc m k (cpfs v)))
                                               (do (log! :warn (str "Dropping obsolete attr: " k)) m)))
                                           {} x)
                  (vector? x)   (->> (mapv cpfs x) (remove nil?) vec)
                  :else         x))]
    (cpfs proj)))

;;; ----------------------- Backup and recover project and system DB ---------------------
(defn backup-proj-db
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

(defn ^:diag backup-proj-dbs
  "Backup the project databases one each to edn files. This will overwrite same-named files in tar-dir.
   Example usage: (backup-proj-dbs)."
  [& {:keys [target-dir] :or {target-dir "data/projects/"}}]
  (doseq [id (list-projects)]
    (backup-proj-db id {:target-dir target-dir})))

(defn ^:diag backup-system-db
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

(defn recreate-project-db!
  "Recreate a DB for each project using EDN files."
  [id]
  (let [backup-file (format "data/projects/%s.edn" (name id))]
    (if (.exists (io/file backup-file))
      (let [cfg (db-cfg-map {:type :project :id id})
            files-dir (-> cfg :base-dir (str "/projects/" (name id) "/files"))]
        (when (and (-> cfg :store :path io/as-file .isDirectory) (d/database-exists? cfg))
          (d/delete-database cfg))
        (when-not (-> cfg :store :path io/as-file .isDirectory)
          (-> cfg :store :path io/make-parents)
          (-> cfg :store :path io/as-file .mkdir))
        (d/create-database cfg)
        (register-db id cfg)
        (let [conn (connect-atm id)]
          (d/transact conn db-schema-proj)
          (d/transact conn (->> backup-file slurp edn/read-string)))
        (when-not (-> files-dir io/as-file .isDirectory)
          (-> files-dir io/as-file .mkdir))
        cfg)
    (log! :error (str "Not recreating DB because backup file does not exist: " backup-file)))))

(def keep-db? #{:him})
(defn ^:diag recreate-dbs!
  "Recreate the system DB on storage from backup.
   For each project it lists, recreate it from backup if such backup exists."
  []
  (swap! sutil/databases-atm
         #(reduce-kv (fn [res k v] (if (keep-db? k) (assoc res k v) res)) {} %))
  (recreate-system-db!)
  (log! :info (str "Recreating these projects: " (list-projects)))
  (doseq [pid (list-projects)]
    (recreate-project-db! pid)))

(defn ^:diag unknown-projects
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
            new-id   (-> new-name str/lower-case (str/replace #"\s+" "-") keyword)]
        (-> proj-info
            (assoc :project/name new-name)
            (assoc :project/id new-id))))))

(defn max-msg-id
  "Return the current highest message ID used in the project."
  [pid]
  (let [ids (d/q '[:find [?msg-id ...]
                   :where [_ :message/id ?msg-id]]
                 @(connect-atm pid))]
    (if (empty? ids) 0 (apply max ids))))

;;; See "Map forms" at https://docs.datomic.com/pro/transactions/transactions.html
;;; This is typical: You get the eid of the thing you want to add properties to.
;;; You specify that as the :db/id and then just add whatever you want for the properties.
;;; If the property is cardinality many, it will add values, not overwrite them.
(defn add-msg
  "Create a message object and add it to current conversation of the database with :project/id = id."
  [{:keys [pid cid from text table tags question-type]}]
  (assert (keyword? cid))
  (assert (#{:system :human :surrogate :developer-injected} from))
  (assert (string? text))
  (log! :debug (str "add-msg: pid = " pid "text = " text))
  (if-let [conn (connect-atm pid)]
    (let [msg-id (inc (max-msg-id pid))]
      (d/transact conn {:tx-data [{:db/id (conversation-exists? pid cid)
                                   :conversation/messages (cond-> #:message{:id msg-id :from from :time (now) :content text}
                                                            table            (assoc :message/table table)
                                                            (not-empty tags) (assoc :message/tags tags)
                                                            question-type    (assoc :message/question-type question-type))}]}))
    (throw (ex-info "Could not connect to DB." {:pid pid}))))

(defn get-budget
  "Return the :converation/interviewer-budget."
  [pid cid]
   (d/q '[:find ?budget .
          :in $ ?cid
          :where
          [?e :conversation/id ?cid]
          [?e :conversation/interviewer-budget ?budget]]
        @(connect-atm pid) cid))

(defn put-budget!
  [pid cid val]
  (let [conn-atm (connect-atm pid)
        eid (d/q '[:find ?eid .
                   :in $ ?cid
                   :where [?eid :conversation/id ?cid]] @conn-atm cid)]
    (if eid
      (d/transact conn-atm {:tx-data [{:db/id eid :conversation/interviewer-budget val}]})
      (log! :error (str "No such conversation: " cid)))))

(defn add-project-to-system
  "Add the argument project (a db-cfg map) to the system database."
  [id project-name dir]
  (let [conn-atm (connect-atm :system)
        eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn-atm)]
    (d/transact conn-atm {:tx-data [{:db/id eid
                                     :system/projects {:project/id id
                                                       :project/name project-name
                                                       :project/dir dir}}]})))

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

(defn add-conversation-intros
  "Add an intro describing the topic and rationale of the conversation."
  [pid]
  (doseq [cid [:process :data :resources :optimality]]
    (add-msg  {:pid pid :cid cid :from :system :text (get conversation-intros cid) :tags [:conversation-intro]})))

(s/def ::project-info (s/keys :req [:project/id :project/name]))

;;; (db/create-proj-db! {:project/id :remove-me :project/name "remove me"} {} {:force? true})
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
         files-dir (-> cfg :base-dir (str "/" pname  "/files"))]
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
                                              :project/current-conversation :process
                                              :project/claims [{:claim/string (str `(~'project-id ~id))}
                                                               {:claim/string (str `(~'project-name ~id ~pname))}]
                                              :project/conversations [{:conversation/id :process :conversation/interviewer-budget 1.0}
                                                                      {:conversation/id :data :conversation/interviewer-budget 1.0}
                                                                      {:conversation/id :resources :conversation/interviewer-budget 1.0}
                                                                      {:conversation/id :optimality :conversation/interviewer-budget 1.0}]}]})
     (add-conversation-intros id)
     (when (not-empty additional-info)
       (d/transact (connect-atm id) additional-info))
     ;; Add knowledge of this project to the system db.
     (log! :info (str "Created project database for " id))
     id)))

(defn ^:diag delete-project!
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

(defn assert-conversation-done!
  "Set the conversation's :converation/done? attribute to true."
  [pid cid]
  (if-let [eid (conversation-exists? pid cid)]
    (d/transact (connect-atm pid) {:tx-data [[:db/add eid :conversation/done? true]]})
    (log! :error (str "No such conversation: pid = " pid " cid = " cid))))

(defn ^:diag retract-conversation-done!
  [pid cid]
  (if-let [eid (conversation-exists? pid cid)]
    (d/transact (connect-atm pid) {:tx-data [[:db/add eid :conversation/done? false]]})
    (log! :error (str "No such conversation: pid = " pid " cid = " cid))))

(defn conversation-done?
  "Returns true if the conversation is completed, as far as the interviewer is concerned."
  [pid cid]
  (if-let [eid (conversation-exists? pid cid)]
    (d/q '[:find ?done .
           :in $ ?eid
           :where [?eid :conversation/done? ?done]]
         @(connect-atm pid) eid)
    (log! :error (str "No such conversation: pid = " pid " cid = " cid))))

(defn ^:diag update-project-for-schema!
  "This has the ADDITIONAL side-effect of writing a backup file."
  [pid]
  (backup-proj-db pid)
  (recreate-project-db! pid))

(defn ^:diag update-all-projects-for-schema!
  []
  (doseq [p (list-projects)]
    (update-project-for-schema! p)))

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
