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
   [scheduling-tbd.llm   :as llm]
   [scheduling-tbd.specs :as spec]
   [scheduling-tbd.sutil :as sutil :refer [register-db connect-atm datahike-schema db-cfg-map resolve-db-id default-llm-provider]]
   [scheduling-tbd.util  :as util :refer [now]]
   [taoensso.timbre :as log :refer [debug]]))

(def db-schema-sys+
  "Defines content that manages project DBs and their analysis including:
     - The project's name and db directory
     - Planning domains, methods, operators, and axioms"
  {;; ---------------------- agent (system agents shared by projects)
   :agent/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A unique ID for each agent to ensure only one of each type is available."}
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
   :system/default-project-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword,
        :doc "a keyword providing a project-id clients get when starting up."}
   :system/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "the value 'SYSTEM' to represent a single object holding data such as the current project name."}
   :system/agents
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "an agent (OpenAI Assistant) that outputs a vector of clojure maps in response to queries."}
   ;; ---------------------- openai (about all stbd-related assistants on the running account)
   :system/openai-assistants
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/string
        :doc "All stbd-related assistants in existance. (See recreate-system-db for how this is updated.)"}})


;;;========================================================== Project DBs ==========================================
(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db."
  {;; ---------------------- agent
   :agent/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A unique ID for each agent to ensure only one of each type is available."}
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

   ;; ---------------------- conversation
   :conversation/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword identifying the kind of conversation; so far just #{:process :data :resource}."}
   :conversation/state-vector
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "keywords indicating the state of the conversation (i.e. what has been discussed."}
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
        :doc "msg-text and msg-link objects."}
   :message/from
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The agent issuing the message, #{:human :surrogate :system}."}
   :message/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long :unique :db.unique/identity
        :doc (str "The unique ID of a message. These are natural numbers starting at 0, but owing to 'LLM:' prompts, "
                  "which aren't stored, some values can be skipped.")}
   :message/question-name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "A label (from interview instructions) associated with this question or answer."}
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
   :project/planning-problem
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "an object with keys :problem/domain, :problem/goal-string, and :problem/state-string at least."}
   :project/processes
      #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
           :doc "the project's process objects; everything about processes."}
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
        :doc "An OpenAI assistant thread (a string) uniquely identifying the thread that this surrogate uses."}

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

;;; ---------------------------------- agents ----------------------------------------------
(defn add-agent!
  "Create an agent, an OpenAI Assistant that responds to various queries with a vector of Clojure maps.
   Store what is needed to identify it in system DB."
  [{:keys [id base-type instructions llm-provider response-format project-thread? model-class]
    :or {llm-provider @default-llm-provider model-class :gpt}}]
  (let [user (-> (System/getenv) (get "USER"))
        assist (llm/make-assistant :name (name id)
                                   :llm-provider llm-provider
                                   :instructions instructions
                                   :response-format response-format
                                   :metadata {:usage :stbd-agent :user user})
        aid    (:id assist)
        thread (when-not project-thread?
                 (llm/make-thread {:assistant-id aid
                                   :llm-provider llm-provider
                                   :metadata {:usage :stbd-agent :user user}}))
        tid    (:id thread)
        conn   (connect-atm :system)
        eid    (d/q '[:find ?eid .
                      :where [?eid :system/name "SYSTEM"]]
                    @(connect-atm :system))]
    (d/transact conn {:tx-data [{:db/id eid
                                 :system/agents (cond-> {:agent/id id
                                                         :agent/base-type base-type
                                                         :agent/llm-provider llm-provider
                                                         :agent/model-class model-class
                                                         :agent/assistant-id aid}
                                                  tid (assoc :agent/thread-id tid))}]})))

;;; (db/add-agent! {:id :process-dur-agent,,,}
(def known-agent-info
  "The actual agent :id is the one provided here with -<llm-provider> added."
  [{:id :process-interview-agent
    :project-thread?  true
    #_#_:model-class :analysis
    :instruction-path "data/instructions/interviewer-process.txt"
    :response-format (-> "data/instructions/interviewer-response-format.edn" slurp edn/read-string)}

   {:id :answers-the-question?
    :instruction-path "data/instructions/answers-the-question.txt"
    :response-format (-> "data/instructions/boolean-response-format.edn" slurp edn/read-string)}

   {:id :process-dur-agent
    :instruction-path "data/instructions/process-dur-agent.txt"}

   {:id :process-ordering-agent
    :instruction-path "data/instructions/process-ordering-agent.txt"}

   {:id :text-function-agent
    :instruction-path "data/instructions/text-function-agent.txt"}])

(defn agent-exists?
  "Return true if agent still exists (LLM-providers typiclly don't keep them forever)."
  [aid]
  (d/q '[:find ?eid .
         :in $ ?aid
         :where [?eid :system/openai-assistants ?aid]]
       @(connect-atm :system) aid))

;;; ------------------------------------------------- projects and system db generally ----------------------
(defn project-exists?
  "If a project with argument :project/id (a keyword) exists, return the root entity ID of the project
   (the entity id of the map containing :project/id in the database named by the argumen proj-id)."
  [pid]
  (assert (keyword? pid))
  (when (some #(= % pid) (sutil/db-ids)) ; (sutil/db-ids) includes non-project IDs.
    (d/q '[:find ?e .
           :in $ ?pid
           :where
           [?e :project/id ?pid]]
         @(connect-atm pid) pid)))

(defn conversation-exists?
  "Return the eid of the conversation if it exists."
  ([pid] (conversation-exists? pid (d/q '[:find ?conv-id . :where [_ :project/current-conversation ?conv-id]] @(connect-atm pid))))
  ([pid conv-id]
   (assert (#{:process :data :resource} conv-id))
   (d/q '[:find ?eid .
          :in $ ?conv-id
          :where
          [?eid :conversation/id ?conv-id]]
        @(connect-atm pid) conv-id)))

(defn get-conversation-eids
  "Return a vector of the EIDs of the argument conversation's messages."
  [pid conv-id]
  (when-let [eid (conversation-exists? pid conv-id)]
    (->> (dp/pull @(connect-atm pid) '[*] eid)
         :conversation/messages
         (mapv :db/id))))

(defn change-conversation
  [{:keys [pid conv-id] :as obj}]
  (try
    (assert (#{:process :data :resource} conv-id))
    (let [conn (connect-atm pid)
          eid (project-exists? pid)]
      (if eid
        (d/transact conn {:tx-data [[:db/add eid :project/current-conversation conv-id]]})
        (log/info "No change with change-conversation (1):" obj)))
    (catch Throwable _e (log/info "No change with change-conversation (2):" obj) nil))
  nil)

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
     ;; Otherwise we list using system db. These are the 'legitmate' projects in the project datebase
     (-> (d/q '[:find [?proj-id ...]
                :where
                [?e :project/id  ?proj-id]]
              @(connect-atm :system))
         sort
         vec))))

(defn get-surrogate-info [pid]
  (when (project-exists? pid)
    (when-let [eid (d/q '[:find ?sur . :where [_ :project/surrogate ?sur]]  @(connect-atm pid))]
      (dp/pull @(connect-atm pid) '[*] eid))))

(def message-keep-set "A set of properties with root :conversation/messages used to retrieve typically relevant message content."
  #{:conversation/id :conversation/messages :message/id :message/from :message/content :message/time :message/question-name})

(defn get-conversation
  "For the argument project (pid) return a vector of messages sorted by their :message/id."
  ([pid] (get-conversation pid (d/q '[:find ?conv-id . :where [_ :project/current-conversation ?conv-id]] @(connect-atm pid))))
  ([pid conversation]
   (assert (#{:process :data :resource} conversation))
   (if-let [eid (conversation-exists? pid conversation)]
     (->> (resolve-db-id {:db/id eid}
                         (connect-atm pid)
                         :keep-set message-keep-set)
          :conversation/messages
          (sort-by :message/id)
          vec)
     [])))

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
  "Return the planning state set (a collection of ground propositions) for the argument project, or #{} if none."
  [pid]
  (if (= pid :START-A-NEW-PROJECT)
    '#{(proj-id START-A-NEW-PROJECT)} ; Special treatment for new projects.
    (if-let [state-str (d/q '[:find ?s .
                              :where
                              [_ :project/planning-problem ?pp]
                              [?pp :problem/state-string ?s]]
                            @(connect-atm pid))]
      (-> state-str edn/read-string set)
      #{})))

(defn put-planning-state
  "Write the complete state to the project database. Argument is a vector or set. "
  [pid state]
  (assert (every? #(s/valid? ::spec/ground-positive-proposition %) state))
  ;;(log/info "put-planning-state: state =" state)
  (if (empty? state)
    (throw (ex-info "state is empty" {:pid pid}))
    (let [state-set (set state)
          conn (connect-atm pid)
          eid (d/q '[:find ?eid . :where [?eid :problem/state-string]] @conn)]
      (d/transact conn {:tx-data [[:db/add eid :problem/state-string (str state-set)]]}))))

(defn add-planning-state
  "Add the argument vector of ground proposition to state, a set. Return the new state."
  [pid more-state]
  (assert (every? #(s/valid? ::spec/ground-positive-proposition %) more-state))
  ;;(log/info "add-planning-state: more-state = " more-state)
  (let [new-state (into (get-planning-state pid) more-state)
        conn (connect-atm pid)
        eid (d/q '[:find ?eid . :where [?eid :problem/state-string]] @conn)]
    (d/transact conn {:tx-data [[:db/add eid :problem/state-string (str new-state)]]})
    new-state))

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
      (log/info "Writing system DB to" filename)
      (spit filename s)))

;;; OpenAI may delete them after 30 days. https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
(defn update-assistants!
  "List what assistants the LLM-provider's currently maintains that have metadata indicating that
   they are part of our project (where they are called agents). Store this in the sytems DB."
  []
  (let [conn (connect-atm :system)
        eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn)
        old-ones (d/q '[:find [?s ...] :where [_ :system/openai-assistants ?s]] @conn)
        new-ones (llm/list-assistants)]
    (d/transact conn {:tx-data (vec (for [o old-ones] [:db/retract eid :system/openai-assistants o]))})
    (d/transact conn {:tx-data (vec (for [n new-ones] [:db/add     eid :system/openai-assistants n]))})))

(defn recreate-system-agents!
  "Recreate the system agents for the agent infos given (db/known-agent-info if called with no args)."
  ([] (recreate-system-agents! known-agent-info))
  ([infos]
   (doseq [llm-provider [:openai]]
     (doseq [{:keys [id instruction-path response-format project-thread? model-class]
              :or {model-class :gpt}} infos]
       (cond-> {}
         true (assoc :id (-> id name (str "-" (name llm-provider)) keyword))
         true (assoc :base-type id)
         true (assoc :llm-provider llm-provider)
         true (assoc :instructions (slurp instruction-path))
         true (assoc :model-class model-class)
         project-thread? (assoc :project-thread? true)
         response-format (assoc :response-format response-format)
         true add-agent!)))))

(defn recreate-system-db!
  "Recreate the system database from an EDN file
   By default this creates new agents.
     :update-assistants! - if true, list extant OpenAI assistants. (OpenAI doesn't keep them forever.)
     :new-agents? - Make new agents based on instructions."
  [& {:keys [target-dir new-agents? update-assistants?]
      :or {target-dir "data/" new-agents? true update-assistants? true}}]
  (if (.exists (io/file (str target-dir "system-db.edn")))
    (let [cfg (db-cfg-map {:type :system})]
      (log/info "Recreating the system database.")
      (when (d/database-exists? cfg) (d/delete-database cfg))
      (d/create-database cfg)
      (register-db :system cfg)
      (let [conn (connect-atm :system)]
        (d/transact conn db-schema-sys)
        (d/transact conn (-> "data/system-db.edn" slurp edn/read-string))
        (when update-assistants? (update-assistants!))
        (when new-agents? (recreate-system-agents!)); This is just about system agents, of course.
        cfg))
      (log/error "Not recreating system DB: No backup file.")))

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
    (log/error "Not recreating DB because backup file does not exist:" backup-file))))

(def keep-db? #{:him})
(defn ^:diag recreate-dbs!
  "Recreate the system DB on storage from backup.
   For each project it lists, recreate it from backup if such backup exists."
  []
  (swap! sutil/databases-atm
         #(reduce-kv (fn [res k v] (if (keep-db? k) (assoc res k v) res)) {} %))
  (recreate-system-db!)
  (log/info "Recreating these projects:" (list-projects))
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
            new-id   (-> new-name (str/replace #"\s+" "-") keyword)]
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
  [{:keys[pid from text tags question-type]}]
  (assert (#{:system :human :surrogate :developer-injected} from))
  (assert (string? text))
  ;;(log/info "add-msg: pid =" pid "text =" text)
  (if-let [conn (connect-atm pid)]
    (let [msg-id (inc (max-msg-id pid))]
      (d/transact conn {:tx-data [{:db/id (conversation-exists? pid) ; defaults to current conversation
                                   :conversation/messages (cond-> #:message{:id msg-id :from from :time (now) :content text}
                                                            (not-empty tags) (assoc :message/tags tags)
                                                            question-type    (assoc :message/question-name question-type))}]}))
    (throw (ex-info "Could not connect to DB." {:pid pid}))))

(defn add-project-to-system
  "Add the argument project (a db-cfg map) to the system database."
  [id proj-name dir]
  (d/transact (connect-atm :system)
              {:tx-data [{:project/id id
                          :project/name proj-name
                          :project/dir dir}]}))

(s/def ::project-info (s/keys :req [:project/id :project/name]))

;;; (db/create-proj-db! {:project/id :remove-me :project/name "remove me"} {} {:force? true})
(defn create-proj-db!
  "Create a project database for the argument project.
   The project-info map must include :project/id and :project/name.
     proj-info  - map containing at least :project/id and :project/name.
     additional - a vector of maps to add to the database.
     opts -  {:force-this-name? - overwrite project with same name}
   This always destroys DBs with the same name as that calculated here.
   Return the PID of the project."
  ([proj-info] (create-proj-db! proj-info {} {}))
  ([proj-info additional-info] (create-proj-db! proj-info additional-info {}))
  ([proj-info additional-info {:keys [in-mem? force-this-name?] :as opts}]
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
                                              :project/conversations [{:conversation/id :process}
                                                                      {:conversation/id :data}
                                                                      {:conversation/id :resource}]}]})
     (when (not-empty additional-info)
       (d/transact (connect-atm id) additional-info))
     ;; Add knowledge of this project to the system db.
     (log/info "Created project database for" id)
     id)))

(defn delete-project!
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

(defn ^:diag known-agents
  "Return a set of all the agents recorded in the system DB."
  []
  (-> (d/q '[:find [?id ...] :where [_ :agent/id ?id]] @(connect-atm :system))
      set))

(defn agent-info
  "Return a map of agent info (keys :aid :tid, :role, and sometimes, :expertise) for a ordinary agent or surrogate."
  [agent]
  (cond (contains? agent :agent/id)       (reduce-kv (fn [m k v]
                                                       (cond (= k :agent/assistant-id) (assoc m :aid v)
                                                             (= k :agent/thread-id)    (assoc m :tid v)
                                                             (= k :agent/base-type)    (assoc m :role v)
                                                             :else m))
                                                     {}
                                                     agent)
        (contains? agent :surrogate/id)   (reduce-kv (fn [m k v]
                                                       (cond (= k :surrogate/assistant-id)         (assoc m :aid v)
                                                             (= k :surrogate/thread-id)            (assoc m :tid v)
                                                             (= k :surrogate/id)                   (assoc m :role :surrogate)
                                                             (= k :surrogate/subject-of-expertise) (assoc m :expertise v)
                                                             :else m))
                                                     {}
                                                     agent)))

(defn get-agent
  "Return a map of {:aid <string> and :tid <string> for the argument agent-id (a keyword)
   from either the system DB, or if :pid is provided, the project DB.
   If :db-attrs? then return the object with DB-native attributes, not :aid :tid."
  [& {:keys [base-type llm-provider pid db-attrs?] :or {llm-provider @default-llm-provider} :as obj}]
  (reset! diag obj)
  (let [conn (if pid @(connect-atm pid) @(connect-atm :system))
        ent (when base-type
              (d/q '[:find ?e .
                     :in $ ?base-type ?llm-provider
                     :where
                     [?e :agent/base-type ?base-type]
                     [?e :agent/llm-provider ?llm-provider]]
                   conn base-type llm-provider))]
    (when ent
      (let [a-map (dp/pull conn '[*] ent)]
        (if db-attrs? a-map (agent-info a-map))))))

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
