(ns scheduling-tbd.schema
  (:require
   [scheduling-tbd.sutil :as sutil :refer [datahike-schema]]))

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
   :message/graph
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "An optional graph that is the response, or part of the response of a user."}
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
        :doc "An optional table that is the response, or part of the response of a user, or is produced by an interviewer.."}
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
   :project/execution-status ; ToDo: If we keep this at all, it would be an annotation on an ordinary :message/content.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc ":running means resume-conversation can work (not that it necessarily is)
              :paused means resume-conversation will not work."}
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
   :project/summary-dstructs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "'summary' data structures indexed by their EADS-id, :dstruct/id."}
   :project/surrogate
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the project's surrogate object, if any."}
   :project/surrogate? ; ToDo: Not used?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "true if domain expertise is provided by an artificial agent."}
   :project/tables
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "true if domain expertise is provided by an artificial agent."}})

(def db-schema-sys (-> db-schema-sys+ (merge db-schema-agent+) datahike-schema))
(def db-schema-proj (-> db-schema-proj+ (merge db-schema-agent+) datahike-schema))
(def project-schema-key? (-> db-schema-proj+ (merge db-schema-agent+) keys set))
