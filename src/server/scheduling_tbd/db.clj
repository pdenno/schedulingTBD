(ns scheduling-tbd.db
  "System and project database schemas and database initialization.
   There are other databases, see for example, him.clj."
  (:require
   [clojure.core :as c]
   [clojure.edn  :as edn]
   [clojure.instant]
   [clojure.java.io               :as io]
   [clojure.pprint                :refer [pprint]]
   [clojure.set                   :as set]
   [clojure.spec.alpha            :as s]
   [clojure.string                :as str]
   [datahike.api                  :as d]
   [datahike.pull-api             :as dp]
   [mount.core                    :as mount :refer [defstate]]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm datahike-schema db-cfg-map register-db resolve-db-id]]
   [scheduling-tbd.util           :as util :refer [now]]
   [taoensso.telemere             :refer [log!]]))

(def db-schema-agent+
  "Defines properties that can be used for agents, which can be stored either in the system DB or a project DB.
   This common information is merged into other system and project schema."
  {:agent/id
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
   :agent/llm-provider
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "Currently either :openai or :azure."}
   :agent/model-class
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "One of the classes of LLM model defined in the system. See llm.clj."}
   :agent/response-format-path
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "as it is used by OpenAI circa 2025."}
   :agent/surrogate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "True if the agent is a human surrogate."}
   :agent/system-instruction
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "This is only used with surrogates, it is the system instruction verbatim. Idea is I can't run SUR: for things like a music school."}
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
   :EADS/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "A unique ID for each EADS. The namespace of the keyword is the cid, e.g. :process/flow-shop."}
   :EADS/msg-str
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The stringified message object, it can be edn/read-string. It is the EDN version of the JSON in resources used by ork."}
   :EADS/can-produce-visuals
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "Indicates the purpose and instructions given."}
   :EADS/specs
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "An object where the keys name spec levels (e.g. full) and the values are keywords identifying the spec in the registry."}
   :EADS/cid
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The conversation for which this EADS is used"}

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

   ;; ------------------------- specs
   :spec/full
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "a keyword identifying the full spec in the clojure spec registry. Used, for example, to check an EADS data structure."}

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
        :doc "spec objects used for checking completion of EADS, etc."}
   :system/warm-ups
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "warm-up objects for various conversations."}})

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
                  "If the conversation doesn't have one, functions such as ork/get-EADS-id can make one using the project's orchestrator agent."
                  "Though only one EADS can truely be active at a time, we index the active-EADS-id by [pid, cid] because other conversations "
                  "could still need work.")}
   :conversation/EADS
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "Legacy EADS attribute (needed to recreate old dbs)"}
   :conversation/status
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc (str "A keyword that specifies the status of the conversation. Thus far, the only values are :eads-exhausted :in-progress, and "
                  ":not-started. :in-progress only means that this conversation can be pursued further. For this conversation to be the one "
                  "currently being pursued, it must also be the case that :project/active-conversation is this conversation.")}
   :conversation/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword uniquely identifying the kind of conversation; so far just #{:process :data :resources :optimality}."}
   :conversation/interviewer-budget
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/double
        :doc "the budget a number (0 <= budget <= 1) indicating how much more resources the interviewer is allowed to expend."}
   :conversation/messages
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the messages between the interviewer and interviewees of the conversation."}
   :conversation/done?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean
        :doc "boolean that asserts whether the conversation is done"}

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
   :message/data-structure
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string that can be edn/read-string into an EADS data structure inferred from conversation so far."}
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

   ;; ---------------------- project
   :project/code
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string}
        
   :project/active-conversation
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc (str "The conversation most recently busy. Note that several conversations can still need work, and there can be an "
                  ":conversation/active-EADS-id on several, however, this is the conversation to start if coming back to the project.")}
   :project/current-conversation
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc (str "The conversation most recently busy. Note that several conversations can still need work, and there can be an "
                  ":conversation/active-EADS-id on several, however, this is the conversation to start if coming back to the project.")}
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
(def db-schema-sys  (-> db-schema-sys+   (merge db-schema-agent+) datahike-schema))
(def db-schema-proj (-> db-schema-proj+  (merge db-schema-agent+) datahike-schema))
(def project-schema-key? (-> db-schema-proj+ (merge db-schema-agent+) keys set))

;;; ------------------------------------------------- projects db generally ----------------------
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
                [?e :project/id  ?proj-id]]
              @(connect-atm :system))
         sort
         vec))))

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

(defn ^:admin backup-proj-dbs
  "Backup the project databases one each to edn files. This will overwrite same-named files in tar-dir.
   Example usage: (backup-proj-dbs)."
  [& {:keys [target-dir] :or {target-dir "data/projects/"}}]
  (doseq [id (list-projects)]
    (backup-proj-db id {:target-dir target-dir})))

(defn ^:admin backup-system-db
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

(declare add-msg)

(defn add-conversation-intros
  "Add an intro describing the topic and rationale of the conversation."
  [pid]
  (doseq [cid [:process :data :resources :optimality]]
    (add-msg  {:pid pid :cid cid :from :system :text (get conversation-intros cid) :tags [:conversation-intro]})))

(defn get-active-cid
  "Get what the DB asserts is the project's current conversation CID, or :process if it doesn't have a value."
  [pid]
  (or (d/q '[:find ?cid . :where [_ :project/active-conversation ?cid]] @(connect-atm pid))
      :process))

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
    (let [msg-id (inc (max-msg-id pid cid))]
      (d/transact conn {:tx-data [{:db/id (conversation-exists? pid cid)
                                   :conversation/messages (cond-> #:message{:id msg-id :from from :time (now) :content text}
                                                            table            (assoc :message/table table)
                                                            (not-empty tags) (assoc :message/tags tags)
                                                            question-type    (assoc :message/question-type question-type)
                                                            pursuing-EADS    (assoc :message/pursuing-EADS pursuing-EADS))}]})
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
  [pid cid mid info]
  (let [msg (get-msg pid cid mid)]
    (if (not-empty msg)
      (d/transact (connect-atm pid) {:tx-data [(merge msg info)]})
      (log! :warn (str "Could not find msg for update-msg: pid = " pid " cid = " cid " mid = " mid)))))

;;; ----------------------------------------- Budget ---------------------------------------------
(defn get-budget
  "Return the :conversation/interviewer-budget."
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

;;; ----------------------------------------- EADS ---------------------------------------------
(defn ^:admin list-system-EADS
  "Return a sorted vector of EADS-ids (keywords) known to the system db."
  []
  (-> (d/q '[:find [?eads-id ...]
             :where [_ :EADS/id ?eads-id]]
           @(sutil/connect-atm :system))
      sort
      vec))

;;; ToDo: I think maybe this isn't needed.
(defn get-EADS-dstructs
  "Return a vector of maps of data structures where :message/id is where the :message/data-structure was created."
  [pid cid]
  (->> (d/q '[:find ?id ?ds
              :keys message/id message/EADS-data-structure
              :in $ ?cid
              :where
              [?e :conversation/id ?cid]
              [?conv :conversation/messages ?m]
              [?m :message/EADS-data-structure ?ds]
              [?m :message/id ?id]]
            @(connect-atm pid) cid)
       (mapv #(update % :message/EADS-data-structure edn/read-string))
       not-empty))

;;; ToDo: Merge this and the above.
(defn get-EADS-ds
  "Return the most recent EADS string from messages in the argument conversation.
   Return null string if no EADS found."
  [pid cid]
  (let [conn @(connect-atm pid)
        eids (if (= cid :all)
               (d/q '[:find [?eid ...] :where [?eid :message/EADS-data-structure]] conn)
               (d/q '[:find [?eid ...]
                      :in $ ?cid
                      :where
                      [?c-eid :conversation/id ?cid]
                      [?c-eid :conversation/messages ?eid]
                      [?eid :message/EADS-data-structure]]
                    conn cid))
        code-msgs (dp/pull-many conn '[*] eids)]
    (if (not-empty code-msgs)
      (-> (apply max-key #(-> % :message/time inst-ms) code-msgs) :message/EADS-data-structure)
      "")))

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
  "Return the EADS-id (keyword) for whatever EADS is active in the given project and conversation."
  [pid cid]
  (d/q '[:find ?eads-id .
         :in $ ?cid
         :where
         [?e :conversation/id ?cid]
         [?e :conversation/active-EADS ?eads-id]]
       @(connect-atm pid) cid))

(defn put-active-EADS-id
  "Set :conversation/active-EADS."
  [pid cid eads-id]
  (let [eid (conversation-exists? pid cid)]
    (d/transact @(connect-atm pid)
                {:tx-data [{:db/id eid :conversation/active-eads eads-id}]})))

(defn put-EADS-instructions!
  "Update the system DB with a (presumably) new version of the argument EADS instructions.
   Of course, this is a development-time activity."
  [eads-instructions]
  (let [id (-> eads-instructions :EADS :EADS-id)
        [ns nam] ((juxt namespace name) id)
        db-obj {:EADS/id id
                :EADS/specs #:spec{:full (keyword nam "EADS-message")}
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
  (doseq [eads-id (list-system-EADS)]
    (if-let [eads-instructions (-> eads-id get-EADS-instructions not-empty)]
      (sutil/update-resources-EADS-json! eads-instructions)
      (log! :error (str "No such EADS instructions " eads-id)))))

;;; ----------------------------------------- Project ---------------------------------------------
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
            new-id   (-> new-name str/lower-case (str/replace #"\s+" "-") keyword)]
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
    :conversation/status :in-progress ; <==================
    :conversation/interviewer-budget 1.0}
   {:conversation/id :data
    :conversation/status :not-started
    :conversation/interviewer-budget 1.0}
   {:conversation/id :resources
    :conversation/status :not-started
    :conversation/interviewer-budget 1.0}
   {:conversation/id :optimality
    :conversation/status :not-started
    :conversation/interviewer-budget 1.0}])

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
         files-dir (-> cfg :base-dir (str "/projects/" pname  "/files"))]
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

(defn ^:admin update-project-for-schema!
  "This has the ADDITIONAL side-effect of writing a backup file."
  [pid]
  (backup-proj-db pid)
  (recreate-project-db! pid))

(defn ^:admin update-all-projects-for-schema!
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
