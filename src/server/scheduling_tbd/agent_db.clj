(ns scheduling-tbd.agent-db
  "These agent-related utilities rely on system and project DBs, unlike everything in llm.clj.
   Most of the work here concerns (1) ensuring that assistants and threads have not been deleted
   by the llm provider, and (2) implementing query-agent."
  (:require
   [clojure.datafy          :refer [datafy]]
   [clojure.edn             :as edn]
   [clojure.java.io         :as io]
   [clojure.pprint          :refer [pprint]]
   [clojure.spec.alpha      :as s]
   [datahike.api            :as d]
   [datahike.pull-api       :as dp]
   [mount.core              :as mount :refer [defstate]]
   [scheduling-tbd.llm      :as llm]
   [scheduling-tbd.sutil    :as sutil :refer [connect-atm default-llm-provider]]
   [scheduling-tbd.util     :as util]
   [taoensso.telemere       :as tel :refer [log!]]
   [wkok.openai-clojure.api :as openai]))

;;; There are three types of agents (:agent-type):
;;;   1) :system :  The agent and a single thread is shared by all projects
;;;       -- It is entirely stored in the system db. Examples: response-analysis-agent, text-to-var.
;;;   2) :project : It is the exclusive property of a project, such as the surrogate expert (only example, so far):
;;;       -- It is entirely stored in the project. The base-type is the pid.
;;;   3) :shared-assistant : All such agents use the same assistant (thus same instructions) but have a thread defined in the project.
;;;       -- Thus :system DB has all agent information but the tid; the project DB has the same plus the tid.
;;;          Examples: the interviewers.
;;; If agent-type is :system, the information maintained for the agent is found in the the system DB.
;;; If agent-type is :project, the information maintained for the agent is found in the the project DB.

(s/def ::agent-id (s/or :system keyword? :project ::agent-id-map))
(s/def ::agent-id-map (s/keys :req-un [::pid ::base-type]))

;;; Throughout the project:
;;;   pid = project-id, a keyword
;;;   cid = conversation id, a keyword #{:process :data :resources :optimality}
;;;   aid = llm-provider's assistant-id, a string.
;;;   tid = llm-provider's thread-id, a string.

;;; Updating agents is a bit tricky. Obviously, if the llm-provider no longer possesses the assistant or thread
;;; a new agent needs to be created. New agents are also created for :system agent-type when the instructions
;;; files from which vector stores are created are updated. By default, outdated files is not sufficient reason
;;; to recreate :shared-assistant agents; if the assistant and thread still exist, it reflects an investment in
;;; context that we don't want to lose. This all goes out the window in 2026, when OpeanAI stops supporting threads.
;;; Use of shared-assistants should probably go away, because threads and maybe even agent ids is going away.

;;; ToDo: Given our desire to use LLMs other than just OpenAI/Azure, I think we should start using project agents everywhere.
;;; Further, we probably will be using techniques like "conversation-history" and inserting instructions with each call
;;; to meet the requirements of Anthropic-style interaction. Maybe as a step towards this, we'd make conversation-history an method.
;;; The idea being that we'd call a conversation-history for surrogates, orchestrators, and interviewers, each of which would
;;; pull different things out of the project DB.

(def ^:diag diag (atom nil))

(s/def ::agent-type #(#{:system :project :shared-assistant} %))
(s/def ::base-type keyword?)
(s/def ::expertise string?)
(s/def ::instruction-path string?)
(s/def ::instruction-string string?)
(s/def ::response-format-path string?) ;; ToDo: Valid JSON schema.
(s/def ::llm-provider #{:openai :azure})
(s/def ::surrogate? boolean?)
(s/def ::tools (s/and vector? (s/coll-of ::tool)))
(s/def ::tool (s/keys :req-un [::type]))
(s/def ::vector-store-paths (s/coll-of string?))
(s/def ::pid keyword?)

(s/def ::agent-info-base ; no PID
  (s/and (s/keys :req-un [::base-type ::agent-type]
                 :opt-un [::instruction-path ::instruction-string ::response-format-path ::llm-provider ::model-class
                          ::tools ::tool-resources ::vector-store-paths ::expertise ::surrogate?])))

(s/def ::agent-info (s/and ::agent-info-base
                           (s/or  :system          #(= (:agent-type %) :system)
                                  :project          (s/and #(= (:agent-type %) :project)          (s/keys :req-un [::pid]))
                                  :shared-assistant (s/and #(= (:agent-type %) :shared-assistant) (s/keys :req-un [::pid])))))

(def agent-infos
  "Used to keep information needed to create system agents and orchestrator agents.
   See resources/agents/agent-infos.edn"
  (atom nil))

(defn agent-log
  "Log info-string to agent log"
  ([msg-text] (agent-log msg-text {}))
  ([msg-text {:keys [console? level] :or {level :info}}]
   (tel/with-kind-filter {:allow :agents}
     (tel/signal!
      {:kind :agents, :level :info, :msg msg-text}))
   (when console? (log! level msg-text))))

(defn agent-db2proj
  "Return a map of agent info translated to project attributes (aid, tid, base-type, expertise, surrogate?)."
  [agent]
  (reduce-kv (fn [m k v]
               (cond (= k :agent/assistant-id)         (assoc m :aid v)
                     (= k :agent/thread-id)            (assoc m :tid v)
                     :else                             (assoc m (-> k name keyword) v)))
             {}
             agent))

(defn agent-eid
  "Return the entity-id of the agent of given base-type in the db at conn-atm (a connection to the system db, or a project db)."
  [base-type llm-provider conn-atm]
  (d/q '[:find ?e .
         :in $ ?base-type ?llm-provider
         :where
         [?e :agent/base-type ?base-type]
         [?e :agent/llm-provider ?llm-provider]]
       @conn-atm base-type llm-provider))

(defn get-agent
  "Return the agent map in db attributes. Returns nil if it doesn't exist."
  ([id] (get-agent id {}))
  ([id {:keys [llm-provider] :or {llm-provider @default-llm-provider} :as opts}]
   (s/valid? ::agent-id id)
   (let [agent (if (keyword? id)
                 (when-let [eid (agent-eid id llm-provider (connect-atm :system))]
                   (dp/pull @(connect-atm :system) '[*] eid))
                 (when-let [eid (agent-eid (:base-type id) llm-provider (connect-atm (:pid id)))]
                   (dp/pull @(connect-atm (:pid id)) '[*] eid)))]
     (when agent ; ToDo: This should probably be temporary, while migrating to projects with timestamps.
       (cond-> agent
         true                               (dissoc :db/id)
         (-> agent :agent/timestamp not)    (assoc :agent/timestamp #inst "2024-12-08T16:12:48.505-00:00"))))))

(def agent-db-info?
  "I don't want to :require db.clj because it needs system-agents.clj, which uses agent-db.clj, so this list
   will have to be updated by hand."
  #{:agent/agent-type :agent/assistant-id :agent/base-type :agent/expertise :agent/id :agent/instruction-path
    :agent/llm-provider :agent/model-class :agent/response-format-path :agent/surrogate? :agent/system-instruction
    :agent/thread-id :agent/timestamp :agent/tools :agent/vector-store-paths})

(defn put-agent!
  "Add the agent to one or more DBs."
  [{:keys [agent-type pid] :as agent-map}]
  (let [agent-map (reduce-kv (fn [m k v] (if (agent-db-info? k) (assoc m k v) m)) {} agent-map)
        eid-sys (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @(connect-atm :system))
        eid-pid (when pid (d/q '[:find ?eid . :where [?eid :project/id]] @(connect-atm pid)))]
    (when (and (= agent-type :shared-assistant) (not pid))
      (throw (ex-info "Cannot put-agent! shared-assistant without providing a PID." {:agent-map agent-map})))
    (when (#{:shared-assistant :system} agent-type)
      (d/transact (connect-atm :system)
                  {:tx-data [{:db/id eid-sys :system/agents agent-map}]}))
    (when pid ; The other half of the shared assistant or just project (e.g. surrogate).
      (d/transact (connect-atm pid)
                  {:tx-data [{:db/id eid-pid :project/agents agent-map}]}))
    agent-map))

(defn make-agent-assistant
  "Create an agent sans :agent/thread-id (even if this will eventually be needed, (#{:project :system} agent-type).
   Returns the object in DB attributes."
  [{:keys [base-type agent-type llm-provider model-class surrogate? expertise instruction-path instruction-string
           response-format-path vector-store-paths tools]
    :or {llm-provider @default-llm-provider
         model-class :gpt} :as agent-info}]
  ;(log! :info (str "Creating agent: " base-type))
  (s/valid? ::agent-info agent-info)
  (let [user (-> (System/getenv) (get "USER"))
        a-name (-> base-type name (str "-" (name llm-provider)) keyword)
        file-objs (doall (mapv #(llm/upload-file {:fname %}) (map #(-> % io/resource io/file str) vector-store-paths)))
        v-store (when (not-empty file-objs) (llm/make-vector-store {:file-ids (mapv :id file-objs)}))
        assist (llm/make-assistant (cond-> {}
                                     true                  (assoc :name a-name)
                                     true                  (assoc :metadata {:usage :stbd-agent :user user :base-type base-type})
                                     instruction-path      (assoc :instructions (-> instruction-path io/resource slurp))
                                     instruction-string    (assoc :instructions instruction-string)
                                     tools                 (assoc :tools tools)
                                     response-format-path  (assoc :response-format (-> response-format-path io/resource slurp edn/read-string))
                                     v-store               (assoc :tool-resources {"file_search" {"vector_store_ids" [(:id v-store)]}})))
        aid    (:id assist)
        agent (cond-> {:agent/id a-name
                       :agent/base-type base-type
                       :agent/agent-type agent-type
                       :agent/llm-provider llm-provider
                       :agent/model-class model-class
                       :agent/timestamp (util/now)
                       :agent/assistant-id aid}
                surrogate?     (assoc :agent/surrogate? true)
                expertise      (assoc :agent/expertise expertise))]
    (agent-log (str "\n\n Creating agent (no thread yet) " base-type ":\n" (with-out-str (pprint agent))))
    agent))

(defn newest-file-modification-date
  "Return the modification date (Epoch seconds) of the most recently modified file used to define the agent."
  [agent-info]
  (let [{:keys [instruction-path response-format-path vector-store-paths]} agent-info
        files (cond-> []
                instruction-path (conj instruction-path)
                response-format-path (conj response-format-path)
                vector-store-paths (into vector-store-paths))
        zippy  (reset! diag files)
        newest (when (not-empty files)
                 (apply max (mapv #(->> % io/resource io/file .lastModified) files)))]
    (when newest
      (new java.util.Date newest))))

(defn remake-needs
  "This makes PRAGMATIC and POLICY-BASED decisions on remaking an agent or creating a newest thread for it.
   It updates the argument status object with :remake? and :make-thread? appropriately.
   The PRAGMATIC decision is simply that if the llm-provider doesn't have the assistant or thread, the agent is recreated.
   The POLICY-BASED decision is that if the agent is :share-assistant and the llm-provider has the stuff,
   it won't be recreated, even if the assistant instructions are outdated relative to what the agent is using.
   This choice is to preserve the context already built up by the current agent.
   You can force a fresh instance in ensure-agent! with :force-new? = true. This is the default for surrogate experts."
  [{:keys [base-type agent-type missing-here missing-at-provider outdated? substitute-aid force-new?] :as status}]
  (let [missing? (or missing-here missing-at-provider)
        res (case agent-type
              (:project :system)    (cond-> status
                                      (or missing? outdated? force-new?)    (-> (assoc :make-agent? true) (assoc :make-thread? true)))
              :shared-assistant     (cond-> status
                                      force-new?                        (-> (assoc :make-agent? true)     (assoc :make-thread? true))
                                      (:assistant missing-at-provider)  (-> (assoc :make-agent? true)     (assoc :make-thread? true))
                                      (:thread missing-at-provider)         (assoc :make-thread? true)))]
    (when substitute-aid
      (log! :info (str "Agent " base-type " will use an updated assistant: " substitute-aid "."))
      (agent-log (str "Agent " base-type " will use an updated assistant: " substitute-aid ".")))
    (when (:make-agent? res)
      (log! :info (str "Agent " base-type " will be created."))
      (agent-log (str "\n Agent " base-type " will be recreated.")))
    (when (:make-thread? res) ; ToDo: Could pass in PID here and mention it.
      (log! :info (str "A thread will be made for agent " base-type " agent-type " agent-type "."))
      (agent-log (str "\n\n A thread will be made for agent " base-type " agent type " agent-type ".")))
    res))

(defn db-agents
  "Return a map with indexes :system and/or :project which indicates what is known about the subject agent in that context."
  [{:keys [base-type agent-type pid llm-provider]
    :or {llm-provider @default-llm-provider}
    :as _agent-info}]
  (letfn [(get-agent-info [conn-atm]
            (when-let [eid (agent-eid base-type llm-provider conn-atm)]
              (dp/pull @conn-atm '[*] eid)))]
    (case agent-type
      :system            {:system   (get-agent-info (connect-atm :system))}
      :project           {:project  (get-agent-info (connect-atm pid))}
      :shared-assistant  {:system (get-agent-info (connect-atm :system))
                          :project (if pid (get-agent-info (connect-atm pid)) {})})))

(defn agent-status-shared-assistant
  "Provide a bit more information (than with agent-status-basic) for agent-type = :shared-assistant,
   namely, :same-assistant? :system-more-modern?, and, of course, look at the project to get the :agent/thread-id."
  [{:keys [base-type agent-type pid] :as agent-info}]
  (let [db-info (db-agents agent-info)
        files-modify-date (newest-file-modification-date agent-info)
        outdated? (and files-modify-date (== 1 (compare files-modify-date (-> db-info :system :agent/timestamp))))
        system-aid  (-> db-info :system :agent/assistant-id)
        project-aid (-> db-info :project :agent/assistant-id)
        project-tid (-> db-info :project :agent/thread-id)
        same-assistant? (= system-aid project-aid)
        system-more-modern? (when (not same-assistant?)
                              (== 1 (compare (-> db-info :system :agent/timestamp) (-> db-info :project :agent/timestamp))))
        missing-here (cond-> #{}
                       (not project-aid)      (conj :assistant)
                       (not project-tid)      (conj :thread))
        project-provider-aid? (llm/get-assistant project-aid)
        system-provider-aid?  (llm/get-assistant system-aid)
        substitute-aid        (when (and system-provider-aid? (not project-provider-aid?)) system-aid)
        provider-tid?  (llm/get-thread project-tid) ;(when system-tid  (llm/get-thread project-tid))
        missing-provider (cond-> #{}
                           (and (not system-provider-aid?)
                                (not project-provider-aid?)) (conj :assistant)
                           (and pid (not provider-tid?))     (conj :thread))
        status (cond-> (merge agent-info {:base-type base-type,
                                          :agent-type agent-type
                                          :same-assistant? same-assistant?
                                          :agent-date (-> db-info :system :agent/timestamp)})
                 pid                                    (assoc :project-has-thread project-tid)
                 substitute-aid                         (assoc :substitute-aid substitute-aid)
                 (not pid)                              (assoc :project-has-thread :not-applicable)
                 system-more-modern?                    (assoc :system-more-modern? true)
                 files-modify-date                      (assoc :files-data files-modify-date)
                 outdated?                              (assoc :outdated? true)
                 (not-empty missing-here)               (assoc :missing-here missing-here)
                 (not-empty missing-provider)           (assoc :missing-at-provider missing-provider))]
    (remake-needs status)))

(defn agent-status-basic
  "Find the agent status information for :project and :system agents."
  [{:keys [base-type llm-provider pid]
    :or {llm-provider @default-llm-provider} :as agent}]
  (let [files-modify-date (newest-file-modification-date agent)
        conn-atm (if pid (connect-atm pid) (connect-atm :system))
        eid (agent-eid base-type llm-provider conn-atm)
        {:agent/keys [timestamp assistant-id thread-id]} (when eid (dp/pull @conn-atm '[*] eid))
        outdated? (and files-modify-date
                       (== 1 (compare files-modify-date timestamp)))
        missing-here  (cond-> #{}
                        (not assistant-id)      (conj :assistant)
                        (not thread-id)         (conj :thread))
        provider-aid? (when assistant-id (llm/get-assistant assistant-id))
        provider-tid? (when thread-id    (llm/get-thread thread-id))
        missing-provider (cond-> #{}
                           (not provider-aid?)            (conj :assistant)
                           (and pid (not provider-tid?))  (conj :thread))
        status (cond-> agent ; {:base-type base-type, :agent-type agent-type}
                 files-modify-date                      (assoc :files-data files-modify-date)
                 timestamp                              (assoc :agent-date timestamp)
                 outdated?                              (assoc :outdated? true)
                 (not eid)                              (assoc :missing-here #{:assistant :thread})
                 (and eid (not-empty missing-here))     (assoc :missing-here missing-here)
                 (and eid (not-empty missing-provider)) (assoc :missing-at-provider missing-provider))]
    (remake-needs status)))

(defn merge-with-agent-info-atom
  "Merge the argument information about the agent with that about it on agent-infos,
         with the argument taking precedence."
  [info]
  (assert (map? info))
  (if-let [agent-info (not-empty (get @agent-infos (:base-type info)))]
    (merge agent-info info)
    (do (log! :warn (str "Nothing known about agent " (with-out-str (pprint info))))
        info)))

;;; OpenAI may delete them after 30 days. https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
(defn agent-status
  "Get status of agents created by means of agent-infos.
   Compare the most-recently modified file used to create the agent to the agent creation timestamp.
   Return a map with
     {:missing-here?         -- a boolean indicating that no agent matches the parameters in the relevant db.
      :missing-at-provider?  -- a boolean indicating that the llm-provider no longer possesses an assistant with the :agent/assistant-id in the DB.
      :outdated?  -- a boolean indicating that the :agent/timestamp is older than the most recently modified files used to create it,
      :files-date -- the .lastModified (as Instant) of the most recently modified file used to create the agent, and
      :agent-date -- if not missing?, the timepoint (Instant) when the agent was created.}

   :agent-date is not provided if the agent is not present in the any database we maintain."
  [id]
  (s/assert ::agent-id id)
  (let [agent (-> id get-agent agent-db2proj)]
    (case (:agent-type agent)
      (:system :project) (agent-status-basic agent)
      :shared-assistant  (agent-status-shared-assistant agent))))

(defn make-agent-thread
  "Return a tid for the argument agent, which must have an assisatnt-id.
   Write to the agent-log about it."
  [{:agent/keys [assistant-id pid agent-type] :as agent}]
  (let [tid (-> (llm/make-thread :assistant-id assistant-id) :id)
        agent (assoc agent :agent/thread-id tid)]
    (case agent-type

      :system
      (do (log! :info (str "Adding thread " tid " to system DB.\n" (with-out-str (pprint agent))))
          (agent-log (str "\n\n Adding thread " tid " to system DB\n" (with-out-str (pprint agent)))))

      (:project :shared-assistant)
      (log! :debug (str "Adding thread " tid " to project " pid ".\n" (with-out-str (pprint agent))))
      (agent-log (str "\n\n Adding thread " tid " to project " pid ".\n" (with-out-str (pprint agent)))))
    tid))

;;; OpenAI may delete them after 30 days. https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
;;; Once the agent is created, and checks on aid and sid memoized, this executes in less than 5 milliseconds.
(defn ensure-agent!
  "Return agent map with project keys (:aid, tid...)  if the agent exists.
   Otherwise create the agent, store it and return the info.
   The two ways to identify what agent you are looking for are:
      1) a keyword uniquely naming the agent (if it is a system agent)
      2) a map providing the :pid, :base-type (if it is a project agent).

   In either case, you can provide a second argument, a map of options:
      :make-agent? - true if start from scratch
      :make-thread? - deprecated with migration from OpenAI assistants.
      :substitute-aid - deprecated with migration from OpenAI assistants."
  ([id] (ensure-agent! id {}))
  ([id opts]
   (s/assert ::agent-id id)
   (let [needs (agent-status id)
         {:keys [make-agent? make-thread? substitute-aid]} (merge needs opts)
         agent (-> id get-agent agent-db2proj)
         agent (if make-agent?
                 (make-agent-assistant agent) ; Returns agent in DB attributes.
                 (get-agent id))              ; Returns agent in DB attributes.
         agent  (cond-> agent
                  substitute-aid   (assoc :agent/assistant-id substitute-aid)
                  make-thread?     (assoc :agent/thread-id (make-agent-thread agent)))]
     (put-agent! agent)
     (agent-db2proj agent))))

;;; ----------------------------------- Higher-level usages ------------------------------------------

(defn openai-messages-matching
  "Argument is a vector of openai messages from an assistant.
   Return a vector of messages matching the argument conditions.
     :role  - #{'assistant', 'user'}
     :text  - A complete match on the (-> % :content first :text value).
     :after - Messages with :created_at >= than this."
  [msg-list {:keys [role text after]}]
  (cond->> msg-list
    role     (filterv #(= role (:role %)))
    text     (filterv #(= text (-> % :content first :text :value)))
    after    (filterv #(<= after (:created_at %)))))

(defn response-msg
  "Return the text that is response to the argument question."
  [question msg-list]
  (when-let [question-time (-> (openai-messages-matching msg-list {:role "user" :text question}) first :created_at)]
    (when-let [responses (openai-messages-matching msg-list {:role "assistant" :after question-time})]
      (->> responses (sort-by :created_at) first :content first :text :value))))

;;; You can also use this with role 'assistant', but the use cases might be a bit esoteric. (I can't think of any.)
;;; https://platform.openai.com/docs/api-reference/messages/createMessage#messages-createmessage-role
(defn query-on-thread-aux
  "Create a message for ROLE on the project's (PID) thread and run it, returning the result text.
    aid      - assistant ID (the OpenAI notion)
    tid      - thread ID (the OpenAI notion)
    role     - #{'user' 'assistant'},
    query-text - a string.
   Returns text but uses promesa internally to deal with errors."
  [aid tid role query-text timeout-secs llm-provider]
  (assert (string? aid))
  (assert (string? tid))
  (assert (#{"user" "assistant"} role))
  (assert (number? timeout-secs))
  (assert (keyword? llm-provider))
  (assert (and (string? query-text) (not-empty query-text)))
  (let [creds (sutil/api-credentials llm-provider)
        ;; Apparently the thread_id links the run to msg.
        _msg (openai/create-message {:thread_id tid :role role :content query-text} creds)
        ;; https://platform.openai.com/docs/assistants/overview?context=without-streaming
        ;; Once all the user Messages have been added to the Thread, you can Run the Thread with any Assistant.
        run (openai/create-run {:thread_id tid :assistant_id aid} creds)
        timestamp (inst-ms (java.time.Instant/now))
        timeout   (+ timestamp (* timeout-secs 1000))]
    (loop [now timeout]
      (Thread/sleep 1000)
      (let [r (openai/retrieve-run {:thread_id tid :run-id (:id run)} creds)
            msg-list (-> (openai/list-messages {:thread_id tid :limit 20} creds) :data) ; ToDo: 20 is a guess.
            response (response-msg query-text msg-list)]
        (cond (> now timeout)                        (do (log! :warn "Timeout")
                                                         (throw (ex-info "query-on-thread: Timeout:" {:query-text query-text}))),

              (and (= "completed" (:status r))
                   (not-empty response))              (sutil/markdown2html response),

              (and (= "completed" (:status r))
                   (empty? response))                 (do (log! :warn "empty response")
                                                          (throw (ex-info "query-on-thread empty response:" {:status (:status r)}))),


              (#{"expired" "failed"} (:status r))     (do (log! :warn (str "failed/expired last_error = " (:last_error r)))
                                                          (throw (ex-info "query-on-thread failed:" {:status (:status r)}))),

              :else                                   (recur (inst-ms (java.time.Instant/now))))))))

(defn query-on-thread
  "Wrap query-on-thread-aux to allow multiple tries at the same query.
    :test-fn a function that should return true on a valid result from the response. It defaults to a function that returns true.
    :preprocesss-fn is a function that is called before test-fn; it defaults to identity."
  [& {:keys [aid tid role query-text timeout-secs llm-provider test-fn preprocess-fn asked-role asking-role]
      :or {test-fn (fn [_] true),
           preprocess-fn identity
           ;asked-role :unknown
           ;asking-role :unknown
           llm-provider @default-llm-provider
           timeout-secs 60
           role "user"} :as obj}]
  (let [obj (cond-> obj ; All recursive calls will contains? :tries.
              (or (not (contains? obj :tries))
                  (and (contains? obj :tries) (-> obj :tries nil?))) (assoc :tries 1))]
    (assert (< (:tries obj) 10))
    (if (> (:tries obj) 0)
      (try
        ;(agent-log (str "\n\n" (name asking-role) " ===> " query-text))
        (let [raw (query-on-thread-aux aid tid role query-text timeout-secs llm-provider)
              res (preprocess-fn raw)]
          ;(agent-log (str "\n" (name asked-role) " <=== " raw))
          (if (test-fn res) res (throw (ex-info "Try again" {:res res}))))
        (catch Exception e
          (let [d-e (datafy e)]
            (log! :warn (str "query-on-thread failed (tries = " (:tries obj) "):\n "
                             "\nmessage: " (-> d-e :via first :message)
                             "\ncause: " (-> d-e :data :body)
                             "\ntrace: " (with-out-str (pprint (:trace d-e))))))
          (query-on-thread (update obj :tries dec))))
      (log! :warn "Query on thread exhausted all tries."))))

(defn query-agent
  "Make a query to a named agent. Convenience function for query-on-thread.
    opts-map might include:
       :test-fn - a function to test the response of validity,
       :tries - how many times to ask the agent while test-fn fails.
       :asked-role - defaults to the base-type, so entirely unnecessary if agent-or-info is the agent keyword.
       :asking-role - useful for logging and debugging, typically some interviewer keyword."
  ([agent-or-info text] (query-agent agent-or-info text {}))
  ([agent-or-info text opts-map]
   (try
     (let [agent (cond (keyword? agent-or-info)                   (ensure-agent! {:base-type agent-or-info})
                       (and (map? agent-or-info)
                            (contains? agent-or-info :aid)
                            (contains? agent-or-info :tid))       agent-or-info
                       (and (map? agent-or-info)
                            (contains? agent-or-info :base-type)) (ensure-agent! agent-or-info))
           {:keys [aid tid base-type]} agent]
       (assert (string? aid))
       (assert (string? tid))
       (query-on-thread (merge opts-map {:aid aid
                                         :tid tid
                                         :query-text text})))
     (catch Exception e
       (log! :error (str "query-agent failed: " e))))))

;;; -------------------- Starting and stopping -------------------------
(defn init-agent-infos-from-edn
  "Set static information for agent-infos atom.
   Other parts of the program, such as ork, will may add to it at startup."
  []
  (reset! agent-infos (-> "agents/agent-infos.edn" io/resource slurp edn/read-string)))

(defn init-agents!
  "Things done on start and restart."
  []
  (init-agent-infos-from-edn)
  :ok)

(defstate agent-db
  :start (init-agents!))
