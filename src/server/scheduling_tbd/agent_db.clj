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
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.llm      :as llm]
   [scheduling-tbd.specs    :as specs]
   [scheduling-tbd.sutil    :as sutil :refer [connect-atm default-llm-provider]]
   [scheduling-tbd.util     :as util :refer [now]]
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
(s/def ::agent-id (s/or :system-agent keyword? :project-agent ::agent-id-map))
(s/def ::agent-id-map (s/keys :req-un [::pid ::base-type]))
(s/def ::base-type keyword?)
(s/def ::pid keyword?)

(defn system-agent-id?
  "A system agent-id is a keyword; the others are maps with :pid and :base-type."
  [id]
  (keyword? id))

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

;;; (adb/make-agent-basics :foo {:agent/agent-type :project})
;;; (adb/make-agent-basics {:base-type :sur-foo :pid :sur-foo} {:agent/agent-type :project :agent/surrogate? true})
(defn make-agent-basics
  "Create essential map attributes of an agent (everything but aid and tid) using a template (if available) and a props map.
   The props map contains agent-namespaced properties that have the same name as agent-db (namespaced) properties.
   Options will override anything found in the template (if one is available)"
  ([agent-id] (make-agent-basics agent-id {}))
  ([agent-id props]
   (reset! diag [agent-id props])
   (assert (every? specs/agent-key? (keys props)))
   (let [btyp (if (keyword? agent-id) agent-id (:base-type agent-id))
         template (db/get-agent btyp nil)
         props (merge template props)
         {:agent/keys [base-type agent-type llm-provider model-class surrogate? expertise instruction-path
                       instruction-string response-format-path vector-store-paths tools pid]
          :or {llm-provider @default-llm-provider model-class :gpt}} props
         pid (or pid (:pid agent-id)) ; Will still be nil if it is a template.
         agent-type (if surrogate? :project agent-type)
         base-type (or btyp base-type)]
     (assert base-type)
     (assert (#{:system :project :shared-assistant} agent-type))
     (cond-> {:agent/agent-id (-> base-type name (str "-" (name llm-provider)) keyword)
              :agent/base-type base-type
              :agent/agent-type agent-type
              :agent/llm-provider llm-provider
              :agent/model-class model-class
              :agent/timestamp (now)}
       surrogate?            (assoc :agent/surrogate? true)
       expertise             (assoc :agent/expertise expertise)
       instruction-path      (assoc :agent/instruction-path instruction-path)
       instruction-string    (assoc :agent/instruction-string instruction-string)
       response-format-path  (assoc :agent/response-format-path response-format-path)
       vector-store-paths    (assoc :agent/vector-store-paths vector-store-paths)
       tools                 (assoc :agent/tools tools)
       pid                   (assoc :agent/pid pid)))))

(defn get-llm-assistant-id
  "Create an agent sans :agent/thread-id (even if this will eventually be needed, (#{:project :system} agent-type).
   Returns the object in DB attributes."
  [{:agent/keys [base-type llm-provider model-class instruction-path instruction-string response-format-path vector-store-paths tools]
    :or {llm-provider @default-llm-provider
         model-class :gpt} :as agent-info}]
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
                                     v-store               (assoc :tool-resources {"file_search" {"vector_store_ids" [(:id v-store)]}})))]
    (agent-log (str "\n\n Added aid " (:id assist) " to agent " a-name))
    (:id assist)))

(defn assistant-creation-inst
  "Return the instant object marking when the OpenAI assistant was created."
  [aid]
  (-> aid llm/get-assistant :created_at (* 1000) java.util.Date.))

(defn newest-file-modification-date
  "Return the modification date (instant object) of the most recently modified file used to define the agent."
  [agent-info]
  (let [{:keys [instruction-path response-format-path vector-store-paths]} agent-info
        files (cond-> []
                instruction-path (conj instruction-path)
                response-format-path (conj response-format-path)
                vector-store-paths (into vector-store-paths))
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
   This choice is to preserve the context already built up by the current agent."
  [{:keys [missing-here missing-at-provider outdated? substitute-aid] :as status}
   {:keys [base-type agent-type pid] :as agent}]
  (let [missing? (or missing-here missing-at-provider)
        res (case agent-type
              (:project :system)    (cond-> status
                                      (or missing? outdated?)           (-> (assoc :make-assistant? true) (assoc :make-thread? true)))
              :shared-assistant     (cond-> status
                                      outdated?                         (-> (assoc :make-assistant? true) (assoc :make-thread? true))
                                      (:assistant missing-at-provider)  (-> (assoc :make-assistant? true) (assoc :make-thread? true))
                                      (:thread missing-at-provider)         (assoc :make-thread? true)
                                      (and pid (:thread missing-here))  (assoc :make-thread? true)))] ; Not a system template, project affiliated.
    (when substitute-aid
      (log! :info (str "Agent " base-type " will use an updated assistant: " substitute-aid "."))
      (agent-log (str "Agent " base-type " will use an updated assistant: " substitute-aid ".")))
    (when (:make-assistant? res)
      (log! :info (str "Agent " base-type " will be created."))
      (agent-log (str "\n Agent " base-type " will be recreated.")))
    (when (:make-thread? res) ; ToDo: Could pass in PID here and mention it.
      (log! :info (str "A thread will be made for agent " base-type " agent-type " agent-type "."))
      (agent-log (str "\n\n A thread will be made for agent " base-type " agent type " agent-type ".")))
    res))

(defn agent-status-shared-assistant
  "Provide a bit more information (than with agent-status-basic) for agent-type = :shared-assistant,
   namely, :same-assistant? :system-more-modern?, and, of course, look at the project to get the :agent/thread-id."
  [{:keys [base-type agent-type pid timestamp aid tid] :as agent}]
  (let [files-modify-date (newest-file-modification-date agent)
        outdated? (or (not aid) (and files-modify-date (== 1 (compare files-modify-date  (assistant-creation-inst aid)))))
        system-agent (db/get-agent base-type)
        system-aid  (:agent/assistant-id system-agent)
        project-aid aid
        project-tid tid
        same-assistant? (= system-aid project-aid)
        system-more-modern? (when (not same-assistant?)
                              (== 1 (compare (:agent/timestamp system-agent) timestamp)))
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
                           (and pid (not provider-tid?))     (conj :thread))]
    (cond-> {}
      same-assistant?                        (assoc :same-assistant? true)
      pid                                    (assoc :project-has-thread project-tid)
      substitute-aid                         (assoc :substitute-aid substitute-aid)
      (not pid)                              (assoc :project-has-thread :not-applicable)
      system-more-modern?                    (assoc :system-more-modern? true)
      files-modify-date                      (assoc :files-data files-modify-date)
      outdated?                              (assoc :outdated? true)
      (not-empty missing-here)               (assoc :missing-here missing-here)
      (not-empty missing-provider)           (assoc :missing-at-provider missing-provider))))

(defn agent-status-basic
  "Find the agent status information for :project and :system agents."
  [{:keys [base-type llm-provider pid timestamp aid tid llm-provider] :as agent}]
  (let [files-modify-date (newest-file-modification-date agent)
        outdated? (or (not aid) (and files-modify-date(== 1 (compare files-modify-date (assistant-creation-inst aid)))))
        missing-here  (cond-> #{}
                        (not aid)      (conj :assistant)
                        (not tid)      (conj :thread))
        provider-aid? (when aid (llm/get-assistant aid))
        provider-tid? (when tid (llm/get-thread tid))
        missing-provider (cond-> #{}
                           (not provider-aid?)            (conj :assistant)
                           (and pid (not provider-tid?))  (conj :thread))]
     (cond-> {}
       files-modify-date                      (assoc :files-data files-modify-date)
       timestamp                              (assoc :agent-date timestamp)
       outdated?                              (assoc :outdated? true)
       (not-empty missing-here)               (assoc :missing-here missing-here)
       (not-empty missing-provider)           (assoc :missing-at-provider missing-provider))))

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
  (let [pid (:pid id)
        agent (cond-> (db/get-agent id)
                true         agent-db2proj
                pid          (assoc :pid pid))]
    (-> (case (:agent-type agent)
          (:system :project) (agent-status-basic agent)
          :shared-assistant  (agent-status-shared-assistant agent))
        (remake-needs agent))))

(defn get-llm-thread-id
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
   Otherwise create the agent, store it and return the same.
   The two ways to identify what agent you are looking for are:
      1) a keyword uniquely naming the agent (if it is a system agent)
      2) a map providing the :pid, :base-type (if it is a project agent).

   In either case, you can provide a second argument, a map of options:
      :force-new? - if true, create the agent anew.
      :substitute-aid - deprecated with migration from OpenAI assistants.
   If the agent is a system agent and :shared-assistant, not tid is created."
  ([agent-id] (ensure-agent! agent-id {}))
  ([agent-id {:keys [substitute-aid] :as opts-and-agent-props}]
   (s/assert ::agent-id agent-id)
   (let [{:keys [opts props]} (reduce-kv (fn [m k v] (if (= "agent" (namespace k))
                                                       (assoc-in m [:props k] v)
                                                       (assoc-in m [:opts k] v)))
                                         {:props {} :opts {}}
                                         opts-and-agent-props)
         {:keys [force-new?]} opts
         pid   (:pid agent-id)
         props (cond-> props pid (assoc :agent/pid pid))
         {:keys [make-assistant? make-thread?]} (agent-status agent-id)
         agent (make-agent-basics agent-id props)
         make-assistant? (and (or make-assistant? force-new?) (not substitute-aid))
         make-thread? (and (or make-thread? force-new?)      ; outdated, or forced...
                           (not (and (system-agent-id? agent-id)   ; ...but not the system 'template' for a shared assistant.
                                     (= :shared-assistant (:agent/agent-type agent)))))
         agent (cond-> agent
                 make-assistant?  (assoc :agent/assistant-id (get-llm-assistant-id agent))
                 substitute-aid   (assoc :agent/assistant-id substitute-aid)
                 make-thread?     (assoc :agent/thread-id (get-llm-thread-id agent)))]
     (db/put-agent! agent-id agent)
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
    tid      - thread ID (ttheOpenAI notion)
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
           llm-provider @default-llm-provider
           timeout-secs 60
           role "user"} :as obj}]
  (let [obj (cond-> obj ; All recursive calls will contains? :tries.
              (or (not (contains? obj :tries))
                  (and (contains? obj :tries) (-> obj :tries nil?))) (assoc :tries 1))]
    (assert (< (:tries obj) 10))
    (if (> (:tries obj) 0)
      (try
        (let [raw (query-on-thread-aux aid tid role query-text timeout-secs llm-provider)
              res (preprocess-fn raw)]
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
  ([agent-or-id text] (query-agent agent-or-id text {}))
  ([agent-or-id text opts-map]
   (try
     (let [agent (cond (and (map? agent-or-id)
                            (contains? agent-or-id :aid)
                            (contains? agent-or-id :tid))    agent-or-id
                       (s/valid? ::agent-id agent-or-id)     (ensure-agent! agent-or-id)
                       :else                                 (log! :error (str "query-agent: Invalid agent spec: " agent-or-id)))
           {:keys [aid tid base-type]} agent]
       (assert (string? aid))
       (assert (string? tid))
       (query-on-thread (merge opts-map {:aid aid
                                         :tid tid
                                         :query-text text})))
     (catch Exception e
       (log! :error (str "query-agent failed: " e))))))
