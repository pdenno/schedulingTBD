(ns scheduling-tbd.agent-db
  "These agent-related utilities rely on system and project DBs, unlike llm.clj."
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

(def ^:diag diag (atom nil))


(s/def ::agent-type #(#{:system :project :shared-assistant} %))
(s/def ::base-type keyword?)
(s/def ::pid keyword?) ; So far, only :project agent types are surrogates (and :base-type = pid), but other project agents will need this explicitly.
(s/def ::expertise string?)
(s/def ::instruction-path string?)
(s/def ::instruction-string string?)
(s/def ::response-format-path string?) ;; ToDo: Valid JSON schema.
(s/def ::llm-provider #{:openai :azure})
(s/def ::surrogate? boolean?)
(s/def ::tools (s/and vector? (s/coll-of ::tool)))
(s/def ::tool (s/keys :req-un [::type]))
(s/def ::vector-store-paths (s/coll-of string?))


(s/def ::agent-info (s/and (s/keys :req-un [::base-type ::agent-type]
                                   :opt-un [::instruction-path ::instruction-string ::response-format-path ::llm-provider ::model-class
                                            ::tools ::tool-resources ::vector-store-paths ::expertise ::surrogate? ::pid])
                           #(or (contains? % :instruction-path) (contains? % :instruction-string))))

(def agent-infos
  "This is a map, indexed by base-type with values being maps providing information sufficient to create/recreate agents.
   In addition to those found in agents/agent-infos.edn, we add to this surrogate domain experts when they are started."
  (atom (reduce (fn [res info] (assoc res (:base-type info) info))
                {}
                (-> (io/resource "agents/agent-infos.edn")
                    slurp
                    edn/read-string))))

(defn check-agent-infos
  []
  (doseq [[base-type info] (seq @agent-infos)]
    (when-not (s/valid? ::agent-info info)
      (log! :error (str base-type " is not a valid agent-info:\n" (with-out-str (pprint info)))))))

(check-agent-infos)

(defn add-agent-info!
  "Add an agent info at the given base-type."
  [base-type info]
  (if (s/valid? ::agent-info info)
    (swap! agent-infos #(assoc % base-type info))
    (log! :error (str "Invalid agent-info: " info))))

(defn agent-db2proj
  "Return a map of agent info translated to project attributes (aid, tid, role, expertise, surrogate?)."
  [agent]
  (reduce-kv (fn [m k v]
               (cond (= k :agent/assistant-id)         (assoc m :aid v)
                     (= k :agent/thread-id)            (assoc m :tid v)
                     (= k :agent/base-type)            (assoc m :role v)
                     (= k :agent/expertise)            (assoc m :expertise v)
                     (= k :agent/surrogate?)           (assoc m :surrogate? true)
                     :else m))
             {}
             agent))

(defn agent-eid
  "Return the entity-id of the agent of given base-type in the db at conn-atm (a connection to the system db, or a project db)."
  [base-type llm-provider conn-atm]
  ;;{:post [(-> % :aid string?) (-> % :tid string?)]}
  (d/q '[:find ?e .
         :in $ ?base-type ?llm-provider
         :where
         [?e :agent/base-type ?base-type]
         [?e :agent/llm-provider ?llm-provider]]
       @conn-atm base-type llm-provider))

#_(defn agent-base-type
  "Return the base-type (keyword) of the agent with the given aid or :unknown-<aid> if you can't find the agent.
   This only looks for system-level agents, so :unknown-<aid> will happen occassionally."
  [aid]
  (or
   (d/q '[:find ?base-type .
          :in $ ?aid
          :where
          [?eid :agent/assistant-id ?aid]
          [?eid :agent/base-type ?base-type]]
        @(connect-atm :system) aid)
   (-> (format "unknown-%s" aid) keyword)))

;;; ToDo: In ensure-agent, use agent-info to determine what needs to checked as up-to-date and where to collect data.
;;;       Likewise, have add-agent use this information to decide where to put things.
;;;       I think the project needs both the assistant-id and the thread-id, otherwise the assistant might be destroyed and we won't know that the thread is useless.

;;; (adb/add-agent! (some #(when (= :response-analysis-agent (:base-type %)) %) adb/agent-infos))
(defn add-agent!
  "Create an agent (assistant + thread) and store it in one or more databases.
   There are three types of agents (:agent-type):
     1) :system-agent :  The agent and a single thread is shared by all projects
           -- It is entirely stored in the system db.
     2) :project-agent : It is the exclusive property of a project, such as the surrogate expert:
           -- It is entirely stored in the project. The base-type is the pid.
     3) :shared-assistant : All such agents use the same assistant (thus same instructions) but have a thread defined in the project.
           -- Thus :system DB has everything but the tid; project DB has everything.

   Returns the object in DB attributes, combined, if necessary."
  [{:keys [base-type agent-type llm-provider pid model-class surrogate? expertise instruction-path instruction-string
           response-format-path vector-store-paths tools]
    :or {llm-provider @default-llm-provider
         model-class :gpt} :as agent-info}]
  (log! :info (str "Creating agent: " base-type))
  (tel/with-kind-filter {:allow :agents}
    (tel/signal! {:kind :agents :level :info :msg (str "\n\n Creating agent " base-type "")}))
  (s/valid? ::agent-info agent-info)
  (let [pid (or pid (when surrogate? base-type))
        user (-> (System/getenv) (get "USER"))
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
        thread (when (not= agent-type :shared-assistant)
                 (llm/make-thread {:assistant-id aid
                                   :llm-provider llm-provider
                                   :metadata {:usage :stbd-agent :user user :base-type base-type}}))
        tid     (:id thread)
        eid-sys (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @(connect-atm :system))
        eid-pid (when pid (d/q '[:find ?eid . :where [?eid :project/id]] @(connect-atm pid)))
        a-map (cond-> {:agent/id a-name
                       :agent/base-type base-type
                       :agent/llm-provider llm-provider
                       :agent/model-class model-class
                       :agent/timestamp (util/now)
                       :agent/assistant-id aid}
                tid         (assoc :agent/thread-id tid)
                surrogate?  (assoc :agent/surrogate? true)
                expertise   (assoc :agent/expertise expertise))]
    ;(reset! diag a-map)
    (when (= agent-type :shared-assistant)
      (d/transact (connect-atm :system)
                  {:tx-data [{:db/id eid-sys :system/agents (dissoc a-map :agent/thread)}]}))
    (when (or pid (= agent-type :project)) ; The other half of the shared assistant or just project (e.g. surrogate).
      (d/transact (connect-atm pid)
                  {:tx-data [{:db/id eid-pid :project/agents a-map}]}))
    (when (= agent-type :system)
      (d/transact (connect-atm :system)
                  {:tx-data [{:db/id eid-sys :system/agents a-map}]}))))


(defn newest-file-modification-date
  "Return the modification date (Epoch seconds) of the most recently modified file used to define the agent."
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

(defn need-to-remake?
  "This makes a pragmatic and policy-based decision on remaking an agent (e.g. for ensure-agent!).
   The pragmatic decision is simply if the llm-provider doesn't have the assistant or thread, the agent is recreated.
   The policy-based decision is that if the agent is :share-assistant and the llm-provider has the stuff,
   it won't be recreated, even if the assistant instructions are outdated relative to what the agent is using.
   This choice is to preserve the context already built up by the current agent.
   You can force creation in ensure-agent with :force? = true.

   Warns appropriately."
  [{:keys [base-type agent-type missing-here missing-at-provider outdated?]}]
  (let [missing? (or missing-here missing-at-provider)
        where (if missing-here "in databases" "at llm-provider")
        make? (or missing?
                  (and outdated? (not= agent-type :shared-assistant)))]
    (when outdated? ; ToDo: Add verbose? flag?
      (log! :warn (str "Agent " base-type " is outdated."))
      (tel/with-kind-filter {:allow :agents}
        (tel/signal! {:kind :agents :level :info :msg (str "\n\n Agent " base-type " is outdated.")})))
    (when missing?
      (log! :warn (str "Agent " base-type " missing " where "."))
      (tel/with-kind-filter {:allow :agents}
        (tel/signal! {:kind :agents :level :info :msg (str "\n\n Agent " base-type " missing " where ".")})))
    make?))

(defn enrich-agent-info
  "Calls to agent-status and add-agent! need the info argument to qualify as s/valid? ::agent-info.
   Calls to ensure-agent! do not. This adds information from the agent-info, which must be in adb.agent-infos."
  [{:keys [base-type] :as info}]
  (let [more-info (get @agent-infos base-type)]
    (when-not more-info
      (log! :error (str "No base-type " base-type " found in agent-infos. Did you add it?")))
    (merge more-info info)))

;;; OpenAI may delete them after 30 days. https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
(defn agent-status
  "Get status of agents created by means of agent-infos.
   Compare the most-recently modified file used to create the agent to the agent creation timestamp.
   Return a map with
     {:missing-here?         -- a boolean indicating that no agent matches the parameters in the relevant db.
      :missing-at-provider?  -- a boolean indicating that the llm-provider no longer maintains an assistant with the :agent/assistant-id in the DB.
      :outdated?  -- a boolean indicating that the :agent/timestamp is older than the most recently modified files used to create it,
      :files-date -- the .lastModified (as Instant) of the most recently modified file used to create the agent, and
      :agent-date -- if not missing?, the timepoint (Instant) when the agent was created.}

   :agent-date is not provided if the agent is not present in the any database we maintain."
  [agent-info ]
  (let [agent-info (enrich-agent-info agent-info)
        _ (s/valid? ::agent-info agent-info)
        {:keys [base-type agent-type llm-provider surrogate? pid]
         :or {llm-provider @default-llm-provider}} agent-info
        pid (or pid (when surrogate? base-type))
        files-modify-date (newest-file-modification-date agent-info)
        conn-atm (if pid (connect-atm pid) (connect-atm :system))
        eid (agent-eid base-type llm-provider conn-atm)
        {:agent/keys [timestamp assistant-id thread-id]} (when eid (dp/pull @conn-atm '[*] eid))
        outdated? (and files-modify-date
                       (== 1 (compare files-modify-date timestamp)))
        missing-here  (cond-> #{}
                        (not assistant-id)                          (conj :assistant)
                        (and (not= agent-type :shared-assistant)
                             (not pid)
                             (not thread-id))                       (conj :thread))
        provider-aid? (when assistant-id (llm/get-assistant assistant-id))
        provider-tid? (when thread-id    (llm/get-thread thread-id))
        missing-provider    (cond-> #{}
                              (not provider-aid?)            (conj :assistant)
                              (and pid (not provider-tid?))  (conj :thread))
        status (cond-> {:base-type base-type, :agent-type agent-type}
                 files-modify-date                      (assoc :files-data files-modify-date)
                 timestamp                              (assoc :agent-date timestamp)
                 outdated?                              (assoc :outdated? true)
                 (not eid)                              (assoc :missing-here #{:assistant :thread})
                 (and eid (not-empty missing-here))     (assoc :missing-here missing-here)
                 (and eid (not-empty missing-provider)) (assoc :missing-at-provider missing-provider))]
    (when (when (= agent-type :shared-assistant) (not pid))
      (log! :warn (str "Shared assistant should specify PID for status info: " base-type)))
    (assoc status :need-to-remake? (need-to-remake? status))))

(defn get-agent
  "Return the agent map in db attributes."
  [agent-info]
  (let [agent-info (enrich-agent-info agent-info)
        _ (s/valid? ::agent-info agent-info)
        {:keys [base-type llm-provider pid surrogate? agent-type]
         :or {llm-provider @default-llm-provider}} agent-info
        pid (or pid (when surrogate? base-type))
        conn (connect-atm (or pid :system))]
    (dp/pull @conn '[*] (agent-eid base-type llm-provider conn))))

;;; OpenAI may delete them after 30 days. https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
(defn ensure-agent!
  "Return agent map with project keys (:aid, tid...)  if the agent exists.
   Otherwise create the agent, store it and return the info."
  [& {:as agent-info}]
  (let [agent-info (enrich-agent-info agent-info)
        _ (s/valid? ::agent-info agent-info)
        {:keys [base-type llm-provider pid surrogate? force-new?]
         :or {llm-provider @default-llm-provider}} agent-info
        pid (or pid (when surrogate? base-type))
        remake? (-> (agent-status agent-info) :need-to-remake?)]
    (-> (cond force-new?          (add-agent! agent-info)
              (not remake?)       (get-agent  agent-info)
              :else               (add-agent! agent-info))
        agent-db2proj)))

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
  [& {:keys [aid tid role query-text timeout-secs llm-provider test-fn preprocess-fn base-type]
      :or {test-fn (fn [_] true),
           preprocess-fn identity
           base-type :unknown
           llm-provider @default-llm-provider
           timeout-secs 60
           role "user"} :as obj}]
  (let [obj (cond-> obj ; All recursive calls will contains? :tries.
              (or (not (contains? obj :tries))
                  (and (contains? obj :tries) (-> obj :tries nil?))) (assoc :tries 1))]
    (assert (< (:tries obj) 10))
    (if (> (:tries obj) 0)
      (try
        (do (tel/with-kind-filter {:allow :agents}
              (tel/signal! {:kind :agents :level :info :msg (str "\n\n" (name base-type) " ===> " query-text)}))
            (let [raw (query-on-thread-aux aid tid role query-text timeout-secs llm-provider)
                  res (preprocess-fn raw)]
              (tel/with-kind-filter {:allow :agents}
                (tel/signal! {:kind :agents :level :info :msg (str "\n" (name base-type) " <=== " raw)}))
              (if (test-fn res) res (throw (ex-info "Try again" {:res res})))))
            (catch Exception e
             (let [d-e (datafy e)]
               (log! :warn (str "query-on-thread failed (tries = " (:tries obj) "): "
                                (or (:cause d-e) (-> d-e :via first :message)))))
             (query-on-thread (update obj :tries dec))))
      (log! :warn "Query on thread exhausted all tries.")))) ; ToDo: Or throw?

(defn query-agent
  "Make a query to a named agent. Convenience function for query-on-thread."
  [query-text & {:keys [llm-provider] :or {llm-provider @default-llm-provider} :as opts}]
  (let [{:keys [aid tid]} (ensure-agent! opts)]
    (if-not (and aid tid)
      (throw (ex-info "Could not find the agent requested. (Not created for this LLM provider?):" {:llm-provider llm-provider}))
      (try (query-on-thread {:aid aid :tid tid :query-text query-text})
           (catch Exception e
             (log! :error (str "query-agent failed: " e)))))))


;;; ToDo: The :system/openai-assistants can go away.
#_(defn update-assistants!
  "List what assistants the LLM-provider's currently maintains that have metadata indicating that
   they are part of our project (where they are called agents). Store this in the sytems DB."
  []
  (let [conn-atm (connect-atm :system)
        eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn-atm)
        old-ones (d/q '[:find [?s ...] :where [_ :system/openai-assistants ?s]] @conn-atm)
        new-ones (llm/list-assistants)]
    (d/transact conn-atm {:tx-data (vec (for [o old-ones] [:db/retract eid :system/openai-assistants o]))})
    (d/transact conn-atm {:tx-data (vec (for [n new-ones] [:db/add     eid :system/openai-assistants n]))})))

;;; ToDo: Probably don't want to update outdated shared-assistant agents by default,
;;;       Only update these if the assitant or thread has disappeared.
;;;       But warn about everything here.
(def update-outdated? (atom true)) ; <============ ToDo: Probably want something more fine-grained for this

(defn init-agents!
  "Warn or update system and project agents, depending on the value of the update-outdated? atom."
  []
  (check-agent-infos))


(defstate agents
  :start (init-agents!))
