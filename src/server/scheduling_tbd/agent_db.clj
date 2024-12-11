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
(s/def ::expertise string?)
(s/def ::instruction-path string?)
(s/def ::instruction-string string?)
(s/def ::response-format-path string?) ;; ToDo: Valid JSON schema.
(s/def ::llm-provider #{:openai :azure})
(s/def ::surrogate? boolean?)
(s/def ::tools (s/and vector? (s/coll-of ::tool)))
(s/def ::tool (s/keys :req-un [::type]))
(s/def ::vector-store-paths (s/coll-of string?))

(s/def ::agent-info-decl ; This is for checking what is on the agent-infos atom; they won't have PIDs.
  (s/and (s/keys :req-un [::base-type ::agent-type]
                 :opt-un [::instruction-path ::instruction-string ::response-format-path ::llm-provider ::model-class
                          ::tools ::tool-resources ::vector-store-paths ::expertise ::surrogate?])
         (s/or  :system           (s/and #(= (:agent-type %) :system)  #(not (:surrogate? %)))
                :project          #(= (:agent-type %) :project)
                :shared-assistant #(= (:agent-type %) :shared-assistant))))

(s/def ::agent-info (s/and ::agent-info-decl
                           #_(s/or  :system          #(= (:agent-type %) :system)
                                  :project          (s/and #(= (:agent-type %) :project)          #(-> % :pid keyword?))
                                  :shared-assistant (s/and #(= (:agent-type %) :shared-assistant) #(-> % :pid keyword?)))))

(def agent-infos
  "This is a map, indexed by base-type with values being maps providing information sufficient to create/recreate agents.
   In addition to those found in agents/agent-infos.edn, we add to this surrogate domain experts when they are started."
  (atom (reduce (fn [res info] (assoc res (:base-type info) info))
                {}
                (-> (io/resource "agents/agent-infos.edn")
                    slurp
                    edn/read-string))))

(defn check-agent-infos
  "Check the atom infos, they don't need pid."
  []
  (doseq [[base-type info] (seq @agent-infos)]
    (when-not (s/valid? ::agent-info-decl info)
      (log! :error (str base-type " is not a valid agent-info:\n" (with-out-str (pprint info)))))))

(check-agent-infos)

(defn add-agent-info!
  "Add an agent info at the given base-type."
  [base-type {:keys [surrogate?] :as info}]
  (let [info (cond-> info
               surrogate? (assoc :pid base-type))]
    (if (s/valid? ::agent-info info)
      (swap! agent-infos #(assoc % base-type info))
      (log! :error (str "Invalid agent-info: " info)))))

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

;;; (adb/add-agent! (some #(when (= :response-analysis-agent (:base-type %)) %) adb/agent-infos))

;;;   There are three types of agents (:agent-type):
;;;     1) :system-agent :  The agent and a single thread is shared by all projects
;;;           -- It is entirely stored in the system db.
;;;     2) :project-agent : It is the exclusive property of a project, such as the surrogate expert:
;;;           -- It is entirely stored in the project. The base-type is the pid.
;;;     3) :shared-assistant : All such agents use the same assistant (thus same instructions) but have a thread defined in the project.
;;;           -- Thus :system DB has everything but the tid; project DB has everything.
(defn add-agent!
  "Create an agent or parts thereof and and store the information in one or more databases.
   (#{:project :system}  (:agent-type %)) - Create one complete agent and put it in the associated DB.
   (#{:shared-assistant} (:agent-type %)) - Create one agent SANS THREAD, put it in the :system database,
                                            and if you have the pid, put it in the project database.
   Returns the object created in DB attributes."
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
        thread (when (#{:project :system} agent-type)
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
                tid            (assoc :agent/thread-id tid)
                surrogate?     (assoc :agent/surrogate? true)
                expertise      (assoc :agent/expertise expertise))]
    (when (= agent-type :shared-assistant)
      (d/transact (connect-atm :system)
                  {:tx-data [{:db/id eid-sys :system/agents (dissoc a-map :agent/thread)}]}))
    (when pid ; The other half of the shared assistant or just project (e.g. surrogate).
      (d/transact (connect-atm pid)
                  {:tx-data [{:db/id eid-pid :project/agents a-map}]}))
    (when (= agent-type :system)
      (d/transact (connect-atm :system)
                  {:tx-data [{:db/id eid-sys :system/agents a-map}]}))
    a-map))

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

(defn remake-needs
  "This makes pragmatic and policy-based decisions on remaking an agent or creating a newest thread for it.
   It updates the argument status object with :remake? and :make-thread? appropriately.
   The pragmatic decision is simply if the llm-provider doesn't have the assistant or thread, the agent is recreated.
   The policy-based decision is that if the agent is :share-assistant and the llm-provider has the stuff,
   it won't be recreated, even if the assistant instructions are outdated relative to what the agent is using.
   This choice is to preserve the context already built up by the current agent.
   You can force a fresh instance in ensure-agent! with :force? = true."
  [{:keys [base-type agent-type missing-here missing-at-provider outdated?] :as status}]
  (let [missing? (or missing-here missing-at-provider)
        where (if missing-here "in databases" "at llm-provider")
        make? (if (or missing?
                      (and outdated? (not= agent-type :shared-assistant)))
                true false)
        res (case agent-type
              (:project :system)    (cond-> status
                                      (or missing? outdated?)           (assoc :remake-agent? true))
              :shared-assistant     (cond-> status
                                      (:assistant missing-at-provider)  (assoc :remake-agent? true)
                                      (:thread missing-at-provider)     (assoc :make-thread? true)))]
    (when (:remake-agent? res)
      (log! :warn (str "Agent " base-type " will be recreated."))
      (tel/with-kind-filter {:allow :agents}
        (tel/signal! {:kind :agents :level :info :msg (str "\n Agent " base-type " will be recreated.")})))
    (when (:make-thread? true) ; ToDo: Could pass in PID here and mention it.
      (log! :warn (str "A thread will be made for agent " base-type "."))
      (tel/with-kind-filter {:allow :agents}
        (tel/signal! {:kind :agents :level :info :msg (str "\n\n A thread will be made for agent " base-type ".")})))
    res))

(defn enrich-agent-info
  "Calls to agent-status and add-agent! need the info argument to qualify as s/valid? ::agent-info.
   Calls to ensure-agent! do not. This adds information from the agent-info, which must be in adb.agent-infos."
  [{:keys [base-type] :as info}]
  (assert base-type)
  (let [more-info (get @agent-infos base-type)
        info (merge more-info info)
        res (cond-> info
              (:surrogate? info)   (assoc :pid base-type))]
    (s/assert ::agent-info res)
    res))

(defn db-agents
  "Return a map with indexes :system and/or :project which indicates what is known about the subject agent in that context."
  [{:keys [base-type agent-type pid llm-provider] :as _agent-info}]
  (letfn [(get-agent-info [conn-atm]
            (let [eid (agent-eid base-type llm-provider conn-atm)]
              (dp/pull @conn-atm '[*] eid)))]
    (case agent-type
      :system            {:system   (get-agent-info (connect-atm :system))}
      :project           {:project  (get-agent-info (connect-atm pid))}
      :shared-assistant  {:system   (get-agent-info (connect-atm :system))
                          :project  (get-agent-info (connect-atm pid))})))

(defn agent-status-shared-assistant
  "Provide a bit more information (than with agent-status-basic) for agent-type = :shared-assistant,
   namely, :same-assistant? :system-more-modern?, and, of course, look at the project to get the :agent/thread-id."
  [{:keys [base-type agent-type pid llm-provider]
    :or {llm-provider @default-llm-provider} :as agent-info}]
  (let [db-info (db-agents agent-info)
        files-modify-date (newest-file-modification-date agent-info)
        outdated? (and files-modify-date (== 1 (compare files-modify-date (-> db-info :system :agent/timestamp))))
        assistant-id (-> db-info :system :agent/assistant-id)
        thread-id (-> db-info :project :agent/thread-id)
        same-assistant? (= assistant-id (-> db-info :project :agent/assistant-id))
        system-more-modern? (when (not same-assistant?)
                              (== 1 (compare (-> db-info :system :agent/timestamp) (-> db-info :project :agent/timestamp))))
        missing-here  (cond-> #{}
                        (not assistant-id)      (conj :assistant)
                        (not thread-id)         (conj :thread))
        provider-aid? (when assistant-id (llm/get-assistant assistant-id))
        provider-tid? (when thread-id    (llm/get-thread thread-id))
        missing-provider (cond-> #{}
                           (not provider-aid?)            (conj :assistant)
                           (and pid (not provider-tid?))  (conj :thread))
        status (cond-> {:base-type base-type,
                        :agent-type agent-type
                        :same-assistant? same-assistant?
                        :agent-date (-> db-info :system :agent/timestamp)}
                 system-more-modern?                    (assoc :system-more-modern? true)
                 files-modify-date                      (assoc :files-data files-modify-date)
                 outdated?                              (assoc :outdated? true)
                 (not-empty missing-here)               (assoc :missing-here missing-here)
                 (not-empty missing-provider)           (assoc :missing-at-provider missing-provider))]
    (remake-needs status)))

(defn agent-status-basic
  "Find the agent status information for :project and :system agents."
  [{:keys [base-type agent-type llm-provider surrogate? pid]
    :or {llm-provider @default-llm-provider} :as agent-info}]
  (let [files-modify-date (newest-file-modification-date agent-info)
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
        status (cond-> {:base-type base-type, :agent-type agent-type}
                 files-modify-date                      (assoc :files-data files-modify-date)
                 timestamp                              (assoc :agent-date timestamp)
                 outdated?                              (assoc :outdated? true)
                 (not eid)                              (assoc :missing-here #{:assistant :thread})
                 (and eid (not-empty missing-here))     (assoc :missing-here missing-here)
                 (and eid (not-empty missing-provider)) (assoc :missing-at-provider missing-provider))]
    (remake-needs status)))

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
  [agent-info]
  (let [{:keys [agent-type] :as info} (enrich-agent-info agent-info)]
    (case agent-type
      (:system :project) (agent-status-basic info)
      :shared-assistant  (agent-status-shared-assistant info))))

(defn get-agent
  "Return the agent map in db attributes."
  [agent-info]
  (let [{:keys [base-type llm-provider pid]
         :or {llm-provider @default-llm-provider}} (enrich-agent-info agent-info)
        conn (connect-atm (or pid :system))]
    (dp/pull @conn '[*] (agent-eid base-type llm-provider conn))))

(defn add-thread! ; <================================================== I think this needs to COPY THE AGENT and add the thread.
  "Create on the llm-provider a thread and add it to the project's db."
  [{:agent/keys [assistant-id] :as agent}
   {:keys [pid llm-provider base-type] :or {llm-provider @default-llm-provider} :as agent-info}]
  (let [tid (-> (llm/make-thread :assistant-id assistant-id) :id)
        conn-atm (connect-atm pid)
        eid (agent-eid base-type llm-provider conn-atm)]
    (log! :info (str "Adding thread " tid " to project " pid "."))
    (tel/with-kind-filter {:allow :agents}
      (tel/signal! {:kind :agents :level :info :msg (str "\n\n Adding thread " tid " to project " pid ".")}))
    (d/transact conn-atm {:tx-data [[:db/add eid :agent/thread-id tid]]})))

;;; OpenAI may delete them after 30 days. https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
(defn ensure-agent!
  "Return agent map with project keys (:aid, tid...)  if the agent exists.
   Otherwise create the agent, store it and return the info."
  [& {:as agent-info}]
  (let [{:keys [base-type surrogate? force-new?] :as info} (enrich-agent-info agent-info)
        {:keys [remake-agent? make-thread?] :as status} (agent-status info)]
    (cond-> (cond force-new?           (add-agent! info)
                  (not remake-agent?)  (get-agent  info)
                  :else                (add-agent! info))
      make-thread?  (add-thread! info)
      true          agent-db2proj
      true          (assoc :base-type base-type)
      surrogate?    (assoc :surrogate? true))))

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
