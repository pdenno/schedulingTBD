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

;;; Much of the code here relies on agent-infos, a "database on an atom" that describes agent types. (See Clojure specs below.)
;;; Where agent-type is :system, the values for agent-info are obtained from a .edn file on startup.
;;; Also at startup, extant projects are scanned for surrogates, which are also added to the atom DB.
;;; More entries can be made at run-time, for example, when a surrogate is created.

;;; Throughout the project:
;;;   pid = project-id, a keyword
;;;   cid = conversation id, a keyword #{:process :data :resources :optimality}
;;;   aid = llm-provider's assistant-id, a string.
;;;   tid = llm-provider's thread-id, a string.

;;; There are three types of agents (:agent-type):
;;;   1) :system :  The agent and a single thread is shared by all projects
;;;       -- It is entirely stored in the system db.
;;;   2) :project : It is the exclusive property of a project, such as the surrogate expert (only example, so far):
;;;       -- It is entirely stored in the project. The base-type is the pid.
;;;   3) :shared-assistant : All such agents use the same assistant (thus same instructions) but have a thread defined in the project.
;;;       -- Thus :system DB has all agent information but the tid; the project DB has the same plus the tid.

;;; Updating agents is a bit tricky. Obviously, if the llm-provider no longer possesses the assistant or thread
;;; a new agent needs to be created. New agents are also created for :system agent-type when the instructions
;;; files from which vector stores are created are updated. By default, outdated files is not sufficient reason
;;; to recreate :shared-assistant agents; if the assistant and thread still exist, it reflects an investment in
;;; context that we don't want to lose.

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

;;; ToDo: Fix this!
(s/def ::agent-info (s/and ::agent-info-decl
                           #_(s/or  :system          #(= (:agent-type %) :system)
                                    :project          (s/and #(= (:agent-type %) :project)          #(-> % :pid keyword?))
                                    :shared-assistant (s/and #(= (:agent-type %) :shared-assistant) #(-> % :pid keyword?)))))

;;; This might better be called agent-type-infos.
;;;  "This is a map, indexed by base-type with values being maps providing information sufficient to create/recreate agents.
;;;   In addition to those found in agents/agent-infos.edn, we add to this surrogate domain experts when they are started."
(defonce agent-infos
  (atom (reduce (fn [res info] (assoc res (:base-type info) info))
                {}
                (-> "agents/agent-infos.edn"
                    io/resource
                    slurp
                    edn/read-string))))

(defn check-agent-infos
  "Do s/valid? on ::agent-info-decl, an ::agent-info sans pid."
  []
  (doseq [[base-type info] (seq @agent-infos)]
    (when-not (s/valid? ::agent-info-decl info)
      (log! :error (str base-type " is not a valid agent-info:\n" (with-out-str (pprint info)))))))

(check-agent-infos)

(defn put-agent-info!
  "Add an agent info at the given base-type."
  [base-type {:keys [surrogate? agent-type] :as info}]
  (assert (#{:system :project :shared-assistant} agent-type))
  (assert (keyword? base-type))
  (let [info (cond-> (assoc info :base-type base-type)
               surrogate? (assoc :pid base-type))]
    (if (s/valid? ::agent-info info)
      (swap! agent-infos #(assoc % base-type info))
      (log! :error (str "Invalid agent-info: " info)))))

(defn agent-db2proj
  "Return a map of agent info translated to project attributes (aid, tid, base-type, expertise, surrogate?)."
  [agent]
  (reduce-kv (fn [m k v]
               (cond (= k :agent/assistant-id)         (assoc m :aid v)
                     (= k :agent/thread-id)            (assoc m :tid v)
                     (= k :agent/base-type)            (assoc m :base-type v)
                     (= k :agent/expertise)            (assoc m :expertise v)
                     (= k :agent/surrogate?)           (assoc m :surrogate? true)
                     :else m))
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

(defn enrich-agent-info
  "Calls to agent-status and put-agent! need the info argument to qualify as s/valid? ::agent-info.
   Calls to ensure-agent! do not. This adds information from the agent-info, which must be in adb.agent-infos."
  [{:keys [base-type] :as info}]
  (assert base-type)
  (let [more-info (get @agent-infos base-type)]
    (when-not more-info
      (throw (ex-info "Unknown agent base-type:" {:base-type base-type})))
    (let [info (merge more-info info)
          res (cond-> info
                (:surrogate? info)   (assoc :pid base-type))]
      (s/assert ::agent-info res)
      res)))

(defn get-agent
  "Return the agent map in db attributes. If there is no eid, even though the pid is provided,
   then it pulls from the system DB, if it is not there also, then it returns nil."
  [agent-info]
  (let [{:keys [base-type llm-provider pid]
         :or {llm-provider @default-llm-provider}} (enrich-agent-info agent-info)
        eid (and pid (agent-eid base-type llm-provider (connect-atm pid)))
        agent (or (and eid (not-empty (dp/pull @(connect-atm pid) '[*] eid)))
                  (when-let [eid (agent-eid base-type llm-provider (connect-atm :system))]
                    (dp/pull @(connect-atm :system) '[*] eid)))]
      (when agent ; ToDo: This should probably be temporary, while migrating to projects with timestamps.
        (cond-> agent
          (-> agent :agent/timestamp not)  (assoc :agent/timestamp #inst "2024-12-08T16:12:48.505-00:00")))))

(defn put-agent!
  "Add the agent to one or more DBs."
  [agent-map {:keys [agent-type pid] :as _agent-info}]
  (let [eid-sys (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @(connect-atm :system))
        eid-pid (when pid (d/q '[:find ?eid . :where [?eid :project/id]] @(connect-atm pid)))]
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
  (log! :info (str "Creating agent: " base-type))
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
    (tel/with-kind-filter {:allow :agents}
      (tel/signal! {:kind :agents :level :info
                    :msg (str "\n\n Creating agent (no thread yet) " base-type ":\n" (with-out-str agent))}))
    agent))


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
      (tel/with-kind-filter {:allow :agents}
        (tel/signal! {:kind :agents :level :info :msg
                      (str "Agent " base-type " will use an updated assistant: " substitute-aid ".")})))
    (when (:make-agent? res)
      (log! :info (str "Agent " base-type " will be created."))
      (tel/with-kind-filter {:allow :agents}
        (tel/signal! {:kind :agents :level :info :msg (str "\n Agent " base-type " will be recreated.")})))
    (when (:make-thread? res) ; ToDo: Could pass in PID here and mention it.
      (log! :info (str "A thread will be made for agent " base-type " agent-type " agent-type "."))
      (tel/with-kind-filter {:allow :agents}
        (tel/signal! {:kind :agents :level :info :msg
                      (str "\n\n A thread will be made for agent " base-type " agent type " agent-type ".")})))
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
  [{:keys [base-type llm-provider pid force-new?]
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
        status (cond-> agent-info ; {:base-type base-type, :agent-type agent-type}
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
      :missing-at-provider?  -- a boolean indicating that the llm-provider no longer possesses an assistant with the :agent/assistant-id in the DB.
      :outdated?  -- a boolean indicating that the :agent/timestamp is older than the most recently modified files used to create it,
      :files-date -- the .lastModified (as Instant) of the most recently modified file used to create the agent, and
      :agent-date -- if not missing?, the timepoint (Instant) when the agent was created.}

   :agent-date is not provided if the agent is not present in the any database we maintain."
  [agent-info]
  (let [{:keys [agent-type] :as info} (enrich-agent-info agent-info)]
    (case agent-type
      (:system :project) (agent-status-basic info)
      :shared-assistant  (agent-status-shared-assistant info))))

(defn add-thread!
  "Create on the llm-provider a thread and add it to the project's db if pid is provided.
   Otherwise assume it is in the :system db.
   Return the DB agent object."
  [{:agent/keys [assistant-id] :as agent}
   {:keys [pid llm-provider base-type agent-type] :or {llm-provider @default-llm-provider} :as _agent-info}]
  (let [tid (-> (llm/make-thread :assistant-id assistant-id) :id)
        agent (assoc agent :agent/thread-id tid)]
    (case agent-type
      :system
      (let [conn-atm (connect-atm :system)
            eid (d/q '[:find ?eid . :where [?eid :system/agents]] @conn-atm)]
        (d/transact conn-atm {:tx-data [{:db/id eid :system/agents agent}]})
        (log! :info (str "Adding thread " tid " to system DB.\n" (with-out-str (pprint agent))))
        (tel/with-kind-filter {:allow :agents}
          (tel/signal! {:kind :agents :level :info
                        :msg (str "\n\n Adding thread " tid " to system DB\n" (with-out-str (pprint agent)))}))
        (dp/pull @conn-atm '[*] (agent-eid base-type llm-provider conn-atm)))

      (:project :shared-assistant)
      (let [conn-atm (connect-atm pid)
            eid (d/q '[:find ?eid . :where [?eid :project/id]] @conn-atm)]
        (d/transact conn-atm {:tx-data [{:db/id eid :project/agents agent}]})
        (log! :debug (str "Adding thread " tid " to project " pid ".\n" (with-out-str (pprint agent))))
        (tel/with-kind-filter {:allow :agents}
          (tel/signal! {:kind :agents :level :info
                        :msg (str "\n\n Adding thread " tid " to project " pid ".\n" (with-out-str (pprint agent)))}))
        (dp/pull @conn-atm '[*] (agent-eid base-type llm-provider conn-atm))))))

;;; OpenAI may delete them after 30 days. https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
;;; Once the agent is created, and checks on aid and sid memoized, this executes in less than 5 milliseconds.
(defn ensure-agent!
  "Return agent map with project keys (:aid, tid...)  if the agent exists.
   Otherwise create the agent, store it and return the info.
   The argument agent-info need only contain one or two keys:
     - For agents with agent-type = :system, it need only have :base-type.
     - For agents of type :project or :shared-assistant it should additional have the pid.
   The agent-info is 'enriched' here with information from the agent-infos map."
  [info]
  (let [{:keys [make-agent? make-thread? substitute-aid]} (-> info enrich-agent-info agent-status)
        agent (if make-agent?
                (-> info make-agent-assistant (put-agent! info))
                (get-agent info))
        agent  (cond-> agent
                 substitute-aid   (assoc :agent/assistant-id substitute-aid)
                 make-thread?     (add-thread! info))]
    (-> agent (dissoc :db/id) agent-db2proj)))

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
           asked-role :unknown
           asking-role :unknown
           llm-provider @default-llm-provider
           timeout-secs 60
           role "user"} :as obj}]
  (let [obj (cond-> obj ; All recursive calls will contains? :tries.
              (or (not (contains? obj :tries))
                  (and (contains? obj :tries) (-> obj :tries nil?))) (assoc :tries 1))]
    (assert (< (:tries obj) 10))
    (if (> (:tries obj) 0)
      (try
        (tel/with-kind-filter {:allow :agents}
          (tel/signal! {:kind :agents :level :info :msg (str "\n\n" (name asking-role) " ===> " query-text)}))
        (let [raw (query-on-thread-aux aid tid role query-text timeout-secs llm-provider)
              res (preprocess-fn raw)]
          (tel/with-kind-filter {:allow :agents}
            (tel/signal! {:kind :agents :level :info :msg (str "\n" (name asked-role) " <=== " raw)}))
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
  ([agent-or-info text {:keys [asked-role] :as opts-map}]
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
                                         :query-text text
                                         :asked-role (or asked-role base-type)})))
     (catch Exception e
       (log! :error (str "query-agent failed: " e))))))

(defn agent-log
  "Log info-string to agent log"
  [& args]
  (let [date (str (java.util.Date.))]
    (tel/with-kind-filter {:allow :agents}
      (tel/signal!
       {:kind :agents :level :info
        :msg (str "===== " date " " (apply str args))}))))

;;; -------------------- Starting and stopping -------------------------
(defn init-agents!
  "Things done on start and restart."
  []
  (check-agent-infos))

(defstate agents
  :start (init-agents!))
