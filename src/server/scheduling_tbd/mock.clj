(ns scheduling-tbd.mock
  "LLM response mocking system for testing.
   Provides project-based mock responses using pre-recorded conversation data."
  (:require
   [clojure.spec.alpha   :as s]
   [datahike.api         :as d]
   [datahike.pull-api    :as dp]
   [scheduling-tbd.schema :as schema :refer [db-schema-proj]]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm db-cfg-map get-mocked-by-role! log! mocking? register-db resolve-db-id shadow-pid]]))

(s/def ::agent-id (s/or :system-agent keyword? :project-agent ::agent-id-map))
(s/def ::agent-id-map (s/keys :req-un [::pid ::base-type]))
(s/def ::base-type keyword?)
(s/def ::pid keyword?)

(def mocking-state-blank
  "We use this to reset things after done mocking."
  {:pid-of-original nil       ; This is set by run-mocking
   :conversation-pos 0
   :last-text nil
   :ork-EADS nil ; set in run-mocking and updated as we work.
   :script []})  ; set in run-mocking. ; set in run-mocking.

(def mocking-state
  "This is the atom that is initialized to the script of the mocked project.
   The atom's :conversation-pos and :ork-pos are updated as mocking proceeds."
  (atom mocking-state-blank))

(def mocked-type?
  "At the current level of implementation of mocking, only the following agent types are mocked."
  #{:interviewer :interviewee :orchestrator})

(defn get-agent-from-wherever
  "If the agent-id is a map the agent should be found at pid-of-original,
   otherwise it is in the system DB. We'll never use it anyway, but this ensures
   that a new one won't be created."
  [agent-id]
  (s/valid? ::agent-id agent-id)
  (if (keyword agent-id)
    (if-let [eid (d/q '[:find ?eid .
                        :in $ ?base-type
                        :where [?eid :agent/base-type ?base-type]]
                      @(connect-atm :system) agent-id)]
      (resolve-db-id {:db/id eid} (connect-atm :system))
      (log! :error (str "Expected to find an agent: " agent-id)))
    (let [{:keys [base-type pid]} agent-id]
      (if-let [eid (d/q '[:find ?eid .
                          :in $ ?base-type
                          :where [?eid :agent/base-type ?base-type]]
                        @(connect-atm pid) base-type)]
        (resolve-db-id {:db/id eid} (connect-atm pid))
        (log! :error (str "Expected to find an agent: " agent-id))))))

(defn ensure-mocked-agent
  [agent-id]
  (let [{:agent/keys [assistant-id thread-id] :as agent}
        (if (map? agent-id)
          (let [{:keys [base-type pid]} agent-id]
            (get-agent-from-wherever {:pid (sutil/normal-pid pid) :base-type base-type}))
          (get-agent-from-wherever (sutil/normal-pid agent-id)))]
    (cond-> agent
      assistant-id (assoc :aid assistant-id)
      thread-id (assoc :tid thread-id))))

(defn agent-type
  "Return one of #{:interviewer :interviewee :orchestrator :other} describing the type of agent."
  [agent-id]
  (let [{:agent/keys [base-type surrogate?]} (get-agent-from-wherever agent-id)]
    (cond surrogate?                                 :interviewee
          (#{:process-interview-agent
             :data-interview-agent
             :resources-interview-agent
             :optimality-interview-agent} base-type) :interviewer
          (= base-type :orchestrator-agent)          :orchestrator
          :else                                      :other)))

(def conversation-defaults
  [{:conversation/id :process
    :conversation/status :in-progress} ; We assume things start here.
   {:conversation/id :data
    :conversation/status :not-started}
   {:conversation/id :resources
    :conversation/status :not-started}
   {:conversation/id :optimality
    :conversation/status :not-started}])

(defn make-shadow-db!
  "Create an in-memory copy of the database at the argument pid (without using any functions from db.clj!)."
  [pid]
  (let [id (shadow-pid pid)
        pname (str "shadow of " (name pid))
        cfg (db-cfg-map {:type :project :id id :in-mem? true})]
    (when (d/database-exists? cfg) (d/delete-database cfg))
    (d/create-database cfg)
    (register-db id cfg)
    ;; Add to project db
    (d/transact (connect-atm id) db-schema-proj)
    (d/transact (connect-atm id) {:tx-data [{:project/id id
                                             :project/name pname
                                             :project/execution-status :running
                                             :project/active-conversation :process
                                             :project/claims [{:claim/string (str `(~'project-id ~id))}
                                                              {:claim/string (str `(~'project-name ~id ~pname))}
                                                              {:claim/string (str `(~'surrogate? ~id))}]
                                             :project/conversations conversation-defaults}]})
    id))

(defn mocked-agent?
  "Return true if the agent is a type that is mocked."
  [agent-id]
  (mocked-type? (agent-type agent-id)))

(defmethod get-mocked-by-role! :interviewee
  [_tag _agent_id]
  (log! :info (str "Mocked interviewee:")))

(defmethod get-mocked-by-role! :interviewer
  [_tag _agent_id]
  (log! :info (str "Mocked interviewer:")))

(defmethod get-mocked-by-role! :orchestrator
  [_tag _agent_id]
  (log! :info (str "Mocked orchestrator")))

(defn get-mocked-response!
  "Return the string that the LLM would have returned at this point in the proces.
   This is the key function of the mocking capability!"
  [agent-id]
  (get-mocked-by-role! (agent-type agent-id)))

(defn prepare-to-mock!
  "Run mocking on the argument project."
  [pid]
  (if @mocking?
    (log! :error "Already mocking")
    (let [conn-atm (sutil/connect-atm pid)
          script (->> (d/q '[:find [?db-id ...]
                             :where [?db-id :message/pursuing-EADS]]
                           @conn-atm)
                      (dp/pull-many @conn-atm '[*])
                      (sort-by :message/time))]
      (make-shadow-db! pid)
      (if (not-empty script)
        (do
          (log! :info (str "Setting up mocking with " (count script) " messages from project " pid))
          ;; Reset mocking state
          (reset! mocking-state {:conversation-pos 0
                                 :pid-of-original pid
                                 :ork-EADS (some (fn [s-line]
                                                   (when (contains? s-line :message/pursuing-EADS)
                                                     (:message/pursuing-EADS s-line)))
                                                 script)
                                 :script script})
          ;; Enable mocking
          (reset! mocking? true)
          (log! :info "Mocking enabled"))
        (log! :warn (str "No messages found for project " pid ", mocking not enabled"))))))


(defn ^:diag stop-mocking!
  "Stop mocking and reset state."
  []
  (reset! mocking? false)
  (reset! mocking-state {:conversation-pos 0
                         :ork-EADS nil
                         :script []})
  (log! :info "Mocking disabled"))

(defn set-mock-enabled!
  "Enable or disable mocking."
  [enabled?]
  (reset! mocking? enabled?)
  (when-not enabled?
    (reset! mocking-state {:conversation-pos 0
                           :ork-EADS nil
                           :script []})))

;;; Typical usage from user namespace:
;;; (mock/with-mock-project :sur-craft-beer-4script (scheduling-tbd.iviewr.interviewers/resume-conversation {:client-id :console :pid :sur-craft-beer-4script :cid :process}))
;;; (mock/with-mock-project :sur-craft-beer-4script (inv/resume-conversation {:client-id (ws/recent-client!) :pid :sur-craft-beer-4script :cid :process}))
(defmacro with-mock-project
  "Execute the given function with mocking enabled for the specified project."
  [pid & body]
  `(let [original-state# @mocking?]
     (try
       ;; Set up mocking for the specific project
       (do
         (prepare-to-mock! ~pid)
         ~@body
         (log! :info "Ran the body"))
       (finally
         (reset! mocking? original-state#)
         (reset! mocking-state mocking-state-blank)))))
