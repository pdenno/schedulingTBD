(ns scheduling-tbd.mock
  "LLM response mocking system for testing.
   Provides project-based mock responses using pre-recorded conversation data."
  (:require
   [clojure.edn          :as edn]
   [clojure.spec.alpha   :as s]
   [clojure.string       :as str]
   [datahike.api         :as d]
   [datahike.pull-api    :as dp]
   [scheduling-tbd.schema :as schema :refer [db-schema-proj]]
   [scheduling-tbd.specs  :as specs]
   [scheduling-tbd.sutil  :as sutil :refer [ai-response2clj connect-atm db-cfg-map elide log! mocking?
                                            normal-pid register-db resolve-db-id shadow-pid]]))

(def ^:diag diag (atom nil))

(s/def ::agent-id (s/or :system-agent keyword? :project-agent ::agent-id-map))
(s/def ::agent-id-map (s/keys :req-un [::pid ::base-type]))
(s/def ::base-type keyword?)
(s/def ::pid keyword?)

(def mocking-state-blank
  "We use this to reset things after done mocking."
  {:pid-of-original nil       ; This is set by run-mocking
   :last-question nil
   :ork-EADS nil ; set in run-mocking and updated as we work.
   :script []})  ; set in run-mocking. ; set in run-mocking.

(def mocking-state
  "This is the atom that is initialized to the script of the mocked project.
   The atom's :conversation-pos and :ork-pos are updated as mocking proceeds."
  (atom mocking-state-blank))

(def mocked-type?
  "At the current level of implementation of mocking, only the following agent types are mocked."
  #{:interviewer :interviewee :orchestrator})

(def old-shadow-db
  "pid of the previous mocked id. This is used to clean up destroy the old db when
   last mocking session was with a different DB than this one."
  (atom nil))


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
    (let [{:keys [base-type pid]} agent-id
          base-type (normal-pid base-type)
          pid       (normal-pid pid)]
      (if-let [eid (d/q '[:find ?eid .
                          :in $ ?base-type
                          :where [?eid :agent/base-type ?base-type]]
                        @(connect-atm pid) base-type)]
        (resolve-db-id {:db/id eid} (connect-atm pid))
        (log! :error (str "Expected to find an agent: " agent-id))))))

(defn ensure-mocked-agent
  [agent-id]
  (let [{:agent/keys [assistant-id thread-id base-type] :as agent}
        (if (map? agent-id)
          (let [{:keys [base-type pid]} agent-id]
            (get-agent-from-wherever {:pid (sutil/normal-pid pid) :base-type base-type}))
          (get-agent-from-wherever (sutil/normal-pid agent-id)))]
    (cond-> agent
      assistant-id (assoc :aid assistant-id)
      thread-id (assoc :tid thread-id)
      base-type (assoc :base-type base-type))))

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

(defn destroy-shadow-db!
  [pid]
  (when (connect-atm pid {:error? false})
    (let [cfg (sutil/get-db-cfg pid)]
      (if (= :mem (-> cfg :store :backend))
        (do (d/delete-database cfg)
            (sutil/deregister-db pid)
            true)
        (log! :error "Mocking DB is not :mem")))))

(defn make-shadow-db!
  "Create an in-memory copy of the database at the argument pid (without using any functions from db.clj!).
   System DB knows nothing about it, but it is registered, so sutil/connect-atm works.
   Returns the the shadow-pid (keyword) of the argument."
  [pid]
  (let [id (shadow-pid pid)
        pname (str "shadow of " (name pid))
        cfg (db-cfg-map {:type :project :id id :in-mem? true})]
    (when (d/database-exists? cfg) (destroy-shadow-db! id))
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
                                                              {:claim/string (str `(~'surrogate ~id))}]
                                             :project/conversations conversation-defaults}]})
    id))

(defn mocked-agent?
  "Return true if the agent is a type that is mocked."
  [agent-id]
  (mocked-type? (agent-type agent-id)))

(defn get-mocked-dispatch
  [tag _agent-id]
  tag)

(defn ai-str2msg
  "Return a s/valid? interviewing message-type for the argument string."
  [s]
  (let [msg-obj (ai-response2clj s)
        msg-obj (cond-> msg-obj
                  true   (update :message-type keyword)
                  (contains? msg-obj :interviewee-type)                 (update :interviewee-type keyword))]
    (if (s/valid? ::specs/interviewer-msg msg-obj)
      msg-obj
      (throw (ex-info "Invalid message:" {:msg-string s})))))

(def ok-msg (sutil/clj2json-pretty {:message-type :STATUS :status "ok"}))

(defmulti mocked-response-by-role! #'get-mocked-dispatch)

;;; ---------------------------------- Interviewer ---------------------------------------------
(defn script-from
  "Return the portion of argument script vector after the entry with :message/content = after-text."
  [script after-text]
  (let [found? (atom nil)
        result (atom [])]
    (doseq [msg script]
      (when @found? (swap! result #(conj % msg)))
      (when (= (:message/content msg) after-text) (reset! found? true)))
    @result))

(defn get-next-question
  "Return the question that should be asked next."
  [{:keys [last-question script] :as _state}]
  (let [script (if (empty? last-question) script (script-from script last-question))
        question (-> (some #(when (= (:message/from %) :system) %) script) :message/content)]
    (if (empty? question)
      (log! :warn "get-next-question: No questions left")
      question)))

;;; (mock/set-mocking-state-to-start! :sur-craft-beer-4script)
;;; (mock/next-question-to-ask!) ;...
(defn next-question-to-ask!
  "Return a validated question-to-ask message and update mocking-state.
   The question is acquired from mocking-state, but the budget is from the shadow DB."
  []
  (let [q (get-next-question @mocking-state)
        msg {:message-type :QUESTION-TO-ASK
             :question q}]
    (when q
      (s/assert :iviewr/question-to-ask msg)
      (swap! mocking-state #(assoc % :last-question q))
      msg)))

(defn next-refinement-msg
  "The interviewer received an interviewees-respond message and have provide the corresponding DSR message.
   Turns out, we put the whole DSR in the message untouched, so just get it.
   This doesn't touch change mocking-state; only get-question-to-ask does that."
  []
  (let [{:keys [last-question script]} @mocking-state
        script (if (empty? last-question) script (script-from script last-question))
        dsr (some #(when (contains? % :message/EADS-data-structure)
                     (:message/EADS-data-structure %)) script)]
    (if (empty? dsr)
      (log! :warn "get-refinement-msg: No DSR found.")
      (edn/read-string dsr))))

(defmethod mocked-response-by-role! :interviewer
  [tag text]
  (log! :info (str "Mocked " (name tag) " " (elide text 130)))
  (let [msg (ai-str2msg text)]
    (cond  (s/valid? :iviewr/conversation-history msg)            ok-msg
           (s/valid? :iviewr/eads-instructions msg)               ok-msg
           (s/valid? :iviewr/supply-question msg)                 (-> (next-question-to-ask!) sutil/clj2json-pretty)
           (s/valid? :iviewr/interviewees-respond msg)            (-> (next-refinement-msg)  sutil/clj2json-pretty)
           :else                                                  (throw (ex-info "Invalid message to interviewer:" {:text text})))))

;;; ---------------------------------- Interviewee ---------------------------------------------
;;; ToDo: This will need to be updated in the case the the interviewees are human.
(defn get-next-response
  "Return what the interviewees' said in response to the question.
   No need to update state, I think."
  [question-text]
  (let [{:keys [last-question script]} @mocking-state]
    (when-not (= (str/trim question-text) (str/trim last-question))
      (log! :error (str "Mocking out of sync at question: \n  question-text: "
                        question-text
                        "\n last-question: " last-question)))
    (let [script (script-from script question-text)
          a (-> (some #(when (= (:message/from %) :surrogate) %) script) :message/content)
          msg {:message-type :INTERVIEWEES-RESPOND
               :response a}]
      (if a
        (s/assert :iviewr/interviewees-respond msg)
        msg))))

(defmethod mocked-response-by-role! :interviewee
  [_tag question-text]
  (-> (get-next-response question-text) sutil/clj2json-pretty))

;;; ---------------------------------- orchestrator ---------------------------------------------
;;; Orchestrator only gets :iviewr/conversation-history and :iviewr/backchannel-communication (not implemented) messages.
;;; Orchestrator only generates :iviewr/pursue-eads and backchannel-coms (not implemented) messages.

(defmethod mocked-response-by-role! :orchestrator
  [tag text]
  (log! :info (str "Mocked " (name tag) " " (elide text 130)))
  (let [msg (ai-str2msg text)]
    (cond  (s/valid? :iviewr/conversation-history msg)
           (if (-> msg :activty empty?)
             ;; Then haven't started talking. mocking-state.ork-EADS was initialized to the warm-up EADS instructions.
             (-> {:message-type "PURSUE-EADS" :EADS-id (:ork-EADS @mocking-state)} sutil/clj2json-pretty)
             :NYI)

          (s/valid? :iviewr/backchannel-coms msg)
          ok-msg

          :else
          (throw (ex-info "Invalid message to interviewer:" {:text text})))))

(defn get-mocked-response!
  "Return the string that the LLM would have returned at this point in the proces.
   This is the key function of the mocking capability!"
  [agent-id text]
  (mocked-response-by-role! (agent-type agent-id) text))

(defn set-mocking-state-to-start!
  [pid]
  (let [conn-atm (sutil/connect-atm pid)
        script (->> (d/q '[:find [?db-id ...]
                           :where [?db-id :message/pursuing-EADS]]
                         @conn-atm)
                    (dp/pull-many @conn-atm '[*])
                    (sort-by :message/time))]
    (reset! mocking-state {:conversation-pos 0
                           :pid-of-original pid
                           :ork-EADS (some (fn [s-line]
                                             (when (contains? s-line :message/pursuing-EADS)
                                               (:message/pursuing-EADS s-line)))
                                           script)
                           :script script}))
  true)

(defn prepare-to-mock!
  "Run mocking on the argument project."
  [pid]
  (when @old-shadow-db
    (destroy-shadow-db! @old-shadow-db))
  (reset! old-shadow-db (make-shadow-db! pid))
  (let [{:keys [script]} (set-mocking-state-to-start! pid)]
    (if (not-empty script)
      (do
        (log! :info (str "Setting up mocking with " (count script) " messages from project " pid))
        (reset! mocking? true)
        (log! :info "Mocking enabled"))
      (log! :warn (str "No messages found for project " pid ", mocking not enabled")))))

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
  `(do
     (prepare-to-mock! ~pid)
     ~@body
     (log! :info "Ran the body")))
