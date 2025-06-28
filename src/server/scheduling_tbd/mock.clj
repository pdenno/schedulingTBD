(ns scheduling-tbd.mock
  "LLM response mocking system for testing.
   Provides project-based mock responses using pre-recorded conversation data.
   Typical responses are CLJ message translated to a JSON string to look like they are from the LLM, and, of course,
   translated back to CLJ later for processes!"
  (:require
   [clojure.edn          :as edn]
   [clojure.spec.alpha   :as s]
   [clojure.string       :as str]
   [datahike.api         :as d]
   [datahike.pull-api    :as dp]
   [scheduling-tbd.schema :as schema :refer [db-schema-proj]]
   [scheduling-tbd.specs  :as specs]
   [scheduling-tbd.sutil  :as sutil :refer [ai-response2clj connect-atm db-cfg-map log! mocking?
                                            normal-pid register-db resolve-db-id shadow-pid]]))

(def ^:diag diag (atom nil))

(s/def ::agent-id (s/or :system-agent keyword? :project-agent ::agent-id-map))
(s/def ::agent-id-map (s/keys :req-un [::pid ::base-type]))
(s/def ::base-type keyword?)
(s/def ::pid keyword?)

(def mocking-state
  "This is the atom that is initialized to the script of the mocked project.
   The atom's :conversation-pos and :ork-pos are updated as mocking proceeds."
  (atom   {:pid-of-original nil       ; This is set by run-mocking
           :last-question nil
           :messages-remaining-cnt 0
           :ork-EADS nil ; set in run-mocking and updated as we work.
           :script []}))

(add-watch mocking-state :watcher
           (fn [_key _atom old-state new-state]
             (log! :info (str " old m-r: "
                              (:messages-remaining-cnt old-state)
                              " new new m-r: "
                              (:messages-remaining-cnt new-state)))))

(def mocked-type?
  "At the current level of implementation of mocking, only the following agent types are mocked."
  #{:interviewer :interviewees :orchestrator})

(def old-shadow-db
  "pid of the previous mocked id. This is used to clean up destroy the old db when
   last mocking session was with a different DB than this one."
  (atom nil))

(defn continue-mocking?
  "Returns true when it is okay to continue.
   This is called in interviewers.clj too. The idea is to not set mocking? to false until
   the code in with-mock-project has had time to clean up."
  []
  (and (not (= :done (:status @mocking-state)))
       (> (:messages-remaining-cnt @mocking-state) 0)))

(defn stop-mocking!
  ([] (stop-mocking! "default reason"))
  ([reason]
   (log! :warn (str "stop-mocking! [" reason "] (Mocking status set to :done and messages-remaining to 0.)"))
   (throw (ex-info "too early?" {}))
  (swap! mocking-state
         #(-> % (assoc :status :done)
                (assoc :messages-remaining-cnt 0)))))

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

(defn mocking-role
  "Return one of #{:interviewer :interviewees :orchestrator :other} describing the type of agent."
  [agent-id]
  (let [{:agent/keys [base-type surrogate?]} (get-agent-from-wherever agent-id)]
    (cond surrogate?                                 :interviewees
          (#{:process-interview-agent
             :data-interview-agent
             :resources-interview-agent
             :optimality-interview-agent} base-type) :interviewer
          (= base-type :orchestrator-agent)          :orchestrator
          :else                                      :other)))

;;; ---------------------------------------------------- project ---------------------------------------------------------
(defn project-exists?
  "If a project with argument :project/id (a keyword) exists, return the root entity ID of the project
   (the entity id of the map containing :project/id in the database named by the argumen project-id)."
  [pid]
  (assert (keyword? pid))
  (and (some #(= % pid) (sutil/db-ids))
       (d/q '[:find ?e .
              :in $ ?pid
              :where
              [?e :project/id ?pid]]
            @(connect-atm pid) pid)))


(def conversation-defaults
  [{:conversation/id :process
    :conversation/status :in-progress} ; We assume things start here.
   {:conversation/id :data
    :conversation/status :not-started}
   {:conversation/id :resources
    :conversation/status :not-started}
   {:conversation/id :optimality
    :conversation/status :not-started}])

;;; Maybe this should not be used;
(defn ^:private destroy-shadow-db!
  [pid]
  (when (connect-atm pid {:error? false})
    (let [cfg (sutil/get-db-cfg pid)]
      (if (= :mem (-> cfg :store :backend))
        (when (d/database-exists? cfg)
          (d/delete-database cfg)
          (sutil/deregister-db pid)
          true)
        (log! :error "Mocking DB is not :mem")))))

;;; This is like db/add-project-to-system, except that it doesn't take a third argument, the directory;
;;; there is no directory associated with a in-memory database! Further, it is marked as :project/in-memory? true.
(defn ^:private add-temp-project-to-system!
  "Add the argument project (a db-cfg map) to the system database."
  [id project-name]
  (let [conn-atm (connect-atm :system)
        eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn-atm)]
    (d/transact conn-atm {:tx-data [{:db/id eid
                                     :system/projects {:project/id id
                                                       :project/in-memory? true
                                                       :project/name project-name}}]})))

(defn ^:private delete-temp-project-from-system!
  [pid]
  (when (project-exists? pid)
    (let [conn-atm (connect-atm pid)]
      (when-let [s-eid (d/q '[:find ?e . :in $ ?pid :where [?e :project/id ?pid]] @conn-atm pid)]
        (let [obj (resolve-db-id {:db/id s-eid} conn-atm)]
          (d/transact (connect-atm :system) {:tx-data (for [[k v] obj] [:db/retract s-eid k v])}))))))

(defn delete-project!
  [pid]
  (destroy-shadow-db! pid)
  (delete-temp-project-from-system! pid))

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
    (add-temp-project-to-system! id pname)
    id))

;;; ----------------------------------------- Method calling ---------------------------------------------
(defn mocked-agent?
  "Return true if the agent is a type that is mocked."
  [agent-id]
  (mocked-type? (mocking-role agent-id)))

(defn ^:private get-mocked-dispatch
  [tag _agent-id]
  tag)

(defn ^:private update-activities
  "The vector of activities in a conversation-history message may have a summary-ds.
   This must be as string to pass (s/valid? ::specs/interview-message)."
  [msg]
  (if (contains? msg :activity)
    (update msg :activity (fn [acts] (mapv (fn [act] (cond-> act
                                                       (contains? act :summary-DS)     (update :summary-DS str)
                                                       (contains? act :pursuing-EADS)  (update :pursuing-EADS keyword)))
                                           acts)))
    msg))

(defn ^:private ai-str2msg
  "Return a s/valid? interviewing message-type for the argument string."
  [s]
  (let [msg-obj (ai-response2clj s)
        msg-obj (cond-> msg-obj
                  true   (update :message-type keyword)
                  (contains? msg-obj :interviewee-type)                 (update :interviewee-type keyword)
                  (contains? msg-obj :activity)                          update-activities)]
    (if (s/valid? ::specs/interviewer-msg msg-obj)
      msg-obj
      (throw (ex-info "Invalid message:" {:msg-string s})))))

(defn ^:private msg2json
  "We use this so that we don't hit the assert check for nil on sutil/clj2json-pretty."
  [msg]
  (when msg (sutil/clj2json-pretty msg)))

(def ok-msg (msg2json {:message-type :STATUS :status "ok"}))

(defmulti ^:private mocked-response-by-role! #'get-mocked-dispatch)

;;; ---------------------------------- Interviewer ---------------------------------------------
(defn ^:private script-from
  "Return the portion of argument script vector after the entry with :message/content = after-text."
  [script after-text]
  (let [found? (atom nil)
        result (atom [])
        after-text (str/trim after-text)]
    (doseq [msg script]
      (when @found? (swap! result #(conj % msg)))
      (when (= (-> msg :message/content str/trim) after-text) (reset! found? true)))
    (when (empty? @result) (stop-mocking! "script-from: Empty script."))
    @result))

(defn ^:private get-next-question
  "Return the question that should be asked next."
  [{:keys [last-question script] :as _state}]
  (let [script (if (empty? last-question) script (script-from script last-question))
        question (-> (some #(when (= (:message/from %) :system) %) script) :message/content)]
    (if (empty? question)
        (do (stop-mocking! "get-next-question: No questions left")
            {:message-type :mocking-complete})
        question)))

;;; (mock/set-mocking-state-to-start! :sur-craft-beer-4script)
;;; (mock/next-question-to-ask!) ;...
(defn ^:private next-question-to-ask!
  "Return a validated question-to-ask message and update mocking-state.
   The question is acquired from mocking-state, but the budget is from the shadow DB."
  []
  (let [q (get-next-question @mocking-state)
        msg {:message-type :QUESTION-TO-ASK
             :question q}]
    (when-not (and (map? q) (= (:message-type q) :mocking-complete))
      (s/assert :iviewr/question-to-ask msg)
      (swap! mocking-state #(assoc % :last-question q))
      msg)))

(defn ^:private next-refinement-msg
  "The interviewer received an interviewees-respond message and have provide the corresponding DSR message.
   Turns out, we put the whole DSR in the message untouched, so just get it.
   This doesn't touch change mocking-state; only get-question-to-ask does that."
  []
  (let [{:keys [last-question script]} @mocking-state
        script (if (empty? last-question) script (script-from script last-question))
        dsr (some #(when (contains? % :message/EADS-data-structure)
                     (:message/EADS-data-structure %))
                  script)
        msg (when dsr (edn/read-string dsr))]
    (s/assert :iviewr/data-structure-refinement msg)
    msg))

;;; Interviewer interactions:
;;;   conversation-history -> ok
;;;   eads-instructions -> ok
;;;   supply-question -> question-to-ask
;;;   interviewees-respond -> data-structure-refinement
(defmethod mocked-response-by-role! :interviewer
  [_tag text]
  (when (continue-mocking?)
    (let [msg (ai-str2msg text)]
      (cond  (s/valid? :iviewr/conversation-history msg)            ok-msg
             (s/valid? :iviewr/eads-instructions msg)               ok-msg
             (s/valid? :iviewr/supply-question msg)                 (-> (next-question-to-ask!) msg2json)
             (s/valid? :iviewr/interviewees-respond msg)            (-> (next-refinement-msg)  msg2json)
             :else                                                  (throw (ex-info "Invalid message to interviewer:" {:text text}))))))

;;; ---------------------------------- Interviewees ---------------------------------------------
;;; ToDo: This will need to be updated in the case the the interviewees are human.
(defn get-next-response
  "Return what the interviewees' said in response to the question.
   No need to update state, I think."
  [question-text]
  (let [{:keys [last-question script]} @mocking-state
        question-text (str/trim question-text)]
    (when-not (= question-text (str/trim last-question))
      (log! :error (str "Intervieees: Mocking out of sync at question: \n  question-text: "
                        question-text
                        "\n last-question: " last-question)))
    (let [script (script-from script question-text)
          response (-> (some #(when (= (:message/from %) :surrogate) %) script) :message/content)]
      ;; Surrogates return a string. So we could return a string here. But the return is to inv/chat-pair-interviewees-aux,
      ;; which will turn it into a message like this if it isn't one already.
      ;; I suppose to exactly mock, we'd return a string here, but I think it is better to nail down what
      ;; the string is about right here, and let the translation back to clj happen (in is translated to JSON below).
      {:dispatch-key :domain-expert-says :msg-text response})))

;;; Interviewees interactions:
;;;   question (a string) ->  answer (a string)
(defmethod mocked-response-by-role! :interviewees
  [_tag question-text]
  (when (continue-mocking?)
    (-> question-text get-next-response msg2json)))

;;; ---------------------------------- orchestrator ---------------------------------------------
;;; Orchestrator only gets :iviewr/conversation-history and :iviewr/backchannel-communication (not implemented) messages.
;;; Orchestrator only generates :iviewr/pursue-eads and backchannel-coms (not implemented) messages.
(defn next-eads-instructions
  "Return a pursue-eads message relevant for the next message in conversation."
  []
  (let [{:keys [last-question script]} @mocking-state
        script (if last-question (script-from script last-question) script)
        eads-id (-> (some #(when (contains? % :message/pursuing-EADS) %) script)
                    :message/pursuing-EADS)
        msg {:message-type :PURSUE-EADS :EADS-id eads-id}]
    (if (or (empty? script) (nil? eads-id))
      (do (stop-mocking! "next-eads-instruction: empty script") nil)
      (s/assert :iviewr/pursue-eads msg))))

;;; Interviewees interactions:
;;;   conversation-history -> pursue-eads
(defmethod mocked-response-by-role! :orchestrator
  [_tag text]
  (when (continue-mocking?)
    (let [msg (ai-str2msg text)]
      (when (s/valid? :iviewr/conversation-history msg)
        (when-let [pursue-msg (next-eads-instructions)]
          (when (s/valid? :iviewr/pursue-eads pursue-msg)
              (msg2json pursue-msg)))))))

;;; ---------------------------------- Initiating mocking --------------------------
(defn set-mocking-state-to-start!
  [pid]
  (let [conn-atm (sutil/connect-atm pid)
        script (->> (d/q '[:find [?db-id ...]
                           :where [?db-id :message/pursuing-EADS]]
                         @conn-atm)
                    (dp/pull-many @conn-atm '[*])
                    (sort-by :message/time)
                    vec)]
    (reset! mocking-state {:pid-of-original pid
                           :ork-EADS (some (fn [s-line]
                                             (when (contains? s-line :message/pursuing-EADS)
                                               (:message/pursuing-EADS s-line)))
                                           script)
                           :messages-remaining-cnt (count script)
                           :script script})))

(defn prepare-to-mock!
  "Run mocking on the argument project."
  [pid]
  (when @old-shadow-db
    (destroy-shadow-db! @old-shadow-db))
  (reset! old-shadow-db (make-shadow-db! pid))
  (reset! mocking? true)
  (let [{:keys [script]} (set-mocking-state-to-start! pid)]
    (if (not-empty script)
      (do
        (log! :info (str "Setting up mocking with " (count script) " messages from project " pid))
        (reset! mocking? true)
        (log! :info "Mocking enabled"))
      (do
        (log! :warn (str "No messages found for project " pid ", mocking not enabled"))
        (reset! mocking? true)))))

;;; ToDo: I'm not sure where this really belongs, but for now it is on get-mocked-response!,
;;;       meaning that it happens on the agent query.
(defn decrement-messages-remaining!
  "To ensure the system stops (!) decrement the mocking-state's :messages-remaining-cnt."
  []
  (swap! mocking-state #(update % :messages-remaining-cnt dec)))

;;; ----------------------------------------------- Toplevel fn (only thing that need be public here?) ---------------------------------
(defn get-mocked-response!
  "Return the string that the LLM would have returned at this point in the process.
   This is the key function of the mocking capability!"
  [agent-id text]
  (let [role (mocking-role agent-id)]
    (log! :info (str "Mocking " (name role) ", remaining-messages = " (:messages-remaining-cnt @mocking-state)))
    (when (<= (:messages-remaining-cnt @mocking-state) 0)
      (swap! mocking-state #(assoc % :status :done)))
    (if (continue-mocking?)
      (let [res (mocked-response-by-role! role text)]
        (decrement-messages-remaining!)
        res)
      (msg2json {:message-type :mocking-complete}))))

;;; Typical usage from user namespace:
;;; (mock/with-mock-project :sur-craft-beer-4script (scheduling-tbd.iviewr.interviewers/resume-conversation {:client-id :console :pid :sur-craft-beer-4script :cid :process}))
;;; (mock/with-mock-project :sur-craft-beer-4script (inv/resume-conversation {:client-id (ws/recent-client!) :pid :sur-craft-beer-4script :cid :process}))
(defmacro with-mock-project
  "Execute the given function with mocking enabled for the specified project."
  [pid & body]
  `(try (do
          (prepare-to-mock! ~pid)
          ~@body
          (reset! mocking? false)
          (stop-mocking! "with-mock-project: ran the body"))
        #_(finally ; This seems to be happening ASYNC!!!
          (stop-mocking! "with-mock-project: finally")
          (reset! mocking? false)))) ; Don't set mocking to false in development because you will to walk through calls.
