(ns scheduling-tbd.operators
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.core.unify    :as uni]
   [clojure.pprint        :refer [cl-format]]
   [clojure.spec.alpha    :as s]
   [promesa.core          :as p]
   [promesa.exec          :as px]
   [scheduling-tbd.db     :as db]
   [scheduling-tbd.domain.process.interview :as inv]
   [scheduling-tbd.llm    :as llm]
   [scheduling-tbd.specs  :as spec]
   [scheduling-tbd.sutil  :as sutil :refer [find-fact]]
   [scheduling-tbd.web.websockets  :as ws]
   [taoensso.timbre       :as log]))

;;; Two method types are associated with each plan operator. For both method types, a 'tag' (keyword) selects the
;;; method to call, either an 'operator' (defined by defoperator) or a db-action (defined by defaction).
;;; Method tags correspond to an operator head predicate in the planning domain. The tag is the predicate symbol keywordized.
;;; For example, an operator head (!query-process-steps ?proj) corresponds to a tag :!query-process-steps.
;;;
;;; Execution of an operator is comprised of the following phases, which are accomplished by the operator methods shown:
;;;    1) A query is presented to the (human or surrogate) agent.                                    - defoperator
;;;    2) The response in collected.                                                                 - defoperator
;;;    3) The response is analyzed, producing new state knowledge.                                   - defaction
;;;    4) The state is updated by operator d-list and a-list actions and written to the project db.  - defaction
;;;    5) Additional comments (but not queries) can be added to the chat                             - defaction
;;;
;;; Note that by these means we don't commit anything to the DB until step (4).
;;; This ensures that when we can restart the project we can put the right question back in play.
;;;
;;; Program behavior differs in places depending on whether the agent is human or surrogate. Most obviously,
;;; in Step (1) ws/send-msg is used for humans, whereas llm/query-on-thread is used for surrogates.
;;; For the most part, we look at the state vector and use the function (surogate? state) to vary the behavior.

(def debugging? (atom false))
(def ^:diag diag (atom nil))
(defonce operator-method? (atom #{})) ; "A set of operator symbols, one for each method defined by defoperator."

(defmacro defoperator
  "Macro to wrap planner operator methods."
  {:clj-kondo/lint-as 'clojure.core/defmacro ; See https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#inline-macro-configuration
   :arglists '([arg-map] & body)} ; You can put more in :arglists, e.g.  :arglists '([[in out] & body] [[in out err] & body])}
  [tag [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(do (swap! operator-method? #(conj % '~tag))
       (defmethod operator-meth ~tag [~arg-map]
         (when @debugging? (println (cl-format nil "==> ~A (op)" ~tag)))
         (let [res# (do ~@body)]
           (if (seq? res#) (doall res#) res#)
           (do (when @debugging?     (println (cl-format nil "<-- ~A (op) returns ~S" ~tag res#)))
               res#)))))

(defn operator-meth-dispatch
  "Parameters to operator-meth have form [plan-step proj-id domain & other-args]
   This dispatch function choose a method by return (:operator plan-step)."
  [obj]
  (if-let [tag (:tag obj)]
    tag
    (throw (ex-info "operator-meth-dispatch: No dispatch value for plan-step" {:obj obj}))))

(defmulti operator-meth #'operator-meth-dispatch)

;;; --------  db-actions is similar but adds a second object, the response from the operator -----------------------------------
(defmacro defaction
  "Macro to wrap methods for updating the project's database for effects from running an operation."
  {:clj-kondo/lint-as 'clojure.core/defmacro
   :arglists '(tag [arg-map] & body)}
  [tag [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod db-action ~tag [~arg-map]
     (when @debugging? (println (cl-format nil "==> ~A (act)" ~tag)))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       (do (when @debugging?     (println (cl-format nil "<-- ~A (act) returns ~S" ~tag res#)))
           res#))))

(defn db-action-dispatch
  "Parameters to db-action is a object with at least a :plan-step in it and a response from operator-meth (user response)."
  [obj]
  ;(log/info "db-action-dispatch: obj =" obj "response =" _response)
  (if-let [tag (:tag obj)]
    tag
    (throw (ex-info "db-action-dispatch: No dispatch value for plan-step" {:obj obj}))))

(defmulti db-action #'db-action-dispatch)

(defn run-operator!
  "Run the action associated with the operator, returning an updated state EXCEPT where fails."
  [state {:operator/keys [head a-list] :as _task} {:keys [inject-failures pid] :as opts}]
  (let [op-tag (-> head first keyword)]
    (cond (some #(uni/unify head %) inject-failures)         (do (log/info "+++Operator FAILURE (INJECTED):" op-tag)
                                                                  (throw (ex-info "run-operator injected failure" {:op-head op-tag})))
          (and
           (not-empty a-list)
           (every?
            (fn [a-item]
              (some #(uni/unify a-item %) state)) a-list))   (log/info "+++Operator" op-tag "pass-through (SATISFIED)")

          (@operator-method? op-tag)                         (do (operator-meth (-> opts
                                                                                    (assoc :state state)
                                                                                    (assoc :tag op-tag)
                                                                                    (dissoc :inject-failures)))
                                                                 (log/info "+++Operator" op-tag "ran (ACTUAL) state = " (db/get-planning-state pid)))

          :else                                               (log/info "+++Operator" op-tag "pass-through (NOT RECOGNIZED)"))))


;;; ToDo: The idea of Skolem's in the add-list needs thought. Is there need for a universal fact?
;;;       For the time being, I'm just adding uniquely
(defn operator-update-state
  "Update the project's DB, specifically :project/state-string with infromation from last response and operator a-list and d-list.
      - old-state   - a set of proposition.
      - proj-id     - task a planner operator destructure
      - bindings    - variable binding established when unifying with this operator.
   Returns the new state."
  [old-state task bindings]
  (assert (every? #(s/valid? ::spec/positive-proposition %) old-state))
  (let [a-list (mapv #(uni/subst % bindings) (:operator/a-list task))
        d-list (mapv #(uni/subst % bindings) (:operator/d-list task))]
    (cond-> old-state
      (not-empty d-list) (->> (remove (fn [fact] (some #(uni/unify fact %) d-list))) set)
      (not-empty a-list) (->> (into a-list) set))))

(defn surrogate?
  "Return true if state has a predicate unifying with (surrogate ?x)."
  [state]
  (find-fact '(surrogate ?x) state))

(defn make-human-project
  "Surrogate already has a project db, but human doesn't. This creates the db and returns and returns state (possibly updated).
   This is called after inv/prelim-analysis, which looks at the human response to define a proj-name predicate."
  [state]
  (log/info "Human project: state =" state)
  (if-let [[_ pname] (find-fact '(proj-name ?x) state)]
    (let [[_ orig-pid] (find-fact '(proj-id ?x) state)
          pid (db/create-proj-db! {:project/name pname :project/id (keyword orig-pid)})]
      (if (not= orig-pid pid) ; creating the DB may assign a different PID. ToDo: Need a (proj-name too).
        (conj (filterv #(not= % orig-pid) state)
              `(~'proj-id ~(name pid)))
        state))
    (throw (ex-info "Couldn't find PID in human project." {:state state}))))

(defn chat-pair-aux
  "Run one query/response pair of chat elements with a human or a surrogate.
   Returns promise which will resolve to the original obj argument except:
     1) :agent-query is adapted from the input argument value for the agent type (human or surrogate)
     2) :response is added. Typically its value is a string."
  [{:keys [pid state agent-query] :as obj}]
  (log/info "Chat pair: surrogate? =" (surrogate? state))
  (let [aid (db/get-assistant-id pid nil)
        tid (db/get-thread-id pid nil)
        agent-type (if (surrogate? state) :surrogate :human)
        prom (if (= :surrogate agent-type)
               (px/submit! (fn []
                             (try
                               (llm/query-on-thread :tid tid :aid aid :query-text agent-query)   ; This can timeout.
                               (catch Exception e {:error e}))))
               (ws/send-to-chat (-> obj
                                    (assoc :msg agent-query)
                                    (assoc :dispatch-key :tbd-says))))]                                             ; This cannot timeout.
    (p/await prom))); You can't put anything else here or a promise will be passed! ; ToDo: Fix this. Put a catch before the p/await!.

(defn chat-pair
  "Call to run the chat and put the query and response into the project's database.
   Returns the argument object with :response set to some text."
  ([obj] (chat-pair obj []))
  ([{:keys [pid state agent-query] :as obj} msg-keys]
   (let [response-text (chat-pair-aux obj)]
     (if (string? response-text)
       (let [user-role (if (surrogate? state) :surrogate :human)]
         (db/add-msg pid :system agent-query (conj msg-keys :query))
         (db/add-msg pid user-role response-text (conj msg-keys :response))
         (assoc obj :response response-text))
       (throw (ex-info "Unknown response from operator" {:response response-text}))))))

;;;=================================================== Operators ======================================
;;; Operator db-actions update state in the db (db/add-plannin-state) and return nil.
;;; The update does not include the call to operator-update-state, which applies the d-list and a-list.
;;; operator-update-state is called by the planner, plan/update-planning.

;;; ----- :!initial-question
(def intro-prompt
  [{:msg-text/string "Describe your most significant scheduling problem in a few sentences"}
   {:human-only [{:msg-text/string " or "}
                 {:msg-link/uri "http://localhost:3300/learn-more"
                  :msg-link/text "learn more about how this works"}]}
   {:msg-text/string "."}])

(defoperator :!initial-question [{:keys [state] :as obj}]
  (let [agent-query (if (surrogate? state)
                      "Describe your most significant scheduling problem in a few sentences."
                      (str "Describe your most significant scheduling problem in a few sentences or "
                           "<a href=\"http://localhost:3300/learn-more\">learn more about how this works</a>."))]
    (-> obj (assoc :agent-query agent-query) (chat-pair [:initial-question]) db-action)))

;;; (op/db-action (assoc op/example :client-id (ws/recent-client!)))
(defaction :!initial-question [{:keys [state response client-id agent-query] :as _obj}]
  (log/info "*******db-action (!initial-question): response =" response "state =" state)
  (let [surrogate? (surrogate? state)
        analysis-state (inv/analyze-intro-response response state)] ; return state props proj-id and proj-name if human, otherwise argument state.
    (when-not surrogate? (make-human-project analysis-state))
    ;;--------  Now human/surrogate can be treated nearly identically ---------
    (let [[_ pid]   (find-fact '(proj-id ?x) analysis-state)
          [_ pname] (find-fact '(proj-name ?x) analysis-state)
          cites-supply? (find-fact '(cites-raw-material-challenge ?x) analysis-state)
          pid (keyword pid)]
      (db/add-planning-state pid analysis-state)
      (db/add-msg pid :system (format "Great, we'll call your project %s." pname) [:informative])
      (when cites-supply?
        (let [msg (str "Though you've cited a challenge with inputs (raw material, workers, or other resources), "
                       "we'd like to put that aside for a minute and talk about the processes that make product.")]
          ;(ws/send-to-chat {:promise? nil :client-id client-id :dispatch-key :tbd-says :msg msg})
          (db/add-msg pid :system msg)))
      ;; Complete preliminary analysis in a parallel agent that, in the case of a human expert, works independently.
      (db/add-planning-state pid (inv/parallel-expert-prelim-analysis pid)))))

;;; ----- :!yes-no-process-steps ---------------------------------------------------------------------------------------------------------------------
(defoperator :!yes-no-process-steps [{:keys [state] :as obj}]
  (let [agent-query (if (surrogate? state)
                      (str "Please list the steps of your process, one per line in the order they are executed to produce a product, "
                           "so it looks like this:\n"
                           "1. (the first step)\n"
                           "2. (the second step)...\n")
                      "Select the process steps from the list on the right that are typically part of your processes. \n (When done hit \"Submit\".)")]
    (-> obj (assoc :agent-query agent-query) (chat-pair [:process-steps]) db-action)))

(defaction :!yes-no-process-steps [{:keys [pid response client-id] :as obj}]
  ;; Nothing to do here but update state from a-list.
  (reset! diag obj)
  (log/info "!yes-no-process-steps (action): response =" response)
  (let [more-state (inv/analyze-process-steps-response obj)
        new-code (inv/mzn-process-steps more-state)
        minizinc-enum-announce
        (str "Okay, we now know enough to get started on a MiniZinc solution. "
             "In the code pane (upper right of the app) we added a <a href=\"http://localhost:3300/mzn-enum\">MiniZinc enum</a>. "
             "The 'enum values' name the steps of your process in the order they are executed for each product.")]
    (db/add-planning-state pid more-state)
    (ws/send-to-chat {:client-id client-id :dispatch-key :update-code :text new-code})
    (db/put-code pid new-code)
    ;; ToDo: This should really be just for humans.
    (ws/send-to-chat {:client-id client-id
                      :dispatch-key :tbd-says
                      :promise? false
                      :msg minizinc-enum-announce})
    (db/add-msg pid :system minizinc-enum-announce [:info-to-user :minizinc])
    more-state))

;;; ----- :!query-process-durs ---------------------------------------------------------------------------------------------------------------------
(defoperator :!query-process-durs [{:keys [state] :as obj}]
  (let [agent-query (if (surrogate? state)
                      (format (str "I suppose processing times for each of the steps you just mentioned might vary from product to product. "
                                   "But generally speaking, how long does each step take? "
                                   "Please produce a list just like the one you did for process steps, one process per line, but add to it the typical processing time "
                                   "so it looks like this:\n"
                                   "1. %s (some amount of time)\n"
                                   "2. %s (some amount of time)...")
                              (-> (find-fact '(process-step ?proj 1 ?process) state) (nth 3))
                              (-> (find-fact '(process-step ?proj 2 ?process) state) (nth 3)))
                      "Provide typical process durations for the tasks on the right.\n(When done hit \"Submit\".)")]
    (-> obj (assoc :agent-query agent-query) (chat-pair [:process-durs]) db-action)))

(defaction :!query-process-durs [{:keys [response pid] :as obj}]
  (log/info "!query-process-durs (action): response =" response "obj =" obj)
  (let [more-state (inv/analyze-process-durs-response obj)]
    (db/add-planning-state pid more-state)))


;;; ======================================================================================================================
;;; ------------------------- Data operators -----------------------------------------------------------------------------
;;; ======================================================================================================================
(defoperator :!run-table-agent [{:keys [_state] :as obj}]
  (log/info "-------------------Run the table agent here.-----------------")
  (-> obj db-action))

(defaction :!run-table-agent [{:keys [state] :as obj}]
  (log/info "-----------------Write table info to the DB here.----------")
  state)
