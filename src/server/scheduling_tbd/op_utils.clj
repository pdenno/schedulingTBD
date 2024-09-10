(ns scheduling-tbd.op-utils
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.core.unify    :as uni]
   [clojure.datafy        :refer [datafy]]
   [clojure.pprint        :refer [cl-format]]
   [promesa.core          :as p]
   [promesa.exec          :as px]
   [scheduling-tbd.db     :as db]
   [scheduling-tbd.llm    :as llm]
   [scheduling-tbd.sutil  :as sutil :refer [find-fact domain-conversation]]
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
  "Macro to wrap methods for updating the project's database for effects from running an operation.
   Returned value is not meaningful."
  {:clj-kondo/lint-as 'clojure.core/defmacro
   :arglists '(tag [arg-map] & body)}
  [tag [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod db-action ~tag [~arg-map]
     (when @debugging? (println (cl-format nil "==> ~A (act)" ~tag)))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       (do (when @debugging? (println (cl-format nil "<-- ~A (act) returns ~S" ~tag res#)))
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
  "Run the action associated with the operator, updating planning-state in the DB."
  [{:operator/keys [head] :as _task} {:keys [inject-failures pid verbose?] :or {verbose? true} :as opts}]
  (when verbose? (log/info "\n\n\n***** run-operator! head =" head))
  (let [state (db/get-planning-state pid)
        op-tag (-> head first keyword)
        done (list 'done (-> op-tag name symbol) (-> pid name symbol))]
    ;; This is done for side effects.
    (cond (some #(uni/unify head %) inject-failures)      (throw (ex-info "run-operator injected failure" {:op-head op-tag}))
          (some #(= done %) state)                        (when verbose? (log/info "+++Operator" op-tag "pass-through (a 'done' fact)"))
          (@operator-method? op-tag)                      (do (operator-meth (-> opts
                                                                                 (assoc :state state)
                                                                                 (assoc :tag op-tag)
                                                                                 (dissoc :inject-failures)))
                                                              (db/add-planning-state pid [done]))
          :else                                           (throw (ex-info "No operator for method" {:op-tag op-tag})))
    nil))

;;; ToDo: The idea of Skolem's in the add-list needs thought. Is there need for a universal fact?
;;;       For the time being, I'm just adding uniquely
(defn operator-update-state!
  "Update the project db's :problem/planning-state by unification with the  a-list and d-list.
      - operator    - the operator in the planning domain from which the d-list and a-list are obtained.
      - bindings    - variable binding established when unifying with this operator's d-list and a-list."
  [pid operator bindings]
  (let [a-list (mapv #(uni/subst % bindings) (:operator/a-list operator))
        d-list (mapv #(uni/subst % bindings) (:operator/d-list operator))
        new-state (cond-> (db/get-planning-state pid)
                    (not-empty d-list) (->> (remove (fn [fact] (some #(uni/unify fact %) d-list))) set)
                    (not-empty a-list) (->> (into a-list) set))]
    (db/put-planning-state pid new-state)))

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

;;; (ou/chat-pair-aux {:pid :sur-fountain-pens :surrogate? true :agent-query "Describe your most significant scheduling problem in a few sentences."} {})
(defn chat-pair-aux
  "Run one query/response pair of chat elements with a human or a surrogate.
   Returns promise which will resolve to the original obj argument except:
     1) :agent-query is adapted from the input argument value for the agent type (human or surrogate)
     2) :response is added. Typically its value is a string."
  [{:keys [pid surrogate? agent-query] :as obj} {:keys [tries preprocess-fn] :or {tries 1 preprocess-fn identity}}]
  (reset! diag obj)
  (let [aid (db/get-assistant-id pid nil)
        tid (db/get-thread-id pid nil)
        prom (if surrogate?
               (px/submit! (fn []
                             (try
                               (llm/query-on-thread :tid tid            ; This can timeout.
                                                    :aid aid
                                                    :query-text agent-query
                                                    :tries tries
                                                    :preprocess-fn preprocess-fn)
                               (catch Exception e {:error e}))))
               (ws/send-to-chat (-> obj
                                    (assoc :msg agent-query)
                                    (assoc :promise? true)
                                    (assoc :dispatch-key :tbd-says))))] ; This cannot timeout.
    (-> prom
       (p/catch (fn [e]
                  (reset! diag (datafy e))
                  (log/error "Failed in chat-pair-aux:" (datafy e))))
        p/await)))

(defn chat-pair
  "Call to run the chat and put the query and response into the project's database.
   Returns the argument object with :response set to some text.
   Note that this also updates the UI (ws/refresh-client)"
  ([obj] (chat-pair obj {}))
  ([{:keys [pid surrogate? agent-query client-id domain-id] :as obj}, {:keys [tags] :or {tags []} :as opts}]
   (let [response-text (chat-pair-aux obj opts)]
     (if (string? response-text)
       (let [user-role (if surrogate? :surrogate :human)]
         (db/add-msg pid :system agent-query (conj tags :query))
         (db/add-msg pid user-role response-text (conj tags :response))
         (ws/refresh-client client-id pid (domain-conversation domain-id)) ; ToDo: fix this.
         (assoc obj :response response-text))
       (throw (ex-info "Unknown response from operator" {:response response-text}))))))
