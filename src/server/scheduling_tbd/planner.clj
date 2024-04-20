(ns scheduling-tbd.planner
  "This provides functions to prune a planning domain and run an interview."
  (:require
   [clojure.core.unify        :as uni]
   [clojure.edn               :as edn]
   [clojure.spec.alpha        :as s]
   [mount.core                :as mount :refer [defstate]]
   [scheduling-tbd.db         :as db]
   [scheduling-tbd.operators  :as op]
   [scheduling-tbd.specs      :as spec]
   [scheduling-tbd.sutil      :as sutil :refer [get-domain register-planning-domain]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.timbre           :as log]))

(def ^:diag diag (atom nil))

;;; The feasibility of a plan, deciding whether it will be returned from find-plans, depends on pre-conditions.
;;; So attention must be paid to the state of the world created through operator add and delete lists.
;;;
;;; I *think* that I can maintain probabilistic data about observations separately (in datahike). This is used in plan selection ("tree search").
;;; This probabilistic data would unify with method and operator pre-conditions, and thereby give input to running ExplainLib analyses selecting a plan.
;;; Question: Do the BNs resemble the plans? Is there a translation from plans into BNs? (Or translation from BNs into plans?)
;;;
;;; To make this comprehensible, I need at least two paths.
;;; Thus one is:   1) initial-question, 2) analysis, 3) choose well-known-process path, 4) check-list-for-processes, 5) confirm system structure
;;; The other is:  1) initial-question, 2) analysis, 3) ask for steps, 4) characterize steps (resources and time)
;;; Examples of first path is craft beer. Example of second path is ice hockey sticks.
;;; Gray area?: music school lesson scheduling.

;;;=================================================== Dynamic planner ===========================================================

;;; ToDo:
;;;   - Use a DB for managing stack/navigation???
;;;      cons: - literal structures, yuk
;;;            - distracting from the development goal, at least early on.
;;;      pros: - potentially better manipulation for backtracking and reordering priorities as costs change.
;;;            - leverage existing structure in shop.clj.
;;;            - persistent! (useful for debugging and saving user's state).
(defn head-of    [elem] (or (:method/head elem)(:operator/head elem) (:axiom/head elem)))

(defn axiom? [elem]
  (when (contains? elem :axiom/head) elem))

(defn method? [elem]
  (when (contains? elem :method/head) elem))

(defn operator? [elem]
  (when (contains? elem :operator/head) elem))

(defn matching-task?
  "Return a map of :elem and :bindings if the elem's head unifies with the argument literal."
  [lit task]
  (when-let [bindings (uni/unify lit (head-of task))]
    {:bindings bindings
     :task task}))

(defn not-lit?
  "Return the positive literal if argument is a negative literal, otherwise nil."
  [lit]
  (when (= 'not (first lit)) (second lit)))

(defn satisfied?
  "Returns truthy if the state satisfies the argument condition.
   (1) If the condition is a positive literal, it is satisfied by unify with some element of the state.
   (2) If condition literal is negative, it is satisfied by its positive variant not unifying with any proposition of the state.
   In case (1) it returns the a map containing the bindings. When the condition and state are identical, this is an empty map.
   In case (2) it just returns the empty map."
  [condition state]
  (if-let [lit (not-lit? condition)]
    (when (not-any? #(uni/unify lit %) state) {})
    (some #(when-let [binds (uni/unify condition %)] binds) state)))

(defn consistent-bindings?
  "Argument is a vector of bindings maps.
   Return a merged binding map if the bindings among the arguments maps are consistent and nil otherwise.
   Examples: (consistent-bindings '[{:a 1 :b 2} {:a 1 :c 3}]) ==> {:a 1 :b 2 :c 3}
             (consistent-bindings '[{:a 1 :b 2} {:b 3}]) ==> nil."
  [bindings]
  (if (empty? bindings)
    {}
    (try (reduce (fn [res mval] ; Value here is a map.
                   (reduce-kv (fn [_ k v]
                                (if (contains? res k)
                                  (if (= v (get res k))
                                    res
                                    (throw (ex-info "Inconsistent bindings" {})))
                                  (assoc res k v)))
                              res
                              mval))
                 (first bindings)
                 (rest bindings))
         (catch Exception _e nil))))

;;; ToDo: This should return the elements with substitutions.
;;; ToDo: The variable bindings must be consistent among the preconditions.
(defn satisfying-elems
  "Return a vector of maps describing the specific tasks (e.g. the operator or specific element of :method/rhsides) and bindings when
   that specific element satisfies state. Return nil otherwise."
  [task state]
  (cond (operator? task)    (let [bindings (mapv #(satisfied? % state) (:operator/preconds task))]
                              (when-let [bindings (and (every? identity bindings) (consistent-bindings? bindings))]
                                [{:bindings bindings :task task}]))

        (method? task)      (reduce (fn [res rhs]
                                      (if-let [bindings (if-let [preconds (:method/preconds rhs)]
                                                          (mapv #(satisfied? % state) preconds)
                                                          {})]
                                        (if-let [bindings (and (every? identity bindings) (consistent-bindings? bindings))]
                                          (conj res {:bindings bindings :task (-> {:method/head (:method/head task)}
                                                                                  (assoc :method/rhs (:method/task-list rhs)))})
                                          res)
                                        res))
                                    []
                                    (:method/rhsides task))))

(defn satisfying-tasks
  "Return edited task patterns for operators and methods matching given, task (a positive literal), patterns, and state."
  [task patterns state]
  (let [matching-tasks (reduce (fn [res pat] (if-let [m (matching-task? task pat)] (conj res m) res))
                               []
                               patterns)
        res (reduce (fn [res {:keys [task bindings]}]
                      (if-let [r (satisfying-elems task state)]
                        (into res (map (fn [sat-elem] (update sat-elem :bindings #(merge bindings %))) r))
                        res))
                    []
                    matching-tasks)]
  res))

;;; This only needs to work on the head partial plan, right? (That's even true once I have alternatives, right?)
;;; Later, this is the place to execute the plan by running operators that interact with the user to update state.
;;; I think I can borrow the inference engine from explain lib. It might also be useful for making full navigations to calculate plan cost.
;;; Currently: (1) don't handle axioms, (2) don't create alternatives (domain doesn't have these anyway), (3) costs.
(defn update-planning
  "Update the state of planning by updating the active partial plan, advancing it by whatever the active satisfying elem requires.
   The active satisfying elem (currently first selem) may have multiple tasks in its RHS.
   Assuming that it does not encounter an unsatisfiable precondition, it iterates through the operators and axioms in the RHS until it encounters a method.
   Operators and axioms can thereby update the state.
   Once it encounters a method (or axiom?), it returns a new partial, which has the un-executed method, and anything after pushed onto new-tasks.

   Backtracking is built-in to this algorithm; partials is a stack of choice points.

   stasks is a vector of 'satisfying tasks' that navigate one edge each to new plans.
   A new partial is generated for each satisfying task in this vector.

   partials is a vector of maps containing a navigation of the planning domain. Each map contains the following keys:
      :plan       - A vector describing what plan steps have been executed, that is, the current traversal of the domain for this partial plan.
                    Its first element in the goal, all others are (ground ?) operator heads.
      :new-tasks  - Maps of the current active edges, about to be traversed (method) or acted upon (operators).
                    Each map has :pred and :bindings from
                    When new-tasks is empty, the plan has been completed.
      :state      - is the state of the world in which the plan is being carried out.
                    It is modified by the actions operators (interacting with the user) and the d-lists and a-lists of operators.

   s-tasks (satisfying-elements) is a map containing the following keys:
      :bindings  - is a map of variable bindings,
      :task      - is information from the operator or method RHS satisfying RHS the preconditions."
  [partials s-tasks {:keys [] :as opts}]
  (let [part (first partials)
        {:keys [task bindings]} (first s-tasks)] ; <======================== Every, not just first. (Probably just a reduce over this?)
    (cond (empty? s-tasks)   (do
                               (log/info "Navigation fails:" part)
                               (-> partials rest vec)) ; No way forward from this navigation. The partial can be removed.

          ;; Execute the operator. If it succeeds, update state, new-tasks, and plan.
          (operator? task)   (let [op-head (-> task :operator/head (uni/subst bindings))
                                   new-partial (try (-> part ; ToDo: Assumes only run-operator can throw.
                                                        (update :state #(op/run-operator % task opts))
                                                        (update :state #(op/operator-update-state % task bindings))
                                                        (update :new-tasks #(-> % rest vec))
                                                        (update :plan #(conj % op-head))
                                                        vector)
                                                    (catch Exception e
                                                      [(assoc part :failure e)]))]
                               (into new-partial (rest partials)))

          ;; Update the task list with the tasks from the RHS
          (method? task)     (let [new-partial (update part :new-tasks #(into (mapv (fn [t] (uni/subst t bindings)) (:method/rhs task))
                                                                              (-> % rest vec)))] ; drop this task; add the steps
                               (into [new-partial] (rest partials))))))

;;; (plan/plan9 project-id :process-interview client-id {:start-facts (db/get-state project-id)}))
(defn ^:diag plan9
  "A dynamic HTN planner.
   Operates on a stack (vector) of 'partials' (described in the docstring of update-planning).
   Initialize the stack to a partial from a problem definition.
   Iterates a process of looking for tasks that satisfy the head new-task, replacing it and running operations."
  [domain-id problem & {:as opts} ]
  (let [elems   (-> (sutil/get-domain domain-id) :domain/elems)
        state   (:state problem) ; A vector of ground literals.
        goal    (:goal problem)] ; A literal.
    (loop [partials [{:plan [] :new-tasks [goal] :state state}]
           cnt 1]
      ;;(log/info "new-tasks =" (-> partials first :new-tasks) "plan =" (-> partials first :plan) "state = "(-> partials first :state))
      (log/info "partial = " (first partials))
      (let [task (-> partials first :new-tasks first) ; <===== The task might have bindings; This needs to be fixed. (Need to know the var that is bound).
            s-tasks (satisfying-tasks task elems (-> partials first :state))]
        (cond
          (empty? partials)                         {:result :failure :reason :no-successful-plans}
          (-> partials first :new-tasks empty?)     {:result :success :plan-info (first partials)}

          (> cnt 10)                                {:result :stopped :partials partials}
          :else  (let [partials (update-planning partials s-tasks opts)
                       partials (if-let [err (-> partials first :failure)]
                                  (do (log/warn "***Plan fails owing to s-tasks" s-tasks)
                                      (log/error err)
                                      (-> partials rest vec))
                                  partials)]
                     (recur partials
                            (inc cnt))))))))

;;;=================================================== End Dynamic planner =======================================================

;;; ToDo: This isn't quite the whole story. What's missing is that there can be multiple binding sets
;;;       owing to data about the same predicate used with different role values. I have something nice
;;;       to deal with this in explainlib.
(defn condition-satisfies?
  "Return true if the positive condition argument unifies with any fact,
   OR if the negative condition argument unifies with no fact.
   Facts are always positive literals."
  [condition facts]
  (s/assert ::spec/proposition condition)
  (if (s/valid? ::spec/negated-proposition condition)
    (let [ncond (second condition)]
      (not-any? #(uni/unify ncond %) facts))
    (some #(uni/unify condition %) facts)))

(defn ^:diag discussion-stopped?
  "Return true if the state vector contains the discussion-stopped fact."
  [state]
  (some #(= 'discussion-stopped (first %)) state))

;;; -------------------------------------- Plan checking --------------------------------------------------------------------
(defn check-domain-to-goals
  "Return nil if domain references the problem otherwise :error-domain-not-addressing-goal.
   This doesn't check whether the problem can be inferred by means of axioms."  ; ToDo: Fix this.
  [domain state-vec goal-vec]
  (let [method-heads (->> domain :domain/elems (filter #(contains? % :method/head)) (mapv :method/head))]
    (doseq [g goal-vec]
      (or (some #(uni/unify g %) method-heads)
          (some #(uni/unify g %) state-vec)
          (throw (ex-info "Goal unifies with neither method-heads nor state-vec."
                          {:goal g :method-heads method-heads :state-vec state-vec}))))))

#_(defn check-goals-are-ground ; ToDo: Possible for plan9?
  [goal-vec]
  (doseq [g goal-vec]
    (when (some #(and (symbol? %) (= "?" (-> % name (subs 0 1)))) (rest g))
      (throw (ex-info "goal vector must be ground:" {:goal g})))))

(defn check-triple
  "Return a keyword naming an obvious error in the domain/problem/execute structure."
  [{:keys [domain problem _execute] :as pass-obj}]
  (let [state-vec (-> problem :problem/state-string edn/read-string vec)
        goal-vec (-> problem :problem/goal-string edn/read-string vec)]
    (check-domain-to-goals domain state-vec goal-vec) ; ToDo: Many more tests like this.
    ;(check-goals-are-ground goal-vec) ; This is something for after translation, thus don't other with it.
    pass-obj))

(defn resume-conversation
  "Start the interview loop. :resume-conversation is a dispatch key from client.
   This is called even for where PID is :START-A-NEW-PROJECT."
  [{:keys [project-id client-id]}]
  #_(plan9 :process-interview
         (db/get-problem project-id)
         {:client-id client-id :pid project-id}))

(defn init-planner!
  []
  (ws/register-ws-dispatch :resume-conversation resume-conversation)
  (register-planning-domain
   :process-interview
   (-> "data/planning-domains/process-interview-1.edn" slurp edn/read-string)))

(defn quit-planner!
  "Quit the planner. It can be restarted with a shell command through mount."
  [])

(defstate planning
  :start (init-planner!)
  :stop  (quit-planner!))
