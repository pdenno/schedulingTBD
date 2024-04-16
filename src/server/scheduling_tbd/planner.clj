(ns scheduling-tbd.planner
  "This provides functions to prune a planning domain and run an interview."
  (:require
   [clojure.core.unify        :as uni]
   [clojure.edn               :as edn]
   [clojure.java.shell        :refer [sh]]
   [clojure.pprint            :refer [cl-format]]
   [clojure.spec.alpha        :as s]
   [clojure.string            :as str]
   [datahike.api              :as d]
   ;;[explainlib.core         :as exp]
   [ezzmq.message             :as zmq]
   [ezzmq.context             :as zmq-ctx]
   [ezzmq.socket              :as zmq-sock]
   [mount.core                :as mount :refer [defstate]]
   [scheduling-tbd.db         :as db]
   [scheduling-tbd.operators  :as op]
   [scheduling-tbd.shop       :as shop]
   [scheduling-tbd.specs      :as spec]
   [scheduling-tbd.sutil      :as sutil :refer [connect-atm resolve-db-id]]
   [scheduling-tbd.web.routes.websockets :as ws]
   [taoensso.timbre           :as log]))

(def planner-endpoint-port 31888)
(def planner-endpoint (format "tcp://*:%s" planner-endpoint-port))
(def planner-executable "I put the planner executable in the project directory" "./pzmq-shop3-2024-02-24")
(def ^:diag diag (atom nil))

;;; The feasibility of a plan, deciding whether it will be returned from find-plans, depends on pre-conditions.
;;; So attention must be paid to the state of the world created through operator add and delete lists.
;;;
;;; I *think* that I can maintain probabilistic data about observations separately (in datahike). This is used in plan selection ("tree search").
;;; This probabilistic data would unify with method and operator pre-conditions, and thereby give input to running ExplainLib analyses selecting a plan.
;;; Question: Do the BNs resemble the plans? Is there a translation from plans into BNs? (Or translation from BNs into plans?)
;;;
;;; I *think* the way to incrementally advance plans is to update the defproblem, by either
;;;   1) updating its ground logical atoms, or
;;;   2) updating its task list.
;;;
;;; Example of (1) is to
;;;   a) update the defproblem with (characterize-process beer-brewing-project)
;;;   b) add ground logical atoms for (project-name beer-brewing-project)
;;;
;;; I *think* it makes sense to store every plan and defproblem (a stack) so as to be able to backtrack.
;;;
;;; This approach might fall a little short from ideal in that I'm going to avoid use of CL functions in plans.
;;; Thus, things won't be as tightly integrated were I to use those. (But what do they do in ordinary SHOP2 anyway?)

;;; To make this comprehensible, I need at least two paths.
;;; Thus one is:   1) initial-question, 2) analysis, 3) choose well-known-process path, 4) check-list-for-processes, 5) confirm system structure
;;; The other is:  1) initial-question, 2) analysis, 3) ask for steps, 4) characterize steps (resources and time)
;;; Examples of first path is craft beer. Example of second path is ice hockey sticks.
;;; Gray area?: music school lesson scheduling.

;;; The challenge I'm facing is how to get partial plans back, so that I can add atoms to state.
;;; Equivalently, this is the problem that I can't update planning state except through the add and delete lists.
;;; Possibly I could add the "special" case with preconditions ((stop-plan)) to all methods automatically.
;;; Every operator that calls out for analysis would add (stop-plan), every method has a special case for stop-plan.
;;; Still, there is the issue of how not to repeat what you've done on replanning. I think that requires no-op if you already have what is needed.

(defn parse-planner-result
  "Owing mostly to the use of strings in messaging, the SHOP planner currently comes back with something
   that has to be cleaned up and interepreted. This takes the string and returns a map with up to three
   values: (1) the form returned (:form), (2) output to std-out (:std-out), and (3) output to err-out (:err-out)."
  [s]
  (let [[plan std err] (str/split s #"BREAK-HERE")
        op-list (str/replace plan #"\n" "")
        [success res] (re-matches #"^\((.*)$" op-list)
        form (cond success                   (try (let [res (edn/read-string res)]
                                                    (cond (= res 'NIL)            nil ; UGH!
                                                          :else                   res))
                                                  (catch Exception _e :error/unreadable))
                   (= op-list ":BAD-INPUT")   :bad-input
                   :else                      :error/uninterpretable)
        ;; ToDo: Investigate this bug.
        err (if (= err " )") nil err)]
    {:form form
     :std-out std
     :err-out err}))

;;;=================================================== Dynamic planner ===========================================================
;;; ToDo:
;;;   - Use a DB for managing stack/navigation???
;;;      cons: - literal structures, yuk
;;;            - distracting from the development goal, at least early on.
;;;      pros: - potentially better manipulation for backtracking and reordering priorities as costs change.
;;;            - leverage existing structure in shop.clj.
;;;            - persistent! (useful for debugging and saving user's state).
(def ^:diag domain (-> "data/planning-domains/process-interview-1.edn" slurp edn/read-string))

(defn head-of    [elem] (or (:method/head elem)(:operator/head elem) (:axiom/head elem)))

(defn axiom? [elem]
  (when (contains? elem :axiom/head) elem))

(defn method? [elem]
  (when (contains? elem :method/head) elem))

(defn operator? [elem]
  (when (contains? elem :operator/head) elem))

(defn matching-task?
  "Return a map of :elem and :bindings if the elem's head unifies with the argument literal."
  [lit elem]
  (when-let [bindings (uni/unify lit (head-of elem))]
    {:bindings bindings
     :elem elem}))

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
   Examples: (consistent-bindings '[{:a 1 :b 2} {:a 1 :c 3}]) ==> {:a1 :b2 :c3}
             (consistent-bindings '[{:a 1 :b 2} {:b 3}]) ==> nil."
  [bindings]
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
       (catch Exception _e nil)))

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
                                      (if-let [bindings (mapv #(satisfied? % state) (:method/preconds rhs))]
                                        (if-let [bindings (and (every? identity bindings) (consistent-bindings? bindings))]
                                          (conj res {:bindings bindings :task (-> {:method/head (:method/head task)}
                                                                                  (assoc :method/rhs (:method/task-list rhs)))})
                                          res)
                                        res))
                                    []
                                    (:method/rhsides task))))

;;; Similar in operator.clj
;;; ToDo: The idea of Skolem's in the add-list needs thought. Is there need for a universal fact?
;;;       For the time being, I'm just adding uniquely
(defn add-del-facts
  "Update the facts by adding, deleting or resetting to empty.
     facts - a set of ::spec/positive-proposition.
     a map - with keys :add and :delete being collections of :specs/positive-proposition.
             These need not be ground propositions; everything that unifies with a form in delete will be deleted.
             A variable in the add list will be treated as a skolem.
             reset? is truthy."
  [facts a-list d-list]
  (assert set? facts)
  (assert (every? #(s/valid? ::spec/positive-proposition %) facts))
  (as-> facts ?f
    (if d-list
      (->> ?f (remove (fn [fact] (some #(uni/unify fact %) d-list))) set)
      ?f)
    (if a-list (into ?f a-list) ?f)))

;;; Similar is in operator.clj
(defn update-state-by-op-actions
  "Update the project's DB, specifically :project/state-string with infromation from last response and operator a-list and d-list.
      - plan-step   - a map such as {:operator :!yes-no-process-steps, :args [aluminium-foil]},
      - proj-id     - the keyword identifying a project by its :project/id.
      - domain-id   - a keyword identifying the domain, for example, :process-interview.
   Returns the new state."
  [old-state op-obj bindings]
  (let [a-list (mapv #(uni/subst % bindings) (:operator/a-list op-obj))
        d-list (mapv #(uni/subst % bindings) (:operator/d-list op-obj))]
    (add-del-facts old-state a-list d-list)))

(defn run-operator
  "Run the action associated with the operator, returning an updated state."
  [op-head state]
  (log/info "Running operator" op-head)
  state)

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

   stasks (satisfying-elements) is a map containing the following keys:
      :bindings  - is a map of variable bindings,
      :task      - is information from the operator or method RHS satisfying RHS the preconditions."
  [partials stasks]
  (let [part (first partials)
        {:keys [task bindings]} (first stasks)] ; <======================== Every, not just first. (Probably just a reduce over this?)
    (reset! diag stasks)
    (cond (empty? stasks)    (-> partials rest vec) ; No way forward from this navigation. The partial can be removed.

          ;; Execute the operator. If it succeeds, update state, new-tasks, and plan. If it fails, [remove this partial???]
          (operator? task)   (let [op-head (-> task :operator/head (uni/subst bindings))
                                    new-partial (try (-> part
                                                         (update :state #(run-operator op-head %))
                                                         (update :state #(update-state-by-op-actions % task bindings))
                                                         (update :new-tasks #(-> % rest vec))
                                                         (update :plan #(conj % op-head))
                                                         vector)
                                                     (catch Exception _e []))]
                                (into new-partial (rest partials)))

          ;; Update the task list with the tasks from the RHS
          (method? task)     (let [new-partial (update part :new-tasks #(into (-> % rest vec) ; drop this task; add the steps
                                                                              (map (fn [t] (uni/subst t bindings)) (:method/rhs task))))]
                               (into [new-partial] (rest partials))))))

;;; Is there is a problem here?
;;; The operator code updates :new-tasks, removing the one it executed.
;;; But how does this mesh with plan9? I think plan9 needs to know whether the head task is a method or operator.
;;; The stuff I'm doing about satisfying-tasks is about navigating next states, but when task is an operator, you just do it.
;;; +I think this means update-planning gets called earlier with operators+ NO.
;;;  I think this means that operator must preserve knowledge of what vars are bound.

;;; (plan/plan9 domain)
(defn ^:diag plan9
  "A dynamic HTN planner.
   Operates on a stack (vector) of 'partials' (described in the docstring of update-planning).
   Initialize the stack to a partial from a problem definition.
   Iterates a process of looking for tasks that satisfy the head new-task, replacing it and running operations."
  [{:domain/keys [elems problem]}]
  (let [state   (:problem/state problem) ; A vector of ground literals.
        goal    (:problem/goal problem)] ; A literal.
    (loop [partials [{:plan [goal] :new-tasks [goal] :state state}]
           cnt 1]
      (log/info "new-tasks =" (-> partials first :new-tasks))
      (let [task (-> partials first :new-tasks first) ; <===== The task might have bindings; This needs to be fixed. (Need to know the var that is bound).
            matching-tasks   (filter #(matching-task? task %) elems)
            satisfying-tasks (reduce (fn [res task]
                                       (if-let [r (satisfying-elems task state)]
                                         (into res r)
                                         res))
                                     []
                                     matching-tasks)]
        (cond
          (-> partials first :new-tasks empty?)     {:state :success :plan (first partials)}
          (empty? partials)                         {:state :failure}
          (> cnt 10)                                {:state :stopped :partials partials}
          :else                                     (recur (update-planning partials satisfying-tasks)
                                                           (inc cnt)))))))

;;;=================================================== End Dynamic planner =======================================================

(defn plan
  "Communicate to shop3 whatever is specified in the arguments, which may include one or more of:
      1) :domain  - a domain in shop format (an s-expression).
      2) :problem - providing a new problem, and
      3) :execute - finding plans for a problem, etc. (See the docs.)
   Return result of execution if (3) is provided, otherwise result from (2) or (1)."
  [{:keys [domain problem execute verbose?] :or {verbose? true} :as _diag}]
  (zmq-ctx/with-new-context
    (let [socket (zmq-sock/socket :req {:connect planner-endpoint})]
      (letfn [(wrap-send [data task]
                (try  (zmq/send-msg socket (str data))
                      (catch Exception e (log/info "plan: zmq exception or send."
                                                   {:task task :msg (.getMessage e)}))))
              (wrap-recv [task]
                (try (let [res (zmq/receive-msg socket {:stringify true})
                           ;; This goes away...but really shouldn't call this except for :execute (and a planning problem).
                           {:keys [form std-out err-out]} (-> res first parse-planner-result)]
                       (when verbose?
                         (when std-out (log/info "Planner std-out:" std-out))
                         (when err-out (log/warn "Planner error-out:" err-out)))
                       form)
                     (catch Exception e (log/info "plan: zmq exception or recv."
                                                  {:task task :msg (.getMessage e)}))))]
        (when domain
          (wrap-send domain :define-domain)
          (wrap-recv        :define-domain))
        (when problem
          (wrap-send problem :define-problem)
          (wrap-recv         :define-problem))
        (when execute
          (wrap-send execute :execute)
          (wrap-recv         :execute))))))

;;; (load-domain "data/planning-domains/process-interview.edn")
(defn ^:diag load-domain
  "Load a planning domain into the database."
  [path & {:keys [_force?] :or {_force? true}}] ; ToDo: check if exists.
  (if-let [conn (connect-atm :planning-domains)]
    (->> path
         slurp
         edn/read-string
         shop/proj2canon
         shop/canon2db
         vector
         (d/transact conn))
    (log/warn "Not loading domain:" path "planning-domains DB does not exist.")))

(defn domain-eid
  "Return the argument domain-name's entity ID."
  [domain-id]
  (d/q '[:find ?eid .
         :in $ ?dname
         :where [?eid :domain/id ?dname]]
       @(connect-atm :planning-domains)
       domain-id))

(defn get-domain ; ToDo: This goes away with SHOP.
  "Return the domain in SHOP format."
  [domain-id & {:keys [form] :or {form :proj}}]
  (let [db-obj (if-let [eid (domain-eid domain-id)]
                 (resolve-db-id {:db/id eid} (connect-atm :planning-domains))
                 (log/error "Domain" domain-id "not found."))]
    (case form
      :db     db-obj
      :proj   (shop/db2proj db-obj)
      :shop   (shop/db2shop db-obj)
      :canon  (shop/db2canon db-obj))))

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

(defn satisfies-facts?
  "Return true if every condition, (a atomic predicate or one negated by (not <predicate>),
   the things in :method/preconds and :operator/preconds, unifies with the argument facts."
  [conditions facts]
  (every? #(condition-satisfies? % facts) conditions))

(defn prune-operators
  "The argument is a vector of planning domain elements (axioms, methods, operators).
   Return a vector with operators that do not satisfy the facts removed."
  [elems facts]
  (->> elems
       (remove #(and (contains? % :operator/head)
                     (not (satisfies-facts? (:operator/preconds %) facts))))
       vec))

(defn prune-method-rhsides
  "Check the :method/preconds of the RHS against the argument facts and if
   they do not all unify, remove this RHS.
   Returns the filterv vector of this test."
  [elems facts]
  (->> elems
       (mapv (fn [elem]
               (if-not (contains? elem :method/head) ; It is an operator or axiom; don't touch it.
                 elem
                 (update elem :method/rhsides
                         (fn [rhsides]
                           (filterv (fn [rhs] (satisfies-facts? (:method/preconds rhs) facts)) rhsides))))))
       ;; If there are no qualifying rhsides, remove the methode entirely.
       (filterv #(if-not (contains? % :method/head)
                   true
                   (-> % :method/rhsides not-empty)))))

(defn prune-domain
  "Return the domain with some operators and methods RHS removed.
   What is removed are those element for which none of the argument facts unify.
   The facts argument is a collection of atoms in the logic sense, that is,
   flat s-expressions representing propositions."
  [domain facts]
  (-> domain
      (update :domain/elems #(prune-operators % facts))
      (update :domain/elems #(prune-method-rhsides % facts))))

;;; ToDo: This can go away with shop.
(defn translate-plan
  "Translate plans from SHOP format, which is upper-case symbols with cost, to lowercase symbols.
  '((!DROP BANJO) 1.0 (!PICKUP KIWI) 1.0)) --> [{:cost 1.0, :operator :!drop, :args [banjo]}
                                                {:cost 1.0, :operator :!pickup, :args [kiwi]}]."
  [plan]
  (when-not (seq? plan) (throw (ex-info "Invalid plan" {:plan plan})))
  (letfn [(plan-obj [plan] ; (((!DROP BANJO) 1.0 (!PICKUP KIWI) 1.0))) --> [{:operator :!drop, :args [banjo] :cost 1.0},...]."
            (let [cnt (atom 1)
                  res (atom '())]
              (doseq [form plan]
                (if (odd? @cnt)
                  (swap! res conj {:op form})
                  (swap! res #(conj (rest %) (assoc (first %) :cost form))))
                (swap! cnt inc))
              (-> res deref reverse vec)))]
    (let [plan (plan-obj plan)]
      (s/assert ::spec/shop-obj-plan plan)
      (mapv (fn [step]
              (let[{:keys [op]} step
                   [oper & args] op]
                (-> step
                    (dissoc :op)
                    (assoc :operator (->> oper name str/lower-case keyword))
                    (assoc :args (mapv (fn [arg] (if (symbol? arg) (-> arg name str/lower-case symbol) arg))
                                       args)))))
            plan))))

(defn execute-plan!
  "Execute the operators of the plan, producing side-effects such as asking the user
   and fact-updates (a map of :add and :delete vectors of propositions).
   Returns a vector ::spec/state-edits object that is the effect of running the plan." ; <======================== Post-SHOP this is good. Put it in the DB. It currently isn't the case.
  [pid client-id state plan domain-id]
  (let [plan (translate-plan plan)]
    (log/info "execute-plan!: pid =" pid "plan =" plan)
    (doseq [plan-step plan] ; after translate-plan plan-steps have :operator :args and :cost.
      (s/assert ::spec/plan-step plan-step)
      (log/info "run plan-step" plan-step)
      (op/operator-meth
       {:plan-step plan-step
        :domain-id domain-id ; This is needed for a-list/d-list work.
        :tag (:operator plan-step)
        :pid pid
        :client-id client-id
        :state state}))
    (db/get-state pid)))

(defn discussion-stopped?
  "Return true if the state vector contains the discussion-stopped fact."
  [state]
  (some #(= 'discussion-stopped (first %)) state))

;;; -------------------------------------- Plan checking --------------------------------------------------------------------
(defn check-domain-to-goals
  "Return nil if domain references the problem otherwise :error-domain-not-addressing-goal.
   This doesn't check whether the problem can be inferred by means of axioms."  ; ToDo: Fix this when SHOP goes away.
  [domain state-vec goal-vec]
  (let [method-heads (->> domain :domain/elems (filter #(contains? % :method/head)) (mapv :method/head))]
    (doseq [g goal-vec]
      (or (some #(uni/unify g %) method-heads)
          (some #(uni/unify g %) state-vec)
          (throw (ex-info "Goal unifies with neither method-heads nor state-vec."
                          {:goal g :method-heads method-heads :state-vec state-vec}))))))

#_(defn check-goals-are-ground ; ToDo: Goes away with SHOP?
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

(defn translate-triple
  "Translate the domain, problem, and execute (not really) statements from proj format to SHOP format."
  [triple state-vec]
  (reset! diag
          (-> triple
              (update :domain shop/proj2shop)
              (update :problem #(shop/problem2shop % state-vec)))))

;;; -------------------------------------- interview loop --------------------------------------------------------------------
;;; (interview-loop "pi")
(defn interview-loop
  "Run a conversation with the users. Specifically,
     (1) Create a proj-format planning domain by applying prune-domain against start-facts and the named domain (domain-name).
     (2) Run the :domain/problem & :domain/execute to produce one or more plans. (For now take the first plan computed.)
     (3) Iteratively run the steps of the plan (operators), each of which can query the user through the UI and modify the state-edits map.
         For example, running a plan operator may involve asking the user a question, an analysis of which adds propositions to the
         :add set of the state-edits map.
     (4) The state-edits resulting from this plan is then used by prune-domain to produce a NEW planning domain.
         Iteratively such new planning domains are processed through steps 1-3. This continues until no more plans can be generated.

     - pid         : a keyword, the :project/id of the project.
     - domain-id   : a keyword, the :domain/id of the domain.
     - start-facts : a vector of seqs describing grounded propositions.
     - problem     : a SHOP problem in proj syntax.

   The motivation for interatively running steps 1-3 (rather than running to completion in a more comprehensive planning domain)
   is that planning is required to be dynamic and on-line; what is learned through asynchronous discussion (a collection of propositions)
   determines how the planning domain can be pruned. By this means, 'effective planning domains' are produce which are much simpler and
   of smaller scope than the all-encompassing 'canonical plan' stored in the planning domains DB.

   Conceptually, interview-loop is a long-running process; it could run for months.
   Implementationally, it can be stopped and restarted from project DB data, which captures the substance of the entire discussion to date.
   Particularly, the DB content captures a state vector which serves as start-facts in restarting the interview-loop after 'hibernation'.

   'triple' below refers to the map containing :domain :problem and :execute in either proj or SHOP form.
   The function returns the state achieved, a collection of :specs/proposition."
  [pid domain-id client-id & {:keys [start-facts problem limit]
                              :or {start-facts '#{(characterize-process new-proj)},
                                   problem (-> domain-id (get-domain {:form :proj}) :domain/problem)
                                   limit 2}}] ; ToDo: Temporary
  (s/assert ::spec/domain-problem problem)
  (let [proj-domain (-> domain-id (get-domain {:form :proj}))
        execute (:domain/execute proj-domain)] ; The execute statement is invariant to state changes.
    (loop [state   start-facts
           problem (-> problem (assoc :problem/state-string (str start-facts)))
           pruned  (prune-domain proj-domain start-facts)
           plans   (-> {:domain pruned :problem problem :execute execute} check-triple (translate-triple state) plan)
           cnt 1]
      (log/info "state = " state)
      (cond (= plans :bad-input)            :bad-input
            (>= cnt limit)                  (sort-by first state)
            (empty? plans)                  (sort-by first state)
            (discussion-stopped? state)     (sort-by first state)
            :else  (let [new-state   (execute-plan! pid client-id state (first plans) domain-id) ; ToDo: Deal with multiple plans.
                         new-problem (assoc problem :problem/state-string (str new-state))
                         new-pruned  (prune-domain proj-domain new-state)]
                     (log/info "new-state = " new-state)
                     (recur new-state
                            new-problem
                            new-pruned
                            (-> {:domain new-pruned :problem new-problem :execute execute} check-triple (translate-triple new-state) plan)
                            (inc cnt)))))))

;;; Section 5.6 Debugging Suggestions
;;; When you have a problem that does not solve as expected, the following general recipe may help you home in on bugs in your domain definition:
;;;
;;; 1. Start by doing (SHOP-TRACE :TASKS) and then try FIND-PLANS again.
;;; 2. In many cases, the domain will be written so that there will be little or no backtracking.
;;;    In this case, examine the output of the traced call to FIND-PLANS and look for the first backtracking point.
;;; 3. The above process should help you identify a particular task, either a primitive or a complex task, as a likely problem spot.
;;;    If it’s a primitive task, the next step is to examine the operator definition. If it’s a complex task, you should
;;;    check the method definitions. If you have any trouble identifying which method definition is relevant,
;;;    you can use (SHOP-TRACE :METHODS) to further focus your attention.
;;; 4. If visual inspection of method and operator definitions does not reveal the problem, you most likely have problems
;;;    with precondition expressions. In this case, try using (SHOP-TRACE :GOALS), rerunning FIND-PLANS and check to see
;;;    what’s happened when your problem method or operator’s preconditions are checked.
;;;
;;; This recipe has proven effective for finding the vast majority of bugs in SHOP2 domains.
(defn ^:diag shop-trace
  "Set tracing to :tasks (default) or :methods :goals"
  ([]     (plan {:execute (list 'shop-trace)}))
  ([what]
   (let [choices #{:tasks :methods :goals}]
     (if (choices what)
       (plan {:execute (list 'shop-trace what)})
       (log/info "shop-trace takes one of :tasks :methods :goals (or no args to see what is being traced).")))))

(defn ^:diag shop-untrace
  "Turn of shop tracing."
  []
  (plan {:execute '(shop-untrace)}))

;;; ========================= Starting, stopping, and testing ===================================
;;; ToDo: Currently shop-planner.lisp returns a string of the content wrapped in parenthesis,
;;;       and I haven't looked into why that is. Also BREAK-HERE could be more obscure.

;;;(plan kiwi-example) ==> ["(((!DROP BANJO) 1.0 (!PICKUP KIWI) 1.0))"]
(def kiwi-example
  "This is an example from usage from the SHOP2 documentation. It is used to test communication with the planner."
  '{:domain (defdomain kiwi-example
              ((:method (swap ?x ?y)
                        just-one-way
                        ((have ?x))
                        ((!drop ?x) (!pickup ?y))
                        ((have ?y))
                        ((!drop ?y) (!pickup ?x)))
               (:operator (!pickup ?a) () () ((have ?a)))
               (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())))
    :problem (defproblem problem1 kiwi-example
               ((have banjo))         ; This is state data.
               ((swap banjo kiwi)))   ; This is a method.
   :execute (find-plans 'problem1 :verbose :plans)
    :answer (((!DROP BANJO) 1.0 (!PICKUP KIWI) 1.0))})

(defn test-the-planner
  "Define a planning domain. Define a problem. Find plans for the problem.
   Check that the plan matches what is expected."
  []
  (log/info "test-planner...")
  (let [{:keys [answer]} kiwi-example
        result (plan kiwi-example)]
    (cond (= result :timeout)                  (log/error "Planning test timeout")
          (= result :planning-failure)         (log/error "Planning exception")
          (= answer result)                    (log/info  "Planner passes test.")
          :else                                (log/error "Planner fails test:" result))
    result))

;;; Run the following when you update interview-for-new-project!:
;;;  (ws/register-ws-dispatch :resume-conversation plan/resume-conversation)
(defn resume-conversation
  "Start the interview loop. :resume-conversation is a dispatch key from client.
   This is called even for where PID is :START-A-NEW-PROJECT."
  [{:keys [project-id client-id]}]
  (log/info "Calling interview-loop for project" project-id)
  (interview-loop project-id :process-interview client-id {:start-facts (db/get-state project-id)}))

;;; This makes recompilation smoother.
(def test-the-planner? false)

;;; From a shell: ./pzmq-shop3-2024-02-15 --non-interactive --disable-debugger --eval '(in-package :shop3-zmq)' --eval '(setf *endpoint* 31888)'
(defn init-planner!
  "Start the planner. This relies on environment variable PLANNER_SBCL, which is just
   the name of the SBCL core file. That file is expected to be in the project directory."
  []
  (load-domain "data/planning-domains/process-interview.edn")
  (ws/register-ws-dispatch :resume-conversation resume-conversation)
  (when test-the-planner?
    (try
      (let [cmd-arg (cl-format nil "'(setf shop3-zmq::*endpoint* ~S)'" planner-endpoint)]
        ;; --eval does not work!
        (future (sh planner-executable "--non-interactive" "--disable-debugger" "--eval"  cmd-arg)))
      (catch Exception e
        (log/error (:message e))
        (throw (ex-info "Running planner didn't work." {:error e}))))
    (Thread/sleep 2000)
    (test-the-planner)))

(defn quit-planner!
  "Quit the planner. It can be restarted with a shell command through mount."
  []
  (let [fut (future
              (zmq-ctx/with-new-context
                (let [socket (zmq-sock/socket :req {:connect planner-endpoint})]
                  (zmq/send-msg socket "(sb-ext:exit)")
                  (zmq/receive-msg socket {:stringify true}))))]
    (when test-the-planner?
      (if (= [":bye!"] (deref fut 3000 :timeout))
        (do (log/info "Planner terminated as requested.") :stopped)
        (do (log/warn "Planner state unknown. (It may not have been running.)") :unknown)))))

(defstate plan-server
  :start (init-planner!)
  :stop  (quit-planner!))
