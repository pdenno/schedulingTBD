(ns scheduling-tbd.planner
  "This provides functions to prune a planning domain and run an interview."
  (:require
   [clojure.core.unify        :as uni]
   [clojure.datafy            :refer [datafy]]
   [clojure.edn               :as edn]
   [datahike.api              :as d]
   [mount.core                :as mount :refer [defstate]]
   [scheduling-tbd.db         :as db]
   [scheduling-tbd.op-utils   :as ou]
   [scheduling-tbd.sutil      :as sutil :refer [connect-atm chat-status find-fact domain-conversation]]
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
;;; ToDo: This should handle axioms too.
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

(defn matching-task?
  "Return a map of :elem and :bindings if the elem's head unifies with the argument literal."
  [lit task]
  (when-let [bindings (uni/unify lit (head-of task))]
    {:bindings bindings
     :task task}))

(defn matching-tasks
  "Return a vector of tasks that unify."
  [task domain-elems]
  (reduce (fn [res pat] (if-let [m (matching-task? task pat)] (conj res m) res))
          []
          domain-elems))

;;; You will need to walk stop-here to pick the one you want, but having done that, do this:
;;; (plan/satisfying-tasks (:task @plan/diag) (:elems @plan/diag) (:state @plan/diag))
(defn satisfying-tasks
  "Return edited task patterns for operators and methods matching given, task (a positive literal), patterns, and state."
  [task patterns state m-tasks]
  (reduce (fn [res {:keys [task bindings]}]
            (if-let [r (satisfying-elems task state)]
              (into res (map (fn [sat-elem] (update sat-elem :bindings #(merge bindings %))) r))
              res))
          []
          m-tasks))

(defn extend-partials
  "Update the partials vector by replacing the head partial with a vector of partials resulting from
   an extension (to a possible next plan task) for each task in s-tasks."
  [partials s-tasks]
  (let [head-partial (first partials)]
    (into (mapv (fn [{:keys [bindings task]}]
                  (let [rhs (:method/rhs task)]
                    (update head-partial :path-tasks (fn [tasks] (into (mapv #(uni/subst % bindings) rhs) (rest tasks))))))
                s-tasks)
          (rest partials))))

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
      :plan        - A vector describing what plan steps have been executed, that is, the current traversal of the domain for this partial plan.
                     Its first element in the goal, all others are (ground ?) operator heads.
      :path-tasks  - Maps of the current active edges, about to be traversed (method) or acted upon (operators).
                     Each map has :pred and :bindings from
                     When path-tasks is empty, the plan has been completed.

   s-tasks (satisfying-elements) is a map containing the following keys:
      :bindings  - is a map of variable bindings,
      :task      - is information from the operator or method RHS satisfying RHS the preconditions."
  [partials s-tasks {:keys [pid] :as opts}]
  (let [part                    (first partials)
        {:keys [task bindings]} (first s-tasks)] ; <======================== ToDo: Every, not just first. (Probably just a reduce over this?)
    (cond (empty? s-tasks)   (do
                               (log/info "Navigation fails:" part)
                               (-> partials rest vec)) ; No way forward from this navigation. The partial can be removed.

          ;; Execute the operator. If it succeeds, update state, path-tasks, and plan.
          (operator? task)   (try (let [_ (ou/run-operator! task opts)
                                        _ (ou/operator-update-state! pid task bindings)
                                        op-head (-> task :operator/head (uni/subst bindings))
                                        new-partial (-> part
                                                        (update :path-tasks #(-> % rest vec))
                                                        (update :plan #(conj % op-head)))]
                                    (into [new-partial] (rest partials)))
                                  (catch Exception e ; Drop this path  ToDo: Also need to unwind state.
                                    (into [(assoc part :error e)] (rest partials))))

          ;; Update the task list with the tasks from the RHS
          (method? task)   (extend-partials partials s-tasks))))

(def stop-here-diag (atom nil))
(defn stop-here [data]
  (reset! stop-here-diag data))

;;; (plan/plan9 :process, '#{(proj-id sur-fountain-pens) (proj-name "SUR Fountain Pens") (surrogate sur-fountain-pens)}, '[(characterize-process sur-fountain-pens)],
;;;  {:client-id "a0e1e948-492c-4860-bede-b8b50cbe0275", :pid :sur-fountain-pens, :conv-id :process, :surrogate? '(surrogate sur-fountain-pens)})

(defn plan9
  "A dynamic HTN planner.
   Operates on a stack (vector) of 'partials' (partial plans, described in the docstring of update-planning).
   Initialize the stack to a partial from a problem definition.
   Iterates a process of looking for tasks that satisfy the head new-task, replacing it and running operations.
   domain-id is a conv-id.

   Note that the planning state is not passed around. Instead, ou/run-operator! and ou/operator-update-state update
   it in the project's DB."
  [goals & {:keys [pid domain-id] :as opts}]  ; shop2? is about running the planner like SHOP2,...
  (reset! diag {:domain-id domain-id :goals goals :opts opts})
  (assert (#{:process :resource :data} (domain-conversation domain-id)))
  (let [elems  (-> (sutil/get-domain domain-id) :domain/elems)]
    (loop [partials [{:plan [] :path-tasks [(first goals)]}] ; ToDo: Only planning first of goals so far.
           cnt 1]
      ;;(log/info "plan =" (-> partials first :plan))
      ;;(log/info "path-tasks =" (-> partials first :path-tasks))
      (let [state (db/get-planning-state pid)
            task (-> partials first :path-tasks first) ; <===== ToDo: Might want bindings on task???
            matching (matching-tasks task elems)
            s-tasks (satisfying-tasks task elems state matching)]
        ;;(log/info "task =" task)
        ;;(log/info "state =" state)
        ;;(log/info "matching =" matching)
        ;;(log/info "satisfying =" s-tasks)
        (when (empty? s-tasks) (stop-here {:task task :partials partials :elems elems}))
        (cond
          (empty? partials)                         {:result :failure :reason :no-successful-plans}
          (-> partials first :path-tasks empty?)    {:result :success :plan-info (first partials)}
          (> cnt 50)                                {:result :stopped :partials partials}
          :else  (let [partials (update-planning partials s-tasks opts)
                       partials (if-let [err (-> partials first :error datafy)]
                                  (let [next-plan (-> partials rest vec)]
                                    ;;(log/error "Plan execution error: cause =" (:cause err) ", data =" (:data err))
                                    (reset! diag {:err err :partials partials :s-tasks s-tasks :opts opts})
                                    (if (empty? next-plan)
                                      (throw (ex-info "No plans remain." {}))
                                      next-plan))
                                  partials)]
                   (recur partials
                          (inc cnt))))))))

;;; -------------------------------------- Plan checking --------------------------------------------------------------------
;;; (what-is-runnable? '(characterize-process sur-fountain-pens) '#{(proj-id sur-fountain-pens) (proj-name "SUR Fountain Pens") (surrogate sur-fountain-pens)})
(defn ^:diag  what-is-runnable?
  "Return the satisfying tasks for a given state (and planning domain)."
  [task pid domain-id]
  (let [elems (-> (sutil/get-domain domain-id) :domain/elems)
        matching (matching-tasks task elems)
        state (db/get-planning-state pid)
        satisfying (satisfying-tasks task elems state matching)]
    (log/info "matching =" matching)
    (log/info "satisfying =" satisfying)
    satisfying))

;;; ToDo: Need to rework project/problem. Maybe the DB can define the problem, but
;;;       I think the way it is doing it now is going to require too much maintenance.
(defn form-goals
  "'problem' is a SHOP2-like problem structure; these are comprised of a state and a vector of goals.
   Return a map with :goals and :state set."
  [pid conv-id]
  (let [state (db/get-planning-state pid)]
    (case conv-id
      :process   (let [pid (-> (find-fact '(proj-id ?p) state) second)]
                   [(list 'characterize-process pid)])
      :data      (->> (filter #(= (first %) 'uploaded-table) state)
                      (mapv #(list 'characterize-table (second %))))
      :resource (->> (filter #(= (first %) 'resource) state)
                     (mapv #(list 'characterize-resource (second %)))))))

(defn plan9-post-actions
  "ws/send-to-chat depending on how planning went."
  [result client-id pid conv-id]
  (log/info "plan9-post-action: result =" result)
  (let [response-to-user (case (:result result)
                           :success (chat-status "That's all the conversation we do right now.")
                           :stopped (chat-status "We stopped intentionally after 50 interactions.")
                           :failure (chat-status (str "We stopped owing to " (:reason result)))
                           nil)]
    ;; ToDo: Even this isn't sufficent at times!
    (when response-to-user
      (ws/send-to-chat {:dispatch-key :tbd-says :promise? false, :client-id client-id, :msg response-to-user}))))

;;; (plan/resume-conversation {:pid :sur-craft-beer :client-id (ws/recent-client!) :conv-id :data})
;;; (plan/resume-conversation {:pid :sur-fountain-pens :conv-id :data :client-id (ws/recent-client!)})
(defn resume-conversation
  "Start the interview loop. :resume-conversation-plan is a dispatch key from client.
   This is called even for where PID is :START-A-NEW-PROJECT."
  [{:keys [client-id pid conv-id] :as args}]
  (assert (string? client-id))
  (try
    (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
    (if (= pid :START-A-NEW-PROJECT)
      ;; -------------------- New project
      (let [goal '(characterize-process START-A-NEW-PROJECT)
            res (plan9 [goal] {:domain-id :process :client-id client-id :pid pid :conv-id conv-id})]
        (plan9-post-actions res client-id pid conv-id))
      ;; -------------------- Typical
      (let [conv-id (or conv-id
                        (d/q '[:find ?conv-id . :where [_ :project/current-conversation ?conv-id]] @(connect-atm pid)))
            state (db/get-planning-state pid)
            surrogate? (find-fact '(surrogate ?x) state)
            goals (form-goals pid conv-id)
            args (assoc args :conv-id conv-id)]
        (log/info "======== resume conversation: planning-state = " state)
        (db/change-conversation args)
        (let [res (plan9 goals {:domain-id conv-id :client-id client-id :pid pid :conv-id conv-id :surrogate? surrogate?})]
          (plan9-post-actions res client-id pid conv-id))))
    (finally
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

(defn init-planner!
  []
  (ws/register-ws-dispatch :resume-conversation-plan resume-conversation))

(defn quit-planner!
  "Quit the planner. It can be restarted with a shell command through mount."
  [])

(defstate planning
  :start (init-planner!)
  :stop  (quit-planner!))
