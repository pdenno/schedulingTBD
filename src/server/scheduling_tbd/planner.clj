(ns scheduling-tbd.planner
  "This provides functions to prune a planning domain and run an interview."
  (:require
   [clojure.edn          :as edn]
   [clojure.java.shell   :refer [sh]]
   [clojure.pprint       :refer [cl-format]]
   [clojure.spec.alpha       :as s]
   [clojure.string           :as str]
   [clojure.core.unify       :as uni]
   [datahike.api             :as d]
   ;;[explainlib.core        :as exp]
   [ezzmq.message            :as zmq]
   [ezzmq.context            :as zmq-ctx]
   [ezzmq.socket             :as zmq-sock]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.operators :as op]
   [scheduling-tbd.shop      :as shop]
   [scheduling-tbd.specs     :as specs]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [taoensso.timbre          :as log]))

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
  (let [[result std err] (str/split s #"BREAK-HERE")
        [success result] (re-matches #"^\((.*)$" result)
        form (if success
               (try (edn/read-string result) (catch Exception _e :error/unreadable))
               :error/uninterpretable)
        ;; ToDo: Investigate this bug.
        err (if (= err " )") nil err)]
    {:form form
     :std-out std
     :err-out err}))

(defn plan
  "Communicate to shop3 whatever is specified in the arguments, which may include one or more of:
      1) :domain  - a domain in shop format (an s-expression).
      2) :problem - providing a new problem, and
      3) :execute - finding plans for a problem, etc. (See the docs.)
   Return result of execution if (3) is provided, otherwise result from (2) or (1)."
  [{:keys [domain problem execute verbose?] :or {verbose? true}}]
  (zmq-ctx/with-new-context
    (let [socket (zmq-sock/socket :req {:connect planner-endpoint})]
      (letfn [(wrap-send [data task]
                (try  (zmq/send-msg socket (str data))
                      (catch Exception e (log/info "plan: zmq exception or send."
                                                   {:task task :msg (.getMessage e)}))))
              (wrap-recv [task]
                (try (let [res (zmq/receive-msg socket {:stringify true})
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
  [domain-name]
  (d/q '[:find ?eid .
         :in $ ?dname
         :where [?eid :domain/ename ?dname]]
       @(connect-atm :planning-domains)
       domain-name))

(defn get-domain
  "Return the domain in SHOP format."
  [domain-name & {:keys [form] :or {form :proj}}]
  (let [db-obj (if-let [eid (domain-eid domain-name)]
                 (resolve-db-id {:db/id eid} (connect-atm :planning-domains))
                 (log/error "Domain" domain-name "not found."))]
    (case form
      :db     db-obj
      :proj   (shop/db2proj db-obj)
      :shop   (shop/db2shop db-obj)
      :canon  (shop/db2canon db-obj))))

(defn condition-satisfies?
  "Return true if the positive condition argument unifies with any fact,
   OR if the negative condition argument unifies with no fact.
   Facts are always positive literals."
  [condition facts]
  (s/assert ::specs/proposition condition)
  (if (s/valid? ::specs/negated-proposition condition)
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

;;; ToDo: The idea of Skolem's in the add-list needs thought. Is there need for a universal fact?
;;;       For the time being, I'm just adding uniquely
(defn update-facts
  "Update the facts by adding, deleting or resetting to empty.
     facts - a set of ::specs/positive-proposition.
     a map - with keys :add and :delete being collections of :specs/positive-proposition.
             These need not be ground propositions; everything that unifies with a
             form in delete will be deleted.
             A variable in the add list will be treated as a skolem.
             reset? is truthy."
  [facts {:keys [add delete reset?]}]
  (assert set? facts)
  (assert (every? #(s/valid? ::specs/positive-proposition %) facts))
  (as-> (if reset? #{} facts) ?f
    (if delete
      (->> ?f (remove (fn [fact] (some #(uni/unify fact %) delete))) set)
      ?f)
    (if add (into ?f add) ?f)))

(defn translate-plans
  "Translate plans from SHOP format, which is upper-case symbols with cost,
   to lowercase symbols. Additionally when regarding the operation name,
   the resulting symbol is ns-qualified in the operators namespace."
  [plans]
  (->> plans
       (map first)
       (mapv #(let[[op & args] %]
                (-> {}
                    (assoc :operator (->> op name str/lower-case keyword))
                    (assoc :args (mapv (fn [arg] (if (symbol? arg) (-> arg name str/lower-case symbol) arg))
                                       args)))))))

;;; ToDo: The operators called from execute-plan are typically long-running.
;;;       There is need, therefore, for means to deal with this (IN THE DB, I think).
;;;       Specifically, there is the need for "restart", in the sense that we can kill
;;;       the app and restart it with the same "last question" and unachieved operators
;;;       awaiting responses.
(defn execute-plan
  "Execute the operators of the plan, producing side-effects such as asking the user
   and fact-updates (a map of :add and :delete vectors of propositions).
   Returns a vector ::specs/state-edits object that is the effect of running the plan."
  [plans facts]
  (let [plan (translate-plans plans)]
    (log/info "execute-plan: plan =" plan)
    (reduce (fn [res plan-step]
              (s/assert ::specs/plan-step plan-step)
              (conj res (op/run-op plan-step facts res)))
            [{:from :start :add #{} :delete #{} }] ; The form of an empty ::specs/state-edits.
            plan)))

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

   The motivation for interatively running steps 1-3 (rather than running to completion in a more comprehensive planning domain)
   is that planning is required to be dynamic and on-line; what is learned through asynchronous discussion (a collection of propositions)
   determines how the planning domain can be pruned. By this means, 'effective planning domains' are produce which are much simpler and
   of smaller scope than the all-encompassing 'canonical plan' stored in the planning domains DB.

   Conceptually, interview-loop is a long-running process; it could run for months.
   Implementationally, it can be stopped and restarted from project DB data, which captures the substance of the entire discussion to date.
   Particularly, the DB content captures a state vector which serves as start-facts in restarting the interview-loop after 'hibernation'.

   FWIW, the function returns the state achieved, a collection of :specs/proposition."
  [domain-name & {:keys [start-facts problem limit]
                  :or {start-facts #{}, limit 3}}]
  (s/assert ::specs/domain-problem problem)
  (let [canon-proj (-> domain-name (get-domain {:form :proj}))
        execute (:domain/execute canon-proj)
        problem (shop/problem2shop problem)]
    (loop [facts start-facts
           domain (-> canon-proj (prune-domain facts) shop/proj2shop)
           cnt 1]
      (if (>= cnt limit) ; This is for testing, and maybe safety.
        facts
        (let [plans (plan {:domain domain :problem problem :execute execute})
              updates (execute-plan plans facts)
              ;; update the fact set in the order operators were applied.
              new-facts (reduce (fn [res update] (update-facts res update)) facts updates)
              pruned-proj (prune-domain canon-proj new-facts)
              new-domain (shop/proj2shop pruned-proj)]
          (log/info "plans =" plans)
          (log/info "fact-updates =" updates)
          (log/info "new-facts =" new-facts)
          (reset! diag {:canon-proj canon-proj :new-facts new-facts :pruned-proj pruned-proj :new-domain new-domain})
          (recur new-facts
                 new-domain
                 (inc cnt)))))))

;;; ToDo: this may be a misnomer; I think I can use it to start a new project too.
;;; ToDo: This assumes that planning domain is "pi"
(defn restart-interview
  "Restart the interview-loop with the given project.
   The project provides :project/state-string which is assigned to :problem/state-string
   in the call to interview-loop. The rest of problem is defined by the planning domain."
  [pid & {:keys [domain-name] :or {domain-name "pi"}}]
  (log/info "Restarting interview for" pid)
  (let [facts-string (-> pid db/get-project :project/state-string)
        domain (get-domain domain-name {:form :proj})
        problem (-> domain
                    :domain/problem
                    (assoc :problem/state-string facts-string))]
    (interview-loop
     domain-name
     :start-facts (edn/read-string facts-string)
     :problem problem)))

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
  {:domain '(defdomain kiwi-example
              ((:operator (!pickup ?a) () () ((have ?a)))
               (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
               (:method (swap ?x ?y)
                        ((have ?x))
                        ((!drop ?x) (!pickup ?y))
                        ((have ?y))
                        ((!drop ?y) (!pickup ?x)))))
   :problem '(defproblem problem1 kiwi-example
               ((have banjo))         ; This is state data.
               ((swap banjo kiwi)))   ; This is a method.
   :execute '(find-plans 'problem1 :verbose :plans)
   :answer '(((!DROP BANJO) 1.0 (!PICKUP KIWI) 1.0))})

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

;;; ToDo: Check for free port.
(def endpoint "The port at which the SHOP2 planner can be reached." 31777)

;;; From a shell: ./pzmq-shop3-2024-02-15 --non-interactive --disable-debugger --eval '(in-package :shop3-zmq)' --eval '(setf *endpoint* 31888)'
(defn init-planner!
  "Start the planner. This relies on environment variable PLANNER_SBCL, which is just
   the name of the SBCL core file. That file is expected to be in the project directory."
  []
  (try
    (let [cmd-arg (cl-format nil "'(setf shop3-zmq::*endpoint* ~S)'" planner-endpoint)]
      ;; --eval does not work!
      (future (sh planner-executable "--non-interactive" "--disable-debugger" "--eval"  cmd-arg)))
    (catch Exception e
      (log/error (:message e))
      (throw (ex-info "Running planner didn't work." {:error e}))))
  (Thread/sleep 2000)
  (test-the-planner))

(defn quit-planner!
  "Quit the planner. It can be restarted with a shell command through mount."
  []
  (let [fut (future
              (zmq-ctx/with-new-context
                (let [socket (zmq-sock/socket :req {:connect planner-endpoint})]
                  (zmq/send-msg socket "(sb-ext:exit)")
                  (zmq/receive-msg socket {:stringify true}))))]
    (if (= [":bye!"] (deref fut 3000 :timeout))
      (do (log/info "Planner terminated as requested.") :stopped)
      (do (log/warn "Planner state unknown. (It may not have been running.)") :unknown))))

(defstate plan-server
  :start (init-planner!)
  :stop (quit-planner!))
