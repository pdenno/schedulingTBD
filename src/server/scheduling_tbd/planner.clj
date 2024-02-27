(ns scheduling-tbd.planner
  "Planning, currently including SHOP3 planner."
  (:require
   [clojure.edn          :as edn]
   [clojure.java.shell   :refer [sh]]
   [clojure.pprint       :refer [cl-format]]
   [clojure.string       :as str]
   [datahike.api         :as d]
   ;;[explainlib.core    :as exp]
   [ezzmq.message        :as zmq]
   [ezzmq.context        :as zmq-ctx]
   [ezzmq.socket         :as zmq-sock]
   [mount.core           :as mount :refer [defstate]]
   [scheduling-tbd.shop  :as shop]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm resolve-db-id db-cfg-map]]
   [taoensso.timbre      :as log]))

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

(def special-method
  "This is used to stop the planner for update from analysis."
  '{:rhs/case-name "special"
    :method/preconditions [(stop-plan)]
    :rhs/terms [(!update-plan-state ?proj)]})

(defn parse-planner-result
  "Owing mostly to the messaging with strings, the SHOP planner currently comes back with something
   that has to be cleaned up and interepreted. This takes the string and returns a map with up to three
   values: (1) the form returned (:form), (2) output to std-out (:std-out), and (3) output to err-out (:err-out)."
  [s]
  (let [[result std err] (str/split s #"BREAK-HERE")
        [success result] (re-matches #"^\((.*)$" result)
        form (if success
               (try (read-string result) (catch Exception _e :error/unreadable))
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
         :where [?eid :domain/name ?dname]]
       @(connect-atm :planning-domains)
       domain-name))

(defn get-domain
  "Return the domain in SHOP format."
  [domain-name]
  (if-let [eid (domain-eid domain-name)]
    (-> (resolve-db-id {:db/id eid} (connect-atm :planning-domains))
        shop/db2shop)
    (log/error "Domain" domain-name "not found.")))

;;; (step-discussion "pi")
(defn ^:diag step-discussion
  "Compute the next step of the discussion for the given planning domain.
   This runs the planner and updates the planning domain by advancing
   the stop-plan pre-condition. Argument is a :domain/name (a string)."
  [domain-name]
  (let [domain (get-domain domain-name)]
    (plan {:domain domain
           :problem '(defproblem  process-interview pi
                       () ; This is state data.
                       ((characterize-process unknown-proj)))
           :execute '(find-plans 'process-interview :verbose :plans)})))

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
        fut (future (try (plan kiwi-example)
                         (catch Exception e
                           (log/error (str "Exception in planner: " (.getMessage e)))
                           :planning-failure)))
        result (deref fut 2000 :timeout)
        result (if (keyword? result) result (-> result first parse-planner-result))]
    (cond (= result :timeout)                     (log/error "Planning test timeout")
          (= result :planning-failure)            (log/error "Planning exception")
          (= answer (:form result))               (log/info  "Planner passes test.")
          :else                                   (log/error "Planner fails test:" result))
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
