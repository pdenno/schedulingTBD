(ns scheduling-tbd.planner
  "Planning, currently including SHOP3 planner."
  (:require
   [clojure.java.shell :refer [sh]]
   ;;[explainlib.core    :as exp]
   [scheduling-tbd.shop :as shop]
   [ezzmq.core :as zmq]             ; SHOP3
   [mount.core :as mount :refer [defstate]]
   [taoensso.timbre :as log]))

(def planner-endpoint "tcp://*:31726")
(def planner-executable "I put the planner executable in the project directory" "./pzmq-shop3-2024-02-15")
(def diag (atom nil))

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

;;; Other things to try:
;;; (zmq/send-msg socket "(shop-trace :operators)") ; Useless, would need further integration with shop2.
;;; (zmq/receive-msg socket {:stringify true})

(defn plan
  "Communicate to shop3 whatever is specified in the arguments, which may include one or more of:
      1) providing a new planning domain,
      2) providing a new problem, and
      3) execute something, e.g. finding plans for a problem.
   Return result of execution if (3) is provided, otherwise result from (2) or (1)."
  [{:keys [domain problem execute]}]
  (zmq/with-new-context
    (let [socket (zmq/socket :req {:connect planner-endpoint})]
      (when domain
        (zmq/send-msg socket (-> domain shop/proj2canon shop/canon2shop str))
        (zmq/receive-msg socket {:stringify true}))
      (when problem
        (zmq/send-msg socket (str problem))
        (zmq/receive-msg socket {:stringify true}))
      (when execute
        (zmq/send-msg socket (str execute))
        (zmq/receive-msg socket {:stringify true})))))

;;; -------------------------- Starting, stopping, and testing ------------------
;;;(defn quit-planner! [])
(def shop2-example
  "This is an example from usage from the SHOP2 documentation. It is used to test communication with the planner."
  {:domain '#:domain{:elems
                     [#:operator{:head (!pickup ?a), :a-list [(have ?a)]}
                      #:operator{:head (!drop ?a), :preconds [(have ?a)], :d-list [(have ?a)]}
                      #:method{:head (swap ?x ?y),
                               :rhsides
                               [#:method{:preconds [(have ?x)], :task-list ((!drop ?x) (!pickup ?y))}
                                #:method{:preconds [(have ?y)], :task-list ((!drop ?y) (!pickup ?x))}]}],
                     :name "basic-example"}
   :problem '(defproblem problem1 basic-example
               ((have banjo))         ; This is state data.
               ((swap banjo kiwi)))   ; This is some method
   :execute '(find-plans 'problem1 :verbose :plans)
   :answer '[(((!DROP BANJO) 1.0 (!PICKUP KIWI) 1.0))]})

(defn test-the-planner
  "Define a planning domain. Define a problem. Find plans for the problem.
   Check that the plan matches what is expected."
  []
  (log/info "test-planner...")
  (let [{:keys [domain problem execute answer]} shop2-example
        result (try (plan {:domain  domain
                           :problem problem
                           :execute execute})
                    (catch Exception e
                      (log/warn (str "Exception in planner: " (.getMessage e)))
                      :planning-exception))]
    (cond (and (not-empty result)
               (every? string? result)
               (= (->> result (mapv read-string)) answer))   (do (log/info "Planner passes test.") :passes)
          (= result :planning-failure)                       (do (log/error "Planning exception")  :planning-failure)
          :else                                              (do (log/error "Planner fails test.") :fails-test))))

(defn init-planner!
  "Start the planner. This relies on environment variable PLANNER_SBCL, which is just
   the name of the SBCL core file. That file is expected to be in the project directory."
  []
  (try (future (sh "/usr/bin/sbcl"
                   "--core"
                   planner-executable
                   "--non-interactive"
                   "--disable-debugger"))
       (catch Exception e
         (log/error (:message e))
         (throw (ex-info "Running planner didn't work." {:error e}))))
  (Thread/sleep 2000)
  (test-the-planner))

(defn quit-planner!
  "Quit the planner. It can be restarted with a shell command through mount."
  []
  (let [fut (future
              (zmq/with-new-context
                (let [socket (zmq/socket :req {:connect planner-endpoint})]
                  (zmq/send-msg socket "(sb-ext:exit)")
                  (zmq/receive-msg socket {:stringify true}))))]
    (if (= [":bye!"] (deref fut 3000 :timeout))
      (do (log/info "Planner terminated as requested.") :stopped)
      (do (log/warn "Planner state unknown. (It may not have been running.)") :unknown))))

(defstate plan-server
  :start (init-planner!)
  :stop (quit-planner!))
