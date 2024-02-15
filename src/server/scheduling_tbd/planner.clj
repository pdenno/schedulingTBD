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

;;; ------ explainlib & clara rules exploration -----------------------
(def shop2-example
  "This is an example from usage from the SHOP2 documentation."
  {:domain
   "(defdomain basic-example
     ((:operator (!pickup ?a) () () ((have ?a)))
      (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
      (:method (swap ?x ?y)
        ((have ?x))
        ((!drop ?x) (!pickup ?y))
        ((have ?y))
        ((!drop ?y) (!pickup ?x)))))"
   :problem
   "(defproblem problem1 basic-example
      ((have banjo)) ((swap banjo kiwi)))"
   :find-plans
   "(find-plans 'problem1 :verbose :plans)"
   :answer
   ["(((!DROP BANJO) 1.0 (!PICKUP KIWI) 1.0))"]})

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

;;; Another challenge is the crappy positional syntax of defdomain, though I think I have a solution for that in the DB.

;;; ToDo: This should go away when SHOP2 is replaced.
(def special-method
  "This is used to stop the planner for update from analysis."
  '{:rhs/case-name "special"
    :method/preconditions [(stop-plan)]
    :rhs/terms [(!update-plan-state ?proj)]})

(def process-interview

(defn tryme []
  (-> process-interview
      (update :domain/elems (fn [elems] (mapv #(if (contains? % :method/head) (update % :method/rhs-pairs conj special-method) %)
                                                elems)))))

;;; Operator: (:operator <head> <pre-conds> <d-list> <a-list> [<cost>]?)
;;; Method:   (:method   <head> [<case-name>? <pre-conds> <task-list>]+)
;;; Problem   (defproblem <problem-name> <domain-name> (<ground-atom>*)  (<task>+))
(def process-interview-lisp
  "This is to run a stubbed-in interview to acquire process knowledge."
  {:domain
   "(defdomain process-interview
     ((:method (characterize-process ?proj)
           ordinary ()  ((!start-interview  ?proj)
                         (get-process-steps ?proj)
                         (get-wip           ?proj)))
      (:method (get-process-steps ?proj)
           well-known ((well-known-process ?proj))         ((!yes-no-process-steps ?proj))
           unknown    ((unknown-process ?proj))            ((!query-process-steps ?proj))
           special    ((stop-plan))                        ((!update-plan-state ?proj)))
      (:method (get-wip ?proj)
           ordinary   ((project-name ?proj))               ((!query-for-wip-spreadsheet ?proj))
           special    ((stop-plan))                        ((!update-plan-state ?proj)))

      (:operator (!start-interview ?proj) ((starting project-init)) () ((stop-plan)))
      (:operator (!yes-no-process-steps ?proj)      ((proj-name ?proj) (well-known-process ?proj)) () ((have-process-steps ?proj)))
      (:operator (!query-process-steps ?proj)       ((proj-name ?proj) (system-model flow)) ()        ((have-process-steps ?proj)))
      (:operator (!yes-no-process-durations ?proj)  ((proj-name ?proj) (well-known-process ?proj)) () ((have-process-durs ?proj)))
      (:operator (!query-for-wip-spreadsheet ?proj) ((proj-name ?proj) (have-process-steps ?proj)) () ((have-wip ?proj)))
      (:operator (!update-plan-state ?proj) () () ())))"
   :problem
   "(defproblem interview-problem process-interview
      ((starting project-init))
       ((characterize-process project-init)))"
   :find-plans
   "(find-plans 'interview-problem :which :all :verbose :long-plans :plan-tree t)"})

(defn tryme []
  (zmq/with-new-context
    (let [socket (zmq/socket :req {:connect planner-endpoint})]
      (zmq/send-msg socket (:domain process-interview-lisp))
      (zmq/receive-msg socket {:stringify true})
      (zmq/send-msg socket (:problem process-interview-lisp))
      (zmq/receive-msg socket {:stringify true})
      ;;(zmq/send-msg socket "(shop-trace :operators)") ; Useless, would need further integration with shop2.
      ;;(zmq/receive-msg socket {:stringify true})
      (zmq/send-msg socket (:find-plans process-interview-lisp))
      (zmq/receive-msg socket {:stringify true}))))



;;; ====================================================
;;; ========================== SHOP3  ==================
;;; ====================================================
;;; -------------------------- Composing a planning domain  ------------------

;;; -------------------------- Composing a planning problem  ------------------


;;; -------------------------- Starting, stopping, and testing ------------------
;;;(defn quit-planner! [])

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

(defn test-planner!
  "Define a planning domain. Define a problem. Find plans for the problem.
   Check that the plan matches what is expected."
  []
  (log/info "test-planner...")
  (zmq/with-new-context
    (let [socket (zmq/socket :req {:connect planner-endpoint})]
      (zmq/send-msg socket (:domain shop2-example))
      (zmq/receive-msg socket {:stringify true})
      (zmq/send-msg socket (:problem shop2-example))
      (zmq/receive-msg socket {:stringify true})
      (zmq/send-msg socket (:find-plans shop2-example))
      (let [result (zmq/receive-msg socket {:stringify true})]
        (if (= (:answer shop2-example) result)
          (do (log/info "Planner passes test.") :passes)
          (do (log/error "Planner fails test.") :fails-test))
        result))))


(defn init-planner
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
  (test-planner!))

(defstate plan-server
  :start (init-planner)
  :stop (quit-planner!))
