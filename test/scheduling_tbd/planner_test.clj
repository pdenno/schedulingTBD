(ns scheduling-tbd.planner-test
  (:require
   [clojure.edn             :as edn]
   [clojure.spec.alpha      :as s]
   [clojure.string          :as str]
   [clojure.test            :refer [deftest is testing]]
   [datahike.api            :as d]
   [scheduling-tbd.planner  :as plan]
   [scheduling-tbd.shop     :as shop :refer [shop2db db-schema-shop2+]]))

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

(def process-interview ; <=============================================================== Update from slime interactive (when it translatess! See below.)
  '{:domain/name "process-interview"  ; We keep this around for testing.
    :domain/elems
    [;; (characterize-process ?proj)...
     {:method/head (characterize-process ?proj)
      :method/rhsides [{:method/case-name "ordinary"
                        :method/task-list [(!start-interview  ?proj) (get-process-steps ?proj) (get-wip ?proj)]}]}

     ;; (get-process-steps ?proj)...
     {:method/head (get-process-steps ?proj)
      :method/rhsides [{:method/case-name "well-known"
                        :method/preconds [(well-known-process ?proj)]
                        :method/task-list [(!yes-no-process-steps ?proj)]}
                       {:method/case-name "unknown"
                        :method/preconds [(unknown-process ?proj)]
                        :method/task-list [(!query-process-steps ?proj)]}]}

     ;; (get-wip ?proj)...
     {:method/head (get-wip ?proj)
      :method/rhsides [{:method/case-name "ordinary"
                        :method/preconds [(project-name ?proj)]
                        :method/task-list [(!query-for-wip-spreadsheet ?proj)]}]}

     ;; (:operator (!start-interview ?proj) ((starting project-init)) () ((stop-plan)))
     {:operator/head (!start-interview ?proj)
      :operator/preconds [(starting project-init)]
      :operator/a-list [(stop-plan)]}

     ;; (:operator (!yes-no-process-steps ?proj) ((proj-name ?proj) (well-known-process ?proj)) () ((have-process-steps ?proj)))

     {:operator/head (!yes-no-process-steps ?proj)
      :operator/preconds [(proj-name ?proj) (well-known-process ?proj)]
      :operator/a-list [(have-process-steps ?proj)]}

     ;; (:operator (!query-process-steps ?proj)  ((proj-name ?proj) (system-model flow)) () ((have-process-steps ?proj)))
     {:operator/head (!query-process-steps ?proj)
      :operator/preconds [(proj-name ?proj) (system-model flow)]
      :operator/a-list [(have-process-steps ?proj)]}

     ;; (:operator (!yes-no-process-durations ?proj)  ((proj-name ?proj) (well-known-process ?proj)) () ((have-process-durs ?proj)))
     {:operator/head (!yes-no-process-durations ?proj)
      :operator/preconds [(proj-name ?proj) (well-known-process ?proj)]
      :operator/a-list [(have-process-durs ?proj)]}

     ;; (:operator (!query-for-wip-spreadsheet ?proj) ((proj-name ?proj) (have-process-steps ?proj)) () ((have-wip ?proj)))
     {:operator/head (!query-for-wip-spreadsheet ?proj)
      :operator/preconds [(proj-name ?proj) (have-process-steps ?proj)]
      :operator/a-list [(have-wip ?proj)]}

     ;; (:operator (!update-plan-state ?proj) () () ())))"
     {:operator/head (!update-plan-state ?proj)}]})

(def example
  '(defdomain process-interview
    ((:method (characterize-process ?proj)
       ORDINARY
       ((plan-state started))
       ((!start-interview ?proj)
        (get-process-steps ?proj)
        (!tbd-wait-response ?proj)))      ; This will add

     (:method (get-process-steps ?proj)
       WELL-KNOWN ((well-known-process ?proj))
                  ((!yes-no-process-steps ?proj))

       UNKNOWN    ((unknown-process ?proj))
                  ((!query-unknown-process-steps ?proj)))

     (:operator (!start-interview ?proj)
                ((plan-state started))
                ()
                ((initial-question ?proj)))

     (:operator (!yes-no-process-steps ?proj)
                ((proj-name ?proj) (well-known-process ?proj))
                ()
                ((have-process-steps ?proj)))

     (:operator (!query-unknown-process-steps ?proj)
                ((proj-name ?proj))
                ()
                ((have-process-steps ?proj)))

     (:operator (!tbd-wait-response ?proj)
                ((proj-name ?proj))
                ((plan-state started))
                ((plan-state stopped)))

     (:method (print-current-state)       ; <========================= Useful!
       ((eval (print-current-state))) ())

     (:method (print-current-tasks)
       ((eval (print-current-tasks))) ())

     (:method (print-current-plan)
       ((eval (print-current-plan))) ()))))

;;; (-> example shop/shop2db shop/db2proj) ;<============================== Broken

(defn tryme []
  (plan/plan {:domain process-interview
              :problem '(defproblem test-problem process-interview
                          ()
                          ((characterize-process some-interview)))
              :execute '(find-plans 'test-problem :verbose :plans)}))
