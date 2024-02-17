(ns scheduling-tbd.planner-test
  (:require
   [clojure.edn             :as edn]
   [clojure.spec.alpha      :as s]
   [clojure.string          :as str]
   [clojure.test            :refer [deftest is testing]]
   [datahike.api            :as d]
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
