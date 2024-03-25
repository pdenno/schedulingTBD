(ns scheduling-tbd.planner-test
  (:require
   [clojure.edn             :as edn]
   [clojure.pprint          :refer [cl-format]]
   [clojure.spec.alpha      :as s]
   [clojure.test            :refer [deftest is testing]]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.planner  :as plan]
   [scheduling-tbd.specs    :as specs]
   [scheduling-tbd.web.routes.websockets :as ws]
   [taoensso.timbre          :as log]))

(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (alias 'uni 'clojure.core.unify)
  (alias 'str 'clojure.string)
  (alias 'd   'datahike.api)
  (alias 'dp  'datahike.pull-api)
  (alias 'dom 'scheduling-tbd.domain)
  (alias 'how 'scheduling-tbd.how-made)
  (alias 'llm 'scheduling-tbd.llm)
  (alias 'op 'scheduling-tbd.operators)
  (alias 'resp 'scheduling-tbd.web.controllers.respond)
  (alias 'shop 'scheduling-tbd.shop)
  (alias 'spec 'scheduling-tbd.specs)
  (alias 'sutil 'scheduling-tbd.sutil)
  (alias 'ws 'scheduling-tbd.web.routes.websockets))

(deftest valid-problem
  (testing "That the spec for planning problems works."
    (is (s/valid? ::specs/domain-problem
                  {:problem/name "process-interview"
                   :problem/domain "pi"
                   :problem/goal-string  "[(characterize-process craft-beer)]"
                   :problem/state-string "[(proj-name craft-beer) (ongoing-discussion craft-beer) (well-known-process craft-beer)]"}))
    (is (not (s/valid? ::specs/domain-problem
                       {:problem/name 1
                        :problem/domain "pi"
                        :problem/goal-string  "[(characterize-process craft-beer)]"
                        :problem/state-string "[(proj-name craft-beer) (ongoing-discussion craft-beer) (well-known-process craft-beer)]"})))))

;;; (tryme :snowboards-production-scheduling)
;;; (tryme :aluminium-foil-production-scheduling)
(defn ^:diag tryme []
  (plan/load-domain "data/planning-domains/process-interview.edn")
  (db/recreate-dbs!)
  (plan/interview-loop
   :new-project
   :process-interview
   (ws/any-client!)))

(defn ^:diag tryme-old
  ([] (tryme-old :snowboards-production-scheduling))
  ([proj-id]
   (plan/load-domain "data/planning-domains/process-interview.edn")
   (db/recreate-dbs!)
   (let [state-set (-> proj-id
                       db/get-project
                       :project/state-string
                       edn/read-string)]
     (log/info "state-vec =" (cl-format nil "~{~%~A~^,~}" (sort-by first state-set)))
     (plan/interview-loop
      proj-id
      :process-interview
      (ws/any-client!)
      {:start-facts state-set}))))
