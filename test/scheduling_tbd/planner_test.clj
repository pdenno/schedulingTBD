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
  (alias 'uni    'clojure.core.unify)
  (alias 'str    'clojure.string)
  (alias 'd      'datahike.api)
  (alias 'dp     'datahike.pull-api)
  (alias 'mount  'mount.core)
  (alias 'p      'promesa.core)
  (alias 'px     'promesa.exec)
  (alias 'dom    'scheduling-tbd.domain)
  ;(alias 'domt    'scheduling-tbd.domain-test)
  (alias 'how    'scheduling-tbd.how-made)
  (alias 'llm    'scheduling-tbd.llm)
  ;(alias 'llmt   'scheduling-tbd.llm-test)
  (alias 'op     'scheduling-tbd.operators)
  (alias 'resp   'scheduling-tbd.web.controllers.respond)
  (alias 'shop   'scheduling-tbd.shop)
  (alias 'spec   'scheduling-tbd.specs)
  (alias 'sutil  'scheduling-tbd.sutil)
  (alias 'sur    'scheduling-tbd.surrogate)
  ;(alias 'surt   'scheduling-tbd.surrogate-test)
  (alias 'util   'scheduling-tbd.util)
  (alias 'ws     'scheduling-tbd.web.routes.websockets)
  (alias 'openai 'wkok.openai-clojure.api))


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

(defn ^:diag tryme []
  (plan/load-domain "data/planning-domains/process-interview.edn")
  (plan/interview-loop
   :sur-plate-glass
   :process-interview
   (ws/recent-client!)
   {:start-facts (db/get-state :sur-plate-glass)}))

(defn aaa []
  (println "We are a medium-sized craft beer brewery. We produce about 100,000 barrels/year.\n   We run several products simultaneously and simply would like to be able to have the beer bottled and ready\n   to ship as near as possible to the dates defined in our sales plan."))

(defn ^:diag tryme-0 []
  (plan/load-domain "data/planning-domains/process-interview.edn")
  (plan/interview-loop
   :START-A-NEW-PROJECT
   :process-interview
   (ws/recent-client!)
   {:start-facts (db/get-state :START-A-NEW-PROJECT)}))


;;; (tryme :snowboards-production-scheduling)
;;; (tryme :aluminium-foil-production-scheduling)
(defn ^:diag tryme-2 []
  (plan/load-domain "data/planning-domains/process-interview.edn")
  (plan/interview-loop
   :craft-beer-brewery-scheduling
   :process-interview
   {:start-facts (db/get-state :craft-beer-brewery-scheduling)}
   (ws/recent-client!)))
