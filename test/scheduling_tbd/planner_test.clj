(ns scheduling-tbd.planner-test
  (:require
   [clojure.edn             :as edn]
   [clojure.spec.alpha      :as s]
   [clojure.string          :as str]
   [clojure.test            :refer [deftest is testing]]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.planner  :as plan]
   [scheduling-tbd.shop     :as shop]
   [scheduling-tbd.specs    :as specs]
   [taoensso.timbre          :as log]))

(deftest valid-problem
  (testing "That the spec for planning problems works."
    (is (s/valid? ::specs/domain-problem
                  {:problem/name "process-interview"
                   :problem/domain "pi"
                   :problem/goal-string  "[(characterize-process craft-beer)]"
                   :problem/state-string "[(proj-name craft-beer) (ongoing-discussion craft-beer) (well-known-process craft-beer)]"}))))

(defn tryme []
  (plan/load-domain "data/planning-domains/process-interview.edn")
  (let [state-vec (-> :craft-beer-brewery-scheduling
                      db/get-project
                      :project/state-string
                      edn/read-string)]
    (log/info "state-vec =" state-vec)
    (plan/interview-loop "pi"
                         :start-facts state-vec)))
