(ns scheduling-tbd.flow-shop-test
  (:require
   [clojure.test                  :refer [deftest is testing]]
   [clojure.spec.alpha            :as s]
   [develop.repl                  :refer [ns-setup! ns-fix-setup!]]
   [scheduling-tbd.interviewing.domain.process.flow-shop :as fshop]
   [scheduling-tbd.interviewing.eads-util                :as eadsu]))

(defn remove-annotations
  [obj]
  (letfn [(ca [obj]
            (cond (and (map? obj) (contains? obj :val) (contains? obj :comment))   (:val obj)
                  (map? obj)                                                       (reduce-kv (fn [m k v] (assoc m k (ca v))) {} obj)
                  (vector? obj)                                                    (mapv ca obj)
                  :else                                                            obj))]
    (ca obj)))

(def example-flow-shop-graph "The EADS in flow_shop.clj is an example graph." (-> fshop/flow-shop :EADS remove-annotations (dissoc :EADS-id)))

(deftest graph-is-good
  (testing "that the graph derived from the flow-shop EADS (pencil making) is good enough for use with our Mermaid code."
    (is (and (s/valid? :flow-shop/graph               example-flow-shop-graph)
             (eadsu/outputs-exist-where-inputs-claim? example-flow-shop-graph)
             (eadsu/inputs-match-in-hierarchy?        example-flow-shop-graph)))))
