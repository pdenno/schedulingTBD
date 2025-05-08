(ns scheduling-tbd.flow-shop-test
  (:require
   [clojure.test                  :refer [deftest is testing]]
   [develop.repl                  :refer [ns-setup! ns-fix-setup!]]
   [scheduling-tbd.interviewing.domain.process.flow-shop :as fshop]))

(defn remove-annotations
  [obj]
  (letfn [(ca [obj]
            (cond (and (map? obj) (contains? obj :val) (contains? obj :comment))   (:val obj)
                  (map? obj)                                                       (reduce-kv (fn [m k v] (assoc m k (ca v))) {} obj)
                  (vector? obj)                                                    (mapv ca obj)
                  :else                                                            obj))]
    (ca obj)))

(def example-flow-shop-graph "The EADS in flow_shop.clj is an example graph." (-> fshop/flow-shop :EADS remove-annotations))
