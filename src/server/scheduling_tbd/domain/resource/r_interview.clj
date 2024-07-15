(ns scheduling-tbd.domain.resource.r-interview
  (:require
   [clojure.edn                   :as edn]
   [scheduling-tbd.sutil          :as sutil :refer [register-planning-domain]]))

(register-planning-domain :resource (-> "data/planning-domains/resource-interview.edn" slurp edn/read-string))
