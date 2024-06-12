(ns scheduling-tbd.domain.data.d-interview
  (:require
   [clojure.edn                   :as edn]
   [scheduling-tbd.sutil          :as sutil :refer [register-planning-domain]]))

(register-planning-domain :data (-> "data/planning-domains/data-interview.edn" slurp edn/read-string))
