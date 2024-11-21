(ns scheduling-tbd.domain.data-analysis
    "Planning operators for the data interview"
  (:require
   [scheduling-tbd.response-utils                 :as ru :refer [defanalyze]]
   [taoensso.telemere                             :refer [log!]]))

(def ^:diag diag (atom nil))

(defanalyze :data/warm-up [{:keys [response _client-id _pid] :as _ctx}]
  (log! :debug (str "*******analysis :data/warm-up, response = " response)))
