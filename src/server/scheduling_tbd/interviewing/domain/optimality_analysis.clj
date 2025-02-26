(ns scheduling-tbd.interviewing.domain.optimality-analysis
    "Planning operators for the resource interview."
  (:require
   [scheduling-tbd.response-utils                 :as ru :refer [defanalyze]]
   [taoensso.telemere                             :refer [log!]]))

(def ^:diag diag (atom nil))

(defanalyze :optimality/warm-up  [{:keys [response _client-id _pid] :as _ctx}]
  (log! :debug (str "*******analysis :optimality/warm-up, response = " response)))
