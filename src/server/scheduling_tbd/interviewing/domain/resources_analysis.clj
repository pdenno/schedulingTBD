(ns scheduling-tbd.interviewing.domain.resources-analysis
    "Planning operators for the resource interview."
  (:require
   [scheduling-tbd.interviewing.response-utils    :as ru :refer [defanalyze]]
   [taoensso.telemere                             :refer [log!]]))

(def ^:diag diag (atom nil))

(defanalyze :resources/warm-up  [{:keys [response _client-id _pid] :as _ctx}]
  (log! :debug (str "*******analysis :resources/warm-up, response = " response)))
