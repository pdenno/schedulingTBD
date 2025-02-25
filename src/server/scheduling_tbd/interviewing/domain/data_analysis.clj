(ns scheduling-tbd.interviewing.domain.data-analysis
    "Planning operators for the data interview"
  (:require
   [scheduling-tbd.interviewing.response-utils  :as ru :refer [defanalyze]]
   [taoensso.telemere                           :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

(def warm-up-question
  (str "What kind of spreadsheets do you use to help with your production scheduling?"))

(defanalyze :data/warm-up [{:keys [response _client-id _pid] :as _ctx}]
  (log! :debug (str "*******analysis :data/warm-up, response = " response)))
