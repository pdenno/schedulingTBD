(ns scheduling-tbd.interviewing.domain.data-analysis
  "Planning operators for the data interview"
  (:require
   [scheduling-tbd.interviewing.response-utils  :as ru :refer [analyze-warm-up]]
   [taoensso.telemere                           :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

(def warm-up-question
  (str "What kind of spreadsheets do you use to help with your production scheduling?"))

(defmethod analyze-warm-up :data [_tag response]
  (log! :debug (str "*******analysis :data/warm-up, response = " response)))
