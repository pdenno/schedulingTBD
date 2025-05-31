(ns scheduling-tbd.iviewr.domain.optimality.optimality-analysis
  "Planning operators for the resource interview."
  (:require
   [scheduling-tbd.iviewr.response-utils :as ru :refer [analyze-warm-up]]
   [taoensso.telemere                             :refer [log!]]))

(def ^:diag diag (atom nil))

(defmethod analyze-warm-up :optimality [_tag response]
  (log! :debug (str "*******analysis :optimality/warm-up, response = " response)))
