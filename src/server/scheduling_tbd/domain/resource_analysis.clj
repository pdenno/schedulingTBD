(ns scheduling-tbd.domain.resource-analysis
    "Planning operators for the resource interview."
  (:require
   [scheduling-tbd.response-utils                 :as ru :refer [defanalyze]]
   [taoensso.telemere.timbre                             :as log]))

(def ^:diag diag (atom nil))

;;;=================================================== Operators ======================================
;;; Operator db-actions update state in the db (db/add-planning-state) and return nil.
;;; The update does not include the call to operator-update-state, which applies the d-list and a-list.
;;; operator-update-state is called by the planner, plan/update-planning.

(defanalyze :initial-resource-question [{:keys [state] :as obj}]
  (log/info "-----------------Write table info to the DB here.----------")
  state)
