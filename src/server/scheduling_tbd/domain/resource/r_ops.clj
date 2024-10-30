(ns scheduling-tbd.domain.resource.r-ops
    "Planning operators for the resource interview."
  (:require
   [scheduling-tbd.op-utils                       :as ou :refer [defaction]]
   [taoensso.timbre                               :as log]))

(def ^:diag diag (atom nil))

;;;=================================================== Operators ======================================
;;; Operator db-actions update state in the db (db/add-planning-state) and return nil.
;;; The update does not include the call to operator-update-state, which applies the d-list and a-list.
;;; operator-update-state is called by the planner, plan/update-planning.

(defaction :!initial-resource-question [{:keys [state] :as obj}]
  (log/info "-----------------Write table info to the DB here.----------")
  state)
