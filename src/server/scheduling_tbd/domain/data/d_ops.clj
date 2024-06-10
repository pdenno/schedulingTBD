(ns scheduling-tbd.domain.data.d-ops
    "Planning operators for the data interview"
  (:require
   [scheduling-tbd.db                             :as db]
   [scheduling-tbd.domain.data.d-interview        :as inv]
   [scheduling-tbd.op-utils                       :as ou :refer [chat-pair db-action defoperator defaction make-human-project surrogate?]]
   [scheduling-tbd.sutil                          :as sutil :refer [find-fact]]
   [scheduling-tbd.web.websockets                 :as ws]
   [taoensso.timbre                               :as log]))

(def ^:diag diag (atom nil))

;;;=================================================== Operators ======================================
;;; Operator db-actions update state in the db (db/add-planning-state) and return nil.
;;; The update does not include the call to operator-update-state, which applies the d-list and a-list.
;;; operator-update-state is called by the planner, plan/update-planning.

(defoperator :!run-table-agent [{:keys [_state] :as obj}]
  (log/info "-------------------Run the table agent here.-----------------")
  (-> obj db-action))

(defaction :!run-table-agent [{:keys [state] :as obj}]
  (log/info "-----------------Write table info to the DB here.----------")
  state)
