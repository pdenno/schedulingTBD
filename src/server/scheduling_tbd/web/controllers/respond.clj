(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.walk             :as walk :refer [keywordize-keys]]
   [promesa.core             :as p]
   [ring.util.http-response  :as http]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.planner   :as plan]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [scheduling-tbd.web.routes.websockets :as ws]
   [taoensso.timbre          :as log])
  (:import
   [java.util Date]))

(def ^:diag diag (atom {}))

(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}})."
  [request]
  (let [{:keys [client-id project-id]} (-> request :query-params keywordize-keys)
        project-id (keyword project-id)
        eid (db/project-exists? project-id)
        msgs (when eid (db/get-messages project-id))]
    (ws/set-current-project client-id project-id)
    (log/info "get-conversation for" project-id)
    (cond (= project-id :START-A-NEW-PROJECT)     (http/ok {:conv-for project-id :conv []})
          msgs                                    (http/ok {:conv-for project-id :conv msgs})
          :else                                   (http/not-found))))

(def new-proj-entry {:project/id :START-A-NEW-PROJECT :project/name "START A NEW PROJECT"})

(defn list-projects
  "Return a map containing :current-project and :others, which is a sorted list of every other project in the DB."
  [request]
  (letfn [(resolve-proj-info [pid]
            (resolve-db-id {:db/id (db/project-exists? pid)}
                           (connect-atm pid)
                           :keep-set #{:project/name :project/id}))]
    (let [proj-infos (mapv resolve-proj-info (db/list-projects))
          client-id (-> request :query-params :client-id)
          current (or (when-let [proj (get @ws/client-current-proj client-id)] (resolve-proj-info proj))
                      (db/default-project)
                      new-proj-entry)
          others (filterv #(not= % current) proj-infos)]
      (log/info "Call to list-projects")
      (http/ok
       (cond-> {:current-project current}
         (not-empty others) (assoc :others others))))))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))
