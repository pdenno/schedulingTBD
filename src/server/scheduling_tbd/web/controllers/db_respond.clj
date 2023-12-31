(ns scheduling-tbd.web.controllers.db-respond
  "HTTP responses not directly related to responding to user-says."
  (:require
   [clojure.walk            :as walk :refer [keywordize-keys]]
   [datahike.api            :as d]
   [ring.util.http-response :as http]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.sutil    :as sutil :refer [connect-atm]]
   [taoensso.timbre         :as log])
  (:import
   [java.util Date]))

(def diag (atom {}))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))

(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}})."
  [request]
  (let [{:keys [project-id]} (-> request :query-params keywordize-keys)]
    (log/info "get-conversation for" project-id)
    (if-let [conn-atm (-> project-id keyword connect-atm)]
      (let [msgs (-> (d/q '[:find [?e ...] :where [?e :message/id]] @conn-atm) sort)]
        (http/ok
         {:conv-for project-id
          :conv (mapv #(sutil/resolve-db-id {:db/id %} conn-atm) msgs)}))
      (http/not-found))))

(defn list-projects
  "Return a map containing :current-project and :others, which is a sorted list of every other project in the DB."
  [_request]
  (let [current-project (db/current-project-id)
        others (as-> (db/list-projects) ?o
                 (map :project/id ?o)
                 (set ?o)
                 (disj ?o current-project)
                 (sort ?o)
                 (vec ?o))]
    (log/info "Call to get-projects")
    (http/ok
     (cond-> {}
       current-project    (assoc :current-project current-project)
       (not-empty others) (assoc :others others)))))

(defn set-current-project
  [request]
  (let [{:keys [project-id]} (-> request :query-params keywordize-keys)
        project-id (keyword project-id)]
      (log/info "db-resp/set-current-project:" project-id)
      (if (db/project-exists? project-id)
        (do (db/set-current-project project-id)
            (http/ok {:project-id (name project-id)}))
        (http/not-found))))
