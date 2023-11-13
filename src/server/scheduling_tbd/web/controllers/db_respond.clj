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
   Example usage (get-conversation {:query-params {:project-id \"craft-beer-brewery-scheduling\"}})."
  [& request]
  (let [{:keys [project-id]} (-> request :query-params keywordize-keys)
        project-id (or project-id (db/current-project-id))
        conn-atm (-> project-id keyword connect-atm)
        msgs (-> (d/q '[:find [?e ...] :where [?e :message/id]] @conn-atm) sort)]
    (log/info "get-conversation for" project-id)
    (http/ok
     (mapv #(sutil/resolve-db-id {:db/id %} conn-atm) msgs))))

(defn get-projects
  "Return a vector of all the :project/name in the DB. The first one in the vector is the current project."
  [_request]
  (let [projects (d/q '[:find [?name ...] :where [_ :project/name ?name]] @(connect-atm :system))
        current-project-name (when-let [proj (db/current-project-id)] (name proj))
        projects (-> projects set (disj current-project-name) seq (conj current-project-name) vec)]
    (log/info "Call to get-projects")
    (http/ok
     (cond-> {}
       (not-empty projects) (assoc :projects projects)))))
