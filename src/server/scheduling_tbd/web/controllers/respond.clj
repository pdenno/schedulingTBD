(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.edn              :as edn]
   [clojure.java.io          :as io]
   [clojure.walk             :as walk :refer [keywordize-keys]]
   [ring.util.http-response  :as http]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [taoensso.timbre          :as log])
  (:import
   [java.util Date]))

(def ^:diag diag (atom {}))

;;; (resp/get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}})
(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}})."
  [request]
  (let [{:keys [project-id]} (-> request :query-params keywordize-keys)
        project-id (keyword project-id)
        eid (db/project-exists? project-id)
        msgs (when eid (db/get-messages project-id))
        code (when eid (db/get-code project-id))]
    (log/info "get-conversation for" project-id)
    (cond (= project-id :START-A-NEW-PROJECT)     (http/ok {:conv-for project-id :conv []})
          msgs                                    (http/ok {:conv-for project-id :conv msgs :code code})
          :else                                   (http/not-found))))

(def new-proj-entry {:project/id :START-A-NEW-PROJECT :project/name "START A NEW PROJECT"})

(defn list-projects
  "Return a map containing :current-project and :others, which is a sorted list of every other project in the DB."
  [_request]
  (letfn [(resolve-proj-info [pid]
            (resolve-db-id {:db/id (db/project-exists? pid)}
                           (connect-atm pid)
                           :keep-set #{:project/name :project/id :project/surrogate?}))]
    (let [proj-infos (mapv resolve-proj-info (db/list-projects))
          current (or (db/default-project) new-proj-entry) ; ToDo: Client could tell you what its current project is.
          others (filterv #(not= % current) proj-infos)]
      ;(log/info "Call to list-projects")
      (http/ok
       (cond-> {:current-project current}
         (not-empty others) (assoc :others others))))))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))

(defn upload-file
  [request]
  (let [dir-root (System/getenv "SCHEDULING_TBD_DB")
        params (get request :multipart-params)
        project-id (-> params (get "project-id") edn/read-string name)
        {:keys [filename size tempfile _content-type]} (get params "file")
        proj-filename (str dir-root "/projects/" project-id "/files/" filename)]
    (if tempfile
      (try
        (log/info (str "upload-file: Copying " tempfile "(size" size ") to " proj-filename))
        (io/copy tempfile (io/as-file proj-filename))
        (io/delete-file tempfile)
        (http/ok {:name filename :size size})
        (catch Throwable e
          (reset! diag e)
          (http/internal-server-error "Error reading multipart.")))
      (http/internal-server-error "Did not find multipart file."))))
