(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.edn              :as edn]
   [clojure.java.io          :as io]
   [clojure.walk             :as walk :refer [keywordize-keys]]
   [datahike.api             :as d]
   [ring.util.http-response  :as http]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [taoensso.timbre          :as log])
  (:import
   [java.util Date]))

(def ^:diag diag (atom {}))

;;; (resp/get-conversation {:query-params {:project-id "sur-craft-beer"}})
(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   get-conversation always returns the conversation corresponding to :project/current-converation in the project's DB.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}}).
   Note that this can CHANGE :project/current-conversation."
  [request]
  (let [{:keys [project-id conv-id]} (-> request :query-params keywordize-keys)]
    (when conv-id (db/change-conversation {:pid (keyword project-id) :conv-id (keyword conv-id)}))
    (let [project-id (keyword project-id)
          eid (db/project-exists? project-id)
          conv-id (when eid
                    (-> (or conv-id
                            (d/q '[:find ?conv-id . :where [_ :project/current-conversation ?conv-id]] @(connect-atm project-id)))
                        keyword))
          msgs    (when (and eid conv-id) (db/get-messages project-id conv-id))
          code    (when eid (db/get-code project-id))]
      (log/info "get-conversation for" project-id "conv-id =" conv-id)
      (cond (= project-id :START-A-NEW-PROJECT)     (http/ok {:project-id project-id :conv []})
            (not conv-id)                           (http/ok {:project-id project-id :conv []})
            msgs                                    (http/ok {:project-id project-id :conv msgs :conv-id conv-id :code code})
            :else                                   (http/not-found)))))

(def new-proj-entry {:project/id :START-A-NEW-PROJECT :project/name "START A NEW PROJECT"})

(defn list-projects
  "Return a map containing :current-project, :conv-id, and :others, which is a sorted list of every other project in the DB."
  [_request]
  (letfn [(resolve-proj-info [pid]
            (resolve-db-id {:db/id (db/project-exists? pid)}
                           (connect-atm pid)
                           :keep-set #{:project/name :project/id :project/surrogate?}))]
    (let [proj-infos (mapv resolve-proj-info (db/list-projects))
          current (or (db/default-project) new-proj-entry) ; ToDo: Client could tell you what its current project is.
          conv-id (or (d/q '[:find ?conv-id . :where [_ :project/current-conversation ?conv-id]] @(-> current :project/id connect-atm))
                      :process)
          others (filterv #(not= % current) proj-infos)]
      (http/ok
       (cond-> {:current-project current, :conv-id conv-id}
         (not-empty others) (assoc :others others))))))


(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))

(defn upload-file
  [request]
  (reset! diag request)
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
        (catch Throwable _e
          (http/internal-server-error "Error reading multipart.")))
      (http/internal-server-error "Did not find multipart file."))))
