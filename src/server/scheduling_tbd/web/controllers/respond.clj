(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.edn              :as edn]
   [clojure.java.io          :as io]
   [datahike.api             :as d]
   [mount.core               :as mount :refer [defstate]]
   [ring.util.http-response  :as http]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [taoensso.telemere        :refer[log!]])
  (:import
   [java.util Date]))

(def ^:diag diag (atom {}))

;;; (resp/get-conversation {:query-params {:project-id "sur-craft-beer"}})
(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   get-conversation always returns the conversation corresponding to :project/active-conversation in the project's DB.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}}).
   Note that this can CHANGE :project/active-conversation. Note also that we don't send the CID." ; Is not sending the CID okay?
  [request]
  (let [{:keys [project-id cid client-id]}  (-> request :query-params (update-keys keyword))
        pid (keyword project-id)
        cid (if cid (keyword cid) (db/get-active-cid pid))
        eid (d/q '[:find ?eid . :where [?eid :project/id]] @(connect-atm pid))]
    (d/transact (connect-atm pid) {:tx-data [{:db/id eid :project/active-conversation cid}]})
    (log! :debug (str "get-conversation (1): pid = " pid " cid = " cid " client-id = " client-id))
    (let [eid (db/project-exists? pid)
          pname (db/get-project-name pid)
          msgs (if eid (-> (db/get-conversation pid cid) :conversation/messages) [])] ; ToDo: Trim some of conversation?
      (http/ok {:project-id pid :project-name pname :conv msgs :cid cid}))))

(defn list-projects
    "Return a map containing :current-project, :cid, and :others, which is a sorted list of every other project in the system DB.
     Note that the server doesn't have a notion of current-project. (How could it?) Thus current conversation is out the window too."
  [_request]
  (letfn [(resolve-proj-info [pid]
            (when-let [eid (db/project-exists? pid)]
              (resolve-db-id {:db/id eid}
                             (connect-atm pid)
                             :keep-set #{:project/name :project/id :project/surrogate?})))]
    (let [proj-infos (->> (db/list-projects)
                          (mapv resolve-proj-info)
                          (remove nil?))
          {:project/keys [id] :as current} (db/default-project)
          cid (or (db/get-active-cid id) :process)
          others (filterv #(not= % current) proj-infos)]
      (http/ok
       (cond-> {:current-project current, :cid cid}
         (not-empty others) (assoc :others others))))))

(defn healthcheck
  [_request]
  (log! :info "Doing a health check.")
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
        (log! :info (str "upload-file: Copying " tempfile "(size" size ") to " proj-filename))
        (io/copy tempfile (io/as-file proj-filename))
        (io/delete-file tempfile)
        (http/ok {:name filename :size size})
        (catch Throwable _e
          (http/internal-server-error "Error reading multipart.")))
      (http/internal-server-error "Did not find multipart file."))))

(defn run-minizinc
  [request]
  (log! :info "Call to run-minizinc")
  (http/ok {:mzn-output "Success!"}))

(defn respond-init []
  (mount/stop  (find-var 'scheduling-tbd.web.handler/app))
  (mount/start (find-var 'scheduling-tbd.web.handler/app))

  #_(mount/stop  (find-var 'scheduling-tbd.core/server))
  #_(mount/start (find-var 'scheduling-tbd.core/server))
  [:htt-responses])

(defstate http-responses
  :start (respond-init))
