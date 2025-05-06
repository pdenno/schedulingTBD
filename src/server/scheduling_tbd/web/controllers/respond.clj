(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.edn              :as edn]
   [clojure.java.io          :as io]
   [ring.util.http-response  :as http]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [taoensso.telemere        :refer[log!]])
  (:import
   [java.util Date]))

(def ^:diag diag (atom {}))

;;; (resp/get-conversation {:query-params {:project-id "sur-craft-beer"}})

(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   get-conversation always returns the conversation corresponding to :project/current-converation in the project's DB.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}}).
   Note that this can CHANGE :project/current-conversation. Note also that we don't send the CID." ; Is not sending the CID okay?
  [request]
  (let [{:keys [project-id cid client-id]}  (-> request :query-params (update-keys keyword))
        pid (keyword project-id)
        cid (if cid (keyword cid) (db/get-current-cid pid))]
    (log! :debug (str "get-conversation (1): pid = " pid " cid = " cid " client-id = " client-id))
    (let [eid (db/project-exists? pid)
          pname (db/get-project-name pid)
          msgs (if eid (-> (db/get-conversation pid cid) :conversation/messages) []) ; ToDo: Trim some of conversation?
          code (if eid (db/get-code pid) "")]
      (http/ok {:project-id pid :project-name pname :conv msgs :cid cid :code code}))))

(defn list-projects
    "Return a map containing :current-project, :cid, and :others, which is a sorted list of every other project in the system DB.
     Note that the server doesn't have a notion of current-project. (How could it?) Thus current conversation is out the window too."
  [_request]
  (letfn [(resolve-proj-info [pid]
            (resolve-db-id {:db/id (db/project-exists? pid)}
                           (connect-atm pid)
                           :keep-set #{:project/name :project/id :project/surrogate?}))]
    (let [proj-infos (mapv resolve-proj-info (db/list-projects))
          {:project/keys [id] :as current} (db/default-project)
          cid (or (db/get-current-cid id) :process)
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
