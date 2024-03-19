(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.walk             :as walk :refer [keywordize-keys]]
   [mount.core :as mount :refer [defstate]]
   [promesa.core             :as p]
   [ring.util.http-response  :as http]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.operators :as ops]
   [scheduling-tbd.planner   :as plan]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [scheduling-tbd.surrogate :as sur]
   [taoensso.timbre          :as log])
  (:import
   [java.util Date]))

(def ^:diag diag (atom {}))

(def client-current-proj "Current project (keyword) indexed by client-id (string)."  (atom {}))
(defn clear-client-current-proj [] (reset! client-current-proj {}))

(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}})."
  [request]
  (let [{:keys [project-id]} (-> request :query-params keywordize-keys)
        project-id (keyword project-id)
        eid (db/project-exists? project-id)
        msgs (when eid (db/get-messages project-id))]
    (log/info "get-conversation for" project-id)
    (cond (project-id :START-A-NEW-PROJECT)       (http/ok {:conv-for project-id :conv []})
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
          current (or (when-let [proj (get @client-current-proj client-id)] (resolve-proj-info proj))
                      (first proj-infos)
                      new-proj-entry)
          others (filterv #(not= % current) proj-infos)]
      (log/info "Call to list-projects")
      (http/ok
       (cond-> {:current-project current}
         (not-empty others) (assoc :others others))))))

(defmacro report-long-running
  "Return the string from writing to *out* after this runs in a future."
  [[timeout] & body]
  `(-> (p/future (with-out-str ~@body))
       (p/await ~timeout)
       (p/then #(log/info "Long-running:" %))
       (p/catch #(log/warn "Long-running (exception):" %))))

(defn set-current-project
  [request]
  (let [{:keys [project-id client-id]} (:body-params request)
        project-id (keyword project-id)]
    (log/info "db-resp/set-current-project: project-id =" project-id "client-id =" client-id)
      (cond (db/project-exists? project-id)          (do (db/set-current-project project-id)
                                                         (swap! client-current-proj #(assoc % client-id project-id))
                                                         (report-long-running (plan/restart-interview project-id))
                                                         (http/ok {:project/id project-id
                                                                   :project/name (-> (db/get-project project-id) :project/name)}))

            (= project-id :start-a-new-project)      (do (report-long-running (plan/interview-loop project-id :process-interview))
                                                         (http/ok {:message/ack true}))

            :else                                    (http/not-found))))

(defn wrap-response
  "Wrap text such that it can appear as the text message in the conversation."
  [response-text]
  (log/info "response-text = " response-text)
  (let [project-id (db/current-project-id)
        msg-id (db/next-msg-id project-id)]
    (db/inc-msg-id! project-id)
    (db/message-form msg-id :system response-text)))

(defn user-says
  "Handler function for http://api/user-says."
  [request]
  (when-let [{:keys [user-text client-id :promise/pending-keys]} (:body-params request)]
    (assert (uuid? (parse-uuid client-id)))
    (let [pending-keys (mapv keyword pending-keys)] ; At least the swagger API can send them as strings.
      (log/info "user-text = " user-text "pending-keys = " pending-keys)
      (if-let [[_ question] (re-matches #"\s*LLM:(.*)" user-text)]
        (-> question llm/llm-directly wrap-response http/ok) ; These are intentionally Not tracked in the DB. ToDo: Then whay does wrap-response do db/inc-msg-id?
        (if-let [[_ surrogate-role] (re-matches #"\s*SUR:(.*)" user-text)]
          (-> (sur/start-surrogate surrogate-role) http/ok)
          (do (ops/dispatch-response user-text pending-keys)
              (http/ok {:message/ack true})))))))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))

(defstate respond-state
  "Clean up client-current-project"
  :start (clear-client-current-proj))
