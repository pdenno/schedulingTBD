(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.string          :as str]
   [datahike.api            :as d]
   [ring.util.http-response :as http]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.domain   :as domain]
   [scheduling-tbd.sutil    :refer [connect-atm]]
   [taoensso.timbre :as log])
  (:import
   [java.util Date]))

(def ^:diag diag (atom {}))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))

(defn initial-projects
  "List all the projects in the DB. :initial-prompt comes along for the ride."
  [_request]
  (let [project-list (d/q '[:find [?name ...] :where [_ :project/name ?name]] @(connect-atm :system))
        current-project-name (when-let [proj (db/current-project-id)]
                               (-> proj name (str/replace #"-" " ")))]
    (http/ok
     (cond-> {}
       (not-empty project-list) (assoc :projects project-list)
       (not-empty project-list) (assoc :current-project-name current-project-name)
       true (assoc :initial-prompt (d/q '[:find ?prompt .
                                          :where [_ :system/initial-prompt ?prompt]]
                                        @(connect-atm :system)))))))

(defn op-start-project
  "Summarize user-text as a project name. Execute plan operations to start a project about user-text."
  [user-text]
  (let [summary (domain/project-name user-text)
        id (-> summary str/lower-case (str/replace #"\s+" "-") keyword)
        proj-info  {:project/id id
                    :project/name summary
                    :project/desc user-text ; <==== ToDo: Save this as :msg-id 1 (0 is the "Describe your scheduling problem" message).
                    #_#_:project/industry _industry}
        proj-info (db/create-proj-db! proj-info) ; May rename the project-info.
        id (:project/id proj-info)]
    (db/add-msg id (d/q '[:find ?prompt . :where [_ :system/initial-prompt ?prompt]] @(connect-atm :system)) :system)
    (db/add-msg id user-text :user)
    ;; ToDo:
    ;; 0) Call the planner and make this code an operator!
    ;; 1) Set :system/current-project.
    ;; 2) Store first two messages (prompt and user's first contribution).
    ;; 3) Check that there isn't a project by that name.
    ;; 4) Let user change the name of the project.
    (let [response (str "Great! We'll call your project '" (:project/name proj-info) "'. ")]
      (log/info "op-start-project: Responding with: " response)
      (db/add-msg id response :system))))

;;; [conn (connect-atm (db/current-project-id))]
;;; (let [{:keys [decision-objective probability]} (llm/find-objective-sentence craft-brewing-desc)] ...) ; <======= First y/n!

(defn plan-response
  "Top-level function to respond to a user's message."
  [user-text]
  (log/info "Got user-text")
  (op-start-project user-text))

(defn respond
  "Handler function for http://api/user-says."
  [request]
  (when-let [user-text (get-in request [:body-params :user-text])]
    (log/info "user-text = " user-text)
    (let [response (plan-response user-text)]
      (log/info "response = " response)
      (http/ok response))))
