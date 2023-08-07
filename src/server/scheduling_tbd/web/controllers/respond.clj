(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.string          :as str]
   [datahike.api            :as d]
   [ring.util.http-response :as http]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.llm      :as llm]
   [scheduling-tbd.sutil    :refer [connect-atm]]
   [taoensso.timbre :as log])
  (:import
   [java.util Date]))

(def diag (atom {}))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))

(defn list-projects
  "List all the projects in the DB."
  [_request]
  (http/ok
   {:projects
    (-> (d/q '[:find [?name ...] :where [_ :project/name ?name]]
             @(connect-atm :system))
        sort
        vec)
    :current-project
    (-> (d/q '[:find ?name .
               :where
               [?entity :system/name "SYSTEM"]
               [?entity :system/current-project ?name]]
             @(connect-atm :system))
        name
        (str/replace #"-" " "))}))

;;; (let [{:keys [decision-objective probability]} (llm/find-objective-sentence craft-brewing-desc)] ...) ; <======= First y/n!

(defn plan-response
  "Top-level function to respond to a user's message."
  [user-text]
  (log/info "Got user-text")
  (let [{:keys [summary industry]} (llm/project-name user-text)
        id (-> summary str/lower-case (str/replace #"\s+" "-") keyword)
        proj-info  {:project/id id
                    :project/name summary
                    :project/desc user-text ; <==== ToDo: Save this as :msg-id 1 (0 is the "Describe your scheduling problem" message).
                    :project/industry industry}
        proj-info (db/create-proj-db! proj-info)] ; May rename the project-info.
    ;; ToDo:
    ;; 0) Call the planner and make this code an operator!
    ;; 1) Set :system/current-project.
    ;; 2) Store first two messages (prompt and user's first contribution).
    ;; 3) Check that there isn't a project by that name.
    ;; 4) Let user change the name of the project.
    {:msg-id 2 ; ToDo: Use a link to change the name.
     :msg (str "Great! We'll call your project '" (:project/name proj-info) "'. ")}))

(defn respond
  "Handler function for http://api/ask-user."
  [request]
  (when-let [user-text (get-in request [:body-params :user-text])]
    (log/info "user-text = " user-text)
    (let [response (plan-response user-text)]
      (http/ok response))))
