(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.walk             :as walk :refer [keywordize-keys]]
   [datahike.api             :as d]
   [ring.util.http-response  :as http]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.operators :as ops]
   [scheduling-tbd.planner   :as plan]
   [scheduling-tbd.surrogate :as sur]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [taoensso.timbre          :as log])
  (:import
   [java.util Date]))

(def ^:diag diag (atom {}))

(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}})."
  [request]
  (let [{:keys [project-id]} (-> request :query-params keywordize-keys)]
    (log/info "get-conversation for" project-id)
    (if-let [proj (-> project-id keyword db/get-project)]
      (http/ok
       {:conv-for project-id
        :conv (->> proj :project/messages (sort-by :message/id) vec)})
      (http/not-found))))

(defn list-projects
  "Return a map containing :current-project and :others, which is a sorted list of every other project in the DB."
  [_request]
  (letfn [(name&id [obj] (reduce-kv (fn [m k v] (if (#{:project/name :project/id} k) (assoc m k v) m)) {} obj))]
    (let [current-project (-> (db/current-project-id) db/get-project name&id)
          cid    (:project/id current-project)
          others (->> (db/list-projects)
                      (filter #(not= % cid))
                      (mapv  #(-> % db/get-project name&id)))]
    (log/info "Call to list-projects")
    (http/ok
     (cond-> {}
       current-project    (assoc :current-project current-project)
       (not-empty others) (assoc :others others))))))

(defn set-current-project
  [request]
  (let [{:keys [project-id]} (-> request :query-params keywordize-keys)
        project-id (keyword project-id)]
      (log/info "db-resp/set-current-project:" project-id)
      (if (db/project-exists? project-id)
        (do (db/set-current-project project-id)
            (plan/restart-interview project-id)
            (http/ok {:project-id (name project-id)}))
        (http/not-found))))

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
  (reset! diag request)
  (when-let [{:keys [user-text client-id :promise/pending-keys]} (get request :body-params)]
    (assert (uuid? (parse-uuid client-id)))
    (let [pending-keys (mapv keyword pending-keys)] ; At least the swagger API can send them as strings.
      (log/info "user-text = " user-text "pending-keys = " pending-keys)
      (if-let [[_ question] (re-matches #"\s*LLM:(.*)" user-text)]
        (-> question llm/llm-directly wrap-response http/ok) ; These are intentionally Not tracked in the DB. ToDo: Then whay does wrap-response do db/inc-msg-id?
        (if-let [[_ surrogate-role] (re-matches #"\s*SUR:(.*)" user-text)]
          (-> (sur/start-surrogate surrogate-role) http/ok)
          (do (ops/dispatch-response user-text pending-keys)
              (http/ok {:message/ack true})))))))

(def intro-message
  "The first message of a conversation."
  [{:msg-text/string "Describe your scheduling problem in a few sentences or "}
   {:msg-link/uri "http://localhost:3300/learn-more"
    :msg-link/text "learn more about how this works"}
   {:msg-text/string "."}])

(defn start-new-project
  "User chose to start a new project. Respond with the intro message."
  [request]
  (when-let [{:keys [client-id]} (-> request :query-params keywordize-keys)]
    (log/info "Start new project for" client-id)
    (http/ok {:message/content intro-message})))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))
