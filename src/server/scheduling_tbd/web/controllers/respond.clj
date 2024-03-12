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

#_(defn get-conversation
  "Return a sorted vector of the messages of the argument project or current project if not specified.
   Example usage (get-conversation {:query-params {:project-id :craft-beer-brewery-scheduling}})."
  [request]
  (log/info "get-conversation start..")
  (let [{:keys [project-id]} (-> request :query-params keywordize-keys)]
    (log/info "get-conversation for" project-id)
    (if-let [conn-atm (-> project-id keyword connect-atm)]
      (let [msgs (-> (d/q '[:find [?e ...] :where [?e :message/id]] @conn-atm) sort)]
        (http/ok
         {:conv-for project-id
          :conv (mapv #(resolve-db-id {:db/id %} conn-atm) msgs)}))
      (http/not-found))))

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
  (when-let [{:keys [user-text :promise/clear-keys]} (get request :body-params)]
    (log/info "user-text = " user-text "clear-keys = " clear-keys)
    (if-let [[_ question] (re-matches #"\s*LLM:(.*)" user-text)]
      (-> question llm/llm-directly wrap-response http/ok) ; These are intentionally Not tracked in the DB. ToDo: Then whay does wrap-response do db/inc-msg-id?
      (let [response (if-let [[_ surrogate-role] (re-matches #"\s*SUR:(.*)" user-text)]
                       (sur/start-surrogate surrogate-role)
                       (ops/dispatch-response user-text clear-keys))]
        (log/info "response = " response)
        (http/ok response)))))

(defn healthcheck
  [_request]
  (log/info "Doing a health check.")
  (http/ok
    {:time     (str (Date. (System/currentTimeMillis)))
     :up-since (str (Date. (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))))}))
