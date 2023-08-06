(ns scheduling-tbd.web.controllers.respond
  (:require
   [clojure.string          :as str]
   [datahike.api            :as d]
   [ring.util.http-response :as http]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.llm      :as llm]
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

(defn plan-response
  "Top-level function to respond to a user's message."
  [user-text]
  (log/info "Got user-text")
  (let [{:keys [summary industry]} (llm/project-name user-text)
        #_{:keys [decision-objective probability]} #_(llm/find-objective-sentence craft-brewing-desc) ; <======= First y/n!
        id (-> summary str/lower-case (str/replace #"\s+" "-") keyword)]
    (log/info "Computed summary = " summary)
    (db/create-proj-db!
     {:project/id id
      :project/name summary
      :project/desc user-text ; <==== To:do Save this as :msg-id 1 (0 is the "Describe your scheduling problem" message).
      :project/industry industry})
    {:msg-id 2
     :msg (str "We'll call your project '" (name id) "'.")}))

(defn respond
  "Handler function for http://api/ask-user."
  [request]
  (when-let [user-text (get-in request [:body-params :user-text])]
    (log/info "user-text = " user-text)
    (let [response (plan-response user-text)]
      (http/ok response))))
