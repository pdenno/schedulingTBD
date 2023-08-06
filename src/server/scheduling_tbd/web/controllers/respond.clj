(ns scheduling-tbd.web.controllers.respond
  (:require
   [ring.util.http-response :as http]
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
  [user-text]
  {:msg-id 222
   :msg (str "Sounds good to me: " "'" user-text "'")})

(defn respond
  "Top-level (called from router) function to respond to a user's message."
  [request]
  (when-let [user-text (get-in request [:body-params :user-text])]
    (log/info "user-text = " user-text)
    (let [response (plan-response user-text)]
      (http/ok response))))
