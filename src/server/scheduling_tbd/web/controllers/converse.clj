(ns scheduling-tbd.web.controllers.converse
  "Planning and responding to HTTP user-says."
  (:require
   [clojure.string           :as str]
   [datahike.api             :as d]
   [ring.util.http-response  :as http]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.domain    :as dom]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.operators :as ops]
   [scheduling-tbd.surrogate :as sur]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm]]
   [taoensso.timbre :as log]))

;;; ToDo: Need to have some kind of a planning algorithm to kick things off starting with the planned remark about
;;;       not focusing on the raw materials supply chain, but rather the process steps being scheduled.
;;;       I could use the SHOP3 stuff for this, but it might be better to look elsewhere.

(def ^:diag diag (atom {}))

(defn wrap-response
  "Wrap text such that it can appear as the text message in the conversation."
  [response-text]
  (let [project-id (db/current-project-id)
        msg-id (db/next-msg-id project-id)]
    (db/inc-msg-id! project-id)
    (db/message-form msg-id :system response-text)))

(defn reply
  "Handler function for http://api/user-says."
  [request]
  (when-let [user-text (get-in request [:body-params :user-text])]
    (log/info "user-text = " user-text)
    (if-let [[_ question] (re-matches #"\s*LLM:(.*)" user-text)]
      (-> question llm/llm-directly wrap-response) ; These are intentionally Not tracked in the DB.
      (let [response (if-let [[_ surrogate-role] (re-matches #"\s*SUR:(.*)" user-text)]
                       (sur/start-surrogate surrogate-role)
                       (ops/dispatch-response user-text))]
        (log/info "response = " response)
        (http/ok response)))))
