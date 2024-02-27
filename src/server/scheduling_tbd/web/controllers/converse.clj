(ns scheduling-tbd.web.controllers.converse
  "Planning and responding to HTTP user-says."
  (:require
   [clojure.string          :as str]
   [datahike.api            :as d]
   [ring.util.http-response :as http]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.domain   :as dom]
   [scheduling-tbd.llm      :as llm]
   [scheduling-tbd.sutil    :as sutil :refer [connect-atm]]
   [taoensso.timbre :as log]))

;;; ToDo: Need to have some kind of a planning algorithm to kick things off starting with the planned remark about
;;;       not focusing on the raw materials supply chain, but rather the process steps being scheduled.
;;;       I could use the SHOP3 stuff for this, but it might be better to look elsewhere.

(def ^:diag diag (atom {}))

(def intro-message
  "The first message of a conversation."
  [{:msg-text/string "Describe your scheduling problem in a few sentences or "}
   {:msg-link/uri "http://localhost:3300/learn-more"
    :msg-link/text "learn more about how this works"}
   {:msg-text/string "."}])

(defn op-start-project
  "Summarize user-text as a project name. Execute plan operations to start a project about user-text."
  [user-text]
  (let [summary (dom/project-name user-text)
        id (-> summary str/lower-case (str/replace #"\s+" "-") keyword)
        proj-info  {:project/id id
                    :project/name summary
                    :project/desc user-text ; <==== ToDo: Save this as :msg-id 1 (0 is the "Describe your scheduling problem" message).
                    #_#_:project/industry _industry}
        proj-info (db/create-proj-db! proj-info) ; May rename the project-info.
        proj-id (:project/id proj-info)]
    (db/add-msg proj-id (d/q '[:find ?prompt . :where [_ :system/initial-prompt ?prompt]] @(connect-atm :system)) :system)
    (db/add-msg proj-id user-text :user)
    ;; ToDo:
    ;; 0) Call the planner and make this code an operator!
    ;; 1) Set :system/current-project.
    ;; 2) Store first two messages (prompt and user's first contribution).
    ;; 3) Check that there isn't a project by that name.
    ;; 4) Let user change the name of the project.
    (let [response (str "Great! We'll call your project '" (:project/name proj-info) "'. ")]
      (log/info "op-start-project: Responding with: " response)
      (db/add-msg proj-id response :system))))

;;; [conn (connect-atm (db/current-project-id))]
;;; (let [{:keys [decision-objective probability]} (llm/find-objective-sentence craft-brewing-desc)] ...) ; <======= First y/n!

(defn wrap-response
  "Wrap text such that it can appear as the text message in the conversation."
  [response-text]
  (let [project-id (db/current-project-id)
        msg-id (db/next-msg-id project-id)]
    (db/inc-msg-id! project-id)
    (db/message-form msg-id :system response-text)))

;;; ToDo: This will be updated to do some sort of planning POMDP, etc.
(defn plan-response
  "Top-level function to respond to a user's message."
  [user-text]
  (log/info "Got user-text")
  (if-let [[_ question] (re-matches #"\s*LLM:(.*)" user-text)]
    (-> question llm/llm-directly wrap-response) ; These are intentionally Not tracked in the DB.
    (op-start-project user-text))) ; ToDo: Temporary!  <========================================

(defn reply
  "Handler function for http://api/user-says."
  [request]
  (when-let [user-text (get-in request [:body-params :user-text])]
    (log/info "user-text = " user-text)
    (let [response (plan-response user-text)]
      (log/info "response = " response)
      (http/ok response))))
