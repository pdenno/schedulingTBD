(ns stbd-app.db-access
  "Functions for HTTP-based reading/writing from/to the server."
  (:require
   [ajax.core        :refer [GET]]
   [promesa.core    :as p]
   [stbd-app.util   :refer [register-fn lookup-fn]]
   [stbd-app.ws     :as ws]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

;;; project-infos are maps with keys :project/name and :project/id, at least.
(defn get-project-list
  "Return a promise that will resolve to a map {:current-project <project-info> :others [<project-info>...]}."
  []
  (log/info "get-project-list")
  (let [prom (p/deferred)]
    (GET (str "/api/list-projects?client-id=" @ws/client-id)
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/list-projects"
                                                    {:status status :status-text status-text})))})
    prom))

;;; {:conv-for id :conv [{:message/from :system :message/content [{:msg-text/string "You want to start a new project?"}]}]})
(defn get-conversation
  "Return a promise that will resolve to the vector of a maps representing a conversation.
   What conversation is returned depends on the value of :project/current-conversation in the DB; values are #{:process :resource :data}.
   Example of what the promise resolves to:
   {:project-id :sur-craft-beer :conv-id :process :conv [{:message/from :system :message/content [{:msg-text/string 'Hi!'}]}]}."
  [pid]
  (assert (keyword? pid))
  (log/info "Call to get-conversation for" pid)
  (let [prom (p/deferred)]
    (GET (str "/api/get-conversation?project-id=" (name pid) "&client-id=" @ws/client-id) ; ToDo: martian!
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/get-conversation"
                                                    {:status status :status-text status-text})))})
    prom))

;;; This is used by either the client or the server/planner to update the conversation.
;;; There is a ws dispatch for :resume-conversation (to one of #{:process :data :resource}) which would then require this be called.
(register-fn
 :render-conversation
 (fn [{:keys [pid]}]
   (assert (keyword? pid))
   (-> (get-conversation pid)
       (p/then (fn [resp]
                 (let [resp (cond-> resp
                              (-> resp :conv empty?) (assoc :conv [#:message{:content "No discussion here yet.",
                                                                             :from :system,
                                                                             :time (js/Date. (.now js/Date))}]))]
                   (ws/update-project-info! resp)
                   ;; These are hooks in core.cljs
                   ((lookup-fn :set-conversation) resp)
                   ((lookup-fn :set-code) (:code resp))))))))
