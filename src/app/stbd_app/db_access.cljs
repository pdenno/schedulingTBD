(ns stbd-app.db-access
  "Functions for HTTP-based reading/writing from/to the server."
  (:require
   [ajax.core        :refer [GET]]
   [promesa.core    :as p]
   [stbd-app.util   :refer [register-fn lookup-fn update-common-info!]]
   [stbd-app.ws     :as ws]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

;;; project-infos are maps with keys :project/name and :project/id, at least.
(defn get-project-list
  "Return a promise that will resolve to a map {:current-project <project-info> :others [<project-info>...]}."
  []
  (log/info "get-project-list")
  (let [prom (p/deferred)]
    (GET (str "/api/list-projects?client-id=" ws/client-id)
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/list-projects"
                                                    {:status status :status-text status-text})))})
    prom))

;;; {:conv-for id :conv [{:message/from :system :message/content [{:msg-text/string "You want to start a new project?"}]}]})
(defn get-conversation-http
  "Return a promise that will resolve to the vector of a maps representing a conversation.
   What conversation is returned depends on the value of :project/current-conversation in the DB; values are #{:process :resource :data}.
   Example of what the promise resolves to:
   {:project-id :sur-craft-beer :conv-id :process :conv [{:message/from :system :message/content [{:msg-text/string 'Hi!'}]}]}.
   If the call provides conv-id, :project/current-conversation is set in the DB."
  ([pid] (get-conversation-http pid nil))
  ([pid conv-id]
   (assert (keyword? pid))
   (log/info "Call to get-conversation-http for" pid "conv-id =" conv-id)
   (let [prom (p/deferred)
         url (if conv-id
               (str "/api/get-conversation?project-id=" (name pid) "&conv-id=" (name conv-id) "&client-id=" ws/client-id)
               (str "/api/get-conversation?project-id=" (name pid) "&client-id=" ws/client-id))]
     (GET url
          {:timeout 3000
           :handler (fn [resp] (p/resolve! prom resp))
           :error-handler (fn [{:keys [status status-text]}]
                            (p/reject! prom (ex-info "CLJS-AJAX error on /api/get-conversation"
                                                     {:status status :status-text status-text})))})
     prom)))

;;; This is used by the server/planner to update the conversation.
;;; Unlike chat/get-conversation, it doesn't resume-conversation because planning is already underway.
(register-fn
 :update-conversation-text
 (fn [{:keys [pid conv-id]}]
   (assert (keyword? pid))
   (-> (get-conversation-http pid conv-id)
       (p/then (fn [resp]
                 (log/info "update-conversation-text: msg count =" (-> resp :conv count))
                 (let [resp (cond-> resp
                              (-> resp :conv empty?) (assoc :conv [#:message{:content "No discussion here yet.",
                                                                             :from :system,
                                                                             :time (js/Date. (.now js/Date))}]))]
                   (update-common-info! resp)
                   ((lookup-fn :set-cs-msg-list) (:conv resp))
                   ((lookup-fn :set-code) (:code resp))))))))
