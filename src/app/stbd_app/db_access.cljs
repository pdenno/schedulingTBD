(ns stbd-app.db-access
  "Functions for HTTP-based reading/writing from/to the server."
  (:require
   [ajax.core         :refer [GET]]
   [bling.core        :refer [bling callout point-of-interest print-bling]]
   [promesa.core      :as p]
   [stbd-app.util     :as util :refer [lookup-fn register-fn update-common-info!]]
   [stbd-app.ws       :as ws]
   [taoensso.telemere :refer [log!]]))

(def ^:diag diag (atom nil))

;;; project-infos are maps with keys :project/name and :project/id, at least.
(defn get-project-list
  "Return a promise that will resolve to a map {:current-project <project-info> :others [<project-info>...]}."
  []
  (log! :info "get-project-list")
  (let [prom (p/deferred)]
    (GET (str "/api/list-projects?client-id=" ws/client-id)
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp)) ; callers take care of updating common-info.
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/list-projects"
                                                    {:status status :status-text status-text})))})
    prom))

;;; {:conv-for id :conv [{:message/from :system :message/content [{:msg-text/string "You want to start a new project?"}]}]})
(defn get-conversation-http
  "Return a promise that will resolve to the vector of a maps representing a conversation.
   What conversation is returned depends on the value of :project/current-conversation in the DB; values are #{:process :data :resources :optimality}.
   Example of what the promise resolves to:
   {:project-id :sur-craft-beer :cid :process :conv [{:message/from :system :message/content [{:msg-text/string 'Hi!'}]}]}.
   If the call provides cid, :project/current-conversation is set in the DB."
  ([pid] (get-conversation-http pid nil))
  ([pid cid]
   (assert (keyword? pid))
   (log! :info (str "Call to get-conversation-http for " pid " cid = " cid))
   (let [prom (p/deferred)
         url (if cid
               (str "/api/get-conversation?project-id=" (name pid) "&cid=" (name cid) "&client-id=" ws/client-id)
               (str "/api/get-conversation?project-id=" (name pid) "&client-id=" ws/client-id))]
     (GET url
          {:timeout 3000
           :handler (fn [resp] (p/resolve! prom resp))
           :error-handler (fn [{:keys [status status-text]}]
                            (p/reject! prom (ex-info "CLJS-AJAX error on /api/get-conversation"
                                                     {:status status :status-text status-text})))})
     prom)))

;;; This is used by the server/planner to reload the conversation.
;;; Unlike chat/get-conversation, the function for (lookup-fn :get-conversation),
;;; it doesn't resume-conversation because planning is already underway.
(register-fn
 :update-conversation-text
 (fn [{:keys [pid cid pname]}]
   (assert (keyword? pid))
   (-> (get-conversation-http pid cid)
       (p/then (fn [resp]
                 (log! :info (str "update-conversation-text: msg count = " (-> resp :conv count)))
                 (update-common-info! resp)
                 ((lookup-fn :set-current-project) pname)
                 ((lookup-fn :set-cs-msg-list) (:conv resp))
                 ((lookup-fn :set-code) (:code resp)))))))
