(ns stbd-app.db-access
  "Functions for HTTP-based reading/writing from/to the server."
  (:require
   [ajax.core        :refer [GET POST]]
   [promesa.core    :as p]
   [stbd-app.util   :as util :refer [client-id]]
   [stbd-app.ws     :as ws]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

;;; project-infos are maps with keys :project/name and :project/id, at least.
(defn get-project-list
  "Return a promise that will resolve to a map {:current-project <project-info> :others [<project-info>...]}."
  []
  (let [prom (p/deferred)]
    (GET (str "/api/list-projects?client-id=" client-id)
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/list-projects"
                                                    {:status status :status-text status-text})))})
    prom))

;;; {:conv-for id :conv [{:message/from :system :message/content [{:msg-text/string "You want to start a new project?"}]}]})
(defn get-conversation
  "Return a promise that will resolve to the vector of a maps describing the complete conversation so far.
   Example of what the promise resolves to:
   {:conv-for id :conv [{:message/from :system :message/content [{:msg-text/string 'Hi!'}]}]}."
  [{:project/keys [id]}]
  (log/info "Call to get-conversation for" id)
  (let [prom (p/deferred)]
    (GET (str "/api/get-conversation?project-id=" (name id) "&client-id=" client-id)
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/get-conversation"
                                                    {:status status :status-text status-text})))})
    (ws/send-msg {:dispatch-key :resume-conversation :project-id id})
    (reset! diag prom)
    prom))
