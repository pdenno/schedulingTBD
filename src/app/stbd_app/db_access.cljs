(ns stbd-app.db-access
  "Functions for HTTP-based reading/writing from/to the server."
  (:require
   [ajax.core :refer [GET POST]]
   [promesa.core :as p]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(defn get-project-list
  "Return a promise that will resolve to a map {:current-project <keyword> :others [<keyword>...]}."
  []
  (let [prom (p/deferred)]
    (GET "/api/list-projects"
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/list-projects"
                                                    {:status status :status-text status-text})))})
    prom))

(defn set-current-project
  [project-id]
  (log/info "Call to db-set-current-project: project-id =" project-id)
  (let [prom (p/deferred)]
    (POST (str "/api/set-current-project?project-id=" (name project-id))
          {:params {:project-id project-id} ; ToDo: Really not sure about this!
           :timeout 2000
           :handler (fn [resp] (p/resolve! prom resp))
           :error-handler (fn [{:keys [status status-text]}]
                            (p/reject! prom (ex-info "CLJS-AJAX error on /api/set-current-project"
                                                     {:status status :status-text status-text})))})
    prom))

(defn get-conversation
  "Return a promise that will resolve to the vector of a maps describing the complete conversation so far."
  [project-id]
  (log/info "Call to get-conversation for" project-id)
  (let [prom (p/deferred)]
    (GET (str "/api/get-conversation?project-id=" project-id)
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/get-conversation"
                                                    {:status status :status-text status-text})))})
    prom))

(defn user-says
  "Add to the DB the user's text and respond with more dialog."
  [user-text]
  (let [prom (p/deferred)]
    (POST "/api/user-says"
          {:params {:user-text user-text}
           :timeout 30000
           :handler (fn [resp] (p/resolve! prom resp))
           :error-handler (fn [{:keys [status status-text]}]
                            (p/reject! prom (ex-info "CLJS-AJAX error on /api/user-says" {:status status :status-text status-text})))})
    prom))
