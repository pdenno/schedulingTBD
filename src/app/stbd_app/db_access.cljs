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
  "Return a promise that will resolve to the vector of a maps describing the complete conversation so far.
   Example of what the promise resolves to:
   {:conv-for id :conv [{:message/from :system :message/content [{:msg-text/string 'Hi!'}]}]}."
  [pid]
  (log/info "Call to get-conversation for" pid)
  (let [prom (p/deferred)]
    (GET (str "/api/get-conversation?project-id=" (name pid) "&client-id=" @ws/client-id)
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/get-conversation"
                                                    {:status status :status-text status-text})))})
    prom))

;;; This is like :core-load-proj except that it doesn't do (ws/send-msg {:dispatch-key :resume-conversation...}).
;;; It is used when, for example, you start a surrogate.
(register-fn
 :request-conversation
 (fn [obj] (let [prom (get-conversation (:pid obj))]
             (-> prom
                 (p/then (fn [resp] ((lookup-fn :set-conversation) resp) resp))
                 (p/then (fn [resp] ((lookup-fn :set-code) (:code resp)) resp))))))
