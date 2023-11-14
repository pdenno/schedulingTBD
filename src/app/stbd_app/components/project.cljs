(ns stbd-app.components.project
  "A React component to select projects and get the initial list of projects from the server."
  (:require
   [ajax.core :refer [GET]]
   [applied-science.js-interop :as j]
   ["@mui/material/FormControl$default" :as FormControl]
   ["@mui/material/MenuItem$default" :as MenuItem]
   ["@mui/material/Select$default" :as Select]
   [helix.core    :as helix :refer [defnc $]]
   [helix.hooks   :as hooks]
   [promesa.core  :as p]
   [stbd-app.components.editor :refer [set-editor-text]]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def project-names "A vector of all projects for the system DB." (atom nil))

(defn get-project-list
  "Return a promise that will resolve to a map listing projects and the current project."
  []
  (let [prom (p/deferred)]
    (GET "/api/list-projects"
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/list-projects"
                                                    {:status status :status-text status-text})))})
    prom))

(defnc SelectProject
  [{:keys [projects change-project-fn]}]
  (log/info "In SelectProject: projects = " projects)
  (let [[project set-project] (hooks/use-state (or (first projects) "Start a new project"))]
    ($ FormControl {:size "small" ; small makes a tiny difference
                    :sx {:height "25%"
                         :maxHeight "25%"}}
       ($ Select {:variant "filled"
                  :sx {:style {:height "20px"}}
                  :value project
                  :onChange (fn [_e v]
                              (let [proj-name (j/get-in v [:props :value])]
                                (set-project proj-name)
                                (change-project-fn proj-name) ; communicates up to parent.
                                (set-editor-text "code-editor" "% We will put MiniZinc code here.")))}
          (for [p (into [(first projects) "Start a new project"] (rest projects))]
            ($ MenuItem {:key p :value p} p))))))
