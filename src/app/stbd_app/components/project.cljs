(ns stbd-app.components.project
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

(defn list-projects
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
  [{:keys [projects-info]}]
  (let [[project set-project] (hooks/use-state (:current-project projects-info))]
    ($ FormControl {:size "small" ; small makes a tiny difference
                    :sx {:height "25%"
                         :maxHeight "25%"}}
       ($ Select {:variant "filled"
                  :sx {:style {:height "20px"}}
                  :value project
                  :onChange (fn [_e v]
                              (let [proj-name (j/get-in v [:props :value])
                                    #_#_proj (get-project proj-name)]
                                (set-project proj-name)
                                (set-editor-text "result" "Ctrl-Enter above to execute.")
                                (set-editor-text "code-editor" "Fix this")))}
          (for [p (into ["Start a new project"] (:projects projects-info))]
            ($ MenuItem {:key p :value p} p))))))
