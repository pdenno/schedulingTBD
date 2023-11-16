(ns stbd-app.components.project
  "A React component to select projects and get the initial list of projects from the server."
  (:require
   [ajax.core :refer [GET POST]]
   [applied-science.js-interop :as j]
   ["@mui/material/FormControl$default" :as FormControl]
   ["@mui/material/MenuItem$default" :as MenuItem]
   ["@mui/material/Select$default" :as Select]
   [helix.core    :as helix :refer [defnc $]]
   [helix.hooks   :as hooks]
   [promesa.core  :as p]
   [stbd-app.components.editor :refer [set-editor-text]]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def diag (atom nil))
(def project-names "A vector of all projects for the system DB." (atom nil))

;;; current-project and content of the others vector are keywords.
;;; As a rule, we keep data as it is maintained in the DB until we need a string for React.
;;; Nicolas: Keywords translate to strings with clj->js which is recursive. Or you can use clojure.core/name on keywords or strings.
(defnc SelectProject
  "This calls change-project-fn to have the parent change current-project and others.
    - current-project : the project-id keyword.
    - others : a vector of other projects we know about by way of the DB."
  [{:keys [current-project others change-project-fn]}]
  ;(log/info "In SelectProject: current-project = " current-project "others =" others)
  (let [current-project (or current-project "Waiting...")
        menu-list (atom (-> (into [current-project "START A NEW PROJECT"] others) clj->js))]
    ($ FormControl {:size "small" ; small makes a tiny difference
                    :sx {:height "25%" :maxHeight "25%"}}
       ($ Select {:variant "filled"
                  :sx {:style {:height "20px"}}
                  :value (clj->js current-project)
                  ;; onChange: communicates up to parent to change the conversation too.
                  :onChange (fn [_e v]
                              (let [proj-name (-> (j/get-in v [:props :value]) keyword)]
                                (reset! menu-list (-> (into [proj-name "START A NEW PROJECT"]
                                                            (replace {proj-name current-project} others))
                                                      clj->js))
                                (when (not= (name proj-name) "START A NEW PROJECT") ; ToDo: More here (and menu-list)
                                  (change-project-fn proj-name))))}
          (for [p @menu-list]
            ($ MenuItem {:key p :value p} p))))))
