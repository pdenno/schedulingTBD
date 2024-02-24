(ns stbd-app.components.project
  "A React component to select projects and get the initial list of projects from the server."
  (:require
   [applied-science.js-interop :as j]
   [clojure.string :as str]
   ["@mui/material/FormControl$default" :as FormControl]
   ["@mui/material/MenuItem$default" :as MenuItem]
   ["@mui/material/Select$default" :as Select]
   [helix.core    :as helix :refer [defnc $]]
   [stbd-app.components.editor :refer [set-editor-text]]
   #_[taoensso.timbre :as log :refer-macros [info debug log]]))

(def diag (atom nil))

(defn proj-id2nice
  "For the argument :project/id keyword, return a string with
    - the dashes replaced by spaces, and
    - words capitalized."
  [k]
  (if (= k "START-A-NEW-PROJECT")
    "START A NEW PROJECT"
    (as-> k ?s
      (name ?s)
      (str/split ?s #"-")
      (map str/capitalize ?s)
      (interpose " " ?s)
      (apply str ?s))))

(defn order-projects
  "Return a vector of maps containing :project/id and :project/name where
   current is first, 'new-project' is second and the remainder are all other
   known projects sorted alphabetically."
  [current proj-infos]
  (let [cid (:project/id current)
        new-proj {:project/id :START-A-NEW-PROJECT :project/name "START A NEW PROJECT"}]
     (->> proj-infos
          (filter #(not= cid (:project/id %)))
          (sort-by :project/name)
          (into [current new-proj])
          (map :project/id)
          clj->js)))

;;; current-project and content of the others vector are keywords.
;;; As a rule, we keep data as it is maintained in the DB until we need a string for React.
;;; Nicolas: Keywords translate to strings with clj->js which is recursive. Or you can use clojure.core/name on keywords or strings.
(defnc SelectProject
  "This calls change-project-fn to have the parent change current-project and others.
    - current-proj    : a map containing :project/id and :project/name.
    - proj-infos      : a vector of all other projects, same form as current-proj.
    - change-proj-fn : a function, typically (always?) core/change-project."
  [{:keys [current-proj proj-infos change-proj-fn]}]
  (when (and current-proj proj-infos)
    (let [menu-list (atom (order-projects current-proj proj-infos))]
      ($ FormControl {:size "small" ; small makes a tiny difference
                      :sx {:height "25%" :maxHeight "25%"}}
         ($ Select {:variant "filled"
                    :sx {:style {:height "20px"}}
                    :value (-> current-proj :project/id clj->js)
                    ;; onChange: communicates up to parent to change the conversation too.
                    :onChange (fn [_e v]
                                (let [proj-str (j/get-in v [:props :value])
                                      proj (some #(when (= proj-str (-> % :project/id name)) %) proj-infos)]
                                  (reset! menu-list (order-projects proj proj-infos))
                                  (when (not= proj-str "START-A-NEW-PROJECT")
                                    (change-proj-fn proj))))}
            (for [p @menu-list]
              ($ MenuItem {:key p :value p} (proj-id2nice p))))))))
