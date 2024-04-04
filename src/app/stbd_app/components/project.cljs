(ns stbd-app.components.project
  "A React component to select projects and get the initial list of projects from the server."
  (:require
   [applied-science.js-interop :as j]
   [clojure.string :as str]
   ["@mui/material/FormControl$default" :as FormControl]
   ["@mui/material/MenuItem$default"    :as MenuItem]
   ["@mui/material/Select$default"      :as Select]
   ;;["@mui/material/styles"              :as styles :refer [createTheme themeOptions ThemeProvider]] ; WIP
   [helix.core         :as helix :refer [defnc $]]
   [stbd-app.ws        :as ws]
   [taoensso.timbre    :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

(defn menu-text
  "Add :menu-text to argument project-info."
  [{:project/keys [id]}]
  (if (= id :START-A-NEW-PROJECT)
    "START A NEW PROJECT"
    (as-> id ?s
      (name ?s)
      (str/split ?s #"-")
      (map str/capitalize ?s)
      (interpose " " ?s)
      (apply str ?s))))

(def new-proj-entry {:project/id :START-A-NEW-PROJECT :project/name "START A NEW PROJECT" :menu-text "START A NEW PROJECT"})

(defn order-projects
  "Return a vector of maps containing :project/id and :project/name where
   current is first, 'new-project' is second and the remainder are all other
   known projects sorted alphabetically."
  [current proj-infos]
  (let [current (assoc current :menu-text (menu-text current)) ; Awkward assoc. Necessary!
        proj-infos (mapv #(assoc % :menu-text (menu-text %)) proj-infos)
        others (->> proj-infos (remove #(= current %)) (sort-by :project/name))
        res (cond->> others
              (not= current new-proj-entry) (into [current new-proj-entry])
              (=    current new-proj-entry) (into [new-proj-entry]))]
    res))

;;; current-project and content of the others vector are keywords.
;;; As a rule, we keep data as it is maintained in the DB until we need a string for React.
;;; Keywords translate to strings with clj->js which is recursive. Or you can use clojure.core/name on keywords or strings.
(defnc SelectProject
  "This calls change-project-fn to have the parent change current-project and others.
    - current-proj    : a map containing :project/id and :project/name.
    - proj-infos      : a vector of all projects, includes current-proj.
    - change-proj-fn : a function, typically (always?) core/change-project."
  [{:keys [current-proj proj-infos]}]
  (when current-proj
      (let [proj-infos+ (order-projects current-proj proj-infos)]
        ($ FormControl {:size "small" ; small makes a tiny difference
                        :sx {:margin "2vh" ; Whatever! It isn't being used.
                             :height "2vh"} #_{:height "25%" :maxHeight "25%"}}
           ($ Select {:variant "filled"
                      :sx {:height "3vh"}
                      :value (-> proj-infos+ first :menu-text)
                      ;; onChange: communicates up to parent to change the conversation too.
                      :onChange (fn [_e v]
                                  (let [proj-str (j/get-in v [:props :value])
                                        proj (some #(when (= proj-str (:menu-text %)) %) proj-infos+)]
                                    (@ws/change-proj-fn proj)))}
              (for [p (map :menu-text proj-infos+)]
                ($ MenuItem {:key p :value p} p)))))))

;;; This is WIP
;;; https://zenoo.github.io/mui-theme-creator/
#_(def my-themeOptions
  (clj->js {:pallet {:mode "light"
                     :primary   {:main "#3f51b5"}
                     :secondary {:main "#f50057"}}}))

#_(def useStyles
  (createTheme
   (fn [theme]
     (clj->js
      {:root
       {"boxShadow" "none"}

       :flexGrow
       {:flexGrow 1}

       :signOutButton
       {:maginLeft ((.-spacing theme) 1)}}))))
