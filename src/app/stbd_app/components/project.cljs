(ns stbd-app.components.project
  "A React component to select projects and get the initial list of projects from the server."
  (:require
   [applied-science.js-interop :as j]
   [clojure.string :as str]
   [helix.hooks                :as hooks]
   ["@mui/icons-material/RocketLaunch$default" :as RocketLaunch]
   ["@mui/material/FormControl$default" :as FormControl]
   ["@mui/material/IconButton$default"  :as IconButton]
   ["@mui/material/MenuItem$default"    :as MenuItem]
   ["@mui/material/Select$default"      :as Select]
   ["@mui/material/Stack$default"       :as Stack]
   ;;["@mui/material/styles"              :as styles :refer [createTheme themeOptions ThemeProvider]] ; WIP
   [helix.core         :as helix :refer [defnc $]]
   [stbd-app.ws        :as ws]
   [stbd-app.util      :as util :refer [lookup-fn register-fn update-common-info!]]
   [taoensso.telemere  :refer [log!]]))

(def ^:diag diag (atom nil))

(defn menu-text
  "Add :menu-text to argument project-info."
  [{:project/keys [id]}]
  (when id
    (as-> id ?s ; ToDo: What is the point of :project/name if I'm doing this?
      (name ?s)
      (str/split ?s #"-")
      (map str/capitalize ?s)
      (interpose " " ?s)
      (apply str ?s))))

(defn order-projects
  "Return a vector of maps containing :project/id, :project/name and :menu-text where
   current is first, 'new-project' is second and the remainder are all other
   known projects sorted alphabetically."
  [current proj-infos]
  (let [current (assoc current :menu-text (menu-text current)) ; Awkward assoc. Necessary!
        proj-infos (mapv #(assoc % :menu-text (menu-text %)) proj-infos)
        others (->> proj-infos (remove #(= current %)) (sort-by :project/name))]
    (if (= :START-A-NEW-PROJECT (:project/id current))
      (into [{:project/id :START-A-NEW-PROJECT :menu-text "NEW PROJECT"}] others)
      (into [current] others))))

;;; current-project and content of the others vector are keywords.
(defnc SelectProject
  "This calls chat's :get-conversation, which calls server to the the project, learn the active conversation, and resume the planner.
      - current-proj    : a map containing :project/id and :project/name.
      - proj-infos      : a vector of all projects, includes current-proj."
  [{:keys [current-proj proj-infos]}]
  (let [[current set-current] (hooks/use-state current-proj)
        menu-infos (atom [])]
    (register-fn :set-current-project set-current)
    (when current-proj
      (reset! menu-infos (order-projects (or current current-proj) proj-infos)) ; This one needed or doesn't show first time.
      ($ Stack {:direction "row"}
         ($ IconButton {:onClick
                        (fn [_]
                          (set-current {:project/id :START-A-NEW-PROJECT})
                          (update-common-info! {:project/id :START-A-NEW-PROJECT :cid :process})
                          ((lookup-fn :update-code) {:text "Together, we'll put a MiniZinc solution here soon!"})
                          ((lookup-fn :get-conversation) :START-A-NEW-PROJECT))} ; Calls resume-conversation-plan
            ($ RocketLaunch))
         ($ FormControl {:size "small"} ; small makes a tiny difference, :sx's :margin and :height do not.
            ($ Select {:variant "filled"
                       :sx {:height "3vh"}
                       :value (-> @menu-infos first :menu-text)
                       ;; onChange: communicates up to parent to change the conversation too.
                       :onChange (fn [_e v]
                                   (let [proj-str (j/get-in v [:props :value])
                                         proj (some #(when (= proj-str (:menu-text %)) %) @menu-infos)]
                                     (set-current proj)
                                     (log! :info (str "Select project: proj = " proj))
                                     (reset! menu-infos (order-projects proj proj-infos))
                                     ((lookup-fn :get-conversation) (:project/id proj))))}
               (for [p (map :menu-text @menu-infos)]
                 ($ MenuItem {:key p :value p} p))))))))

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
