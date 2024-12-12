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
   [promesa.core       :as p]
   [stbd-app.db-access :as dba]
   [stbd-app.ws        :as ws    :refer [send-msg]]
   [stbd-app.util      :as util  :refer [common-info lookup-fn register-fn update-common-info!]]
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

;;; ToDo: Find out why current-proj is nil in SelectProject when starting, and sometimes a string here.
(defn order-projects
  "Return a vector of maps containing :project/id, :project/name and :menu-text where
   current is first, 'new-project' is second and the remainder are all other
   known projects sorted alphabetically."
  [current proj-infos]
  (reset! diag {:current current :proj-infos proj-infos})
  (let [current (cond (nil? current)     #:project{:name "New Project" :id :new-project}
                      (map? current)     (assoc current :menu-text (menu-text current)) ; Awkward assoc. Necessary!
                      (string? current)  (-> {:project/name current}
                                             (assoc :project/id (as-> current ?s
                                                                     (str/trim ?s)
                                                                     (str/lower-case ?s)
                                                                     (str/split ?s #"\s+")
                                                                     (interpose "-" ?s)
                                                                     (apply str ?s)
                                                                     (keyword ?s)))
                                             (assoc :menu-text (menu-text current)))
                      :else (log! :error (str "current = " current)))
        proj-infos (mapv #(assoc % :menu-text (menu-text %)) proj-infos)
        others (->> proj-infos (remove #(= current %)) (sort-by :project/name))]
    (if (= :NEW-PROJECT (:project/id current))
      (into [{:project/id :NEW-PROJECT :menu-text "NEW PROJECT"}] others)
      (into [current] others))))

;;; current-project and content of the others vector are keywords.
(defnc SelectProject
  "This calls chat's :get-conversation, which calls server to the the project, learn the active conversation, and resume the planner.
      - current-proj    : a map containing :project/id and :project/name.
      - proj-infos      : a vector of all projects, includes current-proj."
  []
  (let [[current set-current]       (hooks/use-state nil)
        [proj-infos set-proj-infos] (hooks/use-state nil)
        menu-infos (atom [])]
    (letfn [(update-proj-info! [] ; ToDo: Make this so that is usable both in :once and the Select :onChange.
              (-> (dba/get-project-list) ;  Resolves to map with client's :current-project :others, and cid the first two are maps of :projec/id :project/name.
                  (p/then (fn [{:keys [current-project others cid] :as p-list}]
                            (update-common-info! p-list)
                            (set-current current-project)
                            (set-proj-infos (conj others current-project))
                            ((lookup-fn :get-conversation) (:project/id current-project) cid)))))]
      (register-fn :set-current-project set-current) ; ToDo: make this go away.
      (hooks/use-effect :once (update-proj-info!))
      ;; This needed or doesn't show first time:
      ;; Note also that the order current then current-proj! Ugh!
      (reset! menu-infos (order-projects current proj-infos))
      ($ Stack {:direction "row"}
         ($ IconButton {:onClick
                        (fn [_]
                          ((lookup-fn :clear-msgs))
                          (set-current {:project/id :NEW-PROJECT})
                          (update-common-info! {:project/id :NEW-PROJECT :cid :process})
                          ((lookup-fn :update-code) {:text "Together, we'll put a MiniZinc solution here soon!"})
                          (ws/send-msg {:dispatch-key :start-conversation}))}
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
                                     (update-common-info! proj)
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
