(ns stbd-app.components.project
  "A React component to select projects and get the initial list of projects from the server."
  (:require
   [applied-science.js-interop :as j]
   [clojure.string :as str]
   [helix.hooks                :as hooks]
   ["@mui/icons-material/PlayCircleFilledWhiteRounded$default" :as Play]
   ["@mui/icons-material/PauseCircleFilled$default" :as Pause]
   ["@mui/icons-material/RocketLaunch$default" :as RocketLaunch]
   ["@mui/material/Box$default"          :as Box]
   ["@mui/material/ButtonGroup$default"  :as ButtonGroup]
   ["@mui/material/FormControl$default" :as FormControl]
   ["@mui/material/IconButton$default"  :as IconButton]
   ["@mui/material/MenuItem$default"    :as MenuItem]
   ["@mui/material/Select$default"      :as Select]
   ["@mui/material/Stack$default"       :as Stack]
   ["@mui/material/TextField$default"    :as TextField]
   ;;["@mui/material/styles"              :as styles :refer [createTheme themeOptions ThemeProvider]] ; WIP
   [helix.core         :as helix :refer [defnc $]]
   [promesa.core       :as p]
   [stbd-app.db-access :as dba]
   [stbd-app.ws        :as ws]
   [stbd-app.util      :as util  :refer [lookup-fn register-fn common-info update-common-info!]]
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

;;; ToDo: This is currently causing a warning:
;;; A component is changing an uncontrolled input to be controlled.
;;; This is likely caused by the value changing from undefined to a defined value, which should not happen.
;;; Decide between using a controlled or uncontrolled input element for the lifetime of the component.
;;; More info: https://reactjs.org/link/controlled-components
(defnc SelectProject
  "This calls chat's :get-conversation, which calls server to the the project, learn the active conversation, and resume the planner.
      - current-proj    : a map containing :project/id and :project/name.
      - proj-infos      : a vector of all projects, includes current-proj."
  []
  (let [[current set-current]             (hooks/use-state nil)
        [proj-infos set-proj-infos]       (hooks/use-state nil)
        [play-active? set-play-active?]   (hooks/use-state true)
        [pause-active? set-pause-active?] (hooks/use-state false)
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
      #_(hooks/use-effect [play-active?]
        (update-common-info! {:active? (not play-active?)})
        (let [{:keys [active? pid cid]} @common-info] ; If the play button is active, it is because execution is paused.
          (when (and active? pid cid)
            (ws/send-msg {:dispatch-key :set-execution-status! :pid pid :status (if active? :running :paused)}))
          ;;(log! :info (str "Resuming conversation pid = " pid " cid = " cid))
          ;; (ws/send-msg {:dispatch-key :resume-conversation :pid pid :cid cid}))
          ))
      ;; This needed or doesn't show first time:
      ;; Note also that the order current then current-proj! Ugh!
      (reset! menu-infos (order-projects current proj-infos))
      ($ Stack {:direction "row"}
         ($ Box {:sx #js {:minWidth "50px"}})
         ($ ButtonGroup
            ($ IconButton {:onClick (fn [_]
                                      (set-play-active? (not play-active?))
                                      (set-pause-active? (not pause-active?)))}
                  (if play-active?
                    ($ Play {:sx #js {:color "white" :opacity "100%"}})
                    ($ Play {:sx #js {:color "white" :opacity "40%"}})))
            ($ IconButton {:onClick (fn [_]
                                      (set-pause-active? (not pause-active?))
                                      (set-play-active?  (not play-active?)))}
               (if pause-active?
                 ($ Pause  {:sx #js {:color "white" :opacity "100%"}})
                 ($ Pause  {:sx #js {:color "white" :opacity "40%"}}))))
         ($ TextField {:label (if play-active? "paused" "running")
                       :variant "standard" :size "small" :sx #js{:maxWidth "80px" :input {:color "white"}}})
         ($ FormControl {:size "small"} ; small makes a tiny difference, :sx's :margin and :height do not.
            ($ Select {:variant "filled"
                       :sx {:height "3vh"}
                       :value (or (-> @menu-infos first :menu-text) "")
                       ;; onChange: communicates up to parent to change the conversation too.
                       :onChange (fn [_e v]
                                   (let [proj-str (j/get-in v [:props :value])
                                         proj (some #(when (= proj-str (:menu-text %)) %) @menu-infos)]
                                     (set-current proj)
                                     (set-play-active? true)
                                     (update-common-info! proj)
                                     (log! :info (str "Select project: proj = " proj))
                                     (reset! menu-infos (order-projects proj proj-infos))
                                     ((lookup-fn :get-conversation) (:project/id proj))))}
               (for [p (map :menu-text @menu-infos)]
                 ($ MenuItem {:key p :value p} p))))
         ($ IconButton {:onClick
                        (fn [_]
                          ((lookup-fn :clear-msgs))
                          (set-current {:project/id :NEW-PROJECT})
                          (update-common-info! {:project/id :NEW-PROJECT :cid :process})
                          ((lookup-fn :update-code) {:text "Together, we'll put a MiniZinc solution here soon!"})
                          (ws/send-msg {:dispatch-key :start-conversation}))}
            ($ RocketLaunch {:sx #js {:color "white" :opacity "100%"}}))))))

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
