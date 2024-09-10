(ns stbd-app.util
  (:require
   [taoensso.timbre :as log  :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

(def server-port 3300) ; ToDo: Get this from config.

;;; ToDo: Is there a react way? It looks like react doesn't have this notion.
(def ^:diag root "The application's root 'Symbol(react.element)' element" (atom nil))

#_(defn fn? [arg] ; I didn't see this in the docs!
  (= "Function" (j/get-in arg [:constructor :name])))

;;; ToDo: Is there are idiomatic React way to do this? (Find way around structure.)
(def component-refs
  "Some components instances are named and their refs stored here."
  (atom  {}))

(def dispatch-table
  "A map from keys to functions used to call responses from clients."
  (atom nil))

(defn lookup-fn [k] (-> @dispatch-table (get k)))

(defn register-fn
  "Add a function to the websocket dispatch table."
  [k func]
  (swap! dispatch-table #(assoc % k func))
  #_(log/info "Registered function for websocket:" k))

(def common-info "Map tracking current project/id and :conversation/id." (atom {:conversation/id :process}))

(defn update-common-info!
  "Update the common-info atom from an app action or server response."
  [resp]
  (swap! common-info
         (fn [info]
           (let [{:keys [current-project project-id conv-id]} resp
                 {pid :project/id} resp]
             (cond-> info
               (not (contains? resp :conv-id))     (assoc :conversation/id :process)
               current-project   (assoc :project/id current-project)
               project-id        (assoc :project/id project-id)
               pid               (assoc :project/id pid)
               conv-id           (assoc :conversation/id conv-id))))))

(defn now []
  (js/Date. (.now js/Date)))
