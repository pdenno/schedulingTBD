(ns stbd-app.util
  (:require
   [helix.core :as helix :refer [$]]
   [taoensso.telemere :refer [log!]]))

(def ^:diag diag (atom nil))

(defn server-port
  "Get the server port from the current page URL.
   Since the client is served from the same server, we can detect the port dynamically."
  []
  (.-port js/location)) ; ToDo: Get this from config.

;;; ToDo: Is there a react way? It looks like react doesn't have this notion.
(def ^:diag root "The application's root 'Symbol(react.element)' element" (atom nil))

#_(defn fn? [arg] ; I didn't see this in the docs!
    (= "Function" (j/get-in arg [:constructor :name])))

;;; ToDo: Is there are idiomatic React way to do this? (Find way around structure.)
(def component-refs
  "Some components instances are named and their refs stored here."
  (atom {}))

(def dispatch-table
  "A map from keys to functions used to call responses from clients."
  (atom nil))

(defn lookup-fn [k] (-> @dispatch-table (get k)))

(defn register-fn
  "Add a function to the websocket dispatch table."
  [k func]
  (swap! dispatch-table #(assoc % k func))
  (log! :debug (str "Registered function for websocket: " k)))

(def common-info "Map tracking current project/id and :conversation/id."
  (atom {:cid :process}))

(defn update-common-info!
  "Update the common-info atom from an app action or server response.
   Returns the value of the common-info atom."
  [{:keys [current-project project-id cid active?] :as resp}]
  ;(log! :info (str "update-common-info: resp = " resp))
  (swap! common-info
         (fn [info]
           (let [pid (or (:project/id resp) (:pid resp))]
             (cond-> info
               (not (contains? resp :cid)) (assoc :cid :process)
               (boolean? active?) (assoc :active? active?)
               current-project (assoc :pid current-project)
               project-id (assoc :pid project-id)
               pid (assoc :pid pid)
               cid (assoc :cid cid))))))

(defn now []
  (js/Date. (.now js/Date)))
