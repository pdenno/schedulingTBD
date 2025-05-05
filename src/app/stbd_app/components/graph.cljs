(ns stbd-app.components.graph
    (:require
   [applied-science.js-interop :as j]
   [helix.core         :as helix :refer [defnc $]]
   [helix.dom          :as dom]
   [helix.hooks        :as hooks]
   ["mermaid/dist/mermaid.esm.mjs$default" :as Mermaid :refer [initialize render]]
   ["@mui/material/Box$default" :as Box]
   [promesa.core        :as p]
   [stbd-app.util       :as util :refer [register-fn]]
   [taoensso.telemere  :refer [log!]]))

(def ^:diag diag (atom nil))

;;;(def simple-graph "graph TB\na-->b")
;;;(def less-simple-graph "graph TD\nA[Client] --> B[Load Balancer]\nB --> C[Server01]\nB --> D[Server02]")

;;; ToDo: Wrap this in a Box and make scroll bars dynamic.
(defnc GraphPane [{:keys [init-graph]}]
  (let [graph-ref (hooks/use-ref nil)
        [graph-cmd set-graph-cmd] (hooks/use-state init-graph)]
    (hooks/use-effect :once
      (log! :info (str "Initialization: init-graph = " init-graph))
      (register-fn :set-graph-cmd set-graph-cmd)
      (initialize (clj->js {:startOnLoad false, :securityLevel "loose"})))
    (hooks/use-effect [graph-cmd]
      (when-let [gdiv (j/get graph-ref :current)]
        (-> (render "anythingBut-graphDiv" graph-cmd)
            (p/then (fn [svg] (j/assoc! gdiv :innerHTML (j/get svg :svg))))
            (p/catch (fn [error] (log! :error (str "Error in Mermaid: " error)))))))
    ($ Box {:sx #js {:display "flex", :flexDirection "column" :height "100%" :width "100%" :alignItems "stretch" :overflowY "auto"}}
       (dom/div {:ref graph-ref :id "graphDiv"}))))
