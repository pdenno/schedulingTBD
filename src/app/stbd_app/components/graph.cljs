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

(defnc GraphPane [{:keys [init-graph]}]
  (let [graph-ref (hooks/use-ref nil)
        [graph-cmd set-graph-cmd] (hooks/use-state init-graph)]
    (hooks/use-effect :once
      (register-fn :set-graph-pane set-graph-cmd)
      (initialize (clj->js {:startOnLoad false, :securityLevel "loose"})))
    (hooks/use-effect [graph-cmd]
      (when-let [gdiv (j/get graph-ref :current)]
        (-> (render "graph-svg-id" graph-cmd) ; "graph-svg-id" is the svg id, not the dom/div id. It isn't used.
            (p/then (fn [svg] (j/assoc! gdiv :innerHTML (j/get svg :svg))))
            (p/catch (fn [error] (log! :error (str "Error in Mermaid: " error)))))))
    ($ Box {:sx #js {:display "flex", :flexDirection "column" :height "100%" :width "100%" :alignItems "stretch" :overflowY "auto"}}
       (dom/div {:ref graph-ref :id "graphDiv"}))))
