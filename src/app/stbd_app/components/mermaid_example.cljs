(ns stbd-app.components.mermaid-example
    (:require
   [applied-science.js-interop :as j]
   [helix.core         :as helix :refer [defnc $]]
   [helix.dom          :as d]
   [helix.hooks        :as hooks]
   ["mermaid/dist/mermaid.esm.mjs$default" :as Mermaid :refer [initialize render]]
   ["@mui/material/Box$default" :as Box]
   [promesa.core        :as p]
   [stbd-app.util       :as util :refer [lookup-fn register-fn update-common-info!]]
   [stbd-app.ws         :as ws]
   [taoensso.telemere  :refer [log!]]))

(def diag (atom nil))

(defnc MermaidExample [{:keys [graph]}]
  (log! :info (str "Mermaid!: " graph))
  (let [graph-ref (hooks/use-ref nil)]
    (hooks/use-effect :once
      (when-let [gref (j/get graph-ref :current)]
        (log! :info "MermaidExample initializing")
        (initialize (clj->js {:logLevel 1, :startOnLoad false, :securityLevel "loose"}))
        (-> (render "graphDiv" graph)
            (p/then #(reset! diag %))
            (p/then #(j/assoc! gref :innerHTML %)))))
    (d/div {:ref graph-ref :id "graphDiv"})))
