(ns stbd-app.components.mermaid-example
    (:require
   [applied-science.js-interop :as j]
   [helix.core         :as helix :refer [defnc $]]
   [helix.hooks                :as hooks]
   ["mermaid/dist/mermaid.esm.mjs" :as Mermaid]
   ;; ["mermaid/dist/mermaid.esm.mjs$default" :as Mermaid]   ;; Same result
   ["@mui/material/Box$default" :as Box]
   [promesa.core        :as p]
   [stbd-app.util       :as util :refer [lookup-fn register-fn update-common-info!]]
   [stbd-app.ws         :as ws]
   [taoensso.telemere  :refer [log!]]))

(def diag (atom nil))

(defnc MermaidExample [{:keys [graph]}]
  (let [API (j/get-in Mermaid [:default :mermaidAPI])]
    (log! :info (str "Mermaid!: " graph))
    (hooks/use-effect :once
      (log! :info "initializing")
      (.initialize API)
      (-> (.render API "mermaid diagram" "graph TD\n A-->B")
          (p/then (fn [obj] (log! :info (str "obj = " obj)) (reset! diag obj)))
          (p/then (fn [_] (log! :info "After render.")))))
    ($ Box {} "hello")))
