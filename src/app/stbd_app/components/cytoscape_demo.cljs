(ns stbd-app.components.cytoscape-demo
  "ClojureScript port of the GoJS React demo."
  (:require
   [applied-science.js-interop :as j]
   [helix.core :as helix :refer [defnc $]]
   [helix.hooks :as hooks]
   ["cytoscape" :as cytoscape-lib]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/Dialog$default" :as Dialog]
   [taoensso.telemere :refer [log!]]))

(def ^:diag diag (atom nil))


(defnc CytoscapeDemoModal [{:keys [graph]}]
  (let [cy-ref (hooks/use-ref nil)
        modal (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]

    (hooks/use-effect
     [open]
     (log! :info (str "Effect: open=" open " ref=" (j/get cy-ref :current)))
     (when open
       (js/setTimeout
        (fn []
          (log! :info (str "Timeout: ref=" (j/get cy-ref :current)))
          (when (j/get cy-ref :current)
            (try
              (log! :info "About to create Cytoscape...")
              (log! :info (str "cytoscape-lib: " cytoscape-lib))
              (log! :info (str "cytoscape-lib.default: " (.-default cytoscape-lib)))
              (log! :info (str "cytoscape-lib keys: " (js/Object.keys cytoscape-lib)))
              (let [cy-instance (cytoscape-lib
                                 (clj->js
                                  {:container (j/get cy-ref :current)
                                   :elements [{:data {:id "one" :label "Node 1"}}
                                             {:data {:id "two" :label "Node 2"}}
                                             {:data {:source "one" :target "two" :label "Edge 1-2"}}]
                                   :style [{:selector "node"
                                           :style {:background-color "#666" :label "data(label)" :width 60 :height 30}}
                                          {:selector "edge"
                                           :style {:width 3 :line-color "#ccc" :target-arrow-color "#ccc" :target-arrow-shape "triangle"}}]
                                   :layout {:name "grid"}}))]
                (log! :info (str "Cytoscape instance created: " cy-instance)))
              (catch js/Error e
                (log! :info (str "Error creating Cytoscape: " e))))))
        500)))

    (letfn [(handle-open [] (set-open true))
            (handle-close [] (set-open false))]
      ($ Box {:ref modal}
         ($ Button {:onClick handle-open :color "warning"} "Cytoscape Demo")
         ($ Dialog {:open open :onClose handle-close :fullScreen true :maxWidth false}
            ($ "div" {:ref cy-ref
                      :style {:width "600px" :height "400px" :border "2px solid red" :margin "20px"}}))))))
