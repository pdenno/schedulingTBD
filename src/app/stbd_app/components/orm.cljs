(ns stbd-app.components.orm
  "A React component to display Object-Role Modeling (ORM) diagrams using Cytoscape.js."
  (:require
   [applied-science.js-interop :as j]
   [clojure.edn :as edn]
   [helix.core :as helix :refer [defnc $]]
   [helix.hooks :as hooks]
   ["cytoscape" :as cytoscape-lib]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/Dialog$default" :as Dialog]
   [stbd-app.util :as util :refer [register-fn]]
   [taoensso.telemere :refer [log!]]))

(def ^:diag diag (atom nil))

;;; Utility functions for ORM to Cytoscape transformation

(defn entity-nodes
  "Extract entity nodes from ORM data structure."
  [orm-data]
  (let [objects (:inquiry-area-objects orm-data)]
    (map (fn [obj]
           {:data {:id (:object-id obj)
                   :label (:object-id obj)
                   :definition (:definition obj)}
            :classes "entity-node"})
         objects)))

(defn relationship-nodes
  "Extract relationship nodes from ORM data structure."
  [orm-data]
  (let [fact-types (:fact-types orm-data)]
    (map (fn [fact-type]
           {:data {:id (:fact-type-id fact-type)
                   :label (:fact-type-id fact-type)
                   :arity (:arity fact-type)}
            :classes "relationship-node"})
         fact-types)))

(defn relationship-edges
  "Create edges connecting entities to relationships."
  [orm-data]
  (let [fact-types (:fact-types orm-data)]
    (mapcat (fn [fact-type]
              (let [rel-id (:fact-type-id fact-type)
                    objects (:objects fact-type)]
                (map-indexed (fn [idx obj]
                               {:data {:source obj
                                       :target rel-id
                                       :label (str "role-" (inc idx))}
                                :classes "role-edge"})
                             objects)))
            fact-types)))

(defn orm->cytoscape-elements
  "Transform ORM data structure to Cytoscape elements format."
  [orm-data]
  (when orm-data
    (let [parsed-data (if (string? orm-data)
                        (edn/read-string orm-data)
                        orm-data)
          inquiry-area (first (:inquiry-areas parsed-data))
          entities (entity-nodes inquiry-area)
          relationships (relationship-nodes inquiry-area)
          edges (relationship-edges inquiry-area)]
      (concat entities relationships edges))))

(defn orm-stylesheet
  "Cytoscape stylesheet for ORM diagrams."
  []
  [{:selector "node.entity-node"
    :style {:background-color "#e8f4fd"
            :label "data(label)"
            :width 120
            :height 60
            :shape "round-rectangle"
            :text-valign "center"
            :text-halign "center"
            :color "#2c3e50"
            :font-size "12px"
            :font-weight "bold"
            :border-width 2
            :border-color "#3498db"
            :text-wrap "wrap"
            :text-max-width "100px"}}
   {:selector "node.relationship-node"
    :style {:background-color "#fff2cc"
            :label "data(label)"
            :width 100
            :height 40
            :shape "diamond"
            :text-valign "center"
            :text-halign "center"
            :color "#2c3e50"
            :font-size "10px"
            :font-weight "bold"
            :border-width 2
            :border-color "#f39c12"
            :text-wrap "wrap"
            :text-max-width "80px"}}
   {:selector "edge.role-edge"
    :style {:width 2
            :line-color "#7f8c8d"
            :target-arrow-shape "none"
            :curve-style "bezier"
            :label "data(label)"
            :font-size "8px"
            :color "#7f8c8d"
            :text-rotation "autorotate"}}
   {:selector "node:selected"
    :style {:border-width 4
            :border-color "#e74c3c"}}
   {:selector "edge:selected"
    :style {:line-color "#e74c3c"}}])

(defnc ORMModal
  "Modal dialog containing the ORM diagram using Cytoscape.js."
  [{:keys [graph]}]
  (let [cy-ref (hooks/use-ref nil)
        modal (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]

    (hooks/use-effect
     [open graph]
     (log! :info (str "ORM useEffect: open=" open " graph=" (type graph) " ref=" (j/get cy-ref :current)))
     (when open
       (js/setTimeout
        (fn []
          (log! :info (str "ORM setTimeout: ref=" (j/get cy-ref :current) " graph keys=" (when graph (keys (edn/read-string graph)))))
          (when (and (j/get cy-ref :current) graph)
            (try
              (let [parsed-graph (if (string? graph)
                                   (edn/read-string graph)
                                   graph)]
                (log! :info (str "Creating ORM diagram for: " (:EADS-id parsed-graph)))
                (let [elements (orm->cytoscape-elements parsed-graph)]
                  (log! :info (str "ORM elements count: " (count elements)))
                  (log! :info (str "ORM elements sample: " (take 2 elements)))
                  (let [cy-instance (cytoscape-lib
                                     (clj->js
                                      {:container (j/get cy-ref :current)
                                       :elements elements
                                       :style (orm-stylesheet)
                                       :layout {:name "breadthfirst" :directed false :padding 30 :spacingFactor 2.0}
                                       :userZoomingEnabled true
                                       :userPanningEnabled true}))]
                    (log! :info (str "ORM Cytoscape instance created with " (count elements) " elements"))
                    ;; Force fit and center the view
                    (js/setTimeout
                     (fn []
                       (.fit cy-instance)
                       (.center cy-instance)
                       (log! :info "ORM diagram fitted to viewport"))
                     100)
                    (reset! diag {:cy-instance cy-instance :elements elements}))))
              (catch js/Error e
                (log! :info (str "Error creating ORM diagram: " e))))))
        500)))

    (letfn [(handle-open [] (set-open true))
            (handle-close [] (set-open false))]
      ($ Box {:ref modal}
         ($ Button {:onClick handle-open :color "secondary"} "ORM Graph")
         ($ Dialog {:open open :onClose handle-close :fullScreen true :maxWidth false}
            ($ "div" {:style {:width "800px" :height "600px" :margin "20px auto" :position "relative"}}
               ($ "div" {:ref cy-ref
                         :style {:width "800px" :height "600px" :background-color "#f8f9fa"}})))))))