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
                   :definition (:definition obj)
                   :type "entity"}
            :classes "entity-node"})
         objects)))

(defn generate-role-box-svg
  "Generate SVG for role box with specified arity and uniqueness bars"
  [arity uniqueness-pattern]
  (let [compartment-width 30
        compartment-height 25
        total-width (* arity compartment-width)
        bar-height 3

        ;; Generate compartment rectangles
        compartments (for [i (range arity)]
                       (str "<rect x='" (* i compartment-width) "' y='0' "
                            "width='" compartment-width "' height='" compartment-height "' "
                            "fill='none' stroke='black' stroke-width='1'/>"))

        ;; Generate uniqueness bars
        uniqueness-bars (for [i (range arity)
                              :when (get uniqueness-pattern i)]
                          (str "<rect x='" (+ (* i compartment-width) 2) "' y='-" bar-height "' "
                               "width='" (- compartment-width 4) "' height='" bar-height "' "
                               "fill='black'/>"))

        svg-content (str "<svg xmlns='http://www.w3.org/2000/svg' "
                         "width='" total-width "' height='" (+ compartment-height bar-height) "'>"
                         (apply str compartments)
                         (apply str uniqueness-bars)
                         "</svg>")]

    {:svg svg-content
     :width total-width
     :height (+ compartment-height bar-height)
     :compartment-positions (for [i (range arity)]
                              {:x (+ (* i compartment-width) (/ compartment-width 2))
                               :y (/ compartment-height 2)})}))

(defn role-box-nodes
  "Generate role box nodes with custom SVG rendering"
  [orm-data]
  (let [fact-types (:fact-types orm-data)]
    (mapcat (fn [fact-type]
              (let [rel-id (:fact-type-id fact-type)
                    arity (:arity fact-type)
                    _objects (:objects fact-type)
                    uniqueness (:uniqueness fact-type []) ; Default to no uniqueness

                    ;; Generate SVG for this role box
                    svg-data (generate-role-box-svg arity uniqueness)
                    svg-url (str "data:image/svg+xml;charset=utf-8,"
                                 (js/encodeURIComponent (:svg svg-data)))

                    ;; Main role box node
                    main-node {:data {:id rel-id
                                      :label ""
                                      :arity arity
                                      :type "role-box"
                                      :svg-url svg-url
                                      :width (:width svg-data)
                                      :height (:height svg-data)}
                               :classes "role-box-svg"}

                    ;; Invisible anchor nodes for precise edge targeting
                    ;; Temporarily disabled for debugging
                    anchor-nodes []]

                (cons main-node anchor-nodes)))
            fact-types)))

(defn role-edges
  "Create edges connecting entities to role boxes (temporarily simplified)"
  [orm-data]
  (let [fact-types (:fact-types orm-data)]
    (mapcat (fn [fact-type]
              (let [rel-id (:fact-type-id fact-type)
                    objects (:objects fact-type)]
                (map (fn [obj]
                       {:data {:id (str rel-id "-edge-" obj)
                               :source obj
                               :target rel-id}})
                     objects)))
            fact-types)))

(defn orm->cytoscape-elements
  "Transform ORM data structure to Cytoscape elements format with role boxes."
  [orm-data]
  (when orm-data
    (let [parsed-data (if (string? orm-data)
                        (edn/read-string orm-data)
                        orm-data)
          inquiry-area (first (:inquiry-areas parsed-data))
          entities (entity-nodes inquiry-area)
          role-boxes (role-box-nodes inquiry-area)
          edges (role-edges inquiry-area)]
      (concat entities role-boxes edges))))

(defn orm-stylesheet
  "Cytoscape stylesheet for ORM diagrams with custom SVG role boxes"
  []
  [{:selector "node[type='entity']"
    :style {:background-color "#4A90E2"
            :color "white"
            :label "data(label)"
            :text-valign "center"
            :text-halign "center"
            :width "80px"
            :height "40px"
            :shape "rectangle"
            :font-size "12px"
            :border-width "2px"
            :border-color "#2E5C8A"}}

   {:selector "node[type='role-box']"
    :style {:background-color "#FFD700" ; Fallback yellow background
            :background-image (fn [node]
                                (let [node-id (.id node)
                                      svg-url (.data node "svg-url")]
                                  (println "Node ID:" node-id "SVG URL:" (if svg-url (subs svg-url 0 50) "nil"))
                                  svg-url)) ; Fallback yellow background
            ; :background-image "data(svg-url)"  ; Temporarily disabled
            :background-fit "contain"
            :background-clip "none"
            :background-opacity 1
            :width "data(width)"
            :height "data(height)"
            :shape "rectangle"
            :label ""
            :border-width "1px"
            :border-color "#DAA520"}}

   ;; Invisible anchor nodes for precise edge targeting
   {:selector "node.role-anchor"
    :style {:width "2px"
            :height "2px"
            :background-opacity 0
            :border-opacity 0
            :label ""}}

   {:selector "edge"
    :style {:width "2px"
            :line-color "#888"
            :target-arrow-color "#888"
            :target-arrow-shape "triangle"
            :curve-style "bezier"}}])

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
                                       :layout {:name "breadthfirst" :directed false :padding 50 :spacingFactor 2.5}
                                       :userZoomingEnabled true
                                       :userPanningEnabled true}))]
                    (log! :info (str "ORM Cytoscape instance created with " (count elements) " elements"))
                    ;; Force fit and center the view
                    (js/setTimeout
                     (fn []
                       ;; Position role compartments properly after layout
                       (let [role-parents (.nodes cy-instance ".role-box-parent")]
                         (doseq [parent-node (.toArray role-parents)]
                           (let [parent-id (.id (.data parent-node))
                                 children (.nodes cy-instance (str "[parent = '" parent-id "']"))
                                 child-count (.length children)
                                 parent-pos (.position parent-node)
                                 start-x (- (.-x parent-pos) (* child-count 10))
                                 y (.-y parent-pos)]
                             ;; Position compartments in a horizontal line
                             (doseq [i (range child-count)]
                               (let [child (.eq children i)
                                     x (+ start-x (* i 20))]
                                 (.position child #js {:x x :y y}))))))
                       (.fit cy-instance)
                       (.center cy-instance)
                       (log! :info "ORM diagram fitted to viewport"))
                     200)
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
