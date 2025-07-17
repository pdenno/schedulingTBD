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
        total-height (+ compartment-height bar-height)

        ;; Generate white background rectangle
        background (str "<rect x='0' y='-" bar-height "' "
                        "width='" total-width "' height='" total-height "' "
                        "fill='white'/>")

        ;; Generate compartment rectangles
        compartments (for [i (range arity)]
                       (str "<rect x='" (* i compartment-width) "' y='0' "
                            "width='" compartment-width "' height='" compartment-height "' "
                            "fill='white' stroke='black' stroke-width='1'/>"))

        ;; Generate uniqueness bars - check if uniqueness pattern contains this index
        uniqueness-bars (for [[idx _] (map-indexed vector uniqueness-pattern)
                              :when (some #(= (str idx) %) (flatten uniqueness-pattern))]
                          (str "<rect x='" (+ (* idx compartment-width) 2) "' y='-" bar-height "' "
                               "width='" (- compartment-width 4) "' height='" bar-height "' "
                               "fill='black'/>"))

        svg-content (str "<svg xmlns='http://www.w3.org/2000/svg' "
                         "width='" total-width "' height='" total-height "' "
                         "viewBox='0 -" bar-height " " total-width " " total-height "'>"
                         background
                         (apply str compartments)
                         (apply str uniqueness-bars)
                         "</svg>")]

    {:svg svg-content
     :width total-width
     :height total-height
     :compartment-positions (for [i (range arity)]
                              {:x (+ (* i compartment-width) (/ compartment-width 2))
                               :y (/ compartment-height 2)})}))

(defn role-box-nodes
  "Generate role box nodes with custom SVG rendering and anchor nodes for each compartment"
  [orm-data]
  (let [fact-types (:fact-types orm-data)]
    (mapcat (fn [fact-type]
              (let [rel-id (:fact-type-id fact-type)
                    arity (:arity fact-type)
                    objects (:objects fact-type)
                    uniqueness (:uniqueness fact-type []) ; Default to no uniqueness
                    deontic-keys (:deontic-keys fact-type []) ; Get mandatory information
                    label (or (:label fact-type) rel-id) ; Use label if available

                    ;; Generate SVG for this role box
                    svg-data (generate-role-box-svg arity uniqueness)
                    svg-url (str "data:image/svg+xml;charset=utf-8,"
                                 (js/encodeURIComponent (:svg svg-data)))

                    ;; Main role box node
                    main-node {:data {:id rel-id
                                      :label label ; Now shows the role box label
                                      :arity arity
                                      :type "role-box"
                                      :svg-url svg-url
                                      :width (:width svg-data)
                                      :height (:height svg-data)}
                               :classes "role-box-svg"}

                    ;; Create anchor nodes for each compartment for precise edge targeting
                    ;; Remove parent-child relationship and use absolute positioning instead
                    anchor-nodes (for [i (range arity)]
                                   (let [anchor-id (str rel-id "-compartment-" i)
                                         is-mandatory (= "mandatory" (nth deontic-keys i ""))]
                                     {:data {:id anchor-id
                                             :type "role-anchor"
                                             :compartment-index i
                                             :object-id (nth objects i) ; Store which object this compartment represents
                                             :parent-role-box rel-id ; Reference to parent for positioning
                                             :mandatory is-mandatory} ; Add mandatory flag
                                      :classes "role-anchor"}))]

                (cons main-node anchor-nodes)))
            fact-types)))

(defn role-edges
  "Create edges connecting entities to specific role box compartments"
  [orm-data]
  (let [fact-types (:fact-types orm-data)]
    (mapcat (fn [fact-type]
              (let [rel-id (:fact-type-id fact-type)
                    objects (:objects fact-type)
                    deontic-keys (:deontic-keys fact-type [])]
                ;; Create edges from each entity to its corresponding compartment
                (map-indexed (fn [idx obj]
                               (let [compartment-anchor-id (str rel-id "-compartment-" idx)
                                     is-mandatory (= "mandatory" (nth deontic-keys idx ""))]
                                 {:data {:id (str rel-id "-edge-" obj "-" idx)
                                         :source obj
                                         :target compartment-anchor-id
                                         :mandatory is-mandatory ; Add mandatory flag for styling
                                         :label (str obj "-connects-to-" rel-id "-compartment-" idx)}}))
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
            :width "60px" ; Reduced from 80px
            :height "30px" ; Reduced from 40px
            :shape "rectangle"
            :font-size "10px" ; Reduced from 12px
            :border-width "2px"
            :border-color "#2E5C8A"
            :z-index 1}}

   {:selector "node[type='role-box']"
    :style {:background-color "#FFD700" ; Fallback yellow background
            :background-image (fn [node] (.data node "svg-url"))
            :background-fit "contain"
            :background-clip "none"
            :background-opacity 1
            :width "data(width)"
            :height "data(height)"
            :shape "rectangle"
            :label "data(label)" ; Show role box label
            :text-valign "top" ; Position label above box
            :text-margin-y -5 ; Move label above the box
            :font-size "9px" ; Small font for labels
            :color "#333" ; Dark gray for labels
            :border-width "0px" ; Remove border since SVG has its own
            :z-index 2}}

   ;; Invisible anchor nodes for precise edge targeting
   {:selector "node.role-anchor"
    :style {:width "1px"
            :height "1px"
            :background-opacity 0
            :border-opacity 0
            :label ""
            :z-index 0}}

   {:selector "edge"
    :style {:width "2px"
            :line-color "#333"
            :target-arrow-color "#333"
            :target-arrow-shape "none" ; Remove arrows for cleaner ORM look
            :curve-style "straight" ; Straight lines for ORM
            :z-index 3 ; Edges above nodes
            :source-endpoint "outside-to-node"
            :target-endpoint "outside-to-node"}}

   ;; Style for mandatory edges - show dot at source end
   {:selector "edge[mandatory='true']"
    :style {:source-arrow-shape "circle"
            :source-arrow-color "#333"
            :source-distance-from-node 5
            :source-arrow-scale 0.5}}])

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
                                       ;; Change to cose layout for better Y distribution
                                       :layout {:name "cose"
                                                :nodeRepulsion 4000 ; Reduced from 8000
                                                :nodeOverlap 10 ; Reduced from 20
                                                :idealEdgeLength 60 ; Reduced from 100
                                                :edgeElasticity 100
                                                :nestingFactor 5
                                                :gravity 80
                                                :numIter 1000
                                                :initialTemp 200
                                                :coolingFactor 0.95
                                                :minTemp 1.0
                                                :padding 30} ; Reduced from 50
                                       :userZoomingEnabled true
                                       :userPanningEnabled true}))]
                    (log! :info (str "ORM Cytoscape instance created with " (count elements) " elements"))
                    (log! :info (str "Elements breakdown: "
                                     (count (filter #(= (get-in % [:data :type]) "entity") elements)) " entities, "
                                     (count (filter #(= (get-in % [:data :type]) "role-box") elements)) " role-boxes, "
                                     (count (filter #(= (get-in % [:data :type]) "role-anchor") elements)) " anchors, "
                                     (count (filter #(contains? (:data %) :source) elements)) " edges"))

                    ;; Helper function to update anchor positions
                    (letfn [(update-anchors-for-role-box [role-box]
                              (let [role-id (.id role-box)
                                    role-pos (.position role-box)
                                    role-width (.data role-box "width")
                                    role-height (.data role-box "height")
                                    arity (.data role-box "arity")
                                    anchor-nodes (.nodes cy-instance (str "[parent-role-box='" role-id "']"))]
                                (doseq [anchor (.toArray anchor-nodes)]
                                  (let [compartment-idx (.data anchor "compartment-index")
                                        compartment-center-x (+ (.-x role-pos)
                                                                (* role-width (- (/ (+ compartment-idx 0.5) arity) 0.5)))
                                        object-id (.data anchor "object-id")
                                        ; Find the entity node to determine relative position
                                        entity-node (.getElementById cy-instance object-id)
                                        entity-y (when entity-node (.-y (.position entity-node)))
                                        role-y (.-y role-pos)
                                        ; Position at top edge if entity is above, bottom edge if below
                                        anchor-y (if (and entity-y (< entity-y role-y))
                                                   (- role-y (/ role-height 2)) ; Top edge
                                                   (+ role-y (/ role-height 2)))] ; Bottom edge
                                    (.position anchor #js {:x compartment-center-x :y anchor-y})))))]

                      ;; Force fit and center the view after layout
                      (js/setTimeout
                       (fn []
                         ;; Initial positioning of anchors
                         (let [role-boxes (.nodes cy-instance "[type='role-box']")]
                           (doseq [role-box (.toArray role-boxes)]
                             (update-anchors-for-role-box role-box)))

                         ;; Bind drag event to role boxes to keep anchors synchronized
                         (.on (.nodes cy-instance "[type='role-box']") "drag"
                              (fn [evt]
                                (let [dragged-node (.-target evt)]
                                  (update-anchors-for-role-box dragged-node))))

                         ;; Also update anchors when entities are dragged (to adjust top/bottom connection)
                         (.on (.nodes cy-instance "[type='entity']") "drag"
                              (fn [evt]
                                ;; Update all role boxes since entity position affects anchor placement
                                (let [role-boxes (.nodes cy-instance "[type='role-box']")]
                                  (doseq [role-box (.toArray role-boxes)]
                                    (update-anchors-for-role-box role-box)))))

                         (.fit cy-instance)
                         (.center cy-instance)
                         (log! :info "ORM diagram fitted to viewport"))
                       200))
                    (reset! diag {:cy-instance cy-instance :elements elements}))))
              (catch js/Error e
                (log! :info (str "Error creating ORM diagram: " e))))))
        500)))

    (letfn [(handle-open [] (set-open true))
            (handle-close [] (set-open false))]
      ($ Box {:ref modal}
         ($ Button {:onClick handle-open :color "secondary"} "ORM Graph")
         ($ Dialog {:open open
                    :onClose handle-close
                    :fullScreen true
                    :maxWidth false
                    :disableEscapeKeyDown true ; Prevent ESC key from closing
                    :disableBackdropClick true} ; Prevent backdrop click from closing
            ($ Box {:style {:position "relative" :width "100%" :height "100%"}}
               ;; Add close button in top-right corner
               ($ Button {:onClick handle-close
                          :style {:position "absolute" :top "10px" :right "10px" :zIndex 1000}
                          :variant "contained"
                          :color "primary"}
                  "Close Diagram")
               ($ "div" {:style {:width "100%" :height "100%" :position "relative"}}
                  ($ "div" {:ref cy-ref
                            :style {:width "100%" :height "100%" :background-color "#f8f9fa"}}))))))))
