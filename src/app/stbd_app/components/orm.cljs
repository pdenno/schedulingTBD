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

(def ^:diag diag (atom {:instance-count 0 :total-opens 0})) ; Reserved for debugging - do not use in production code
(def graph-state (atom nil)) ; Stores the current Cytoscape instance and elements

;;; Utility functions for ORM to Cytoscape transformation

(defn capture-layout
  "Capture current node positions and view state from Cytoscape instance"
  [cy-instance]
  (when cy-instance
    (let [nodes (.nodes cy-instance)
          positions (reduce (fn [acc node]
                              (assoc acc (.id node) (.position node)))
                            {}
                            (.toArray nodes))
          zoom (.zoom cy-instance)
          pan (.pan cy-instance)]
      {:node-positions positions
       :zoom zoom
       :pan pan})))

(defn apply-layout
  "Apply saved layout to Cytoscape instance"
  [cy-instance layout-data]
  (when (and cy-instance layout-data)
    ;; Apply node positions
    (doseq [[node-id position] (:node-positions layout-data)]
      (when-let [node (.getElementById cy-instance node-id)]
        (.position node position)))
    ;; Apply zoom and pan
    (when-let [zoom (:zoom layout-data)]
      (.zoom cy-instance zoom))
    (when-let [pan (:pan layout-data)]
      (.pan cy-instance pan))))

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
    (mapcat (fn [{:keys [fact-type-id objects uniqueness mandatory?] :as fact-type}]
              (let [rel-id (:fact-type-id fact-type)
                    arity (count objects)
                    label (or (:label fact-type) fact-type-id) ; Use label if available

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
                                   (let [anchor-id (str rel-id "-compartment-" i)]
                                     {:data {:id anchor-id
                                             :type "role-anchor"
                                             :compartment-index i
                                             :object-id (nth objects i) ; Store which object this compartment represents
                                             :parent-role-box rel-id ; Reference to parent for positioning
                                             :mandatory? (#{"must" "should"} (nth mandatory? i nil))}
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
                    mandatory? (:mandatory? fact-type [])]
                ;; Create edges from each entity to its corresponding compartment
                (map-indexed (fn [idx obj]
                               (let [compartment-anchor-id (str rel-id "-compartment-" idx)]
                                 {:data {:id (str rel-id "-edge-" obj "-" idx)
                                         :source obj
                                         :target compartment-anchor-id
                                         :mandatory? (#{"must" "should"} (nth mandatory? idx nil))
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
            :arrow-scale 0.5}}])

(defnc ORMModal
  "Modal dialog containing the ORM diagram using Cytoscape.js."
  [{:keys [graph]}]
  (let [cy-ref (hooks/use-ref nil)
        modal (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]

    ;; Add effect to monitor when dialog actually closes
    (hooks/use-effect
     [open]
     (when (not open)
       ;;(log! :info "Dialog closed (open = false)")
       ;; Ensure cleanup when dialog closes by any means
       (when-let [cy-instance (:cy-instance @graph-state)]
         ;;(log! :info "Cleaning up after dialog close")
         (try
           (.destroy cy-instance)
           ;; Remove registration
           (when (.-removeRegistrationForInstance cytoscape-lib)
             ;;(log! :info "Using removeRegistrationForInstance")
             (.removeRegistrationForInstance cytoscape-lib cy-instance))
           (catch js/Error e
             (log! :error (str "Error during effect cleanup:" e))))
         (when-let [container (j/get cy-ref :current)]
           (set! (.-innerHTML container) ""))
         (reset! graph-state nil))
       ;; Check dimensions after close
       (js/setTimeout
        (fn []
          #_(log! :info (str "After dialog close - Body scrollHeight: " (.-scrollHeight js/document.body)
                           " scrollWidth: " (.-scrollWidth js/document.body)))
          #_(log! :info (str "HTML element - scrollHeight: " (.-scrollHeight js/document.documentElement)
                           " scrollWidth: " (.-scrollWidth js/document.documentElement)))
          #_(log! :info (str "Window innerHeight: " js/window.innerHeight
                           " innerWidth: " js/window.innerWidth))
          #_(log! :info (str "Total canvases: "
                           (.-length (.querySelectorAll js/document "canvas"))))
          ;; Check if any MUI elements remain
          #_(log! :info (str "MUI elements remaining: "
                           "Dialogs: " (.-length (.querySelectorAll js/document ".MuiDialog-root"))
                           " Backdrops: " (.-length (.querySelectorAll js/document ".MuiBackdrop-root"))
                           " Modals: " (.-length (.querySelectorAll js/document ".MuiModal-root")))))
        100)))

    (hooks/use-effect
     [open graph]
     (log! :info (str "ORM useEffect: open=" open " graph=" (type graph) " ref=" (j/get cy-ref :current)))
     (when open
       (js/setTimeout
        (fn []
          #_(log! :info (str "ORM setTimeout: ref=" (j/get cy-ref :current) " graph keys=" (when graph (keys (edn/read-string graph)))))
          ;; Log viewport info BEFORE creating Cytoscape
          #_(log! :info (str "BEFORE Cytoscape - Window innerHeight: " js/window.innerHeight
                           " innerWidth: " js/window.innerWidth))
          #_(log! :info (str "BEFORE - Body scrollHeight: " (.-scrollHeight js/document.body)
                           " offsetHeight: " (.-offsetHeight js/document.body)))
          (when (and (j/get cy-ref :current) graph)
            (try
              ;; Destroy any existing Cytoscape instance before creating a new one
              (when-let [existing-cy (:cy-instance @graph-state)]
                #_(log! :info "Destroying existing Cytoscape instance before creating new one")
                (.destroy existing-cy)
                (reset! graph-state nil))

              ;; Clear the container element to ensure no remnants
              (let [container (j/get cy-ref :current)]
                (set! (.-innerHTML container) "")
                ;; Log the number of canvas elements before creation
                #_(log! :info (str "Canvas elements in container before creation: "
                                 (.-length (.querySelectorAll container "canvas")))))

              (let [parsed-graph (if (string? graph)
                                   (edn/read-string graph)
                                   graph)]
                (log! :info (str "Creating ORM diagram for: " (:EADS-id parsed-graph)))
                (swap! diag update :instance-count inc)
                (log! :info (str "Instance count: " (:instance-count @diag)))
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
                                       :userPanningEnabled true
                                       :minZoom 0.1
                                       :maxZoom 10}))
                        container (j/get cy-ref :current)]
                    (log! :info (str "ORM Cytoscape instance created with " (count elements) " elements"))

                    ;; Set viewport constraints
                    (.on cy-instance "render"
                         (fn []
                           (let [extent (.extent cy-instance)
                                 viewport-width (.-offsetWidth container)
                                 viewport-height (.-offsetHeight container)]
                             (when (or (> (.-w extent) (* viewport-width 10))
                                       (> (.-h extent) (* viewport-height 10)))
                               (.fit cy-instance)))))

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
                    (reset! graph-state {:cy-instance cy-instance :elements elements})
                    ;; Debug: Check document body scroll dimensions
                    (log! :info (str "After creation - Body scrollHeight: " (.-scrollHeight js/document.body)
                                     " scrollWidth: " (.-scrollWidth js/document.body)))
                    ;; Debug: Check what's causing the scroll
                    (js/setTimeout
                     (fn []
                       (log! :info "=== Scroll Debug ===")
                       (log! :info (str "document.body scrollHeight: " (.-scrollHeight js/document.body)
                                        " offsetHeight: " (.-offsetHeight js/document.body)))
                       (log! :info (str "document.documentElement scrollHeight: " (.-scrollHeight js/document.documentElement)
                                        " offsetHeight: " (.-offsetHeight js/document.documentElement)))
                       ;; Check scroll position
                       (log! :info (str "Window scrollY: " js/window.scrollY " scrollX: " js/window.scrollX))
                       ;; Check if body has any padding/margin
                       (let [body-style (js/window.getComputedStyle js/document.body)]
                         (log! :info (str "Body margin: " (.-margin body-style) " padding: " (.-padding body-style))))
                       ;; Find the actual scrolling element
                       (log! :info (str "document.scrollingElement: " js/document.scrollingElement))
                       ;; Check if MUI Dialog is affecting things
                       (when-let [dialog-container (.querySelector js/document ".MuiDialog-container")]
                         (let [rect (.getBoundingClientRect dialog-container)]
                           (log! :info (str "Dialog container dimensions: "))))
                       ;; Check actual vs visual viewport
                       (log! :info (str "visualViewport width: " (.-width js/visualViewport)
                                        " height: " (.-height js/visualViewport)))
                       ;; Check if there's a scale applied
                       (log! :info (str "visualViewport scale: " (.-scale js/visualViewport)))
                       ;; Check root element
                       (let [root (.getElementById js/document "root")]
                         (when root
                           (let [root-rect (.getBoundingClientRect root)]
                             (log! :info (str "Root element dimensions: "
                                              "width: " (.-width root-rect) " height: " (.-height root-rect))))))
                       ;; Check if dialog is visible
                       (when-let [dialog (.querySelector js/document ".MuiDialog-root")]
                         (let [dialog-paper (.querySelector dialog ".MuiDialog-paper")]
                           (when dialog-paper
                             (let [paper-style (js/window.getComputedStyle dialog-paper)]
                               (log! :info (str "Dialog paper - height: " (.-height paper-style)
                                                " max-height: " (.-maxHeight paper-style)
                                                " overflow: " (.-overflow paper-style)))))))
                       ;; Check if something is setting a min-height
                       (log! :info (str "Body computed min-height: "
                                        (.-minHeight (js/window.getComputedStyle js/document.body))))
                       (log! :info (str "HTML computed min-height: "))
                       ;; Check parent containers
                       (let [current-el (j/get cy-ref :current)]
                         (when current-el
                           (log! :info "=== Parent chain dimensions ===")
                           (loop [el current-el
                                  level 0]
                             (when (and el (< level 5))
                               (let [rect (.getBoundingClientRect el)
                                     computed (js/window.getComputedStyle el)]
                                 (log! :info (str "Level " level " - " (.-tagName el)
                                                  " class: " (.-className el)
                                                  " height: " (.-height rect)
                                                  " computed height: " (.-height computed)
                                                  " overflow: " (.-overflow computed)))
                                 (recur (.-parentElement el) (inc level)))))))
                       ;; Check if any parent has a max-height
                       (log! :info "=== Checking for height constraints ===")
                       (let [all-parents (atom [])]
                         (loop [el (j/get cy-ref :current)]
                           (when el
                             (swap! all-parents conj el)
                             (recur (.-parentElement el))))
                         (doseq [el @all-parents]
                           (let [computed (js/window.getComputedStyle el)]
                             (when (or (not= (.-maxHeight computed) "none")
                                       (and (not= (.-height computed) "auto")
                                            (not (.includes (.-height computed) "%"))))
                               (log! :info (str "Height constraint found - " (.-tagName el)
                                                " class: " (.-className el)
                                                " height: " (.-height computed)
                                                " max-height: " (.-maxHeight computed))))))))
                     300)))) ; Wait a bit for layout to settle)))
              (catch js/Error e
                (log! :info (str "Error creating ORM diagram: " e))))))
        500))

     ;; Cleanup function - destroy Cytoscape instance when effect reruns or component unmounts
     (fn []
       (log! :info "ORM useEffect cleanup function called")
       (when-let [cy-instance (:cy-instance @graph-state)]
         (log! :info "Cleaning up Cytoscape instance in useEffect cleanup")
         (try
           ;; Destroy the instance
           (.destroy cy-instance)
           ;; Remove registration
           (when (.-removeRegistrationForInstance cytoscape-lib)
             (.removeRegistrationForInstance cytoscape-lib cy-instance))
           (catch js/Error e
             (log! :error (str "Error during cleanup:" e))))
         ;; Clear the container if it exists
         (when-let [container (j/get cy-ref :current)]
           (set! (.-innerHTML container) ""))
         (reset! graph-state nil))))

    (letfn [(handle-open []
              (swap! diag update :total-opens inc)
              (log! :info (str "Opening dialog - total opens: " (:total-opens @diag)))
              (set-open true))
            (handle-close []
              (log! :info "handle-close called")
              ;; Immediately destroy the instance before setting open to false
              (when-let [cy-instance (:cy-instance @graph-state)]
                (log! :info "Destroying Cytoscape instance on modal close")
                (try
                  ;; Destroy the instance
                  (.destroy cy-instance)
                  ;; Remove registration
                  (when (.-removeRegistrationForInstance cytoscape-lib)
                    (.removeRegistrationForInstance cytoscape-lib cy-instance))
                  (log! :info "cy-instance destroyed successfully")
                  (catch js/Error e
                    (log! :error (str "Error during close cleanup:" e))))
                ;; Clear the container
                (when-let [container (j/get cy-ref :current)]
                  (log! :info "Clearing container innerHTML")
                  ;; Remove all child nodes first
                  (while (.-firstChild container)
                    (.removeChild container (.-firstChild container)))
                  ;; Then clear innerHTML as backup
                  (set! (.-innerHTML container) "")
                  ;; Reset any styles that might have been added
                  (set! (.-position (.-style container)) "")
                  (set! (.-overflow (.-style container)) ""))
                (reset! graph-state nil))
              ;; Debug: Check body dimensions after cleanup
              (log! :info (str "After close - Body scrollHeight: " (.-scrollHeight js/document.body)
                               " scrollWidth: " (.-scrollWidth js/document.body)))
              ;; Check DOM elements
              (log! :info (str "Total canvases: "
                               (.-length (.querySelectorAll js/document "canvas"))))
              (log! :info (str "MUI Dialogs: "
                               (.-length (.querySelectorAll js/document ".MuiDialog-root"))))
              ;; Only set open to false after cleanup is complete
              (set-open false)
              (log! :info "set-open false completed"))]
      ($ Box {:ref modal}
         ($ Button {:onClick handle-open :color "secondary"} "ORM Graph")
         ;; Only render Dialog when open
         (when open
           ($ Dialog {:open true
                      :onClose handle-close
                      :fullScreen true
                      :maxWidth false}
              ($ Box {:style {:position "relative"
                              :width "100%"
                              :height "100vh"}} ; Outer container
                 ;; Cytoscape container fills entire viewport
                 ($ "div" {:ref cy-ref
                           :style {:width "100%"
                                   :height "100%"
                                   :position "absolute"
                                   :top 0
                                   :left 0
                                   :backgroundColor "#f8f9fa"}})
                 ;; Close button overlaid on top
                 ($ Box {:style {:position "absolute"
                                 :top "20px"
                                 :right "20px"
                                 :zIndex 9999}}
                    ($ Button {:onClick handle-close
                               :variant "contained"
                               :color "error"
                               :size "large"
                               :style {:backgroundColor "#f44336" ; Explicit red
                                       :color "white"
                                       :fontWeight "bold"
                                       :boxShadow "0 4px 6px rgba(0,0,0,0.3)"}}
                       "✕ CLOSE")))))))))
