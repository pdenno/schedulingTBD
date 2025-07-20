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
   [stbd-app.util :refer [common-info lookup-fn]]
   [stbd-app.ws :as ws]
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
                              (let [pos (.position node)]
                                (assoc acc (.id node) {:x (.-x pos) :y (.-y pos)})))
                            {}
                            (.toArray nodes))
          zoom (.zoom cy-instance)
          pan-obj (.pan cy-instance)
          pan {:x (.-x pan-obj) :y (.-y pan-obj)}]
      {:node-positions positions
       :zoom zoom
       :pan pan})))

(defn apply-layout
  "Apply saved layout to Cytoscape instance"
  [cy-instance layout-data]
  (let [layout-data (edn/read-string layout-data)]
    (log! :info (str "layout-data = " layout-data))
    (when (and cy-instance layout-data)
      ;; Apply node positions
      (doseq [[node-id position] (:node-positions layout-data)]
        (when-let [node (.getElementById cy-instance node-id)]
          (.position node position)))
      ;; Apply zoom and pan
      (when-let [zoom (:zoom layout-data)]
        (.zoom cy-instance zoom))
      (when-let [pan (:pan layout-data)]
        (.pan cy-instance pan)))))

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

        ;; Generate uniqueness bars - purple bars for alethic constraints
        ;; uniqueness-pattern is typically like [["u1" "u1" ""] ["" "u2" "u2"]]
        ;; A bar appears over a compartment if any uniqueness constraint includes that position
        uniqueness-bars (for [idx (range arity)
                              :let [has-uniqueness (some (fn [constraint-row]
                                                           (not (empty? (nth constraint-row idx ""))))
                                                         uniqueness-pattern)]
                              :when has-uniqueness]
                          (str "<rect x='" (+ (* idx compartment-width) 2) "' y='-" bar-height "' "
                               "width='" (- compartment-width 4) "' height='" bar-height "' "
                               "fill='#9b59b6'/>")) ; Purple for alethic

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
                                   (let [anchor-id (str rel-id "-compartment-" i)
                                         mandatory-value (nth mandatory? i nil)]
                                     {:data {:id anchor-id
                                             :type "role-anchor"
                                             :compartment-index i
                                             :object-id (nth objects i) ; Store which object this compartment represents
                                             :parent-role-box rel-id ; Reference to parent for positioning
                                             :mandatory mandatory-value}
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
                               (let [compartment-anchor-id (str rel-id "-compartment-" idx)
                                     mandatory-value (nth mandatory? idx nil)]
                                 {:data {:id (str rel-id "-edge-" obj "-" idx)
                                         :source obj
                                         :target compartment-anchor-id
                                         :mandatory mandatory-value
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
   ;; Style for alethic constraint (must) - purple dot at source end
   {:selector "edge[mandatory='must']"
    :style {:source-arrow-shape "circle"
            :source-arrow-color "#9b59b6" ; Purple color for alethic
            :source-arrow-fill "filled"
            :source-distance-from-node 3
            :arrow-scale 1.2
            :source-arrow-background-fill "filled"}}

   ;; Style for deontic constraint (should) - blue dot at source end
   {:selector "edge[mandatory='should']"
    :style {:source-arrow-shape "circle"
            :source-arrow-color "#3498db" ; Blue color for deontic
            :source-arrow-fill "filled"
            :source-distance-from-node 3
            :arrow-scale 1.2
            :source-arrow-background-fill "filled"}}])

(defnc ORMModal
  "Modal dialog containing the ORM diagram using Cytoscape.js."
  [{:keys [graph message-id]}]
  (let [cy-ref (hooks/use-ref nil)
        modal (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]

    ;; Add effect to monitor when dialog actually closes
    (hooks/use-effect
     [open]
     (when (not open)
       ;; Ensure cleanup when dialog closes by any means
       (when-let [cy-instance (:cy-instance @graph-state)]
         (try
           (.destroy cy-instance)
           ;; Remove registration
           (when (.-removeRegistrationForInstance cytoscape-lib)
             (.removeRegistrationForInstance cytoscape-lib cy-instance))
           (catch js/Error e
             (log! :error (str "Error during effect cleanup:" e))))
         (when-let [container (j/get cy-ref :current)]
           (set! (.-innerHTML container) ""))
         (reset! graph-state nil))
       ;; Check dimensions after close
       (js/setTimeout (fn []) 100)))

    (hooks/use-effect
     [open graph]
     (when open
       (js/setTimeout
        (fn []
          (when (and (j/get cy-ref :current) graph)
            (try
              ;; Destroy any existing Cytoscape instance before creating a new one
              (when-let [existing-cy (:cy-instance @graph-state)]
                (.destroy existing-cy)
                (reset! graph-state nil))
              ;; Clear the container element to ensure no remnants
              (let [container (j/get cy-ref :current)]
                (set! (.-innerHTML container) ""))
              (let [parsed-graph (if (string? graph)
                                   (edn/read-string graph)
                                   graph)]
                (log! :info (str "Creating ORM diagram for: " (:EADS-id parsed-graph)
                                 "\nMandatory values in data: "
                                 (pr-str (map :mandatory? (:fact-types parsed-graph)))))
                (swap! diag update :instance-count inc)
                (doseq [ft (:fact-types parsed-graph)]
                  (when (some #(= "must" %) (:mandatory? ft))
                    (log! :info (str "Fact type with mandatory: " (:fact-type-id ft)
                                     " mandatory?: " (:mandatory? ft))))
                  (when (:uniqueness ft)
                    (log! :info (str "Fact type with uniqueness: " (:fact-type-id ft)
                                     " uniqueness: " (:uniqueness ft)))))
                (let [elements (orm->cytoscape-elements parsed-graph)
                      saved-layout (get-in parsed-graph [:inquiry-areas 0 :layout])
                      ;; If we have saved layout, apply positions to elements before creating cy instance
                      ;; If we have saved layout, apply positions to elements before creating cy instance
                      elements-with-positions (if saved-layout
                                                (let [positions (:node-positions saved-layout)]
                                                  (log! :info (str "Layout data keys: " (keys saved-layout)))
                                                  (log! :info (str "Number of positions: " (count positions)))
                                                  (mapv (fn [el]
                                                          (let [node-id (get-in el [:data :id])
                                                                pos (get positions node-id)]
                                                            (if pos
                                                              (assoc el :position pos)
                                                              el)))
                                                        elements))
                                                elements)
                      ;; Use preset layout if we have saved positions, otherwise breadthfirst
                      layout-config (if saved-layout
                                      {:name "preset"
                                       :animate false}
                                      {:name "breadthfirst"
                                       :animate false
                                       :animationDuration 0
                                       :fit true
                                       :directed false
                                       :padding 50
                                       :spacingFactor 1.5})
                      cy-instance (cytoscape-lib
                                   (clj->js
                                    {:container (j/get cy-ref :current)
                                     :elements elements-with-positions
                                     :style (orm-stylesheet)
                                     :layout layout-config
                                     :userZoomingEnabled true
                                     :userPanningEnabled true
                                     :minZoom 0.1
                                     :maxZoom 10}))
                      container (j/get cy-ref :current)]

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

                      ;; Check if we have saved layout data
                      ;; Layout is stored with the inquiry-area (one for each)
                    ;; Check if we have saved layout data
                      ;; If using preset layout, apply positions immediately
                    ;; If we have saved layout, apply zoom/pan and update anchors
                    ;; If we have saved layout, apply zoom/pan and update anchors
                    (when saved-layout
                      (log! :info (str "Applying saved layout for inquiry area: "
                                       (get-in parsed-graph [:inquiry-areas 0 :inquiry-area-id])))
                      ;; Apply zoom and pan
                      (when-let [zoom (:zoom saved-layout)]
                        (.zoom cy-instance zoom))
                      (when-let [pan (:pan saved-layout)]
                        (.pan cy-instance (clj->js pan)))
                      ;; Update anchors after positions are set
                      (js/setTimeout
                       (fn []
                         (let [role-boxes (.nodes cy-instance "[type='role-box']")]
                           (doseq [role-box (.toArray role-boxes)]
                             (update-anchors-for-role-box role-box))))
                       50))

                      ;; Wait for layout to complete, then set up anchors
                    (.on cy-instance "layoutstop"
                         (fn []
                             ;; Initial positioning of anchors
                           (let [role-boxes (.nodes cy-instance "[type='role-box']")]
                             (doseq [role-box (.toArray role-boxes)]
                               (update-anchors-for-role-box role-box)))
                           (log! :info "Initial anchor positioning complete")))

                      ;; Set up drag handlers immediately (not inside layoutstop)
                      ;; Bind drag event to role boxes to keep anchors synchronized
                    (.on (.nodes cy-instance "[type='role-box']") "drag"
                         (fn [evt]
                           (let [dragged-node (.-target evt)]
                             (update-anchors-for-role-box dragged-node))))

                      ;; Also update anchors when entities are dragged
                    (.on (.nodes cy-instance "[type='entity']") "drag"
                         (fn [evt]
                             ;; Update all role boxes since entity position affects anchor placement
                           (let [role-boxes (.nodes cy-instance "[type='role-box']")]
                             (doseq [role-box (.toArray role-boxes)]
                               (update-anchors-for-role-box role-box)))))

                    (reset! graph-state {:cy-instance cy-instance
                                         :elements elements
                                         :parsed-graph parsed-graph})

                    ;; Log any ongoing animations or rendering
                    (js/setTimeout
                     (fn []
                       (log! :info "=== Diagnostics starting ===")
                       ;; Check container dimensions
                       (let [container (j/get cy-ref :current)
                             rect (.getBoundingClientRect container)]
                         (log! :info (str "Container dimensions - width: " (.-width rect) " height: " (.-height rect))))

                       ;; Check if container or its parents are changing size
                       (let [initial-sizes (atom {})
                             size-changes (atom [])]
                         ;; Record initial sizes
                         (loop [el (j/get cy-ref :current)
                                level 0]
                           (when (and el (< level 5))
                             (let [rect (.getBoundingClientRect el)]
                               (swap! initial-sizes assoc level {:width (.-width rect) :height (.-height rect)})
                               (recur (.-parentElement el) (inc level)))))

                         ;; Monitor for size changes
                         (let [check-interval (js/setInterval
                                               (fn []
                                                 (loop [el (j/get cy-ref :current)
                                                        level 0]
                                                   (when (and el (< level 5))
                                                     (let [rect (.getBoundingClientRect el)
                                                           initial (get @initial-sizes level)
                                                           current {:width (.-width rect) :height (.-height rect)}]
                                                       (when (or (not= (:width initial) (:width current))
                                                                 (not= (:height initial) (:height current)))
                                                         (swap! size-changes conj {:level level
                                                                                   :from initial
                                                                                   :to current
                                                                                   :element (.-tagName el)}))
                                                       (recur (.-parentElement el) (inc level))))))
                                               100)]

                           ;; Check render counts
                           (let [render-count (atom 0)]
                             (.on cy-instance "render"
                                  (fn []
                                    (swap! render-count inc)))

                             ;; After 2 seconds, report findings
                             (js/setTimeout
                              (fn []
                                (js/clearInterval check-interval)
                                (log! :info (str "Render count in 2 seconds: " @render-count))
                                (log! :info (str "Size changes detected: " (count @size-changes)))
                                (when (pos? (count @size-changes))
                                  (doseq [change (take 5 @size-changes)]
                                    (log! :info (str "Size change at level " (:level change)
                                                     " (" (:element change) "): "
                                                     "from " (:from change) " to " (:to change)))))

                                ;; Check if there's a ResizeObserver or MutationObserver
                                (log! :info (str "Window ResizeObserver: " (exists? js/ResizeObserver)))

                                ;; Check Cytoscape autoungrabify or other settings
                                (log! :info (str "Cytoscape autoungrabify: " (.autoungrabify cy-instance)))
                                (log! :info (str "Cytoscape autolock: " (.autolock cy-instance)))
                                (log! :info (str "Cytoscape zoom: " (.zoom cy-instance)))

                                (.off cy-instance "render"))
                              2000)))))
                     100))))
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
              ;; Capture layout before destroying
              (when-let [cy-instance (:cy-instance @graph-state)]
                (let [layout-data (capture-layout cy-instance)]
                  (log! :info (str "Captured layout data: " (pr-str layout-data)))
                  ;; Send layout data to server
                  (when (and message-id layout-data)
                    (let [{:keys [pid cid]} @common-info
                          parsed-graph (:parsed-graph @graph-state)
                          ;; Get the first inquiry-area-id from the ORM data
                          inquiry-area-id (get-in parsed-graph [:inquiry-areas 0 :inquiry-area-id])
                          ;; Update the parsed graph with the new layout (no double stringification)
                          updated-graph (update-in parsed-graph [:inquiry-areas 0]
                                                   assoc :layout layout-data)]
                      (log! :info (str "Sending ORM layout for message " message-id
                                       " in project " pid " conversation " cid
                                       " inquiry-area: " inquiry-area-id))
                      (ws/send-msg {:dispatch-key :save-orm-layout
                                    :pid pid
                                    :cid cid
                                    :message-id message-id
                                    :inquiry-area-id inquiry-area-id
                                    :layout-data layout-data})
                      ;; Update the local message data
                      (when-let [update-fn (lookup-fn :update-msg-orm)]
                        (log! :info "Updating local message with new layout")
                        (update-fn message-id (str updated-graph))))))
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
                    "✕ CLOSE"))))))))
