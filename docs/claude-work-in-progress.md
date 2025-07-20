# Claude Work in Progress

## Important Notes
1. **DO NOT mark issues as fixed in documentation until they have been tested and verified by the user.**
2. **Debugging atoms**: Each file typically has a `diag` atom marked with `^:diag` metadata for debugging purposes. These should be kept separate from production code. In `orm.cljs`, we use a `graph-state` atom for storing the Cytoscape instance, while `diag` is reserved for debugging.

## Current Issues with ORM Implementation (2025-07-20)

### 1. Layout Persistence Issues
**Problem**: While the server correctly saves layout data, the client doesn't update its local copy after saving.
- Layout is saved to server via websocket on close
- Server stores layout in the inquiry area's `:layout` field  
- Client doesn't refresh the graph data after save, so reopening shows old positions
- **Fix needed**: Update client-side graph data after successful save, or refetch from server

### 2. UI Button Confusion  
**Problem**: The "CLOSE" button both saves and closes, which is unclear to users.
- Current behavior: Single "CLOSE" button triggers layout save and closes dialog
- **Proposed solution**: 
  - Replace "CLOSE" with "SAVE" button that saves layout and closes
  - Add "DISMISS" button that closes without saving changes
  - This gives users explicit control over whether changes are persisted

### 3. ~~Scrollbar Flashing Bug~~ ✅ RESOLVED (2025-07-20)
**Solution**: Removed the MUI Box wrapper around the Cytoscape div.

**Root Cause**: The MUI Box component was interfering with Cytoscape's rendering system, causing:
- Re-rendering cycles from Box prop changes or context updates
- CSS property conflicts affecting canvas sizing calculations  
- Flexbox/Grid layout recalculations triggering resize observers
- MUI's dynamic styling system causing layout thrashing
- Event bubbling interference with Cytoscape's internal state

**Fix Applied**: Mount Cytoscape directly to a plain div without MUI component wrappers.

**Lesson Learned**: Third-party rendering libraries like Cytoscape should be mounted to plain DOM elements rather than within styled component systems to avoid rendering conflicts.

## Successfully Completed Features

### ORM Diagram Implementation ✅
1. **Basic structure**: Entities and role boxes with proper connections
2. **Mandatory constraints**: Purple dots for "must", blue dots for "should"  
3. **Uniqueness constraints**: Purple bars above constrained compartments
4. **Dynamic compartments**: Role boxes resize based on relationship arity
5. **Precise edge targeting**: Edges connect to specific compartments
6. **Drag synchronization**: Anchors follow when nodes are dragged
7. **Save layout infrastructure**: Captures and sends position data to server

### Technical Architecture ✅
- SVG generation for role boxes with compartments
- Anchor nodes for compartment-specific connections
- Proper data structure mapping (objects, reference-modes, mandatory, uniqueness)
- WebSocket integration for saving layouts
- Message ID and inquiry area tracking

## Previous Work Log

### ORM Scrollbar Issue (2025-07-20)
**Initial attempts to fix scrollbar flashing**:

Removed render event handler that was continuously checking viewport:
```clojure
;; REMOVED this problematic code:
(.on cy-instance "render"
     (fn []
       (let [extent (.extent cy-instance)
             viewport-width (.-offsetWidth container)
             viewport-height (.-offsetHeight container)]
         (when (or (> (.-w extent) (* viewport-width 10))
                   (> (.-h extent) (* viewport-height 10)))
           (.fit cy-instance)))))
```

Changed layout configuration:
```clojure
:layout {:name "breadthfirst"  ; Changed from "cose"
         :animate false         ; Disable animation
         :animationDuration 0   ; Ensure no animation
         :fit true
         :directed false
         :padding 50
         :spacingFactor 1.5}
```

### Dialog Layout Fix (2025-07-17)
Fixed initial scroll thumb shrinking issue:
```clojure
;; Simple structure with direct positioning
($ Box {:style {:position "relative" :width "100%" :height "100vh"}}
   ;; Cytoscape fills entire container
   ($ "div" {:ref cy-ref :style {:position "absolute" ...}})
   ;; Button overlaid on top as sibling
   ($ Box {:style {:position "absolute" :top "20px" :right "20px" :zIndex 9999}}
      ($ Button {...} "✕ CLOSE")))
```

### Future Enhancements to Consider
- Role labels within compartments showing reference modes
- Hover states for object definitions
- Inter-fact-type constraints visualization
- Performance optimization for large diagrams
- Multiple inquiry area display/selection
