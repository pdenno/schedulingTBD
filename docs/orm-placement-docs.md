# ORM Diagram User Placement Feature

## Overview

The ORM (Object-Role Modeling) diagram component now supports persistent user placement of nodes. When users drag entities or role boxes to new positions in the diagram, these positions are automatically saved and restored when the diagram is reopened.

## Implementation Details

### Key Functions

#### `capture-layout`
Captures the current state of the Cytoscape diagram including:
- Node positions for all entities and role boxes
- Zoom level
- Pan position (viewport offset)

```clojure
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
```

#### `apply-layout`
Restores a previously saved layout to the diagram:
- Applies saved positions to each node
- Restores zoom level
- Restores pan position

```clojure
(defn apply-layout
  "Apply saved layout to Cytoscape instance"
  [cy-instance layout-data]
  (let [layout-data (edn/read-string layout-data)]
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
```

### Persistence Flow

1. **Opening the Diagram**: When the ORM modal opens, it checks for saved layout data in the graph structure
   - If layout exists, it uses `preset` layout mode with saved positions
   - If no layout exists, it uses `breadthfirst` layout for automatic arrangement

2. **User Interaction**: 
   - Users can drag entities and role boxes freely
   - Role box anchor nodes (invisible connection points) automatically update when dragged
   - All Cytoscape pan/zoom controls remain available

3. **Saving Layout**: When the modal closes via `handle-close`:
   - Current layout is captured using `capture-layout`
   - Layout data is sent to the server via WebSocket
   - Local message data is updated with the new layout
   - The layout is stored with the inquiry area data

### Layout Data Structure

The saved layout contains:
```clojure
{:node-positions {"entity-1" {:x 100 :y 200}
                  "entity-2" {:x 300 :y 200}
                  "role-box-1" {:x 200 :y 300}
                  ;; ... more positions
                  }
 :zoom 1.5
 :pan {:x 50 :y -20}}
```

### Special Handling for Role Boxes

Role boxes have special anchor nodes for precise edge connections. When a role box is dragged:
- The main role box node moves to the new position
- All associated anchor nodes are automatically repositioned
- Anchor positions are calculated based on:
  - The role box position and dimensions
  - The compartment index within the role box
  - The relative position of connected entities (top/bottom edge placement)

## User Experience

1. **First Time Opening**: Diagram arranges nodes automatically using breadth-first layout
2. **Customizing Layout**: Users drag nodes to preferred positions
3. **Closing Dialog**: Positions are automatically saved
4. **Reopening**: Diagram appears exactly as the user left it, including zoom and pan state

## Technical Notes

- Layout data is stored per inquiry area within the ORM data structure
- The WebSocket message `:save-orm-layout` handles server-side persistence
- The component uses React hooks (`use-state`, `use-effect`) for lifecycle management
- Cytoscape instance cleanup is carefully managed to prevent memory leaks
- Anchor node synchronization happens on both role box and entity drag events

## Benefits

- **Consistency**: Users see their diagrams exactly as they arranged them
- **Efficiency**: No need to rearrange nodes repeatedly
- **Flexibility**: Each inquiry area can have its own custom layout
- **Completeness**: Zoom and pan state are also preserved, not just positions