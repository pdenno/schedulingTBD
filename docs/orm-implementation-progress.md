# ORM Diagram Implementation Progress

## Overview
Implementation of Object-Role Modeling (ORM) diagrams using Cytoscape.js with custom SVG role boxes for the schedulingTBD project.

# ORM Diagram Implementation Progress

## Overview
Implementation of Object-Role Modeling (ORM) diagrams using Cytoscape.js with custom SVG role boxes for the schedulingTBD project.

## 🎉 **CURRENT STATUS: Refining the diagram presentation**

### ✅ **Successfully Achieved:**
1. **Basic ORM Structure**: Entities (blue rectangles) and role boxes (yellow rectangles) properly positioned
2. **Edge Connections**: Lines correctly connect entities to role boxes
3. **Dynamic Sizing**: Role boxes resize based on arity (relationship compartment count)
4. **SVG Rendering Mechanism**: Proved that function-based `background-image` works in Cytoscape.js
5. **Compartment Proof-of-Concept**: orm-v9.jpeg showed working 2-compartment role boxes with visible divisions
6. **🎉 BREAKTHROUGH - Dynamic SVG Compartments**: orm-v12.jpeg shows fully working ORM role boxes with proper compartment divisions!
7. **Authentic ORM Appearance**: 2-ary and 3-ary role boxes with black compartment borders
8. **✅ PRECISION EDGE TARGETING**: Implemented anchor nodes for compartment-specific connections
9. **✅ PROPER DATA STRUCTURE MAPPING**: Edges now respect consistent ordering across :objects, :reference-modes, :deontic-keys, :uniqueness
10. **✅ PRODUCTION-READY IMPLEMENTATION**: Clean code, invisible anchors, proper window sizing

### ✅ **Many Issues RESOLVED:**
- **✅ Fixed compartments in v12**: Discovered correct Cytoscape.js API for data access
- **Root cause**: Wrong data access method - needed `.data node "svg-url"` instead of `j/get-in node [:data :svg-url]`
- **✅ Fixed role box targeting**: Implemented anchor nodes to connect entities to specific compartments
- **Root cause**: Edges were connecting to role box centers instead of specific compartments based on :objects array position
- **✅ Fixed Cytoscape.js API calls**: Resolved `TypeError: role_box.data(...).id is not a function`
- **Root cause**: Incorrect API usage - needed `(.id node)` instead of `(.id (.data node))`
- **✅ Fixed anchor positioning math**: Implemented precise compartment center calculations
- **✅ Fixed dialog timeout**: Removed problematic MUI properties causing premature closure
- **✅ Improved screen usage**: Doubled window size from 800x600 to 1600x1000

### 🏗️ **Architecture in Place:**
- **File**: `src/app/stbd_app/components/orm.cljs`
- **SVG Generation**: `generate-role-box-svg` function creates compartmented rectangles with optional uniqueness bars
- **Node Creation**: `role-box-nodes` generates nodes with embedded SVG data
- **Styling**: Function-based background-image approach (proven to work)

## Technical Implementation

### Key Functions (FINAL IMPLEMENTATION)
- `generate-role-box-svg [arity uniqueness-pattern]` - Creates SVG markup for role boxes with compartments and uniqueness bars
- `role-box-nodes [orm-data]` - Generates main role box nodes with SVG data PLUS invisible anchor nodes for each compartment
- `role-edges [orm-data]` - Creates edges connecting entities to specific compartment anchor nodes using map-indexed
- `orm-stylesheet []` - CSS styling with function-based background images and invisible anchor node styling

### Data Structure Mapping (CRITICAL UNDERSTANDING)
The implementation correctly handles the consistent ordering in ORM fact types:
```clojure
{:fact-type-id "ORDER-has-PRODUCT-QUANTITY"
 :objects ["order" "product" "quantity"]           ; Position 0, 1, 2
 :reference-modes ["order-number" "product-code" "quantity"]  ; Same ordering
 :deontic-keys ["mandatory" "" ""]                 ; Same ordering
 :uniqueness [["key1" "key1" ""]]}                 ; Same ordering
```

**CRITICAL**: Each entity connects to its corresponding compartment:
- "order" entity → compartment 0 (leftmost) via anchor `"ORDER-has-PRODUCT-QUANTITY-compartment-0"`
- "product" entity → compartment 1 (middle) via anchor `"ORDER-has-PRODUCT-QUANTITY-compartment-1"`
- "quantity" entity → compartment 2 (rightmost) via anchor `"ORDER-has-PRODUCT-QUANTITY-compartment-2"`

### Anchor Node Architecture (KEY INNOVATION)
```clojure
;; Main role box node
{:data {:id "ORDER-has-PRODUCT-QUANTITY"
        :type "role-box"
        :svg-url "data:image/svg+xml;..."
        :width 90 :height 28 :arity 3}}

;; Invisible anchor nodes (one per compartment)
{:data {:id "ORDER-has-PRODUCT-QUANTITY-compartment-0"
        :type "role-anchor"
        :compartment-index 0
        :object-id "order"
        :parent-role-box "ORDER-has-PRODUCT-QUANTITY"}}
```

### Compartment Positioning Math (FINAL FORMULA)
```
compartment-center-x = role-center-x + role-width * ((compartment-index + 0.5) / arity - 0.5)
```

**Example calculation** for 3-compartment role box at (100, 50) with width 90:
- Compartment 0: 100 + 90 * (0.5/3 - 0.5) = 100 - 15 = 85 (left)
- Compartment 1: 100 + 90 * (1.5/3 - 0.5) = 100 + 0 = 100 (center)
- Compartment 2: 100 + 90 * (2.5/3 - 0.5) = 100 + 15 = 115 (right)

### SVG Role Box Features
- **Variable arity**: 2-5+ compartments supported
- **Uniqueness bars**: Black bars above constrained compartments
- **Standard ORM appearance**: Rectangular compartments with black borders
- **Dynamic sizing**: Width = arity × 30px, height = 25px + 3px for bars

## Evolution Screenshots (COMPLETE JOURNEY)
- **orm-v1.jpeg**: Initial triangular relationship nodes
- **orm-v4.jpeg**: Pipe-separated text labels approach
- **orm-v7.jpeg**: Yellow rectangles with correct structure
- **orm-v9.jpeg**: ✅ **BREAKTHROUGH** - Visible compartments with hardcoded SVG
- **orm-v10.jpeg**: ❌ Lost compartments with dynamic SVG access
- **orm-v11.jpeg**: Debugging data access patterns
- **orm-v12.jpeg**: 🎉 **SUCCESS** - Dynamic SVG compartments working perfectly!
- **orm-v14.jpeg**: ❌ First attempt at anchor positioning - scattered red dots and oversized role boxes
- **orm-v15.jpeg**: ❌ Improved sizing but red dots still misplaced due to coordinate issues
- **orm-v16.jpeg**: ❌ Better screen usage but persistent red dot positioning problems
- **orm-v17.jpeg**: ✅ **BREAKTHROUGH** - Red dots correctly positioned at role box centers (debug mode)
- **orm-v18.jpeg**: ⚠️ **ISSUE** - All nodes on same Y level making connections unclear, anchors not following dragged nodes
- **orm-v19.jpeg**: ✅ **VALIDATED** - Manual node repositioning reveals correct connections
- **orm-v20.jpeg**: 🎯 **IMPROVED** - Cose layout provides Y distribution, drag synchronization works
- **orm-v21.jpeg**: 🎉 **ORM-COMPLIANT** - Edges connect to compartment top/bottom edges based on entity position
- **orm-v22.jpeg**: ✅ **FEATURE-COMPLETE** - All ORM features implemented:
  - White SVG backgrounds with proper viewBox
  - Uniqueness bars displayed correctly above compartments
  - Mandatory dots on entity-side of connections
  - Role box labels displayed above boxes
  - Dialog no longer auto-closes (explicit Close button)
  - Reduced node sizes for better diagram fit
- **orm-v23.jpeg**: 🐛 **SCROLL BUG FIXED** - Full screen display, no scroll bars, but close button hidden
- **orm-v24.jpeg**: 🎉 **FINAL SUCCESS** - Full screen ORM diagram with visible close button!

## Critical Technical Breakthroughs

### Breakthrough #1: Function-Based Properties & Correct Cytoscape.js API
```clojure
;; ❌ Doesn't work - CSS data() syntax
:background-image "data(svg-url)"

;; ❌ Doesn't work - wrong ClojureScript API
:background-image (fn [node] (j/get-in node [:data :svg-url]))

;; ✅ WORKS - proper Cytoscape.js API
:background-image (fn [node] (.data node "svg-url"))
```

### Breakthrough #2: Cytoscape.js Node API Mastery
```clojure
;; ❌ TypeError: role_box.data(...).id is not a function
role-id (.id (.data role-box))

;; ✅ Correct API usage
role-id (.id role-box)               ; Get node ID
object-id (.data node "object-id")  ; Get specific data property
```

### Breakthrough #3: Compartment Positioning Algorithm
The key insight was positioning anchor nodes AFTER layout completion using precise mathematical calculations:

```clojure
;; Position anchors at compartment centers after layout
(js/setTimeout
  (fn []
    (doseq [role-box (.toArray role-boxes)]
      (let [role-pos (.position role-box)
            role-width (.data role-box "width")
            arity (.data role-box "arity")]
        (doseq [anchor (.toArray anchor-nodes)]
          (when (= (.data anchor "parent-role-box") (.id role-box))
            (let [compartment-idx (.data anchor "compartment-index")
                  compartment-center-x (+ (.-x role-pos)
                                          (* role-width (- (/ (+ compartment-idx 0.5) arity) 0.5)))]
              (.position anchor #js {:x compartment-center-x
                                     :y (.-y role-pos)})))))))
  200) ; After layout completes
```

### Breakthrough #4: Layout & Drag Synchronization
Solved two final issues - nodes appearing on same Y level and anchors not following dragged nodes:

```clojure
;; 1. Changed from breadthfirst to cose layout for better Y distribution
:layout {:name "cose"
         :nodeRepulsion 8000
         :idealEdgeLength 100
         :gravity 80}

;; 2. Added drag event handler to keep anchors synchronized
(.on (.nodes cy-instance "[type='role-box']") "drag"
     (fn [evt]
       (let [dragged-node (.-target evt)]
         (update-anchors-for-role-box dragged-node))))
```

### Breakthrough #5: ORM-Compliant Edge Connections
Implemented dynamic top/bottom edge connections following ORM standards:

```clojure
;; Anchors positioned at compartment top/bottom edges based on entity position
(let [entity-y (when entity-node (.-y (.position entity-node)))
      role-y (.-y role-pos)
      anchor-y (if (and entity-y (< entity-y role-y))
                 (- role-y (/ role-height 2))  ; Top edge
                 (+ role-y (/ role-height 2)))] ; Bottom edge
  (.position anchor #js {:x compartment-center-x :y anchor-y}))

;; Updates on both entity and role-box drag for dynamic repositioning
(.on (.nodes cy-instance "[type='entity']") "drag" ...)
```

**Edge Rendering Options:**
1. **Implemented**: Dynamic anchor positioning at compartment edges
2. **Alternative**: Z-index layering to show edges above SVGs (`:z-index 3`)
3. **Result**: Standard ORM appearance with edges connecting to compartment perimeters

## IMPLEMENTATION Underway ✅

### 🎯 **Mostly working**
The ORM diagram tool now correctly displays **authentic Object-Role Modeling diagrams** where:
- ✅ Each entity connects to its designated compartment in role boxes
- ✅ Connections respect the consistent ordering in `:objects`, `:reference-modes`, `:deontic-keys`, `:uniqueness` arrays
- ✅ Role boxes display proper compartment divisions with black borders
- ✅ Uniqueness constraints show as purple bars (purple denoting an alethic constraint) above compartments
- ✅ All positioning is mathematically precise and semantically correct
- ✅ **FIXED BUG**: The 'canvas' on which the diagram is drawn grows continuously and may cause the Chrome browser to crash!
      This behavior was noticed because the scroll thumb (scroll handle) shrinks progressively. THIS could be the reason the diagrams do not remain displayed!
      **ROOT CAUSE**: Do not wrap the Cytoscape component in a MUI Box.
      **SOLUTION**: Unnested the Cytoscape component from the Box (removed the Box).
      **KEY LEARNING**: Be careful how you include a Cytoscape component in an MUI parent.
- ✅ **NEW**: Role box labels are displayed above each role box
- ✅ **NEW**: SVGs have white backgrounds and proper viewBox for correct rendering
- ✅ **NEW**: Dialog stays open until explicitly closed
- ✅ **NEW**: Diagram size optimized for typical screen viewing

### 🚀 **Production Status**
- **Code Quality**: Clean, well-commented, production-ready
- **Performance**: Efficient anchor positioning algorithm with drag synchronization
- **User Experience**: Doubled window size, proper dialog behavior, interactive node dragging
- **Layout**: Improved Y-axis distribution with cose layout algorithm
- **Debugging**: Invisible anchors, clean logs
- **Architecture**: Scalable for future enhancements

### 🎓 **Lessons Learned**
1. **API Mastery**: Understanding Cytoscape.js node API patterns
2. **Timing Matters**: Layout completion before anchor positioning
3. **Mathematical Precision**: Exact compartment center calculations
4. **Data Structure Understanding**: ORM's consistent array ordering principle
5. **Production Polish**: Invisible helpers, clean code, proper sizing

### 📋 **Future Enhancement Opportunities**
- **Role Labels**: Text within compartments showing reference modes
- **Enhanced Uniqueness Patterns**: Complex constraint visualizations
- **Interactive Features**: Hover states showing object definitions
- **Constraint Visualization**: Inter-fact-type constraints and object subtyping
- **Performance Optimization**: For larger diagrams with many fact types

## Architecture Notes
- **Chat integration**: ORM modal triggered by `:message/graph--orm` property
- **Data flow**: EDN string → parsed data → Cytoscape elements → SVG rendering
- **Styling approach**: Function-based properties for dynamic content
- **Fallback**: Yellow background when SVG fails to load

---
*Implementation completed: 2025-07-17*
*Key Innovation: Invisible anchor nodes with mathematical compartment positioning and drag synchronization*
