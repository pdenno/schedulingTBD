# Table EditCell Cursor Position Fix

**Date**: 2025-07-05  
**Component**: `src/app/stbd_app/components/table.cljs`  
**Issue**: EditCell cursor positioning problem during text editing

## Problem Description

When editing table cells in the web interface, users experienced a cursor positioning bug:

1. **First character typed**: Inserted correctly at the cursor position
2. **Subsequent characters**: Jumped to the end of the text field
3. **Result**: Impossible to edit text in the middle of a cell

This made table editing frustrating and error-prone, especially when trying to make small corrections to existing text.

## Root Cause Analysis

The issue was caused by improper React key management in the `EditCell` component:

```clojure
;; PROBLEMATIC CODE (before fix)
($ TextField {:key text  ; ❌ Key changes with every text change!
              :defaultValue text
              ;; ... other props
              })
```

**What was happening:**
1. User types → text content changes → `:key text` changes  
2. React sees different key → destroys old component → creates new component
3. New TextField gets focus but cursor defaults to end position
4. Subsequent keystrokes always appear at the end

## Solution Implemented

### 1. **Stable Key Generation**
```clojure
;; ✅ FIXED: Stable key based on position, not content
:key (str row-id "-" cell-id)  ; Never changes during editing
```

### 2. **Local State Management**
```clojure
;; ✅ Added local state for immediate UI responsiveness
(let [[local-text set-local-text] (hooks/use-state text)]
  ;; ...
```

### 3. **Controlled Component Pattern**
```clojure
;; ✅ Changed from uncontrolled to controlled component
:value local-text          ; Instead of :defaultValue text
:onChange (fn [event]
            (let [val (j/get-in event [:target :value])]
              (set-local-text val)           ; Immediate local update
              (set-table-fn                  ; Persistent table update
               (assoc-in table [:table-body row-id cell-id] val))))
```

## Technical Details

### Before Fix
- **Component Type**: Uncontrolled with unstable key
- **Key Strategy**: `:key text` (content-dependent)
- **State Management**: Direct table state updates only
- **Behavior**: Component recreation on every change

### After Fix  
- **Component Type**: Controlled with stable key
- **Key Strategy**: `:key (str row-id "-" cell-id)` (position-dependent)
- **State Management**: Local state + table state (dual updates)
- **Behavior**: Component persists across text changes

## Code Changes

**File**: `src/app/stbd_app/components/table.cljs`  
**Function**: `EditCell`

```clojure
(defnc EditCell [{:keys [cell-id row-id text table set-table-fn]}]
  (let [[local-text set-local-text] (hooks/use-state text)]
    ($ TableCell {:key cell-id :sx #js {:padding "0"}}
       ($ TextField {:key (str row-id "-" cell-id) ; Stable key
                     :sx #js {:bgcolor "#fff6d9" :alignItems "stretch" :width "100%"}
                     :value local-text          ; Controlled component
                     :autoFocus true
                     :onChange (fn [event]
                                 (let [val (j/get-in event [:target :value])]
                                   (set-local-text val)     ; Local state
                                   (set-table-fn            ; Table state
                                    (assoc-in table [:table-body row-id cell-id] val))))}))))
```

## Benefits

✅ **Cursor Position Maintained**: Users can edit text anywhere in the cell  
✅ **Responsive UI**: Local state provides immediate visual feedback  
✅ **Performance**: Reduces component recreation overhead  
✅ **User Experience**: Natural text editing behavior  
✅ **Data Persistence**: Table state still updates correctly

## Testing

To verify the fix:

1. Open a table in the web interface
2. Click on any cell to enter edit mode
3. Position cursor in the middle of existing text
4. Type several characters
5. **Expected**: All characters appear at cursor position
6. **Previously**: First character correct, rest at end

## React Best Practices Applied

This fix demonstrates important React patterns:

- **Stable Keys**: Use position-based keys, not content-based
- **Controlled Components**: Prefer `:value` + `onChange` over `:defaultValue`
- **Local State**: Use local state for immediate UI updates
- **State Synchronization**: Maintain both local and persistent state as needed

## Related Files

- **Component**: `src/app/stbd_app/components/table.cljs`
- **Documentation**: `docs/fixes/table-editcell-cursor-fix.md` (this file)

## Notes for Future Development

When working with editable table components:

1. Always use stable, position-based keys for form inputs
2. Consider local state for responsive user interactions  
3. Test cursor positioning during rapid typing scenarios
4. Be cautious with content-dependent keys in React components
