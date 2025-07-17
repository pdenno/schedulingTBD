# Fixed Issues Index

This directory contains documentation for various fixes applied to the SchedulingTBD project.

## Table Component Fixes

### [Table EditCell Cursor Position Fix](table-editcell-cursor-fix.md)
**Date**: 2025-07-05  
**Component**: `src/app/stbd_app/components/table.cljs`  
**Issue**: EditCell cursor positioning problem during text editing  
**Status**: ✅ **Fixed**

**Problem**: When editing table cells, the first character typed correctly, but subsequent characters jumped to the end of the text field, making mid-text editing impossible.

**Solution**: 
- Changed from content-dependent key (`:key text`) to stable position-based key
- Implemented controlled component pattern with local state management
- Added dual state updates for responsive UI and data persistence

---

## WebSocket Fixes

### [WebSocket Port Configuration Fix](websocket-port-configuration-fix.md)
**Status**: ✅ **Fixed**  
**Component**: WebSocket configuration system

---

## UI Component Fixes  

### [Surrogate Chat Bubble Fix](surrogate-chat-bubble-fix.md)
**Status**: ✅ **Fixed**  
**Component**: Chat interface components

---

## Guidelines for Adding New Fix Documentation

When documenting a new fix:

1. **Create detailed documentation** in this directory using the pattern: `[component]-[issue-description]-fix.md`
2. **Include**:
   - Date of fix
   - Component/file affected  
   - Clear problem description
   - Root cause analysis
   - Solution implemented
   - Code changes (before/after)
   - Testing instructions
3. **Update this index** with a brief summary
4. **Add inline comments** in the code referencing the documentation
5. **Test thoroughly** before marking as fixed

## Template

Use this template for new fix documentation:

```markdown
# [Component] [Issue Description] Fix

**Date**: YYYY-MM-DD  
**Component**: `path/to/component.cljs`  
**Issue**: Brief description

## Problem Description
[Detailed description of the issue]

## Root Cause Analysis  
[Technical analysis of why the issue occurred]

## Solution Implemented
[Detailed description of the fix]

## Code Changes
[Before/after code examples]

## Testing
[How to verify the fix works]

## Related Files
[List of affected files]
```
