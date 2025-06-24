# Duplicate Chat Bubbles Problem

**Date:** June 24, 2025  
**Issue:** Duplicate chat bubbles appearing on both sides during demo runs with client  
**Status:** ✅ FIXED (June 24, 2025)

## ✅ RESOLUTION

**Fixed:** June 24, 2025

### Root Cause Identified
Two separate `ws/send-to-client` calls were sending the same surrogate responses:
1. `chat-pair-interviewees` function immediately sent `:sur-says` via WebSocket  
2. `q-and-a` function later iterated through `msgs-vec` and sent each message again

This caused surrogate responses to appear twice in the chat interface.

### Solution Implemented
- **Removed** the duplicate `ws/send-to-client` call from `chat-pair-interviewees` function
- **Preserved** the message sending logic in `q-and-a` function to handle all WebSocket communication
- **Added** comments explaining the fix to prevent regression

### Files Modified
- `src/server/scheduling_tbd/iviewr/interviewers.clj` (lines 109-110)
- Removed: `(ws/send-to-client (merge {:client-id client-id :dispatch-key :sur-says} ...))`
- Added explanatory comments about the fix

### Expected Behavior
Messages should now appear only once, on the correct side of the conversation:
- System/interviewer messages: Left side (`:iviewr-says`)
- Surrogate/expert responses: Right side (`:sur-says`)
- No duplicate bubbles on both sides

---

## Problem Description

When running `(user/run-demo!)` with a web client connected, chat bubbles are appearing duplicated - messages show up on both sides of the conversation (both as incoming and outgoing messages).

## Context

- This issue occurs "often (always?)" according to user observation
- The problem appears to be related to recent transition to using `ws/send-to-client`
- This suggests there may be a message routing or dispatch issue
- The duplication affects the visual presentation of the conversation flow

## Potential Root Causes to Investigate

### 1. Double Message Sending
- Messages being sent through multiple pathways
- Both direct database updates AND WebSocket sends
- Overlapping message handling in different parts of the system

### 2. WebSocket Message Routing Issues
- Messages being dispatched to multiple handlers
- Incorrect dispatch keys causing messages to be processed twice
- Client-side message handling duplicating messages

### 3. Database vs WebSocket Sync Issues
- Messages added to database AND sent via WebSocket separately
- Client fetching messages from database while also receiving via WebSocket
- Race conditions between different message sources

### 4. Client-Side Display Logic
- Messages being added to chat UI multiple times
- React state updates causing duplicate renders
- Message deduplication logic missing or broken

## Areas to Investigate

### Server-Side Code
- `scheduling-tbd.iviewr.interviewers` - Q&A flow and message sending
- `scheduling-tbd.surrogate` - Surrogate response handling
- `scheduling-tbd.web.websockets` - WebSocket message sending
- `scheduling-tbd.db` - Database message storage

### Client-Side Code
- `stbd-app.components.chat` - Message display and handling
- `stbd-app.ws` - WebSocket message receiving
- `stbd-app.db-access` - Database message fetching

### Key Functions to Examine
- `ws/send-to-client` usage patterns
- Message addition flows (`add-msg`, `get-conversation`)
- WebSocket dispatch handlers (`:sur-says`, `:iviewr-says`)
- Database message persistence (`db/add-msg`)

## Investigation Plan

1. **Trace message flow** - Follow a single message from creation to display
2. **Identify duplication points** - Find where messages get duplicated
3. **Review recent changes** - Examine transition to `ws/send-to-client`
4. **Check message deduplication** - Verify if proper deduplication exists
5. **Test with logging** - Add debug logging to track message paths

## Expected Resolution

The fix will likely involve:
- Removing duplicate message sending pathways
- Ensuring messages are either sent via WebSocket OR added to database, not both
- Adding proper message deduplication on client side
- Fixing message direction logic (incoming vs outgoing)

## Testing Verification

After fix:
1. Run `(user/run-demo!)` with web client
2. Verify each message appears only once
3. Verify messages appear on correct side (incoming from system/surrogate, outgoing from human)
4. Test with different conversation types (:process, :data, :resources, :optimality)
5. Verify both real and mocked conversations work correctly
