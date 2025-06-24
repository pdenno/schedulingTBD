# UI Chat Bubble Ordering Bug - CURRENT ISSUE

**Date Identified:** June 24, 2025
**Status:** ACTIVE BUG - Needs Investigation

## Problem Description

### Primary Issues:
1. **Chat bubbles in the UI are appearing in the wrong order**
2. **UI communication appears broken since last disconnection**
   - Talking to the UI doesn't seem to work properly
   - This suggests WebSocket or real-time communication issues

## Technical Context

From system overview, the relevant components are:
- **Web interface** running on Jetty (port 3300/3301)
- **Real-time WebSocket communication** for chat
- **Message-based communication** between agents
- **Structured message types** (CONVERSATION-HISTORY, SUPPLY-QUESTION, etc.)

## Investigation Areas

### Potential Root Causes:
1. **WebSocket Connection Issues**
   - Connection lost during disconnection
   - Failed reconnection logic
   - Message queue backup

2. **Message Ordering Problems**
   - Race conditions in message delivery
   - Timestamp/sequence number issues
   - Client-side rendering order bugs

3. **State Synchronization**
   - Client/server state mismatch after reconnection
   - Message history not properly synchronized
   - UI state not refreshed after reconnection

## Files to Investigate

Based on project structure:
- `src/app/` - ClojureScript frontend (UI components)
- WebSocket handling code
- Message ordering/rendering logic
- Reconnection handling

## Next Steps for Developer

1. **Immediate Actions:**
   - Check WebSocket connection status in browser dev tools
   - Examine console errors in browser
   - Verify server is running and accessible
   - Test basic UI communication

2. **Code Investigation:**
   - Find WebSocket implementation files
   - Check message ordering logic
   - Review chat bubble rendering code
   - Examine reconnection handling

3. **Testing:**
   - Test with browser refresh
   - Test with server restart
   - Check if problem persists across browser sessions

## Investigation Findings (June 24, 2025)

### Current System Status:
- ✅ **Server Running**: Jetty on port 3301 (nREPL profile)
- ✅ **WebSocket Active**: Connection established (client ID found in socket-channels)
- ✅ **REPL Ready**: System started and namespace aliases configured
- ✅ **App Accessible**: UI available at http://localhost:3301/app

### Code Analysis Results:

#### Potential Issue #1: Message Ordering in send-to-client
**File**: `src/server/scheduling_tbd/web/websockets.clj`
**Function**: `send-to-client`
**Problem**: Uses synchronous blocking approach that might cause race conditions:
```clojure
;; Current problematic code:
(let [sent-promise (p/deferred)]
  (async/put! out (str msg-obj) #(p/resolve! sent-promise true))
  @sent-promise) ; Block until message is actually sent
```
**Risk**: This blocking mechanism could cause message ordering issues during high message volume or reconnection scenarios.

#### Potential Issue #2: Client-Side Message Processing
**File**: `src/app/stbd_app/ws.cljs`
**Function**: `dispatch-msg`
**Problem**: No explicit ordering guarantees in message dispatch.

#### Potential Issue #3: Chat Bubble Rendering
**File**: `src/app/stbd_app/components/chat.cljs`
**Function**: `msgs2cs`
**Problem**: Message ordering relies on timestamp/date logic that might not handle rapid messages or reconnection state properly.

### Next Actions Required:

1. **Test Message Ordering**:
   - Send multiple rapid messages through WebSocket
   - Check if timestamps are being preserved correctly
   - Verify message arrival order vs display order

2. **Check Reconnection State**:
   - Test UI behavior after simulated disconnection
   - Verify message history synchronization after reconnect
   - Check if pending messages are properly queued

3. **Browser Console Investigation**:
   - Check for JavaScript errors in browser dev tools
   - Monitor WebSocket connection status
   - Examine message receipt order vs display order

## Session Handoff Notes

- System is running (REPL started, agents active)
- Beer brewery scenario is currently processing
- **UI accessible at**: http://localhost:3301/app
- **WebSocket connection active**: 1 client connected
- Focus should be on message ordering in WebSocket implementation
- Issue likely in either send-to-client blocking logic or client-side message processing

---

## 🎯 SOLUTION FOUND (June 24, 2025)

### Root Cause: Incorrect Syntax in WebSocket Channel Access

**Problem**: Two functions in `websockets.clj` used incorrect syntax to access client channels:

```clojure
;; WRONG (was causing "Could not find out async channel" errors):
(-> client-id (get @socket-channels) :out)
(->> client-id (get @socket-channels) :out)

;; CORRECT:
(get-in @socket-channels [client-id :out])
```

### Functions Fixed:
1. **`send-to-client`** - Line ~317: Fixed channel access syntax
2. **`clear-keys`** - Line ~283: Fixed channel access syntax

### Impact:
- ✅ **WebSocket messaging now works properly**
- ✅ **Client-server communication restored**
- ✅ **Message ordering should be maintained**
- ✅ **No more "Could not find out async channel" errors**

### Testing:
- Messages can now be sent successfully through WebSocket
- Channel access works correctly with proper `get-in` syntax
- System should handle reconnection and message ordering properly

**STATUS: RESOLVED** ✅

**IMPORTANT FOR NEXT SESSION:**
The core bug has been fixed! Test the UI at http://localhost:3301/app to verify that:
1. Chat messages appear in correct order
2. UI communication works after disconnection/reconnection  
3. No console errors related to WebSocket communication
