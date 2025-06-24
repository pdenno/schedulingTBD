# Debugging Duplicate Chat Bubbles Issue

**Date:** June 24, 2025  
**Status:** RESOLVED

## Problem Description

User reported duplicate chat bubbles appearing in the web interface, suspecting it might be related to multiple Java processes running.

## Investigation

### Process Analysis
Found 3 Java processes running:
1. **Process 3256661**: shadow-cljs (frontend compilation) - ports 1818, 9631, 7002
2. **Process 3260779**: Main server (SchedulingTBD) - ports 3301, 7888 
3. **Process 3261119**: clojure-mcp server

### Root Cause Discovery
The issue was **NOT** multiple Java processes competing for port 3301 (only one process owned it), but rather **multiple websocket connections** from the frontend:

```clojure
;; Found 2 active websocket connections
(keys @ws/socket-channels)
=> ("e2e79f63-2963-4330-ba72-a92f574d1e87"
    "8c7bd18a-83b5-4d47-bc46-1acae47b2d83")
```

Both connections were active and recent, causing duplicate message delivery to the frontend.

## Solution

### Temporary Fix
Closed the stale websocket connection:

```clojure
(require '[scheduling-tbd.web.websockets :as ws])
(ws/close-ws-channels "e2e79f63-2963-4330-ba72-a92f574d1e87")
```

### Permanent Fix (IMPLEMENTED)
Added automatic duplicate connection prevention in `establish-websocket-handler`:

```clojure
;; Close any existing connection for this client-id to prevent duplicates
(when (get @socket-channels client-id)
  (log! :info (str "Replacing existing connection for client: " client-id))
  (close-ws-channels client-id)
  (Thread/sleep 100)) ; Brief pause to allow cleanup
```

This ensures that when a new websocket connection is established with the same client-id, any existing connection is automatically closed first, preventing duplicate connections and thus duplicate chat bubbles.

### Implementation Details

**File Modified:** `src/server/scheduling_tbd/web/websockets.clj`  
**Function:** `establish-websocket-handler`

**Changes Made:**
1. Added duplicate connection detection using `(get @socket-channels client-id)`
2. Automatic cleanup of existing connections with `(close-ws-channels client-id)`
3. Brief pause `(Thread/sleep 100)` to allow cleanup completion
4. Logging of connection replacement for debugging

**Behavior:**
- When browser refreshes or hot reloading occurs, old connections are automatically cleaned up
- Only one websocket connection per client-id is maintained
- Connection replacement is logged for monitoring
- No breaking changes to existing functionality

## Lessons Learned

1. **Multiple processes** can coexist safely if they use different ports
2. **Websocket connection management** is critical - stale connections cause UI duplication
3. **Frontend hot reloading** can create phantom connections that need cleanup
4. **Always check websocket connection count** when debugging UI duplication issues

## Prevention

- Implement better websocket cleanup on frontend disconnection
- Add connection monitoring/cleanup scheduled tasks
- Consider implementing connection limits per session

## Commands for Future Debugging

```clojure
;; Check active websocket connections
(require '[scheduling-tbd.web.websockets :as ws])
(keys @ws/socket-channels)

;; Check connection details
(doseq [client-id (keys @ws/socket-channels)]
  (println "Client:" client-id)
  (println "  Exiting?" (ws/exiting? client-id))
  (when-let [ping-date (get @ws/ping-dates client-id)]
    (println "  Last ping:" ping-date)))

;; Clean up stale connections
(ws/close-inactive-channels)

;; Force close specific connection
(ws/close-ws-channels "client-id-here")
```

## Demo Testing Notes

### Running the Demo
```clojure
(user/run-demo!) ; Default uses :sur-craft-beer project
(user/run-demo! :sur-music-school) ; Specify different project
```

**Important:** The demo function includes `Thread/sleep` calls to simulate realistic timing, so it doesn't timeout quickly in the REPL. The function is designed to run for an extended period as it simulates a real interview conversation.

### Expected Behavior After Fix
- Single chat bubbles instead of duplicates
- Log message: "Replacing existing connection for client: [client-id]" when connections are cleaned up
- Smooth conversation flow without message duplication

## Related Files
- `src/server/scheduling_tbd/web/websockets.clj` - Websocket management (MODIFIED)
- `env/dev/user.clj` - Contains `run-demo!` function
- Browser dev tools network tab - Monitor connection status
