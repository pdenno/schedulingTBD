# Debugging Duplicate Chat Bubbles - Continued Investigation

**Date:** June 24, 2025  
**Status:** INVESTIGATING - Previous fixes incomplete

## Current Situation

Despite documented fixes, duplicate chat bubbles are still occurring. 

## Investigation Findings

### Multiple Websocket Connections Still Present

Checking active connections:
```clojure
(keys @ws/socket-channels)
=> ("8d069298-4ac1-4a46-a65c-957dbbf03e58" 
    "fa65eec8-98c4-4509-a6b0-ac3963c9dfc6")
```

Both connections are active with recent ping times:
- Connection 1: Last ping 14:09:06
- Connection 2: Last ping 14:11:08  

### Root Cause Analysis

The issue appears to be more complex than previously thought:

1. **Multiple Active Connections**: Despite the "fix" in `establish-websocket-handler` to close existing connections, we still have 2 active websocket connections.

2. **Connection Management Issue**: The connection cleanup in `establish-websocket-handler` may not be working properly because:
   - The `close-ws-channels` function uses promises and delays (1000ms)
   - The cleanup happens asynchronously 
   - New connections might be established before old ones are fully closed

3. **Client-ID vs Connection-ID Confusion**: 
   - `@socket-channels` maps connection-id -> channel info
   - But `send-to-client` uses `client-id` to find the connection
   - If there's confusion between these, messages might be sent to multiple connections

### Key Code Areas

#### `send-to-client` function:
```clojure
(if-let [out (-> client-id (get @socket-channels) :out)]
```
This suggests `client-id` should directly map to a connection in `@socket-channels`.

#### `establish-websocket-handler` connection cleanup:
```clojure
(when (get @socket-channels client-id)
  (log! :info (str "Replacing existing connection for client: " client-id))
  (close-ws-channels client-id)
  (Thread/sleep 100))
```
This cleanup may be racing with new connection establishment.

## Hypothesis

The problem might be that:
1. Multiple browser tabs/windows are connected
2. Hot reloading creates new connections without properly closing old ones
3. The connection cleanup is not synchronous enough
4. There might be client-side code creating multiple websocket connections

## Next Steps

1. **Immediate Fix**: Force close all but one connection
2. **Debug Connection Creation**: Add more logging to see when/why multiple connections are created
3. **Fix Connection Cleanup**: Make the cleanup synchronous or more reliable
4. **Check Client-Side**: Examine if frontend is creating multiple connections

## Investigation Results - June 24, 2025 14:15

### Connection Cleanup Success
- **Before**: 2 active websocket connections
- **After**: 1 active websocket connection
- **Conclusion**: The connection cleanup in `establish-websocket-handler` **did work** but asynchronously

### Frontend Analysis
Examined client-side code:
- `:iviewr-says` handler: calls `(add-msg text :system)` - **single registration**
- `:sur-says` handler: calls `(add-msg text :surrogate)` - **single registration**
- No duplicate event handlers found

### Test Message Results
```clojure
;; Sent test messages
(ws/send-to-client {:dispatch-key :iviewr-says 
                    :client-id client-id 
                    :text "TEST: This is a test message from the interviewer"})
(ws/send-to-client {:dispatch-key :sur-says 
                    :client-id client-id 
                    :text "TEST: This is a test response from the surrogate"})
```
**Result**: User confirmed they saw the test messages

### Current Status
- ✅ Websocket connections reduced to 1
- ✅ Frontend handlers verified as single registration
- ✅ Test messages successfully delivered
- ❓ Need to verify if duplicates still occur in actual demo

## Commands for Debugging

```clojure
;; Check current connections
(require '[scheduling-tbd.web.websockets :as ws])
(keys @ws/socket-channels)

;; Test messages
(let [client-id (ws/recent-client!)]
  (ws/send-to-client {:dispatch-key :iviewr-says :client-id client-id :text "TEST MESSAGE" :promise? false}))

;; Force close specific connections  
(ws/close-ws-channels "connection-id-here")

;; Check connection details
(doseq [client-id (keys @ws/socket-channels)]
  (println "Client:" client-id)
  (println "  Exiting?" (ws/exiting? client-id))
  (when-let [ping-date (get @ws/ping-dates client-id)]
    (println "  Last ping:" ping-date)))
```
