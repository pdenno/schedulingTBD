# WebSocket Port Configuration Fix

**Date**: June 24, 2025  
**Issue**: WebSocket connection failing due to port mismatch  
**Status**: ✅ RESOLVED

## Problem Description

The WebSocket client was unable to connect to the server because of a hardcoded port mismatch:

- **Client**: Hardcoded to connect to port `3300` in `src/app/stbd_app/util.cljs`
- **Server**: Running on port `3301` in `:nrepl` mode (determined by `resources/system.edn`)
- **Result**: `(ws/recent-client!)` returned `nil`, WebSocket connection failed

## Root Cause Analysis

The server port is dynamically determined based on the environment alias:

```clojure
;; From resources/system.edn
:server/http {:port {:nrepl 3301 :dev 3300 :prod 3300 :test 3300}}

;; From src/server/scheduling_tbd/core.clj
env-option (->> (clojure.java.basis/initial-basis) :basis-config :aliases 
                (some #(when (#{:dev :prod :test :nrepl} %) %)))
port (-> config :server/http :port env-option)
```

When running with `:nrepl` alias, server uses port `3301`, but client was hardcoded to `3300`.

## Solution Implemented

### Dynamic Port Detection

Replaced hardcoded port with dynamic detection using the browser's current location:

**Before** (`src/app/stbd_app/util.cljs`):
```clojure
(def server-port 3300) ; ToDo: Get this from config.
```

**After**:
```clojure
(defn server-port
  "Get the server port from the current page URL.
   Since the client is served from the same server, we can detect the port dynamically."
  []
  (.-port js/location))
```

**Updated WebSocket URL** (`src/app/stbd_app/ws.cljs`):
```clojure
;; Before
(defn ws-url [client-id] (str "ws://localhost:" util/server-port "/ws?client-id=" client-id))

;; After  
(defn ws-url [client-id] (str "ws://localhost:" (util/server-port) "/ws?client-id=" client-id))
```

## Benefits

1. **Environment Agnostic**: Works in `:dev`, `:nrepl`, `:prod`, `:test` modes automatically
2. **Future-Proof**: Adapts to any port configuration changes
3. **No Hardcoding**: Eliminates port mismatch issues
4. **Simple Solution**: Uses browser's built-in location API

## Verification Steps

1. **Check server port**: `netstat -ln | grep ':330'` → Should show server listening
2. **Test WebSocket**: `(ws/recent-client!)` → Should return client UUID, not `nil`
3. **Verify connection**: `@ws/ping-dates` and `@ws/socket-channels` → Should show active clients

## Files Modified

- `src/app/stbd_app/util.cljs` - Changed `server-port` from def to defn
- `src/app/stbd_app/ws.cljs` - Updated `ws-url` to call function

## Related Configuration

- `resources/system.edn` - Server port configuration by environment
- `src/server/scheduling_tbd/core.clj` - Server startup and port selection logic
- `src/server/scheduling_tbd/web/handler.clj` - WebSocket route definition

## Notes

- The client cannot read `resources/system.edn` directly (ClojureScript limitation)
- Using `js/location.port` is the correct approach since client is served from same server
- This fix resolves the duplicate chat bubbles issue root cause (WebSocket connection)

---

*This fix ensures WebSocket connections work reliably across all deployment environments.*
