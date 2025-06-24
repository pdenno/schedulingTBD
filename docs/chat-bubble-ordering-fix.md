# Chat Bubble Ordering Fix - EXPERIMENTAL

## Problem

Chat bubbles were appearing out of chronological order in the client, despite having timestamps. This was particularly noticeable when running `(user/run-demo!)` where messages that should appear sequentially would sometimes arrive scrambled.

## Root Cause Analysis

The issue was identified in the `send-to-client` function in `src/server/scheduling_tbd/web/websockets.clj`. Messages were being sent using async `go` blocks:

```clojure
(go (>! out (str msg-obj)))
```

Since `go` blocks execute asynchronously and can be scheduled independently by the core.async thread pool, multiple concurrent calls to `send-to-client` could result in messages arriving at the client out of order.

**Key insight**: The client puts timestamps on messages in the order it processes them, so out-of-order delivery results in out-of-order timestamps and display.

## Attempted Solution

Replaced the async `go` block with a synchronous approach that blocks until each message is actually sent:

```clojure
;; Old (async, could race):
(go (>! out (str msg-obj)))

;; New (synchronous, guaranteed order):
(let [sent-promise (p/deferred)]
  (async/put! out (str msg-obj) #(p/resolve! sent-promise true))
  @sent-promise) ; Block until message is actually sent
```

## Skepticism and Potential Issues

**This fix may not actually solve the problem** for several reasons:

### 1. **Performance and Deadlock Concerns**
- Blocking in `send-to-client` could cause performance issues or deadlocks
- The websocket system was designed to be async for good reasons

### 2. **Deeper Source Issues**
- The real ordering problem might be in the conversation flow logic
- Multiple threads might still be calling `send-to-client` concurrently from different parts of the system
- The mocking system might be sending messages out of original order

### 3. **Channel Behavior**
- Core.async channels might still reorder messages internally
- Buffer behavior could cause issues we haven't considered

### 4. **Timing Issues**
- If the demo sleeps 1-2 seconds between messages and they STILL arrive out of order, the problem might be much deeper
- The issue might be in the client-side processing or display logic

## Alternative Investigation Paths

1. **Trace the actual call sequence** in `run-demo!` to see where messages originate
2. **Examine the mocking system** to see if it's replaying messages out of order
3. **Check thread usage** in the conversation flow
4. **Client-side debugging** to see exact arrival order vs display order
5. **Network buffering issues** at the websocket level

## Testing Required

Before considering this fix successful:

1. Test with a reconnected client
2. Run the full demo and verify message order
3. Check for any performance degradation
4. Monitor for deadlocks or hanging behavior

## Status: EXPERIMENTAL

This fix addresses one potential cause but may not be the root issue. Further investigation needed if problems persist.

## Files Modified

- `src/server/scheduling_tbd/web/websockets.clj` - Updated `send-to-client` function

## Date

June 24, 2025

