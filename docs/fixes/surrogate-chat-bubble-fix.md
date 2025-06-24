# Surrogate Chat Bubble Fix

**Date:** June 24, 2025  
**Issue:** Surrogate chat bubbles appeared empty when running `(user/run-demo!)` with a client  
**Status:** ✅ FIXED

## Problem Description

When running the demo system with a web client, surrogate responses were not displaying in the chat interface. The chat bubbles for surrogate messages appeared but contained no text.

## Root Cause Analysis

The issue was a **key mismatch** between server-side and client-side WebSocket message handling:

### Server-Side Code (sending messages)
- `scheduling-tbd.surrogate/surrogate-follow-up` sends: `{:dispatch-key :sur-says :text answer}`
- `scheduling-tbd.iviewr.interviewers/chat-pair-interviewees` sends: `{:dispatch-key :sur-says :text ...}`

### Client-Side Code (receiving messages)
- `stbd-app.components.chat` expected: `{:dispatch-key :sur-says :msg ...}`

The client-side handler was looking for a `:msg` key, but the server was sending a `:text` key.

## Files Involved

### Server-Side (correct, sending `:text`)
- `src/server/scheduling_tbd/surrogate.clj` - `surrogate-follow-up` function
- `src/server/scheduling_tbd/iviewr/interviewers.clj` - `chat-pair-interviewees` function

### Client-Side (fixed to expect `:text`)
- `src/app/stbd_app/components/chat.cljs` - `:sur-says` message handler

## Solution Applied

Changed the client-side `:sur-says` message handler from:

```clojure
(register-fn :sur-says (fn [{:keys [p-key msg]}]
                         (when p-key (remember-promise p-key))
                         (log! :info (str "sur-says msg: " msg))
                         (add-msg msg :surrogate)))
```

To:

```clojure
(register-fn :sur-says (fn [{:keys [p-key text]}]
                         (when p-key (remember-promise p-key))
                         (log! :info (str "sur-says text: " text))
                         (add-msg text :surrogate)))
```

## Testing

To test this fix:

1. Start the system: `(start)`
2. Connect a web client to the system
3. Run the demo: `(user/run-demo!)`
4. Verify that surrogate responses now appear in chat bubbles

## Additional Notes

- This fix maintains consistency with how `:iviewr-says` messages are handled (they also use `:text`)
- The `:text` key is the standard throughout the system for message content
- The debugging log message was also updated to reflect the correct key name

## Prevention

To prevent similar issues:

- Review WebSocket message schemas in `scheduling-tbd.specs`
- Ensure client and server use consistent key names for message data
- Add integration tests that verify end-to-end message flow
