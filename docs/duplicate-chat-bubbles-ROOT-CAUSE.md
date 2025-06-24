# DUPLICATE CHAT BUBBLES - ROOT CAUSE DISCOVERED

**Date:** June 24, 2025 14:18  
**Status:** ROOT CAUSE IDENTIFIED

## 🎯 Root Cause: Mixed Message Sources Without Coordination

The duplicate chat bubble issue is NOT from duplicate websocket connections, but from **two independent message sources** that don't coordinate properly:

### Message Source #1: Database (`get-conversation`)
```clojure
(defn get-conversation [pid cid]
  ;; ...
  (reset! msgs-atm conv)  ; ← COMPLETELY REPLACES message list
  ((lookup-fn :set-cs-msg-list) conv))
```

### Message Source #2: WebSocket (`add-msg`)
```clojure
(defn add-msg [text from]
  ;; ...
  (swap! msgs-atm conj {:message/content text ...}) ; ← APPENDS to message list
  ((lookup-fn :set-cs-msg-list) @msgs-atm))
```

## 🔍 Evidence: Test Messages at TOP

When test messages appeared at the **TOP** instead of **BOTTOM**, this revealed:
1. Test messages were added via WebSocket (`add-msg` - appends to end)
2. Then `get-conversation` ran and **reset** the entire list with DB messages
3. The display ordering is based on timestamps, putting newer test messages out of order

## 🐛 The Duplication Mechanism

1. **New message arrives via WebSocket** → `add-msg` → appears in chat
2. **Database gets updated** (somewhere in the server-side flow)
3. **`get-conversation` is called** → resets message list with DB messages
4. **Same message now appears twice**: once from WebSocket, once from DB

## 💡 Solution Options

### Option 1: Message Deduplication
Add deduplication logic in `add-msg` and `get-conversation` based on message ID/content/timestamp.

### Option 2: Single Source of Truth
Modify the flow so that:
- WebSocket messages are temporary/preview
- Database is the authoritative source
- Periodically refresh from DB

### Option 3: Coordinated Updates
Ensure that when a message is added via WebSocket, the DB update and subsequent `get-conversation` call are coordinated to prevent duplicates.

### Option 4: Timestamp-Based Merging
Instead of `reset!` in `get-conversation`, merge DB messages with existing messages based on timestamps, removing duplicates.

## 🔧 Recommended Fix: Message Deduplication

Implement deduplication logic that:
1. Assigns unique IDs to messages
2. Filters out duplicates when updating the message list
3. Sorts messages by timestamp regardless of source

## 📁 Key Files to Modify
- `src/app/stbd_app/components/chat.cljs` - `add-msg` and `get-conversation` functions
- Server-side message flow to ensure proper message IDs

## 🧪 Test Plan
1. Fix the message deduplication
2. Send test messages via WebSocket
3. Verify they appear at the bottom (correct timestamp order)
4. Run demo and verify no duplicate bubbles
5. Verify messages appear in correct chronological order
