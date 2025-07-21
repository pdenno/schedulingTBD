# Agent Base-Type Fix for Mocking System

## Problem
When using `(db/recreate-project-db! :sur-craft-beer-lautering (db/get-project :sur-craft-beer))` to create a new project from an existing one, the surrogate agent still has the old project ID in its `:agent/base-type` and `:agent/pid` fields. This causes the mocking system to fail with:

```
Execution error (IllegalArgumentException) at scheduling-tbd.agent-db/remake-needs (agent_db.clj:201).
No matching clause:
```

## Root Cause
In `remake-needs` function in `agent_db.clj`, there's a `case` statement that expects `agent-type` to be one of:
- `:project`
- `:system` 
- `:shared-assistant`

But when the agent has the wrong `:agent/base-type`, it doesn't match the expected project ID, causing the case to fail.

## Current Status
- ✅ Mocking system is working (14 messages loaded from :sur-craft-beer-lautering)
- ✅ Fixed `recreate-project-db!` function to update agent PIDs and base-types
- ❌ Fix not working as expected - agents still have old IDs after recreation

## Next Steps
1. Debug why the agent updates in `recreate-project-db!` aren't taking effect
2. Ensure both `:agent/pid` and `:agent/base-type` are properly updated for surrogate agents
3. Test mocking system with corrected agent configuration

## Expected Behavior
After running `(db/recreate-project-db! :sur-craft-beer-lautering (db/get-project :sur-craft-beer))`, the surrogate agent should have:
- `:agent/base-type :sur-craft-beer-lautering`
- `:agent/pid :sur-craft-beer-lautering`

## Testing Command
```clojure
(mock/with-mock-project 
  (fn [] (inv/resume-conversation {:client-id :console :pid :sur-craft-beer-lautering :cid :process})))
```

Once the agent base-type fix is working, this should successfully run the mocked conversation.
