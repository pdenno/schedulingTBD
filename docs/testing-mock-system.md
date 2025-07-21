# Testing the Mock System with UI

## Current Testing Target

We are working on testing the LLM mocking system with the UI interface. The goal is to verify that mock responses work correctly through the full conversation flow.

## Test Command

The test we're working towards is:

```clojure
(mock/with-mock-project
  (fn [] (inv/resume-conversation {:client-id (ws/get-recent!) :pid :sur-craft-beer-lautering :cid :process})))
```

This command:
1. Enables mocking for a project
2. Gets the most recent websocket client ID
3. Resumes a conversation for the craft beer lautering project in the process conversation
4. Should use mock responses instead of real LLM calls

## System Status

- System started on port 3301 ✅
- Mocking enabled ✅
- Multiple projects available for testing ✅
- Web UI accessible ✅

## Current Issue

Need to work through the dependencies and make sure all required namespaces are available and the mock system integrates properly with the conversation resumption flow.

## Next Steps

1. Require necessary namespaces (inv, ws)
2. Test the resume-conversation flow with mocking
3. Debug any integration issues
4. Verify mock responses appear in the UI
