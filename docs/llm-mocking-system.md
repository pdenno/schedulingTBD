# LLM Response Mocking System

This document describes the LLM response mocking system implemented for SchedulingTBD, which provides fast, reliable, and cost-effective testing capabilities by using pre-recorded conversation data.

## Overview

The mocking system allows the SchedulingTBD application to use pre-recorded conversation responses from `data/projects/*.edn` files instead of making expensive and slow API calls to LLM providers during testing and development.

## Benefits

- **Speed**: Instant responses instead of 30+ second API calls
- **Cost**: Zero API charges during testing
- **Reliability**: Deterministic, reproducible test results
- **Offline Development**: No internet connectivity required
- **Parallel Testing**: Run multiple test suites simultaneously

## Architecture

The mocking system consists of three main components:

1. **Mock Configuration** (`scheduling-tbd.mock`): Core mocking functionality
2. **Agent Integration** (`scheduling-tbd.agent-db`): Integration with the agent query system
3. **Test Utilities**: Helper functions for testing

### Key Files

- `src/server/scheduling_tbd/mock.clj` - Core mocking functionality
- `src/server/scheduling_tbd/agent_db.clj` - Modified to support mocking
- `test/scheduling_tbd/mock_test.clj` - Unit tests for mock system
- `test/scheduling_tbd/integration_test.clj` - Integration tests
- `src/server/scheduling_tbd/demo.clj` - Demonstration script

## Usage

### Environment Variable Configuration

Set the `STBD_MOCK_LLM` environment variable to enable mocking:

```bash
export STBD_MOCK_LLM=true
```

### Programmatic Configuration

```clojure
(require '[scheduling-tbd.mock :as mock])

;; Enable mocking
(mock/set-mock-enabled! true)

;; Check if mocking is enabled
(mock/mock-enabled?) ; => true

;; Disable mocking
(mock/set-mock-enabled! false)
```

### Using Mock Projects in Tests

```clojure
(require '[scheduling-tbd.mock :as mock])

;; Use a specific project file for mocking
(mock/with-mock-project "sur-music-school.edn"
  (fn []
    ;; Your test code here
    ;; All LLM calls will use mock responses from the project file
    (your-function-that-calls-llm)))
```

### Direct Mock Response Testing

```clojure
;; Get a mock response directly
(mock/with-mock-project "sur-music-school.edn"
  (fn []
    (mock/get-mock-response :process 2))) ; => "Mock response string"
```

## Integration with Agent System

The mocking system is integrated at the `query-on-thread-aux` level in `agent_db.clj`. When mocking is enabled:

1. The system checks for a suitable mock response
2. If found, returns the mock response immediately
3. If not found, falls back to real LLM calls
4. Logs the decision for debugging

## Project Data Structure

The system expects project files in the `data/projects/` directory with the following structure:

```edn
[#:project{:conversations
           [#:conversation{:id :process
                           :messages
                           [#:message{:content "System message content"
                                      :from :system
                                      :id 1
                                      :pursuing-EADS :process/scheduling-problem-type
                                      :tags [:query]
                                      :time #inst "2025-06-10T23:18:34.051-00:00"}
                            ;; More messages...
                            ]}
            ;; More conversations...
            ]}]
```

### Response Matching Algorithm

The system matches current conversation context to recorded responses using:

1. **Conversation ID**: Matches `:process`, `:data`, `:resources`, etc.
2. **Message Count**: Uses simple heuristics based on conversation progress
3. **EADS Progression**: Matches `:pursuing-EADS` values when available
4. **Fallback Strategy**: Uses positional matching if exact matches aren't found

## Testing

### Running Tests

```clojure
;; Run mock system tests
(require '[clojure.test :as test])
(test/run-tests 'scheduling-tbd.mock-test)

;; Run integration tests
(test/run-tests 'scheduling-tbd.integration-test)
```

### Example Test

```clojure
(deftest test-mock-integration
  (testing "Mock system integration"
    (mock/with-mock-project "sur-music-school.edn"
      (fn []
        (let [result (your-llm-function "test query")]
          (is (string? result))
          (is (not-empty result)))))))
```

## Development Workflow

### 1. Development with Mocking

```bash
# Enable mocking for development
export STBD_MOCK_LLM=true

# Start your REPL/application
# All LLM calls will use mock responses
```

### 2. Testing Workflow

```clojure
;; In your test setup
(mock/with-mock-project "appropriate-project.edn"
  (fn []
    ;; Run your tests
    (test-your-functionality)))
```

### 3. Production/Real LLM Testing

```bash
# Disable mocking for real LLM calls
unset STBD_MOCK_LLM

# Or programmatically
(mock/set-mock-enabled! false)
```

## Available Project Files

The system includes several pre-recorded project conversations:

- `sur-music-school.edn` - Music school scheduling (recommended for testing)
- `sur-craft-beer.edn` - Brewery scheduling
- `sur-ice-cream.edn` - Ice cream production
- `snowboard-production-scheduling.edn` - Manufacturing scheduling
- And many more...

## Troubleshooting

### Mock Not Working

1. Check if mocking is enabled: `(mock/mock-enabled?)`
2. Verify project file exists: `data/projects/your-file.edn`
3. Check log messages for mock system activity
4. Ensure you're using `with-mock-project` correctly

### No Mock Response Found

1. Check the conversation ID (`:process`, `:data`, etc.)
2. Verify the project file contains appropriate conversations
3. Check log messages for fallback behavior
4. Try a different message count or conversation context

### Performance Issues

1. Mock responses should be instantaneous
2. If seeing delays, check if mocking is actually enabled
3. Verify the system isn't falling back to real LLM calls

## Future Enhancements

### Phase 2 Features
- Improved context matching algorithms
- Support for mixed mock/real LLM scenarios
- Response validation and format checking
- Project selection based on test requirements

### Phase 3 Features
- Automatic conversation recording
- Smart response generation based on conversation patterns
- Multi-modal support for different project types
- Advanced debugging and visualization tools

## Contributing

When adding new tests or features:

1. Use appropriate project files for your test scenarios
2. Add comprehensive test coverage for new functionality
3. Update this documentation for significant changes
4. Ensure both mock and real LLM code paths work correctly

## Implementation Notes

- The system uses dynamic variables for thread-safe mock state management
- Integration is at the lowest level (`query-on-thread-aux`) to catch all LLM calls
- Fallback to real LLM ensures graceful degradation
- Extensive logging helps with debugging and verification
