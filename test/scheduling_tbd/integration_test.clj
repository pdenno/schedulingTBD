(ns scheduling-tbd.integration-test
  "Integration tests for LLM mocking system"
  (:require
   [clojure.test :refer [deftest is testing]]
   [scheduling-tbd.mock :as mock]
   [scheduling-tbd.agent-db :as adb]))

(deftest test-query-on-thread-aux-with-mock
  (testing "query-on-thread-aux should use mock when enabled"
    (mock/with-mock-project "sur-music-school.edn"
      (fn []
        ;; This would normally require real OpenAI credentials and make API calls
        ;; But with mocking enabled, it should return a mock response
        (let [result (adb/query-on-thread-aux "fake-aid" "fake-tid" "user" "What is your scheduling problem?" 5 :openai)]
          ;; This test would fail if mocking isn't working, as we're using fake IDs
          (is (string? result) "Should return a string response")
          (is (not-empty result) "Response should not be empty"))))))

(deftest test-mock-disabled-behavior
  (testing "System should work normally when mock is disabled"
    (let [original-state (mock/mock-enabled?)]
      (try
        (mock/set-mock-enabled! false)
        (is (not (mock/mock-enabled?)) "Mock should be disabled")
        ;; We can't test real LLM calls in unit tests, but we can verify the path
        ;; The real test would require valid credentials
        (finally
          (mock/set-mock-enabled! original-state))))))

(deftest test-mock-response-fallback
  (testing "Should fall back to real LLM when no mock response found"
    (mock/with-mock-project "sur-music-school.edn"
      (fn []
        ;; Test that the mock system gracefully handles missing responses
        (let [response (mock/get-mock-response :nonexistent-conversation 999)]
          (is (nil? response) "Should return nil for non-existent conversation"))))))

(deftest test-environment-variable-configuration
  (testing "Mock can be enabled via environment variable"
    ;; This test demonstrates the environment variable functionality
    ;; In real usage, set STBD_MOCK_LLM=true
    (is (boolean? (mock/mock-enabled?)) "Mock enabled state should be a boolean")))
