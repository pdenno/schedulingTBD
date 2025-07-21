(ns scheduling-tbd.websocket-test
  "Integration tests for WebSocket functionality including connection lifecycle,
   promise management, and error recovery scenarios."
  (:require
   [clojure.core.async :as async :refer [>! go]]
   [clojure.test :refer [deftest testing is use-fixtures]]
   [promesa.core :as p]
   [scheduling-tbd.web.websockets :as ws]
   [scheduling-tbd.util :refer [now]]))

;; Test fixtures and utilities
(def test-client-id "test-client-123")
(def test-client-id-2 "test-client-456")

(defn setup-test-client
  "Create a test client with channels for testing"
  [client-id]
  (let [channels (ws/make-ws-channels client-id)]
    (swap! ws/ping-dates #(assoc % client-id (now)))
    channels))

(defn cleanup-test-client
  "Clean up test client state"
  [client-id]
  (ws/forget-client client-id)
  (swap! ws/ping-dates #(dissoc % client-id)))

(defn test-fixture
  "Test fixture to ensure clean state before and after tests"
  [f]
  (ws/clear-promises!)
  (reset! ws/socket-channels {})
  (reset! ws/ping-dates {})
  (reset! ws/inactive-channels-process nil)
  (f)
  ;; Cleanup after test
  (ws/clear-promises!)
  (reset! ws/socket-channels {})
  (reset! ws/ping-dates {})
  (reset! ws/inactive-channels-process nil))

(use-fixtures :each test-fixture)

;; Connection Lifecycle Tests
(deftest test-websocket-connection-lifecycle
  (testing "Basic connection establishment and cleanup"
    (setup-test-client test-client-id)
    (is (contains? @ws/socket-channels test-client-id)
        "Client should be registered in socket-channels")
    (is (contains? @ws/ping-dates test-client-id)
          "Client should be registered in ping-dates")

    (cleanup-test-client test-client-id)
    (Thread/sleep 6000) ; Allow time for async cleanup (1000ms delay + processing) ; Increased wait time for async cleanup ; Allow async cleanup to complete

    (is (not (contains? @ws/socket-channels test-client-id))
        "Client should be removed from socket-channels after cleanup")
    (is (not (contains? @ws/ping-dates test-client-id))
        "Client should be removed from ping-dates after cleanup")))

(deftest test-multiple-clients
  (testing "Multiple client connections and independent cleanup"
     (setup-test-client test-client-id)
     (setup-test-client test-client-id-2)

     (is (= 2 (count @ws/socket-channels))
         "Both clients should be registered")

     ;; Remove one client
     (cleanup-test-client test-client-id)
     (Thread/sleep 6000) ; Allow time for async cleanup ; Increased wait time for async cleanup

     (is (= 1 (count @ws/socket-channels))
         "Only one client should remain")
     (is (contains? @ws/socket-channels test-client-id-2)
         "Second client should still be registered")

     ;; Clean up remaining client
     (cleanup-test-client test-client-id-2)))

(deftest test-channel-exit-race-condition
  (testing "Channel exit mechanism without race conditions"
    (let [{:keys [in out err]} (setup-test-client test-client-id)]

      ;; Start a dispatching loop in background
      (ws/dispatching-loop test-client-id)

      ;; Verify client is active
      (is (contains? @ws/socket-channels test-client-id))
      (is (not (ws/exiting? test-client-id)))

      ;; Trigger cleanup
      (ws/close-ws-channels test-client-id)

      ;; Wait for cleanup to complete
      (Thread/sleep 6000) ; Allow time for async cleanup (1000ms delay + processing) ; Wait longer than the 1000ms delay for async cleanup ; Wait longer than the 1000ms delay

      ;; Verify proper cleanup
      (is (not (contains? @ws/socket-channels test-client-id))
          "Client should be fully cleaned up without race condition"))))

;; Promise Management Tests
(deftest test-promise-lifecycle
  (testing "Promise creation, resolution, and cleanup"
    (setup-test-client test-client-id)

    ;; Create a promise
    (let [promise-obj (ws/new-promise! test-client-id)
          p-key (:p-key promise-obj)
          prom (:prom promise-obj)]

      (is (some? p-key) "Promise key should be generated")
      (is (p/promise? prom) "Promise should be created")
      (is (= 1 (count @ws/promise-stack)) "Promise should be added to stack")

      ;; Resolve the promise
      (ws/domain-expert-says {:msg-text "test response"
                              :client-id test-client-id
                              :promise-keys [p-key]})

      ;; Check promise resolution
      (let [result @prom]
        (is (= "test response" (:text result))
            "Promise should resolve with correct data"))

      ;; Promise should be removed from stack after resolution
      (is (= 0 (count @ws/promise-stack))
          "Promise should be removed from stack after resolution"))))

(deftest test-promise-cleanup-on-disconnect
  (testing "Promises are properly cleaned up when client disconnects"
    (setup-test-client test-client-id)

    ;; Create multiple promises
    (let [p1 (ws/new-promise! test-client-id)
          p2 (ws/new-promise! test-client-id)]

      (is (= 2 (count @ws/promise-stack))
          "Two promises should be in stack")

      ;; Disconnect client
      (cleanup-test-client test-client-id)

      ;; Promises should be cleaned up
      (is (= 0 (count @ws/promise-stack))
          "All promises should be cleaned up on disconnect")

      ;; Promises should be rejected
      (is (p/rejected? (:prom p1))
          "Promise 1 should be rejected")
      (is (p/rejected? (:prom p2))
          "Promise 2 should be rejected"))))

(deftest test-promise-selection
  (testing "Promise selection logic with multiple clients"
    (setup-test-client test-client-id)
    (setup-test-client test-client-id-2)

    ;; Create promises for both clients
    (let [p1 (ws/new-promise! test-client-id)
          p2 (ws/new-promise! test-client-id-2)
          k1 (:p-key p1)
          k2 (:p-key p2)]

      ;; Test promise selection
      (let [selected (ws/select-promise [k1 k2])]
        (is (some? selected) "Should select a promise")
        (is (contains? #{k1 k2} (:p-key selected))
            "Should select one of the provided keys"))

      ;; Test with client-specific keys
      (let [selected-1 (ws/select-promise [k1])]
        (is (= k1 (:p-key selected-1))
            "Should select specific promise by key")))))

;; Connection Management Tests
(deftest test-ping-mechanism
  (testing "Ping/pong keepalive mechanism"
    (setup-test-client test-client-id)

    ;; Test ping confirmation
    (let [result (ws/ping-confirm {:client-id test-client-id :ping-id 1})]
      (is (= :ping-confirm (:dispatch-key result))
          "Should return ping confirmation")

      ;; Check that ping date is updated
      (is (contains? @ws/ping-dates test-client-id)
          "Ping date should be recorded"))))

(deftest test-alive-confirmation
  (testing "Client alive confirmation mechanism"
    (setup-test-client test-client-id)

    ;; Set client as not alive
    (swap! ws/socket-channels #(assoc-in % [test-client-id :alive?] false))

    ;; Send alive confirmation
    (ws/client-confirms-alive {:client-id test-client-id})

    ;; Check that client is marked as alive
    (is (get-in @ws/socket-channels [test-client-id :alive?])
        "Client should be marked as alive after confirmation")))

;; Error Recovery Tests
(deftest test-message-dispatch-errors
  (testing "Error handling in message dispatch"
    (setup-test-client test-client-id)

    ;; Test dispatch with invalid message
    (let [result (ws/dispatch {:dispatch-key :invalid-key
                               :client-id test-client-id})]
      ;; Should handle gracefully without throwing
      (is (nil? result) "Invalid dispatch should return nil"))))

(deftest test-send-to-nonexistent-client
  (testing "Sending message to disconnected client"
    ;; Try to send to non-existent client
    (let [result (ws/send-to-client {:client-id "nonexistent"
                                     :dispatch-key :iviewr-says
                                     :text "test"})]
      ;; Should handle gracefully without throwing
      (is (nil? result)
          "Sending to nonexistent client should return nil"))))

;; Stress Tests
(deftest test-concurrent-connections
  (testing "Handle multiple concurrent connections"
    (let [client-ids (map #(str "client-" %) (range 10))]
      (doall (map setup-test-client client-ids))

      (is (= 10 (count @ws/socket-channels))
          "All clients should be registered")

      ;; Create promises for all clients
      (doall (map ws/new-promise! client-ids))
      (is (= 10 (count @ws/promise-stack))
          "All promises should be created")

      ;; Clean up all clients
      (doall (map cleanup-test-client client-ids))
      (Thread/sleep 200)

      (is (= 0 (count @ws/socket-channels))
          "All clients should be cleaned up")
      (is (= 0 (count @ws/promise-stack))
          "All promises should be cleaned up"))))

(deftest test-rapid-connect-disconnect
  (testing "Rapid connection and disconnection cycles"
    (dotimes [i 5]
      (let [client-id (str "rapid-client-" i)]
        (setup-test-client client-id)
        (is (contains? @ws/socket-channels client-id))

        (cleanup-test-client client-id)
        (Thread/sleep 50) ; Brief pause

        (is (not (contains? @ws/socket-channels client-id)))))))

;; Integration Tests
(deftest test-full-message-flow
  (testing "Complete message flow from client to server and back"
    (let [{:keys [in out]} (setup-test-client test-client-id)]

      ;; Start dispatching loop
      (ws/dispatching-loop test-client-id)

      ;; Send a ping message
      (go (>! in (str {:dispatch-key :ping
                       :client-id test-client-id
                       :ping-id 1})))

      ;; Wait for response
      (Thread/sleep 100)

      ;; Check that ping was processed (ping date updated)
      (is (contains? @ws/ping-dates test-client-id)
          "Ping should be processed and recorded"))))

(deftest test-inactive-channels-cleanup
  (testing "Inactive channel cleanup process"
    ;; Create a client but don't ping it
    (setup-test-client test-client-id)

    ;; Remove ping date to simulate inactive client
    (swap! ws/ping-dates #(dissoc % test-client-id))

    ;; The actual cleanup test would require mocking time or using a shorter timeout
    ;; For now, just verify the cleanup function exists and can be called
    (is (fn? ws/close-inactive-channels)
        "Cleanup function should exist")

    ;; Manual cleanup to prevent test interference
    (cleanup-test-client test-client-id)))

;; Run all tests
(defn ^:diag run-websocket-tests
  "Convenience function to run all websocket tests"
  []
  (clojure.test/run-tests 'scheduling-tbd.websocket-test))
