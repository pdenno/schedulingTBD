(ns scheduling-tbd.websocket-stress-test
  "Stress testing utilities for WebSocket connections to identify race conditions,
   memory leaks, and performance issues under load."
  (:require
   [clojure.core.async :as async :refer [<! >! go timeout]]
   [clojure.string :as str]
   [promesa.core :as p]
   [scheduling-tbd.web.websockets :as ws]
   [scheduling-tbd.util :refer [now]]
   [taoensso.telemere :refer [log!]]))

;; Stress Test Configuration
(def stress-config
  {:max-clients 50
   :message-burst-size 10
   :test-duration-ms 30000
   :connection-cycle-interval-ms 1000
   :ping-interval-ms 5000})

;; Utilities
(defn generate-client-id []
  (str "stress-client-" (System/currentTimeMillis) "-" (rand-int 10000)))

(defn setup-stress-client
  "Set up a client for stress testing"
  [client-id]
  (let [channels (ws/make-ws-channels client-id)]
    (swap! ws/ping-dates #(assoc % client-id (now)))
    (ws/dispatching-loop client-id)
    channels))

(defn simulate-client-activity
  "Simulate normal client activity (pings, messages) for a duration"
  [client-id duration-ms]
  (go
    (let [start-time (System/currentTimeMillis)
          end-time (+ start-time duration-ms)
          {:keys [in]} (get @ws/socket-channels client-id)]

      (loop []
        (when (< (System/currentTimeMillis) end-time)
          ;; Send periodic pings
          (when in
            (try
              (>! in (str {:dispatch-key :ping
                           :client-id client-id
                           :ping-id (System/currentTimeMillis)}))
              (catch Exception e
                (log! :warn (str "Failed to send ping for " client-id ": " e)))))

          ;; Wait before next ping
          (<! (timeout (:ping-interval-ms stress-config)))
          (recur))))))

(defn stress-test-concurrent-connections
  "Test many concurrent connections"
  []
  (log! :info "Starting concurrent connections stress test")
  (let [client-count (:max-clients stress-config)
        client-ids (repeatedly client-count generate-client-id)
        start-time (System/currentTimeMillis)]

    ;; Create all clients
    (doseq [client-id client-ids]
      (setup-stress-client client-id)
      (simulate-client-activity client-id (:test-duration-ms stress-config)))

    (log! :info (str "Created " client-count " concurrent clients"))

    ;; Monitor system state during test
    (let [_monitor-chan (go
                         (loop []
                           (let [active-clients (count @ws/socket-channels)
                                 active-promises (count @ws/promise-stack)
                                 ping-count (count @ws/ping-dates)]
                             (log! :info (str "Active clients: " active-clients
                                              " Promises: " active-promises
                                              " Ping dates: " ping-count))

                             (<! (timeout 5000)) ; Report every 5 seconds
                             (when (< (System/currentTimeMillis)
                                      (+ start-time (:test-duration-ms stress-config)))
                               (recur)))))]

      ;; Wait for test duration
      (Thread/sleep (:test-duration-ms stress-config))

      ;; Clean up all clients
      (doseq [client-id client-ids]
        (when (contains? @ws/socket-channels client-id)
          (ws/forget-client client-id)))

      ;; Wait for cleanup
      (Thread/sleep 2000)

      ;; Report final state
      (let [remaining-clients (count @ws/socket-channels)
            remaining-promises (count @ws/promise-stack)
            remaining-pings (count @ws/ping-dates)]
        (log! :info (str "After cleanup - Clients: " remaining-clients
                         " Promises: " remaining-promises
                         " Ping dates: " remaining-pings))

        {:test "concurrent-connections"
         :clients-created client-count
         :remaining-clients remaining-clients
         :remaining-promises remaining-promises
         :remaining-pings remaining-pings
         :duration-ms (- (System/currentTimeMillis) start-time)}))))

(defn stress-test-rapid-cycling
  "Test rapid connection/disconnection cycles"
  []
  (log! :info "Starting rapid cycling stress test")
  (let [start-time (System/currentTimeMillis)
        end-time (+ start-time (:test-duration-ms stress-config))
        cycle-count (atom 0)]

    (loop []
      (when (< (System/currentTimeMillis) end-time)
        (let [client-id (generate-client-id)]
          ;; Connect
          (setup-stress-client client-id)

          ;; Brief activity
          (Thread/sleep 100)

          ;; Disconnect
          (ws/forget-client client-id)
          (swap! cycle-count inc)

          ;; Brief pause
          (Thread/sleep (:connection-cycle-interval-ms stress-config))
          (recur))))

    (let [final-clients (count @ws/socket-channels)
          final-promises (count @ws/promise-stack)
          final-pings (count @ws/ping-dates)]
      (log! :info (str "Rapid cycling completed: " @cycle-count " cycles"))

      {:test "rapid-cycling"
       :cycles @cycle-count
       :remaining-clients final-clients
       :remaining-promises final-promises
       :remaining-pings final-pings
       :duration-ms (- (System/currentTimeMillis) start-time)})))

(defn stress-test-promise-handling
  "Test promise creation and resolution under load"
  []
  (log! :info "Starting promise handling stress test")
  (let [client-id (generate-client-id)
        promise-count 100
        start-time (System/currentTimeMillis)]

    ;; Setup client
    (setup-stress-client client-id)

    ;; Create many promises rapidly
    (let [promises (doall (repeatedly promise-count #(ws/new-promise! client-id)))]
      (log! :info (str "Created " promise-count " promises"))

      ;; Resolve them all
      (doseq [i (range promise-count)]
        (let [p-key (:p-key (nth promises i))]
          (ws/domain-expert-says {:msg-text (str "response-" i)
                                  :client-id client-id
                                  :promise-keys [p-key]})))

      ;; Wait for all resolutions
      (Thread/sleep 1000)

      ;; Check that all promises were resolved
      (let [resolved-count (count (filter p/resolved? (map :prom promises)))
            remaining-promises (count @ws/promise-stack)]

        ;; Cleanup
        (ws/forget-client client-id)

        {:test "promise-handling"
         :promises-created promise-count
         :promises-resolved resolved-count
         :remaining-promises remaining-promises
         :duration-ms (- (System/currentTimeMillis) start-time)}))))

(defn stress-test-message-bursts
  "Test handling of message bursts"
  []
  (log! :info "Starting message burst stress test")
  (let [client-id (generate-client-id)
        burst-size (:message-burst-size stress-config)
        burst-count 10
        start-time (System/currentTimeMillis)]

    ;; Setup client
    (let [{:keys [in]} (setup-stress-client client-id)]

      ;; Send bursts of messages
      (dotimes [burst burst-count]
        (log! :debug (str "Sending burst " (inc burst) " of " burst-count))

        ;; Send burst of ping messages
        (dotimes [msg burst-size]
          (go (>! in (str {:dispatch-key :ping
                           :client-id client-id
                           :ping-id (+ (* burst burst-size) msg)}))))

        ;; Brief pause between bursts
        (Thread/sleep 500))

      ;; Wait for processing
      (Thread/sleep 2000)

      ;; Cleanup
      (ws/forget-client client-id)

      {:test "message-bursts"
       :bursts-sent burst-count
       :messages-per-burst burst-size
       :total-messages (* burst-count burst-size)
       :duration-ms (- (System/currentTimeMillis) start-time)})))

(defn stress-test-cleanup-under-load
  "Test cleanup mechanisms under high load"
  []
  (log! :info "Starting cleanup under load stress test")
  (let [client-count 20
        start-time (System/currentTimeMillis)]

    ;; Create many clients
    (let [client-ids (repeatedly client-count generate-client-id)]
      (doseq [client-id client-ids]
        (setup-stress-client client-id)
        ;; Create promises for each client
        (dotimes [_ 5]
          (ws/new-promise! client-id)))

      (log! :info (str "Created " client-count " clients with promises"))

      ;; Trigger cleanup while system is active
      (ws/close-inactive-channels)

      ;; Wait for cleanup
      (Thread/sleep 3000)

      ;; Force cleanup of remaining clients
      (doseq [client-id client-ids]
        (when (contains? @ws/socket-channels client-id)
          (ws/forget-client client-id)))

      ;; Final state check
      (Thread/sleep 1000)

      {:test "cleanup-under-load"
       :clients-created client-count
       :remaining-clients (count @ws/socket-channels)
       :remaining-promises (count @ws/promise-stack)
       :duration-ms (- (System/currentTimeMillis) start-time)})))

(defn run-all-stress-tests
  "Run all stress tests and return results"
  []
  (log! :info "Starting comprehensive WebSocket stress tests")

  ;; Clean state before testing
  (ws/clear-promises!)
  (reset! ws/socket-channels {})
  (reset! ws/ping-dates {})

  (let [results [(stress-test-concurrent-connections)
                 (stress-test-rapid-cycling)
                 (stress-test-promise-handling)
                 (stress-test-message-bursts)
                 (stress-test-cleanup-under-load)]]

    ;; Final cleanup
    (ws/clear-promises!)
    (reset! ws/socket-channels {})
    (reset! ws/ping-dates {})

    (log! :info "All stress tests completed")
    results))

(defn analyze-stress-test-results
  "Analyze stress test results for potential issues"
  [results]
  (doseq [result results]
    (let [test-name (:test result)
          issues (atom [])]

      ;; Check for resource leaks
      (when (> (:remaining-clients result 0) 0)
        (swap! issues conj "Client connections not properly cleaned up"))

      (when (> (:remaining-promises result 0) 0)
        (swap! issues conj "Promises not properly cleaned up"))

      (when (> (:remaining-pings result 0) 0)
        (swap! issues conj "Ping dates not properly cleaned up"))

      ;; Report results
      (if (empty? @issues)
        (log! :info (str test-name " passed - no resource leaks detected"))
        (log! :warn (str test-name " potential issues: " (str/join ", " @issues))))

      ;; Log performance metrics
      (log! :info (str test-name " metrics: " (dissoc result :test))))))

;; Convenience function for running stress tests
(defn ^:diag stress-test-websockets
  "Run comprehensive stress tests on WebSocket system"
  []
  (let [results (run-all-stress-tests)]
    (analyze-stress-test-results results)
    results))
