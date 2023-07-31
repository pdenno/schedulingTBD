(ns scheduling-tbd.planner
  (:require
   [clojure.string :as str]
   [clojure.java.shell :only [sh]]
   [ezzmq.core :as zmq]
   [mount.core :as mount :refer [defstate]]
   [taoensso.timbre :as log]))

(def planner-endpoint "tcp://*:31726")
(def cmd-line "./resources/start-planner.sh")

(def starting-example
  {:domain
   "(defdomain basic-example (
     (:operator (!pickup ?a) () () ((have ?a)))
     (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
     (:method (swap ?x ?y)
       ((have ?x))
       ((!drop ?x) (!pickup ?y))
       ((have ?y))
       ((!drop ?y) (!pickup ?x)))))"
   :problem
   "(defproblem problem1 basic-example
      ((have banjo)) ((swap banjo kiwi)))"
   :find-plans
   "(find-plans 'problem1 :verbose :plans)"})

(def diag (atom nil))

(defn quit-planner!
  "Quit the planner. It can be restarted with a shell command through mount."
  []
  (let [fut (future
              (zmq/with-new-context
                (let [socket (zmq/socket :req {:connect planner-endpoint})]
                  (zmq/send-msg socket "(sb-ext:exit)")
                  (zmq/receive-msg socket {:stringify true}))))]
    (if (= [":bye!"] (deref fut 3000 :timeout))
      (do (log/info "Planner terminated as requested.") :stopped)
      (do (log/warn "Planner state unknown. (It may not have been running.)") :unknown))))

(defn test-planner!
  "Define a planning domain. Define a problem. Find plans for the problem.
   Check that the plan matches what is expected."
  []
  (zmq/with-new-context
    (let [socket (zmq/socket :req {:connect planner-endpoint})]
      (zmq/send-msg socket (:domain starting-example))
      (log/info "Planner:" (zmq/receive-msg socket {:stringify true}))
      (zmq/send-msg socket (:problem starting-example))
      (log/info "Planner:" (zmq/receive-msg socket {:stringify true}))
      (zmq/send-msg socket (:find-plans starting-example))
      (let [result (zmq/receive-msg socket {:stringify true})]
        (log/info "Planner:" result)
        result))))

(defn init-planner []
  (log/info "Starting planner.")
  (future (clojure.java.shell/sh cmd-line))
  (Thread/sleep 2000)
  (log/info "Testing planner.")
  (test-planner!))

(defstate plan-server
  :start (init-planner)
  :stop (quit-planner!))
