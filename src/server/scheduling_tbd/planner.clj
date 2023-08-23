(ns scheduling-tbd.planner
  "Start and stop the SHOP3 planner."
  (:require
   [clojure.java.shell :refer [sh]]
   [ezzmq.core :as zmq]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.db :as db]
   [taoensso.timbre :as log]))

(def planner-endpoint "tcp://*:31726")

;;; ========================== Composing a planning domain  ==================

;;; ========================== Composing a planning problem  ==================


;;; ========================== Starting, stopping, and testing ==================
(def shop2-example
  "This is an example from usage from the SHOP2 documentation."
  {:domain
   "(defdomain basic-example
     ((:operator (!pickup ?a) () () ((have ?a)))
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
   "(find-plans 'problem1 :verbose :plans)"
   :answer
   ["(((!DROP BANJO) 1.0 (!PICKUP KIWI) 1.0))"]})

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
      (zmq/send-msg socket (:domain shop2-example))
      (zmq/receive-msg socket {:stringify true})
      (zmq/send-msg socket (:problem shop2-example))
      (zmq/receive-msg socket {:stringify true})
      (zmq/send-msg socket (:find-plans shop2-example))
      (let [result (zmq/receive-msg socket {:stringify true})]
        (if (= (:answer shop2-example) result)
          (do (log/info "Planner passes test.") :passes)
          (do (log/error "Planner fails test.") :fails-test))))))

(defn init-planner []
  (if-let [planner-path (-> (System/getenv) (get "PLANNER_SBCL"))] ; ToDo: maybe use system.edn instead.
    (do (log/info "Starting planner.")
        (future (sh "/usr/bin/sbcl"
                    "--core"
                    (str "./" planner-path)
                    "--non-interactive"
                    "--disable-debugger"))
        (Thread/sleep 2000)
        (test-planner!))
    (do (log/error "PLANNER_SBCL environment var not set.")
        :fails-env)))

(defstate plan-server
  :start (init-planner)
  :stop (quit-planner!))
