(ns scheduling-tbd.operators
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:require
   [clojure.pprint       :refer [cl-format]]
   [clojure.spec.alpha   :as s]
   [scheduling-tbd.specs :as specs]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm resolve-db-id db-cfg-map]]
   [taoensso.timbre      :as log]))

(def debugging? (atom true))

(defmacro defoperator
  "Macro to wrap methods for translating shop to database format."
  {:clj-kondo/lint-as 'clojure.core/fn ; See https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#inline-macro-configuration
   :arglists '([plan-step facts state-edits & body])} ; You can put more in :arglists, e.g.  :arglists '([[in out] & body] [[in out err] & body])}
  [tag [plan-step facts state-edits & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod run-op ~tag [~plan-step ~facts ~state-edits & [~@more-args]]
     (when @debugging? (println (cl-format nil "==> ~A" ~tag)))
     (let [res# (do ~@body)
           result# (if (seq? res#) (doall res#) res#)]
       (s/assert ::specs/state-edits result#)
       (do (when @debugging?
             (println (cl-format nil "<-- ~A returns ~S" ~tag result#)))
           result#))))

(defn run-op-dispatch
  "Parameters to run-op have form [plan-step facts state-edits & other-args]
   This dispatch function choose a method by return (:operator plan-step)."
  [{:keys [operator]} & _]
  (cond ;; Optional 2nd argument specifies method to call. Order matters!
    (keyword? operator) operator
    :else (throw (ex-info "run-op-dispatch: No dispatch value for operator" {:op operator}))))

(defn dispatch-response
  "Operators wait for responses matching their key. This puts the response on their channel."
  [text]
  :NYI)

(defmulti run-op #'run-op-dispatch)

;;; N.B.: Typically we won't save an interview query message to the DB until after receiving a response to it from the user.
;;;       At that point, we'll also save the :project/state-string. This ensures that when we restart we can use the
;;;       planner to put the right question back in play.
(defoperator :!initial-question [plan-step facts state-edits]
  (log/info "!initial-question: plan-step =" plan-step "facts =" facts "state-edits =" state-edits)
  (let [{:keys [args]} plan-step]
    {:from :!initial-question
     :delete #{}
     :add #{`(~'ongoing-discussion ~@args)}}))

;;; ToDo: Maybe use promesa here???
(defoperator :!yes-no-process-steps [plan-step facts state-edits]
  (log/info "!yes-no-process-steps: plan-step =" plan-step "facts =" facts "state-edits =" state-edits)
  (let [{:keys [args]} plan-step]
    {:from :!yes-no-process-steps
     ;; ToDo: Use operator :add and :delete here!
     :delete #{}
     :add #{`(~'have-process-steps ~@args)}}))
