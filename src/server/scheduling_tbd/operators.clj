(ns scheduling-tbd.operators
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:require
   [clojure.pprint       :refer [cl-format]]
   [clojure.spec.alpha   :as s]
   [promesa.core         :as p]
   [scheduling-tbd.specs :as specs]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm resolve-db-id db-cfg-map]]
   [scheduling-tbd.web.routes.websockets  :as sock]
   [taoensso.timbre      :as log]))

(def debugging? (atom true))

(def intro-message
  "The first message of a conversation."
  [{:msg-text/string "Describe your scheduling problem in a few sentences or "}
   {:msg-link/uri "http://localhost:3300/learn-more"
    :msg-link/text "learn more about how this works"}
   {:msg-text/string "."}])


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

;;; ToDo: Combine this with :!initial-question above.
#_(defn op-start-project
  "Summarize user-text as a project name. Execute plan operations to start a project about user-text."
  [user-text]
  (let [summary (dom/project-name user-text)
        id (-> summary str/lower-case (str/replace #"\s+" "-") keyword)
        proj-info  {:project/id id
                    :project/name summary
                    :project/desc user-text ; <==== ToDo: Save this as :msg-id 1 (0 is the "Describe your scheduling problem" message).
                    #_#_:project/industry _industry}
        proj-info (db/create-proj-db! proj-info) ; May rename the project-info.
        proj-id (:project/id proj-info)]
    (db/add-msg proj-id (d/q '[:find ?prompt . :where [_ :system/initial-prompt ?prompt]] @(connect-atm :system)) :system)
    (db/add-msg proj-id user-text :user)
    ;; ToDo:
    ;; 0) Call the planner and make this code an operator!
    ;; 1) Set :system/current-project.
    ;; 2) Store first two messages (prompt and user's first contribution).
    ;; 3) Check that there isn't a project by that name.
    ;; 4) Let user change the name of the project.
    (let [response (str "Great! We'll call your project '" (:project/name proj-info) "'. ")]
      (log/info "op-start-project: Responding with: " response)
      (db/add-msg proj-id response :system))))

;;; plan-step: [{:operator :!yes-no-process-steps, :args [aluminium-foil]}]
(defoperator :!yes-no-process-steps [plan-step facts state-edits]
  (log/info "!yes-no-process-steps: plan-step =" plan-step "facts =" facts "state-edits =" state-edits)
  (let [{:keys [args]} plan-step
        promise-key (sock/ws-send "In the area to the right, are the process steps listed typically part of your processes? (When done select \"Submit\")")]
    (log/info ":!yes-no-process-steps: promise-key = " promise-key)
    (-> (sock/lookup-promise promise-key)
        (p/then #(log/info "Y/N process-steps: promise-key = " promise-key " answer =" %)))
    {:from :!yes-no-process-steps
     :delete #{}
     :add #{`(~'have-process-steps ~(first args))}}))
