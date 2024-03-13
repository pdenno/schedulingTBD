(ns scheduling-tbd.operators
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:require
   [clojure.core.unify   :as uni]
   [clojure.pprint       :refer [cl-format]]
   [clojure.spec.alpha   :as s]
   [datahike.api         :as d]
   [promesa.core         :as p]
   [scheduling-tbd.db    :as db]
   [scheduling-tbd.specs :as specs]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm resolve-db-id db-cfg-map]]
   [scheduling-tbd.web.routes.websockets  :as ws]
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
   :arglists '([plan-step proj-id domain & body])} ; You can put more in :arglists, e.g.  :arglists '([[in out] & body] [[in out err] & body])}
  [tag [plan-step proj-id domain & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod run-op ~tag [~plan-step ~proj-id ~domain & [~@more-args]]
     (when @debugging? (println (cl-format nil "==> ~A" ~tag)))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       (do (when @debugging?
             (println (cl-format nil "<-- ~A returns ~S" ~tag res#)))
           res#))))

(defn run-op-dispatch
  "Parameters to run-op have form [plan-step proj-id domain & other-args]
   This dispatch function choose a method by return (:operator plan-step)."
  [{:keys [operator]} & _]
  (cond ;; Optional 2nd argument specifies method to call. Order matters!
    (keyword? operator) operator
    :else (throw (ex-info "run-op-dispatch: No dispatch value for operator" {:op operator}))))

(defn dispatch-response
  "Operators wait for responses matching their key. This resolves the promise, allowing continuation
   of the operator's processing."
  [res clear-keys]
  (if-let [p (some #(when-let [prom (ws/lookup-promise %)] prom) clear-keys)]
    (p/resolve! p res)
    (log/warn "dispatch-response: no match to keys:" clear-keys)))

(defmulti run-op #'run-op-dispatch)

;;; -------- Similar for db-actions ----------------------------------------------------------
(defmacro defaction
  "Macro to wrap methods for updating the project's database for effects from running an operation."
  {:clj-kondo/lint-as 'clojure.core/fn
   :arglists '([plan-step proj-id domain & body])}
  [tag [plan-step proj-id domain & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod db-action ~tag [~plan-step ~proj-id ~domain & [~@more-args]]
     (when @debugging? (println (cl-format nil "d=> ~A" ~tag)))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       (do (when @debugging?
             (println (cl-format nil "<-d ~A returns ~S" ~tag res#)))
           res#))))

(defmulti db-action #'run-op-dispatch)

;;; N.B.: Typically we won't save an interview query message to the DB until after receiving a response to it from the user.
;;;       At that point, we'll also save the :project/state-string. This ensures that when we restart we can use the
;;;       planner to put the right question back in play.
(defoperator :!initial-question [plan-step proj-id domain]
  (log/info "!initial-question: plan-step =" plan-step "proj-id =" proj-id "domain =" domain)
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

(def ^:diag diag (atom nil))

;;; plan-step =  {:cost 1.0, :operator :!yes-no-process-steps, :args [craft-beer]}
(defn domain-operator
  "Return the argument operator from the domain."
  [domain operator]
  (->> domain
       :domain/elems
       (some #(when (= (-> % :operator/head first) operator) %))))

;;; ToDo: The idea of Skolem's in the add-list needs thought. Is there need for a universal fact?
;;;       For the time being, I'm just adding uniquely
(defn add-del-facts
  "Update the facts by adding, deleting or resetting to empty.
     facts - a set of ::specs/positive-proposition.
     a map - with keys :add and :delete being collections of :specs/positive-proposition.
             These need not be ground propositions; everything that unifies with a
             form in delete will be deleted.
             A variable in the add list will be treated as a skolem.
             reset? is truthy."
  [facts {:keys [add delete reset?]}]
  (assert set? facts)
  (assert (every? #(s/valid? ::specs/positive-proposition %) facts))
  (as-> (if reset? #{} facts) ?f
    (if delete
      (->> ?f (remove (fn [fact] (some #(uni/unify fact %) delete))) set)
      ?f)
    (if add (into ?f add) ?f)))

(defn advance-plan
  "Update the project's DB, specifically :project/state-string to show how the plan has advanced.
      - plan-step is a map such as {:operator :!yes-no-process-steps, :args [aluminium-foil]},
      - proj-id is the keyword identifying a project by its :project/id.
      - domain-id is a domain object, typically one that has been pruned.
   Returns the new state."
  [plan-step proj-id domain _response]
  ;;(log/info "advance-plan: plan-step = " plan-step "proj-id =" proj-id "domain =" domain)
  (let [facts (db/get-state proj-id)
        {:keys [operator args]} plan-step
        op-sym (-> operator name symbol)
        op-obj (domain-operator domain op-sym) ; ToDo: This simplifies when shop is gone.
        bindings (zipmap (-> op-obj :operator/head rest) args)
        a-list (mapv #(uni/subst % bindings) (:operator/a-list op-obj))
        d-list (mapv #(uni/subst % bindings) (:operator/d-list op-obj))
        new-state (add-del-facts facts {:add a-list :delete d-list})
        eid (db/proj-eid proj-id)]
    (d/transact (connect-atm proj-id) [[:db/add eid :project/state-string (str new-state)]])
    new-state))

(defoperator :!yes-no-process-steps [plan-step proj-id domain]
  (log/info "!yes-no-process-steps: plan-step =" plan-step)
  (-> (ws/send "Select the process steps from the list that are typically part of your processes. \n (When done hit \"Submit\".)")
      (p/then (fn [response] (db-action plan-step proj-id domain response)))))

(defaction :!yes-no-process-steps [plan-step proj-id domain response]
  (advance-plan plan-step proj-id  domain response))

(defoperator :!yes-no-process-durations [plan-step proj-id domain]
  (log/info "!yes-no-process-steps: plan-step =" plan-step)
  (-> (ws/send "Are the process process durations, blah, blah...\n(When done hit \"Submit\".)")
      (p/then (fn [response] (db-action plan-step proj-id domain response)))))

(defaction :!yes-no-process-durations [plan-step proj-id domain response]
  (advance-plan plan-step proj-id  domain response))
