(ns scheduling-tbd.operators
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
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
(def ^:diag diag (atom nil))

(defmacro defoperator
  "Macro to wrap methods for translating shop to database format."
  {:clj-kondo/lint-as 'clojure.core/defmacro ; See https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#inline-macro-configuration
   :arglists '(tag [arg-map] & body)} ; You can put more in :arglists, e.g.  :arglists '([[in out] & body] [[in out err] & body])}
  [tag [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod run-op ~tag [~'_tag ~arg-map]
     (when @debugging? (println (cl-format nil "==> ~A" ~tag)))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       (do (when @debugging?
             (println (cl-format nil "<-- ~A returns ~S" ~tag res#)))
           res#))))

(defn run-op-dispatch
  "Parameters to run-op have form [plan-step proj-id domain & other-args]
   This dispatch function choose a method by return (:operator plan-step)."
  [tag & _]
  (log/info "run-op-dispatch: tag =" tag)
  (cond
    (keyword? tag) tag
    :else (throw (ex-info "run-op-dispatch: No dispatch value for tag" {:tag tag}))))

(defmulti run-op #'run-op-dispatch)

;;; --------  db-actions is similar but adds a second object, the response from the operator -----------------------------------
(defmacro defaction
  "Macro to wrap methods for updating the project's database for effects from running an operation."
  {:clj-kondo/lint-as 'clojure.core/defmacro
   :arglists '(tag [arg-map response] & body)}
  [tag [arg-map response] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod db-action ~tag [~arg-map ~response]
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       res#)))

(defn db-action-dispatch
  "Parameters to db-action is a object with at least a :plan-step in it and a response from run-op (user response)."
  [obj & _]
  (log/info "db-action-dispatch: obj =" obj)
  (if-let [tag (-> obj :plan-step :operator)]
    tag
    (throw (ex-info "db-action-dispatch: No dispatch value for plan-step" {:obj obj}))))

(defmulti db-action #'db-action-dispatch)

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
      - domain is a domain object, typically one that has been pruned. (So it IS small!)
   Returns the new state."
  [{:keys [plan-step proj-id domain]} _response]
  (log/info "advance-plan: plan-step = " plan-step "proj-id =" proj-id "domain =" domain)
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

(def wait-time-for-user-resp "The number of milliseconds to wait for the user to reply to a query." 20000)

(def intro-message
  "The first message of a conversation."
  [{:msg-text/string "Describe your scheduling problem in a few sentences or "}
   {:msg-link/uri "http://localhost:3300/learn-more"
    :msg-link/text "learn more about how this works"}
   {:msg-text/string "."}])

;;; Typically we won't save an interview query message to the DB until after receiving a response to it from the user.
;;; At that point, we'll also save the :project/state-string. This ensures that when we restart we can use the
;;; planner to put the right question back in play.
(defoperator :!initial-question [{:keys [plan-step proj-id client-id domain]}]
  (log/info "!initial-question: plan-step =" plan-step)
  (-> (ws/send-to-chat (str intro-message) :recipient-read-string? true :client-id client-id)
      (p/await wait-time-for-user-resp)
      (p/then (fn [response] (db-action plan-step proj-id domain response)))))

;;; ----- :!yes-no-process-steps
(defoperator :!yes-no-process-steps [{:keys [plan-step proj-id client-id domain] :as obj}]
  (log/info "!yes-no-process-steps: plan-step =" plan-step)
  (-> (ws/send-to-chat "Select the process steps from the list that are typically part of your processes. \n (When done hit \"Submit\".)"
               :client-id client-id)
      (p/await wait-time-for-user-resp)
      (p/then #(do (log/info "After the p/await:" %) %))
      (p/then (fn [response] (db-action obj response)))
      (p/catch #(log/error "!yes-no-process-steps: error =" %))))

(defaction :!yes-no-process-steps [obj response]
  (log/info "Now the db action...")
  (advance-plan obj response))

;;; ----- :!query-process-durs
(defoperator :!query-process-durs [{:keys [plan-step client-id] :as obj}]
  (log/info "!query-process-durs: plan-step =" plan-step)
  (-> (ws/send-to-chat "Are the process process durations, blah, blah...\n(When done hit \"Submit\".)"
               :client-id client-id)
      (p/await wait-time-for-user-resp)
      (p/then (fn [response] (db-action obj response)))
      (p/catch #(log/error "!query-process-durs: error =" %))))

(defaction :!query-process-durs [obj response]
  (log/info "Now the db action...")
  (advance-plan obj response))

;;; ----- :!yes-no-process-steps
(defoperator :!yes-no-process-ordering [{:keys [plan-step client-id] :as obj}]
  (log/info "!yes-no-process-ordering: plan-step =" plan-step)
  (-> (ws/send-to-chat "If the processes listed are not in the correct order, please reorder them. \n (When done hit \"Submit\".)"
               :client-id client-id)
      (p/await wait-time-for-user-resp)
      (p/then (fn [response] (db-action obj response)))
      (p/catch #(log/error "!yes-no-process-ordering: error =" %))))

(defaction :!yes-no-process-ordering [obj response]
  (log/info "Now the db action...")
  (advance-plan obj response))
