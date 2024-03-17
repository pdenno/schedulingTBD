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

(def ok-message "This is useful where, for example, the user is typing unanticpated stuff."
  [{:msg-text/string "ok"}])

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

;;; =============  Promise key management (exploratory code!)  =============================================================
;;;   * By design, the server waits for a response before sending another question (except for maybe some future where
;;;     there is a "Are you still there?").
;;;     - Questions always are transmitted over the websocket; they are the beginning of something new.
;;;   * The user, however can put out as many things as she likes.
;;;   * promise-keys are an attempt to match the question to the responses.
;;;     - When the server asks a question, it adds a promise key to the question; the client manages a set of these keys.
;;;     - When the user does a user-says, it tells the server what keys it has.
;;;     - As the system processes a user-says, it decides what keys to tell the client to erase.
;;;     - The system uses the keys to match a question to a user-says.
;;;       + It picks the key top-most on its stack that is also in the set sent by the client with user-says.
;;;     - When the system has decided where to route the question, it can also tell the client to remove that key from its set.
;;;       + This last step is important because the server needs to get old promise-keys off the stack.
(defn dispatch-response
  "Operators wait for responses matching their key.
   This resolves the promise, allowing continuation  of the operator's processing.
   It also ws/sends a message to the client to clear the chosen key."
  [resp client-keys]
  (let [{:keys [prom prom-key client-id]} (ws/select-promise client-keys)]
    (if (not prom-key)
      {:message/ack true} ; You still need to respond with something!
      (do
        (ws/clear-keys client-id [prom-key])
        (p/resolve! prom resp)))))

(defmulti run-op #'run-op-dispatch)

;;; -------- Similar for db-actions ----------------------------------------------------------
(defmacro defaction
  "Macro to wrap methods for updating the project's database for effects from running an operation."
  {:clj-kondo/lint-as 'clojure.core/fn
   :arglists '([plan-step proj-id domain & body])}
  [tag [plan-step proj-id domain & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod db-action ~tag [~plan-step ~proj-id ~domain & [~@more-args]]
     ;;(when @debugging? (println (cl-format nil "d=> ~A" ~tag)))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       res#)))
;;;       (do (when @debugging?
;;;             (println (cl-format nil "<-d ~A returns ~S" ~tag res#)))
;;;           res#)

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

(def wait-time-for-user-resp "The number of milliseconds to wait for the user to reply to a query." 20000)

;;; ----- :!yes-no-process-steps
(defoperator :!yes-no-process-steps [plan-step proj-id domain]
  (log/info "!yes-no-process-steps: plan-step =" plan-step)
  (-> (ws/send "Select the process steps from the list that are typically part of your processes. \n (When done hit \"Submit\".)")
      (p/await wait-time-for-user-resp)
      (p/then (fn [response] (db-action plan-step proj-id domain response)))))

(defaction :!yes-no-process-steps [plan-step proj-id domain response]
  (advance-plan plan-step proj-id  domain response))

;;; ----- :!query-process-durs
(defoperator :!query-process-durs [plan-step proj-id domain]
  (log/info "!query-process-durs: plan-step =" plan-step)
  (-> (ws/send "Are the process process durations, blah, blah...\n(When done hit \"Submit\".)")
      (p/await wait-time-for-user-resp)
      (p/then (fn [response] (db-action plan-step proj-id domain response)))))

(defaction :!query-process-durs [plan-step proj-id domain response]
  (advance-plan plan-step proj-id  domain response))

;;; ----- :!yes-no-process-steps
(defoperator :!yes-no-process-ordering [plan-step proj-id domain]
  (log/info "!yes-no-process-ordering: plan-step =" plan-step)
  (-> (ws/send "If the processes listed are not in the correct order, please reorder them. \n (When done hit \"Submit\".)")
      (p/await wait-time-for-user-resp)
      (p/then (fn [response] (db-action plan-step proj-id domain response)))))

(defaction :!yes-no-process-ordering [plan-step proj-id domain response]
  (advance-plan plan-step proj-id  domain response))

;;; ----- :!stop-discussion
(defoperator :!stop-discussion [plan-step proj-id domain]
  (log/info "!stop-discussion: plan-step =" plan-step)
  (db-action plan-step proj-id domain nil))

(defaction :!stop-discussion [plan-step proj-id domain response]
  (advance-plan plan-step proj-id  domain response))
