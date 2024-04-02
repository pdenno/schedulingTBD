(ns scheduling-tbd.operators
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.core.unify    :as uni]
   [clojure.pprint        :refer [cl-format]]
   [clojure.spec.alpha    :as s]
   [clojure.string        :as str]
   [datahike.api          :as d]
   [promesa.core          :as p]
   [scheduling-tbd.db     :as db]
   [scheduling-tbd.domain :as dom]
   [scheduling-tbd.llm    :as llm]
   [scheduling-tbd.specs  :as spec]
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm resolve-db-id db-cfg-map]]
   [scheduling-tbd.web.routes.websockets  :as ws]
   [taoensso.timbre       :as log]))

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
  [obj _response]
  ;(log/info "db-action-dispatch: obj =" obj "response =" _response)
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
     facts - a set of ::spec/positive-proposition.
     a map - with keys :add and :delete being collections of :specs/positive-proposition.
             These need not be ground propositions; everything that unifies with a
             form in delete will be deleted.
             A variable in the add list will be treated as a skolem.
             reset? is truthy."
  [facts {:keys [add delete reset?]}]
  (assert set? facts)
  (assert (every? #(s/valid? ::spec/positive-proposition %) facts))
  (as-> (if reset? #{} facts) ?f
    (if delete
      (->> ?f (remove (fn [fact] (some #(uni/unify fact %) delete))) set)
      ?f)
    (if add (into ?f add) ?f)))

(defn find-fact
  "Unify the fact (which need not be ground) to the fact-list"
  [fact fact-list]
  (some #(when (uni/unify fact %) %) fact-list))

(def intro-prompt
  "This is the DB form of the first message of a conversation."
  [{:msg-text/string "Describe your most significant scheduling problem in a few sentences"}
   {:msg-text/string " or"}
   {:msg-link/uri "http://localhost:3300/learn-more"
    :msg-link/text "learn more about how this works"}
   {:msg-text/string "."}])

(defn new-project-advance
  "Stuff done to create a new project through project-advance."
  [{:keys [more-a-list project-name response state-string client-id] :as obj}]
  ;(log/info "npa: obj =" obj)
  ;(reset! diag obj)
  (if-let [pname-fact (find-fact '(project-name ?x) more-a-list)]
    (let [pid   (-> pname-fact second str/lower-case (str/replace #"\s+" "-") keyword)
          pname (->>  (str/split project-name #"\s+") (mapv str/capitalize) (interpose " ") (apply str))
          pid   (db/create-proj-db! {:project/id pid :project/name  pname}) ; pid might not have been unique, thus this returns a new one.
          eid   (db/project-exists? pid)]
      (d/transact (connect-atm pid) [[:db/add eid :project/state-string state-string]])
      (db/add-msg pid :system intro-prompt)
      (db/add-msg pid :user   [{:msg-text/string response}])
      (db/add-msg pid :system [{:msg-text/string (format "Great! We'll call your project '%s'." pname)}])
      ;; Now tell the client to 'look again' because we've added the "Great..." msg to what he sees, and
      ;; we also use this :reload-proj to change the project selected.
      (ws/send-to-chat {:promise? nil :client-id client-id :dispatch-key :reload-proj
                        :new-proj-map {:project/name pname :project/id pid}}))
    (throw (ex-info "Couldn't find a project-name fact while advancing plan." {:more-a-list more-a-list}))))

(defn advance-plan
  "Update the project's DB, specifically :project/state-string to show how the plan has advanced.
      - plan-step is a map such as {:operator :!yes-no-process-steps, :args [aluminium-foil]},
      - proj-id is the keyword identifying a project by its :project/id.
      - domain is a domain object, typically one that has been pruned. (So it IS small!)
   Returns the new state."
  [{:keys [plan-step pid domain more-d-list more-a-list] :as obj}]
  (log/info "ap: obj =" obj)
  (let [facts (db/get-state pid)
        {:keys [operator args]} plan-step
        op-sym (-> operator name symbol)
        op-obj (domain-operator domain op-sym) ; ToDo: This simplifies when shop is gone.
        bindings (zipmap (-> op-obj :operator/head rest) args)
        a-list (into (mapv #(uni/subst % bindings) (:operator/a-list op-obj)) more-a-list)
        d-list (into (mapv #(uni/subst % bindings) (:operator/d-list op-obj)) more-d-list)
        new-state (add-del-facts facts {:add a-list :delete d-list})
        eid (db/project-exists? pid)]
    (cond eid                        (d/transact (connect-atm pid) [[:db/add eid :project/state-string (str new-state)]])
          (= pid :new-project)       (new-project-advance (assoc obj :state-string (str new-state)))
          :else                      (throw (ex-info "Couldn't find proj-id while advancing plan." {:pid pid})))
    new-state))

(def wait-time-for-user-resp "The number of milliseconds to wait for the user to reply to a query." 20000)

(defn msg-vec
  [s]
  (assert (string? s))
  [{:msg-text/string s}])

(defn abstract-chat
  "Chat with a human or a surrogate.
     PID     - a keyword identifying a project (i.e. :project/id).
     TARGET  - #{:human :surrogate}
     MSG-OBJ - an object suitable for ws/send-to-chat, it has ::spec/chat-msg-vec and a :client-id
   If target is :human then the message is sent to ws/send-to-chat, a promise is returned.
   If target is :surrogate, the message is sent to ws/send-to-chat and then a response is ws/send-to-chat.
   In the :surrogate case, what is returned is content useful to db-actions (the discussion)."
  [pid target msg-obj]
  (reset! diag msg-obj)
  (s/assert ::spec/chat-msg-obj msg-obj)
  (case target
    :human      (ws/send-to-chat msg-obj)
    :surrogate  (let [text (-> msg-obj :msg-vec first :msg-text/string)
                      aid (db/get-assistant-id pid)
                      tid (db/get-thread-id pid)]
                  (ws/send-to-chat msg-obj)
                  {:query text
                   :response (llm/query-on-thread :aid aid :tid tid :role "user" :msg-text text)})))


;;; Typically we won't save interview query messages to the DB until after receiving a response to it from the user.
;;; At that point, we'll also save the :project/state-string. This ensures that when we restart we can use the
;;; planner to put the right question back in play.

;;; The 'obj' parameter is a map containing client-id, plan-step, proj-id, and domain, typically.
;;; We carry it forward adding to it and destructuring it in function calls.

;;; ----- :!initial-question-surrogate
(defoperator :!initial-question-surrogate [{:keys [pid client-id] :as obj}]
  (log/info "!initial-question-surrogate: obj =" obj)
  (let [res (abstract-chat pid :surrogate
                           {:msg-vec (-> intro-prompt first (update :msg-text/string #(str % ".")) vector)
                            :dispatch-key :surrogate-says
                            :client-id client-id})]
    (db-action obj res)))

(defaction :!initial-question-surrogate [{:keys [pid client-id] :as obj} reply]
  (log/info "db-action !initial-question-surrogate: response =" reply "pid =" pid)
  (ws/send-to-chat {:dispatch-key :tbd-says       :client-id client-id :msg-vec [{:msg-text/string (:query    reply)}]})
  (ws/send-to-chat {:dispatch-key :surrogate-says :client-id client-id :msg-vec [{:msg-text/string (:response reply)}]})
  (-> obj
      (assoc  :pid pid)
      (update :more-a-list #(into `[(~'ongoing-discussion ~(name pid))] %))
      advance-plan))

;;; ----- :!initial-question
(defoperator :!initial-question [{:keys [pid client-id] :as obj}]
  (-> (abstract-chat pid :human {:msg-vec intro-prompt :client-id client-id})
      (p/then (fn [response] (db-action obj response)))
      (p/catch (fn [err] (log/error "Error in !initial-question:" err)))))

;;; ToDo: Need a comprehensive solution to exceptions. (In the macro, I think.)
;;; It might involve trying the defaction again.
(defaction :!initial-question [obj response]
  (if-let [proj-name (dom/project-name response)]
    (let [proj-name-sym (-> proj-name (str/replace  #"\s+" "-") symbol)]
      (log/info "!initial-question: proj-name =" proj-name)
      (-> obj
          (assoc :response response)
          (assoc  :project-name proj-name)
          (update :more-d-list #(into '[(project-name new-project)] %))
          (update :more-a-list #(into `[(~'project-name ~proj-name-sym)
                                        (~'ongoing-discussion ~proj-name-sym)] %))
          advance-plan))
    (log/warn "Could not compute project-name")))

;;; ----- :!yes-no-process-steps
(defoperator :!yes-no-process-steps [{:keys [plan-step proj-id client-id domain] :as obj}]
  (log/info "!yes-no-process-steps: obj =" obj)
  (-> {:client-id client-id}
      (assoc :msg-vec (msg-vec "Select the process steps from the list that are typically part of your processes. \n (When done hit \"Submit\".)"))
      ws/send-to-chat
      (p/await wait-time-for-user-resp)
      (p/then #(do (log/info "After the p/await:" %) %))
      (p/then  (fn [response] (db-action obj response)))
      (p/catch (fn [err] (log/error "Error in !yes-no-process-steps:" err)))))

(defaction :!yes-no-process-steps [obj response]
  (log/info "!yes-no-process-steps (action): response =" response "obj =" obj)
  (advance-plan obj))

;;; ----- :!query-process-durs
(defoperator :!query-process-durs [{:keys [plan-step client-id] :as obj}]
  (log/info "!query-process-durs: obj =" obj)
  (-> {:client-id client-id}
      (assoc :msg-vec (msg-vec "Provide typical process durations for the tasks on the right.\n(When done hit \"Submit\".)"))
      ws/send-to-chat
      (p/await wait-time-for-user-resp)
      (p/then  (fn [response] (db-action obj response)))
      (p/catch (fn [err] (log/error "Error in !query-process-durs:" err)))))

(defaction :!query-process-durs [obj response]
  (log/info "!query-process-durs (action): response =" response "obj =" obj)
  (advance-plan obj))

;;; ----- :!yes-no-process-steps
(defoperator :!yes-no-process-ordering [{:keys [plan-step client-id] :as obj}]
  (log/info "!yes-no-process-ordering: plan-step =" plan-step)
  (-> {:client-id client-id}
      (assoc :msg-vec (msg-vec "If the processes listed are not in the correct order, please reorder them. \n (When done hit \"Submit\".)"))
      ws/send-to-chat
      (p/await wait-time-for-user-resp)
      (p/then  (fn [response] (db-action obj response)))
      (p/catch (fn [err] (log/error "Error in !yes-no-process-ordering:" err)))))

(defaction :!yes-no-process-ordering [obj response]
  (log/info "!yes-no-process-ordering (action): response =" response "obj =" obj)
  (advance-plan obj))
