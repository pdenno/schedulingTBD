(ns scheduling-tbd.operators
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.core.unify    :as uni]
   [clojure.edn           :as edn]
   [clojure.pprint        :refer [cl-format]]
   [clojure.spec.alpha    :as s]
   [clojure.string        :as str]
   [datahike.api          :as d]
   [promesa.core          :as p]
   [scheduling-tbd.db     :as db]
   [scheduling-tbd.domain :as dom]
   [scheduling-tbd.llm    :as llm]
   [scheduling-tbd.specs  :as spec]
   [scheduling-tbd.shop   :as shop]
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm resolve-db-id db-cfg-map find-fact]]
   [scheduling-tbd.web.routes.websockets  :as ws]
   [taoensso.timbre       :as log]))

;;; Two method types are associated with each plan operator. For both method types, a 'tag' (keyword) selects the
;;; method to call, either an 'operator' (defined by defoperator) or a db-action (defined by defaction).
;;; Method tags correspond to an operator head predicate in the planning domain. The tag is the predicate symbol keywordized.
;;; For example, an operator head (!query-process-steps ?proj) corresponds to a tag :!query-process-steps.
;;;
;;; Execution of an operator is comprised of the following phases, which are accomplished by the operator methods shown:
;;;    1) A query is presented to the (human or surrogate) agent.                                    - defoperator
;;;    2) The response in collected.                                                                 - defoperator
;;;    3) The response is analyzed, producing new state knowledge.                                   - defaction
;;;    4) The state is updated by operator d-list and a-list actions and written to the project db.  - defaction
;;;    5) Additional comments (but not queries) can be added to the chat                             - defaction
;;;
;;; Note that by these means we don't commit anything to the DB until step (4).
;;; This ensures that when we can restart the project we can put the right question back in play.
;;;
;;; Program behavior differs in places depending on whether the agent is human or surrogate. Most obviously,
;;; in Step (1) ws/send-msg is used for humans, whereas llm/query-on-thread is used for surrogates.
;;; For the most part, we look at the state vector and use the function (surogate? state) to vary the behavior.

(def debugging? (atom true))
(def ^:diag diag (atom nil))

(defmacro defoperator
  "Macro to wrap methods for translating shop to database format."
  {:clj-kondo/lint-as 'clojure.core/defmacro ; See https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#inline-macro-configuration
   :arglists '([arg-map] & body)} ; You can put more in :arglists, e.g.  :arglists '([[in out] & body] [[in out err] & body])}
  [tag [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod operator-meth ~tag [~arg-map]
     (when @debugging? (println (cl-format nil "==> ~A" ~tag)))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       (do (when @debugging?
             (println (cl-format nil "<-- ~A returns ~S" ~tag res#)))
           res#))))

;;; ToDo: (docstring) https://stackoverflow.com/questions/22882068/add-optional-docstring-to-def-macros
#_(defmacro defoperator
  "Macro to wrap methods for translating shop to database format."
  {:clj-kondo/lint-as 'clojure.core/defmacro ; See https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#inline-macro-configuration
   :arglists '(tag [arg-map] & body)} ; You can put more in :arglists, e.g.  :arglists '([[in out] & body] [[in out err] & body])}
  [tag doc? [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod operator-meth ~tag [~'_tag ~arg-map]
     (when @debugging? (println (cl-format nil "==> ~A" ~tag)))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       (do (when @debugging?
             (println (cl-format nil "<-- ~A returns ~S" ~tag res#)))
           res#))))

#_(defn operator-meth-dispatch
  "Parameters to operator-meth have form [plan-step proj-id domain & other-args]
   This dispatch function choose a method by return (:operator plan-step)."
  [tag & _]
  (log/info "operator-meth-dispatch: tag =" tag)
  (cond
    (keyword? tag) tag
    :else (throw (ex-info "operator-meth-dispatch: No dispatch value for tag" {:tag tag}))))

(defn operator-meth-dispatch
  "Parameters to operator-meth have form [plan-step proj-id domain & other-args]
   This dispatch function choose a method by return (:operator plan-step)."
  [obj]
  (if-let [tag (:tag obj)]
    tag
    (throw (ex-info "operator-meth-dispatch: No dispatch value for plan-step" {:obj obj}))))

(defmulti operator-meth #'operator-meth-dispatch)

;;; --------  db-actions is similar but adds a second object, the response from the operator -----------------------------------
(defmacro defaction
  "Macro to wrap methods for updating the project's database for effects from running an operation."
  {:clj-kondo/lint-as 'clojure.core/defmacro
   :arglists '(tag [arg-map] & body)}
  [tag [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod db-action ~tag [~arg-map]
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       res#)))

(defn db-action-dispatch
  "Parameters to db-action is a object with at least a :plan-step in it and a response from operator-meth (user response)."
  [obj]
  ;(log/info "db-action-dispatch: obj =" obj "response =" _response)
  (if-let [tag (:tag obj)]
    tag
    (throw (ex-info "db-action-dispatch: No dispatch value for plan-step" {:obj obj}))))

(defmulti db-action #'db-action-dispatch)

;;; -------------------------- Domain manipulation for a-list and d-list -----------------------------
(defn get-domain ; ToDo: This goes away with SHOP. There's a similar one in planner.clj!
  "Return the domain in SHOP format."
  [domain-id]
  (let [eid (d/q '[:find ?eid .
                   :in $ ?dname
                   :where [?eid :domain/id ?dname]]
                 @(connect-atm :planning-domains) domain-id)
        db-obj  (resolve-db-id {:db/id eid} (connect-atm :planning-domains))]
    (shop/db2proj db-obj)))

;;; plan-step =  {:cost 1.0, :operator :!yes-no-process-steps, :args [craft-beer]}
(defn domain-operator
  "Return the argument operator from the domain."
  [domain-id operator]
  (->> (get-domain domain-id)
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


(def intro-prompt
  "This is the DB form of the first message of a conversation."
  [{:msg-text/string "Describe your most significant scheduling problem in a few sentences"}
   {:human-only [{:msg-text/string " or "}
                 {:msg-link/uri "http://localhost:3300/learn-more"
                  :msg-link/text "learn more about how this works"}]}
   {:msg-text/string "."}])

(defn reword-for-agent
  "Return the msg-vec with :human-only or :surrogate-only annotated sections removed as appropriate.
   Remove the annotations too!"
  [msg-vec surrogate?]
  (let [human? (not surrogate?)
        result (reduce (fn [res elem]
                         (cond (and (contains? elem :human-only)     surrogate?)        res
                               (and (contains? elem :surrogate-only) human?)            res
                               (contains? elem :human-only)                            (into res (:human-only elem))
                               (contains? elem :surrogate-only)                        (into res (:surrogate-only elem))
                               :else                                                   (conj res elem)))
                       []
                       msg-vec)]
    (s/assert ::spec/chat-msg-vec result)))

(defn str2msg-vec
  [s]
  (assert (string? s))
  [{:msg-text/string s}])

(defn msg-vec2text
  "Used by the surrogate only, return the string resulting from concatenating the :msg-text/string."
  [v]
  (reduce (fn [s elem]
            (if (contains? elem :msg-text/string)
              (str s (:msg-text/string elem))
              s))
          ""
          v))

(defn new-human-project
  "Create a project for a human given response. Return its PID."
  [project-name] ; ToDo: replace state-string with state.
  (let [pid   (-> project-name str/lower-case (str/replace #"\s+" "-") keyword)
        pname (->>  (str/split project-name #"\s+") (mapv str/capitalize) (interpose " ") (apply str))
        pid   (db/create-proj-db! {:project/id pid :project/name  pname})] ; pid might not have been unique, thus this returns a new one.
      pid))

(defn new-state-from-domain-lists
  "Update the project's DB, specifically :project/state-string with infromation from last response and operator a-list and d-list.
      - plan-step   - a map such as {:operator :!yes-no-process-steps, :args [aluminium-foil]},
      - proj-id     - the keyword identifying a project by its :project/id.
      - domain-id   - a keyword identifying the domain, for example, :process-interview.
   Returns the new state."
  [plan-step domain-id old-state]
  (let [{:keys [operator args]} plan-step
        op-sym (-> operator name symbol)
        op-obj (domain-operator domain-id op-sym) ; ToDo: This simplifies when shop is gone.
        bindings (zipmap (-> op-obj :operator/head rest) args)
        a-list (mapv #(uni/subst % bindings) (:operator/a-list op-obj))
        d-list (mapv #(uni/subst % bindings) (:operator/d-list op-obj))]
    (add-del-facts old-state {:add a-list :delete d-list})))


(def wait-time-for-user-resp "The number of milliseconds to wait for the user to reply to a query." 20000)

(defn surrogate?
  "Return true if state has a predicate unifying with (surrogate ?x)."
  [state]
  (find-fact '(surrogate ?x) state))

;;; Useful example!
(def example '{:plan-step  {:cost 1.0, :operator :!initial-question, :args [start-a-new-project]},
               :domain-id :process-interview,
               :tag :!initial-question,
               :pid :START-A-NEW-PROJECT,
               :client-id "df838eb5-26d6-474f-84b3-910fae59e3a9",
               :state [(proj-id :START-A-NEW-PROJECT)],
               :agent-msg-vec
               [#:msg-text{:string
                           "Describe your most significant scheduling problem in a few sentences"}
                #:msg-text{:string " or "}
                #:msg-link{:uri "http://localhost:3300/learn-more",
                           :text "learn more about how this works"}
                #:msg-text{:string "."}],
               :response
               "We are a medium-sized craft beer brewery. We produce about 100,000 barrels/year.\n   We run several products simultaneously and simply would like to be able to have the beer bottled and ready\n   to ship as near as possible to the dates defined in our sales plan."}
  )

(def diag1 (atom nil))

(defn chat-pair
  "Run one query/response pair of chat elements with a human or a surrogate.
   Returns a promise for which:
     1) :msg-vec is adapted from the input argument value for the agent type (human or surrogate)
     2) :response is added. Typically its value is a string."
  [{:keys [pid state agent-msg-vec] :as obj}]
  (log/info "Chat pair!")
  (if (surrogate? state)
    (let [aid (db/get-assistant-id pid)
          tid (db/get-thread-id pid)
          prom (llm/query-on-thread :tid tid :aid aid :msg-text (msg-vec2text agent-msg-vec))]
      (reset! diag1 prom)
      (p/as-> prom ?r
        (do (log/info "Surrogate User responds: " ?r) ?r)
        (assoc obj :response ?r)
        (p/catch ?r (fn [err] (log/error "chat-pair (surrogate): error =" err)))))
    ;; Human
    (let [prom (ws/send-to-chat (assoc obj :msg-vec agent-msg-vec))]
      (p/as-> prom ?r
        (do (log/info "Human User responds: " ?r) ?r)
        (assoc obj :response ?r)
        (p/catch ?r (fn [err]
                      (reset! diag {:err err})
                      (log/error "chat-pair (human): error =" err)))))))

;;;=================================================== Operators ======================
;;; ----- :!initial-question
;;; (op/operator-meth (assoc op/example :client-id (ws/recent-client!)))
(defoperator :!initial-question [{:keys [state] :as obj}]
  (let [agent-msg-vec (reword-for-agent intro-prompt (surrogate? state))]
    (p/as-> (chat-pair (assoc obj :agent-msg-vec agent-msg-vec)) ?r
      (db-action ?r)
      (p/catch ?r (fn [err] (log/error ":!initial-question (op)" err))))))

;;; (op/db-action (assoc op/example :client-id (ws/recent-client!)))
(defaction :!initial-question [{:keys [state response client-id agent-msg-vec plan-step domain-id] :as _obj}]
  (log/info "db-action (!initial-question): response =" response "state =" state)
  (reset! diag _obj)
  (p/as-> (dom/prelim-analysis response state) ?r
    (do (log/info "prelim-analysis returns ?r =" ?r) ?r)
    (let [new-state (atom ?r)]
      (when (not (surrogate? @new-state))
        ;; Surrogate already has a project db, but human doesn't.
        ;; New PID can defined when we try to make the DB.
        (log/info "This is not a surrogate!:" @new-state)
        (if-let [[_ pname] (find-fact '(proj-name ?x) @new-state)]
          (let [[_ orig-pid] (find-fact '(proj-id ?x) @new-state)
                pid (db/create-proj-db! {:project/name pname :project/id (keyword orig-pid)})]
            (when (not= orig-pid pid)
              (swap! new-state (fn [state]
                                 (conj (filterv #(not= % orig-pid) state)
                                       `(~'proj-id ~(name pid)))))))
          (throw (ex-info "Couldn't find PID in human project." {:?r ?r}))))
      ;; Now human/surrogate can be treated identically.
      (let [[_ pid]   (find-fact '(proj-id ?x) @new-state)
            [_ pname] (find-fact '(proj-name ?x) @new-state)
            pid (keyword pid)
            full-state (new-state-from-domain-lists plan-step domain-id @new-state)]
        (log/info "DB and app actions on PID =" pid)
        (db/add-msg pid :system agent-msg-vec)  ; ToDo: I'm not catching the error when this is wrong!
        (db/add-msg pid :user (str2msg-vec response))
        (db/put-state pid full-state)
        (ws/send-to-chat {:promise? false
                          :client-id client-id
                          :dispatch-key :reload-proj
                          :new-proj-map {:project/id pid :project/name pname}})))
    (p/catch ?r (fn [err] (log/error "!initial-question (action):" err)))))

;;; ----- :!yes-no-process-steps
(defoperator :!yes-no-process-steps [obj]
  (let [msg-vec (str2msg-vec "Select the process steps from the list that are typically part of your processes. \n (When done hit \"Submit\".)")]
    (p/as-> (chat-pair (assoc obj :msg-vec msg-vec)) ?r
      (db-action ?r)
      (p/catch ?r (fn [err] (log/error "!yes-no-process-steps:" err))))))

(defaction :!yes-no-process-steps [{:keys [pid response plan-step domain-id state] :as _obj}]
  ;; Nothing to do here but update state from a-list.
  (log/info "!yes-no-process-steps (action): response =" response)
  (let [full-state (new-state-from-domain-lists plan-step domain-id state)]
    (db/put-state pid full-state)))


;;; ----- :!query-process-durs
(defoperator :!query-process-durs [{:keys [client-id] :as obj}]
  (log/info "!query-process-durs: obj =" obj)
  (-> {:client-id client-id}
      (assoc :msg-vec (str2msg-vec "Provide typical process durations for the tasks on the right.\n(When done hit \"Submit\".)"))
      ws/send-to-chat
      (p/await wait-time-for-user-resp)
      (p/then  (fn [response] (db-action obj response)))
      (p/catch (fn [err] (log/error "Error in !query-process-durs:" err)))))

(defaction :!query-process-durs [{:keys [response] :as obj}]
  (log/info "!query-process-durs (action): response =" response "obj =" obj)
  (update-state obj))

;;; ----- :!yes-no-process-steps
(defoperator :!yes-no-process-ordering [{:keys [plan-step client-id] :as obj}]
  (log/info "!yes-no-process-ordering: plan-step =" plan-step)
  (-> {:client-id client-id}
      (assoc :msg-vec (str2msg-vec "If the processes listed are not in the correct order, please reorder them. \n (When done hit \"Submit\".)"))
      ws/send-to-chat
      (p/await wait-time-for-user-resp)
      (p/then  (fn [response] (db-action obj response)))
      (p/catch (fn [err] (log/error "Error in !yes-no-process-ordering:" err)))))

(defaction :!yes-no-process-ordering [{:keys [response] :as obj}]
  (log/info "!yes-no-process-ordering (action): response =" response "obj =" obj)
  (update-state obj))
