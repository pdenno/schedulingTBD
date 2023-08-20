(ns scheduling-tbd.db
  "Database schema, serialization, and connection implementation."
  (:require
   [clojure.core :as c]
   [clojure.instant]
   [clojure.spec.alpha           :as s]
   [clojure.string               :as str]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.sutil :as sutil :refer [register-db connect-atm]]
   [taoensso.timbre :as log])
  (:import
   java.time.LocalDateTime))

;;; ToDo: Factor SHOP2 stuff to its own
;;; ToDo: Next line makes a choice that has global consequences, so maybe wrap the SHOP translation code in something that toggles this.
(s/check-asserts true)

(defn is-var?
  "Return true if the argument is a symbol beginning with ?"
  [x]
  (and (symbol? x)
       (= \? (-> x name first))))

;;; ======================= SHOP2 Grammar ================================================================
;(s/def ::list-term (s/and seq? #(every? (fn [t] (s/valid? ::term t)) %))) ; I don't allow :list, it is never used.

;(s/def ::atom (s/or :simple-atom   #(s/valid? ::simple-atom %) ; ToDo: Don't need s/valid? here?
;                    :negated-atom  #(s/valid? ::negated-atom %)))

(s/def ::atom (s/and seq? #(-> % first symbol?) #(-> % first is-var? not) #(every? is-var? (rest %))))

(s/def ::negated-atom (s/and seq? #(= (first %) 'not) #(s/valid? ::simple-atom (second %))))

(s/def ::immediate-atom (s/and seq? #(and (= :immediate (first %)) ; I'm not allowing :task
                                          (s/valid? ::simple-atom (rest %)))))

(s/def ::task-atom (s/or :simple    ::atom
                         :immediate ::immediate-atom))

(defn lisp-fn? ; ToDo maybe a call to ask-lisp.
  "Returns true if the argument is bound to a function in lisp."
  [x] ('#{+ - / * > < <= >= = max min} x))
(s/def ::fn-symbol lisp-fn?)

(s/def ::term
  (s/or :var     is-var?
        :symbol  symbol?
        :number  number?
        :list-term ::conjunct))

(s/def ::extended-term
  (s/or :term ::term
        :eval-term ::eval-term
        :call-term ::call-term))

(s/def ::eval-term
  (s/and seq?
         #(= (first %) 'eval)
         #(s/valid? ::lisp-exp (second %))))

(s/def ::call-term
  (s/and seq?
         #(= (nth % 0) 'call)
         #(s/valid? ::fn-symbol (nth % 1))
         #(every? (fn [t] (s/valid? ::extended-term t)) (-> % rest rest))))

(s/def ::var-list (s/and seq? #(every? is-var? %)))

(s/def ::s-term (s/or :atomic #(or (number? %) (symbol? %) (string? %))
                      :exp    ::s-exp))

(s/def ::s-exp (s/and seq?
                      #(-> % (nth 0) symbol?)
                      #(every? (fn [t] (s/valid? ::s-term t)) (rest %))))

(s/def ::logical-exp
  (s/or :atom           ::atom
        :immediate-atom ::immediate-atom
        :conjunct       ::conjunct
        :disjunct       ::disjunct
        :negation       ::negation
        :implication    ::implication
        :universal      ::universal
        :assignment     ::assignment
        :eval           ::eval
        :call           ::call
        :enforce        ::enforce
        :setof          ::setof))

(s/def ::conjunct    (s/and seq? #(every? (fn [l] (s/valid? ::logical-exp l)) %))) ; I'm prohibiting use of optional 'list'.

(s/def ::disjunct    (s/and seq? #(= (nth % 0) 'or)  #(every? (fn [l] (s/valid? ::logical-exp l)) (rest %))))

(s/def ::negation    (s/and seq? #(= (nth % 0) 'not) #(s/valid? ::logical-exp (second %))))

(s/def ::implication (s/and seq?
                            #(= (nth % 0) 'imply)
                            #(s/valid? ::logical-exp (nth % 1))
                            #(s/valid? ::logical-exp (nth % 2))))

(s/def ::universal (s/and seq?
                          #(= (nth % 0) 'forall)
                          #(s/valid? ::var-list (nth % 1))
                          #(s/valid? ::logical-exp (nth % 2))
                          #(s/valid? ::logical-exp (nth % 3))))

(s/def ::assignment (s/and seq?
                           #(= (nth % 0) 'assign)
                           #(is-var? (nth % 1))
                           #(s/valid? ::s-exp (nth % 2))))

(s/def ::eval ::eval-term) ; Same syntax but should evaluate to true or false.

(s/def ::call ::call-term) ; Same syntax but should evaluate to true or false.

(s/def ::enforce (s/and seq?
                        #(= (nth % 0) 'enforce)
                        #(s/valid? ::simple-atom (nth % 1)) ; I think.
                        #(every? (fn [t] (s/valid? ::s-exp t)) (-> % rest rest))))

(s/def ::setof  (s/and seq?
                       #(= (nth % 0) 'setof)
                       #(is-var? (nth % 1))
                       #(s/valid? ::s-exp (nth % 2)) ; ToDo: Not clear that this should be an ::s-exp.
                       #(is-var? (nth % 3))))

;------ toplevel forms ----
(s/def ::method (s/and seq?
                       #(= (nth % 0) :method)
                       #(s/valid? ::simple-atom (nth % 1))
                       #(s/valid? ::precond-clause (nth % 2))
                       #(s/valid? ::subtask-clause (nth % 3))))

(s/def ::precond-clause (s/and seq? #(every? (fn [t] (s/valid? ::logical-exp t)) %)))

(s/def ::unordered-subtask-clause (s/and #(= :unordered (nth % 0))
                                         #(every? (fn [t] (s/valid? ::task-atom t)) (rest %))))

(s/def ::ordered-subtask-clause   (s/or :explicit (s/and #(= :ordered (nth % 0))
                                                         #(every? (fn [t] (s/valid? ::task-atom t)) (rest %)))
                                        :implict #(every? (fn [t] (s/valid? ::task-atom t)) %)))

(s/def ::subtask-clause (s/and seq? (s/or :unordered ::unordered-subtask-clause
                                          :ordered   ::ordered-subtask-clause)))

(s/def ::axiom (s/and seq?
                       #(= (nth % 0) :-)
                       #(s/valid? ::simple-atom (nth % 1))
                       #(every? (fn [e] (s/valid? ::axiom-clause e)) (-> % rest rest))))

(s/def ::unnamed-axiom-clause (s/and seq? #(s/valid? ::logical-exp %)))

(s/def ::named-axiom-clause   (s/and seq?
                                     #(symbol? (nth % 0))
                                     #(s/valid? ::logical-exp (nth % 1))))

(s/def ::axiom-clause (s/or :unnamed-axiom-clause ::unnamed-axiom-clause
                            :named-axiom-clause   ::named-axiom-clause))



;;;----------------------------------------------------------------------------------------------
(def sample-method
  '(:method
    (transport-person ?p ?c2) ; head
    ((at ?p ?c1)  ; precondition
     (aircraft ?a)
     (at ?a ?c3)
     (different ?c1 ?c3))
    ((move-aircraft ?a ?c1) ; subtasks
     (board ?p ?a ?c1)
     (move-aircraft ?a ?c2)
     (debark ?p ?a ?c2))))

(def big-method
  '(:method (transport-person ?p ?c2)
            (:sort-by ?cost < ((at ?p ?c1)
                               (aircraft ?a)
                               (at ?a ?c3)
                               (different ?c1 ?c3)
                               (forall (?c) ((dest ?a ?c)) ((same ?c ?c1)))
                               (imply ((different ?c3 ?c1)) (not (possible-person-in ?c3)))
                               (travel-cost-info ?a ?c3 ?c1 ?cost ?style)))

            ((!!assert ((dest ?a ?c1)))
             (:immediate upper-move-aircraft ?a ?c1 ?style)
             (:immediate board ?p ?a ?c1)
             (!!assert ((dest ?a ?c2)))
             (:immediate upper-move-aircraft-no-style ?a ?c2)
             (:immediate debark ?p ?a ?c2))))
;;;== end trash -------------------------------------------------------------
(defn exp2db-dispatch [exp & [tag]]
  (cond tag                             tag
        (s/valid? ::atom exp)           :atom
        (s/valid? ::task-atom exp)      :task-atom
        (s/valid? ::conjunct exp)       :conjunct
        (s/valid? ::method exp)         :method
        (s/valid? ::axiom exp)          :axiom
        ;(s/valid? ::operator exp)      :operator   ; NYI
        (s/valid? ::negated-atom exp)   :negated-atom
        (s/valid? ::disjunct exp)       :disjunct
        (s/valid? ::implication exp)    :implication
        (s/valid? ::universal exp)      :universal
        (s/valid? ::assignment exp)     :assignment
        (s/valid? ::eval exp)           :eval
        (s/valid? ::call exp)           :call
        (s/valid? ::enforce exp)        :enforce
        (s/valid? ::setof exp)          :setof))

(defmulti exp2db #'exp2db-dispatch)

(defmethod exp2db :atom [exp & _]
  (let [[pred & vars] exp]
    (cond-> {}
      true              (assoc :atom/predicate pred)
      (not-empty vars)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v vars]
                                                   {:role/var v :role/pos (swap! cnt inc)}))
                                               vec)))))

(defmethod exp2db :task-atom [exp & _]
  (if (= :immediate (first exp))
    (-> (exp2db (rest exp) :atom) (assoc :task/immediate? true))
    (exp2db exp :atom)))

;;; Only called explicitly, e.g. (mapv #(exp2db % :logical-exp) pre)), not in dispatch table.
(defmethod exp2db :logical-exp [exp & _]
  (exp2db exp))

;;; Only called explicitly.
(defmethod exp2db :tasks [exp & _]
  (let [tasks (if (-> exp first keyword?) (rest exp) exp)
        cnt (atom 0)]
    (for [t tasks]
      (-> (exp2db t :task-atom)
          (assoc :task/pos (swap! cnt inc))))))

;;; ToDo: task lists can be nested.
;;; (exp2db sample-method)
(defmethod exp2db :method [exp & _]
  (let [[_met head pre tasks] exp]
    (cond-> {}
      true                                         (assoc :method/head (exp2db head :atom))
      (not-empty pre)                              (assoc :method/preconditions (mapv #(exp2db % :logical-exp) pre)) ; ToDo: should be ordered.
      (s/valid? ::unordered-subtask-clause tasks)  (assoc :method/tasks-are-unordered? true)
      (not-empty tasks)                            (assoc :method/task-list (exp2db tasks :tasks)))))

;;; (methods-for 'transport-person)
(defn methods-for
  "Retrieve all method forms for the given predicate symbol."
  [pred-sym]
  (when-let [meth-ents (not-empty
                        (d/q '[:find [?e ...]
                               :in $ ?predicate
                               :where
                               [?e :atom/predicate ?predicate]
                               [_ :method/head ?e]]
                             @(connect-atm :system) pred-sym))]
    meth-ents))

(def db-schema-shop2+
  "Defines schema elements about shop2 constructions.
   This is combined the project-oriented schema elements to define the schema for the system."
  {;; ---------------------- atom
   :atom/negated?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "a string naming the proposition inplied by a atom."}
   :atom/predicate
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol,
        :doc "a string naming the proposition inplied by a atom."}
   :atom/role
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "a string, typically a ?var, naming the proposition inplied by a atom."}
   :role/pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "a number used for ordering roles in a predicate."}
   :role/var
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol,
        :doc "a number used for ordering roles in a predicate."}
   ;; ---------------------- method
   :method/expression
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
           :doc "an s-expression of the precondition consisting of a logical connectives ('and', 'or'), and quantifiers applied to atoms or sub-expressions."}
   :method/head
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
           :doc "a simple-atom indicating task to be completed."}
   :method/preconditions
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "atoms indicating what must be true in order to apply the method."}
   :method/task-list
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "atoms describing a means to achieve the head literal."}
   :method/tasks-are-unordered?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "when true, indicates that the atoms in task-list can be performed in any order. Ordered is the default."}
   ;; ---------------------- task
   :task/pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "indicates the position of the task atom in its task list."}
   :task/immediate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "indicates the position of the task atom in its task list."}
   :task/atom
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "indicates the position of the task atom in its task list."}})

(def db-schema-sys+
  "Defines content that manages project DBs and their analysis including:
     - The project's name and DB directory
     - Planning domains, methods, operators, and axioms"
  {;; ---------------------- project
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword, :unique :db.unique/identity
        :doc "a keyword matching the one in the same named property of a project database"}
   :project/dir
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "a string naming a subdirectory containing a project."}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a string, same as the :project/name in the project's DB.."}
;;; ---------------------- system
   :system/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity
        :doc "the value 'SYSTEM' to represent a single objectin holding data such as the current project name."}
   :system/current-project-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword,
        :doc "a keyword naming the current project; set by user using UI, it is one of the :project/id values in this DB."}
   :system/initial-prompt
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string,
        :doc "The prompt that starts a conversation."}})

(def db-schema-proj+
  "Defines schema for a project plus metadata :mm/info.
   To eliminate confusion and need for back pointers, each project has its own db. "
  {;; ---------------------- message
   :message/from
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "The agent, either :user or :system, issuing the message."}
   :message/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long :unique :db.unique/identity
        :doc "The unique ID of a message. These are sequential natural numbers starting at 0."}
   :message/text
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The text of the message."}
   :message/time
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/instant
        :doc "The time at which the message was sent."}

   ;; ---------------------- project
   :project/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :project/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "4 words or less describing the project; e.g. 'craft brewing'"}
   :project/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "the original paragraph written by the user describing what she/he wants done."}
   :project/industry
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a short description of the industry in which we are doing scheduling."}

   ;; ---------------------- summary
   :summary/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "the value 'SUMMARY'. This is used to keep information about the state of the conversation."}
   :summary/next-msg-id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc "The ID (a natural number) to be assigned to the next message written (either side of conversation)."}

   ;; ---------------------- task type (Of course these are not planner tasks!)
   :task-t/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :task-t/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task; unique to the project."}
   :task-t/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this task; unique to the project."}
   :task-t/pre-task
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a task/name identifying a task that must occur before this task "}
   :task-t/resource-type
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "a keyword naming a resource (type or instance) used in this task"}
   :task-t/duration-est
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to a duration (dur) object"}

   :task-t/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this instance (e.g. in an ontology)."}

   ;; ---------------------- task instance
   :task-i/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :task-i/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task; unique to the project."}
   :task-i/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword
        :doc "a description of this this task; unique to the project."}
   :task-i/pre-task
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a task/name identifying a task that must occur before this task "}
   :task-i/resource-inst
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/keyword
        :doc "a keyword naming a :res-t/id or :res-i/id (type or instance) used in this task"}
   :task-i/duration-est
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "a reference to a duration (dur) object"}
   :task-i/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this instance (e.g. in an ontology)."}

   ;; ---------------------- resource type
   :res-t/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task type; unique to the project."}
   :res-t/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task; unique to the project."}
   :res-t/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this task; unique to the project."}
   :res-t/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this type (e.g. in an ontology)."}

   ;; ---------------------- resource instance
   :res-i/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming the task; unique to the project."}
   :res-i/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a name for conversation about this task"}
   :res-i/desc
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a description of this this task"}
   :res-i/uri
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity
        :doc "a URI pointing to information about this instance (e.g. in an ontology)."}

   ;; ---------------------- work
   :work/id
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword :unique :db.unique/identity
        :doc "a keyword naming something to be accomplished."}
   :work/objective-sentence
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "The sentence from user description best describing the scheduling objective."}})

(defn datahike-schema
  "Create a Datahike-compatible schema from the above."
  [schema]
  (reduce-kv (fn [r k v]
               (conj r (-> v
                           (dissoc :mm/info)
                           (assoc :db/ident k))))
             []
             schema))

(def db-schema-sys  (datahike-schema (merge  db-schema-shop2+ db-schema-sys+)))
(def db-schema-proj (datahike-schema db-schema-proj+))

;;; Atom for configuration map used for connecting to the system db.
;;; It is set by alter-var-root code in this namespace."
(defonce sys-db-cfg (atom nil))

;;; The Datahike datatypes are as follows:
;;; #{:db.type/instant :db.type/boolean :db.type/uuid _:db.type/value_ :db.type/string :db.type/keyword :db.type/number
;;;   :db.type/ref :db.type/bigdec :db.type/float :db.type/bigint :db.type/double :db.type/long :db.type/symbol}
(def initial-prompt
  "The prompt that starts a conversation. This variable might go away as we add authoring capability."
  "[\"Describe your scheduling problem in a few sentences or \"
    {:link-info {:href \"http://localhost:3300/learn-more\"
                 :text \"learn more about how this works\"}}
   \".\"]")

(defn create-sys-db!
  "Create the system database."
  []
  (when (:rebuild-db? @sys-db-cfg)
    (log/info "Recreating the system database.")
    (when (d/database-exists? @sys-db-cfg) (d/delete-database @sys-db-cfg))
    (d/create-database @sys-db-cfg)
    (register-db :system @sys-db-cfg)
    (let [conn (connect-atm :system)]
      (d/transact conn db-schema-sys)
      (d/transact conn [{:system/name "SYSTEM" ; Singleton
                         :system/initial-prompt initial-prompt}])
      conn)))

;;; Atom for the configuration map used for connecting to the project db.
(defonce proj-base-cfg (atom nil))

(defn current-project-id
  "Get the current project from the system database."
  []
  (d/q '[:find ?proj .
         :where
         [?entity :system/name "SYSTEM"]
         [?entity :system/current-project-id ?proj]]
       @(connect-atm :system)))

(defn set-current-project
  "Get the current project from the system database."
  [id]
  (d/transact (connect-atm :system)
              [{:system/name "SYSTEM" ; Singleton
                :system/current-project-id id}]))

(defn list-projects
  "List all the projects known by the system DB."
  []
  (d/q '[:find [?proj-id ...]
         :where [_ :project/id ?proj-id]]
       @(connect-atm :system)))

(def diag (atom nil))

(defn unique-proj
  "If necessary to ensure uniqueness, update the project name and id."
  [proj-info]
  (let [names (d/q '[:find [?name ...]
                      :where [_ :project/name ?name]] @(connect-atm :system))
        name (:project/name proj-info)]
    (if (not-any? #(= name %) names)
      proj-info
      (let [pat (re-pattern (str "^" name "( \\d+)?"))
            nums (map #(let [[success num] (re-matches pat %)]
                         (when success (or num "0"))) names)
            num (->> nums (map read-string) (apply max) inc)
            new-name (str name " " num)
            new-id   (-> new-name (str/replace #"\s+" "-") keyword)]
        (-> proj-info
            (assoc :project/name new-name)
            (assoc :project/id new-id))))))

(s/def ::project-db (s/keys :req [:project/id :project/name]))
(defn create-proj-db!
  "Create a project database for the argument project."
  [proj-info]
  (reset! diag proj-info)
  (if (s/valid? ::project-db proj-info)
    (let [proj-info (unique-proj proj-info)
          id (:project/id proj-info)
          dir (str (-> @proj-base-cfg :store :path-base) (name id))]
       (.mkdir (java.io.File. dir))
       ;; ToDo: :project/id is unique. Don't wipe out an existing project. User could be starting over. Maybe add a number.
       (let [db-cfg (assoc-in @proj-base-cfg [:store :path] dir)]
         (d/create-database db-cfg)
         (register-db id db-cfg))
       (let [conn (connect-atm id)]
         (d/transact conn db-schema-proj)
         (d/transact conn {:tx-data [proj-info
                                     {:summary/name "SUMMARY"
                                      :summary/next-msg-id 0}]})
         ;; Add knowledge of this project to the system db.
         (d/transact (connect-atm :system)
                     {:tx-data [{:system/name "SYSTEM"
                                 :system/current-project-id id}
                                {:project/id   (:project/id   proj-info)
                                 :project/name (:project/name proj-info)
                                 :project/dir dir}]})
         (log/info "Created project database" dir "for" (:project/name proj-info))
       proj-info))
     (throw (ex-info "Project database must provide :proj/name and :proj/id"
                     {:proj-info proj-info}))))

(defn add-msg
  "Create a message object and add it to the database with :project/id = id."
  [id msg-text from]
  (if-let [conn (connect-atm id)] ; ToDo the then part (and thus the if) should not be necessary.
    (let [msg-id (d/q '[:find ?next-id .
                        :where
                        [?e :summary/name "SUMMARY"]
                        [?e :summary/next-msg-id ?next-id]]
                      @conn)
          msg (-> {}
                  (assoc :message/id msg-id)
                  (assoc :message/from from)
                  (assoc :message/time (-> (LocalDateTime/now)
                                           str
                                           clojure.instant/read-instant-date ))
                  (assoc :message/text msg-text))]
      (d/transact conn
                  [msg
                   {:summary/name "SUMMARY"
                    :summary/next-msg-id (inc msg-id)}])
      msg)
    (log/info "Project does not exist" {:id id})))

(defn init-db-cfgs
  "Set sys-db-cfg atoms for system db and the template for the proj-base-cfg (:path-base).
   Recreate the system database if sys-db-cfg.rebuild-db? = true."
  []
  (let [base-dir (or (-> (System/getenv) (get "SCHEDULING_TBD_DB"))
                     (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases."
                                          "\nCreate directories 'projects' and 'system' under it.") {})))]
    (reset! proj-base-cfg {:store {:backend :file :path-base (str base-dir "/projects/")}
                           :schema-flexibility :write})

    (reset! sys-db-cfg {:store {:backend :file :path (str base-dir "/system")}
                        :rebuild-db? true ; <================== Currently, if you rebuild, you need to get rid of project directories.
                        :schema-flexibility :write})
    (when (-> sys-db-cfg deref :rebuild-db?) (create-sys-db!))
    (log/info "Existing projects:" (list-projects))
    {:sys-cfg @sys-db-cfg
     :proj-base @proj-base-cfg}))

(defstate database-cfgs
  :start (init-db-cfgs))
