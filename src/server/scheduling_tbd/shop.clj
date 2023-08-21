(ns scheduling-tbd.shop
  "Database storage and serialization of SHOP planning domain content."
  (:require
   [clojure.spec.alpha           :as s]
   [datahike.api                 :as d]
   [scheduling-tbd.sutil :as sutil :refer [register-db connect-atm]]
   [taoensso.timbre :as log]))

;;; ToDo: Factor SHOP2 stuff to its own
;;; ToDo: Next line makes a choice that has global consequences, so maybe wrap the SHOP translation code in something that toggles this.
(s/check-asserts true)

(defn is-var?
  "Return true if the argument is a symbol beginning with ?"
  [x]
  (and (symbol? x)
       (= \? (-> x name first))))

(defn is-op?
  "Return true if the argument is a symbol beginning with ?"
  [x]
  (and (symbol? x)
       (= \! (-> x name first))))

;;; ======================= SHOP2 Grammar ================================================================
;(s/def ::list-term (s/and seq? #(every? (fn [t] (s/valid? ::term t)) %))) ; I don't allow :list, it is never used.

;(s/def ::atom (s/or :simple-atom   #(s/valid? ::simple-atom %) ; ToDo: Don't need s/valid? here?
;                    :negated-atom  #(s/valid? ::negated-atom %)))

;;;------------------ atoms and terms
(s/def ::atom (s/and seq? #(-> % first symbol?) #(-> % first is-var? not) #(every? is-var? (rest %))))

(s/def ::negated-atom (s/and seq? #(= (first %) 'not) #(s/valid? ::atom (second %))))

(s/def ::immediate-atom (s/and seq? #(and (= :immediate (first %)) ; I'm not allowing :task
                                          (s/valid? ::atom (rest %)))))

(s/def ::task-atom (s/or :simple    ::atom
                         :immediate ::immediate-atom))

;;; Documentation not withstanding, primitive tasks (calls to operations) can also be made in the task list. Thus, examples:
;;;   - (!!assert ((dest ?a ?c2)))
;;;   - (:immediate !!ra ((no-use ?a)) ())
(s/def ::task-atom-extended
  (s/or :task-atom ::task-atom
        :p-op-call ::primitive-op-call))

(s/def ::primitive-op-call
  (s/or :op-call           ::op-call
        :op-call-immediate ::op-call-immediate))

(s/def ::op-call
  (s/and #(-> % (nth 0) is-op?)
         #(every? (fn [a] (s/valid? ::atom-or-atom-list a)) (rest %))))

(s/def ::op-call-immediate
  (s/and #(= :immediate (nth % 0))
         #(s/valid? ::op-call (rest %))))

(s/def ::atom-or-atom-list
  (s/or :atom ::atom
        :atom-list #(every? (fn [a] (s/valid? ::atom a)) %)))

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

;;;------------- s-expression
(s/def ::s-term (s/or :atomic #(or (number? %) (symbol? %) (string? %))
                      :exp    ::s-exp))

(s/def ::s-exp (s/and seq?
                      #(-> % (nth 0) symbol?)
                      #(every? (fn [t] (s/valid? ::s-term t)) (rest %))))

(s/def ::s-exp-extended (s/or :s-exp    ::s-exp
                              :fn-name  symbol?))


;;;----------- logical expression
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
                        #(s/valid? ::atom (nth % 1)) ; I think.
                        #(every? (fn [t] (s/valid? ::s-exp t)) (nthnext % 2))))

(s/def ::setof  (s/and seq?
                       #(= (nth % 0) 'setof)
                       #(is-var? (nth % 1))
                       #(s/valid? ::s-exp (nth % 2)) ; ToDo: Not clear that this should be an ::s-exp.
                       #(is-var? (nth % 3))))

;;;----------- pre-conditions
(s/def ::precondition (s/or :first-satisfier-exp   ::first-satisfier-exp
                            :sort-by-exp           ::sort-by-exp
                            :logical-exp           ::logical-exp))

(s/def ::first-satisfier-exp (s/and seq?
                                    #(= (nth % 0) :first)
                                    #(every? (fn [l] (s/valid? ::logical-exp l)) (rest %))))

(s/def ::sort-by-exp (s/and seq?
                            #(= (nth % 0) :sort-by)
                            #(is-var? (nth % 1))
                            (s/or :with-exp (s/and
                                             #(== (count %) 4)
                                             #(s/valid? ::s-exp-extended (nth % 2))
                                             #(s/valid? ::logical-exp (nth % 3)))
                                  :wo-exp  (s/and
                                            #(== (count %) 3)
                                            #(s/valid? ::logical-exp (nth % 2))))))

;;;------ toplevel forms ----
(s/def ::method (s/and seq?
                       #(= (nth % 0) :method)
                       #(s/valid? ::atom (nth % 1))
                       #(s/valid? ::precond-clause (nth % 2))
                       #(s/valid? ::task-list (nth % 3))))

(s/def ::precond-clause (s/and seq? (s/or :first ::first-satisfier-exp
                                          :sort  ::sort-by-exp
                                          :conjunct #(every? (fn [t] (s/valid? ::precondition t)) %))))

(s/def ::unordered-task-list (s/and #(= :unordered (nth % 0))
                                         #(every? (fn [t] (s/valid? ::task-atom-extended t)) (rest %))))

(s/def ::ordered-task-list   (s/or :explicit (s/and #(= :ordered (nth % 0))
                                                         #(every? (fn [t] (s/valid? ::task-atom-extended t)) (rest %)))
                                   :implict #(every? (fn [t] (s/valid? ::task-atom-extended t)) %)))

(s/def ::task-list (s/and seq? (s/or :unordered ::unordered-task-list
                                     :ordered   ::ordered-task-list)))

(s/def ::axiom (s/and seq?
                       #(= (nth % 0) :-)
                       #(s/valid? ::atom (nth % 1))
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
(defn exp2db-dispatch
  "These are roughly ordered by how often they are encountered in the definition of a planning domain."
  [exp & [tag]]
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
        (s/valid? ::setof exp)          :setof
        :else (log/error "No dispatch value for exp = " exp)))

(defmulti exp2db #'exp2db-dispatch)

(defmethod exp2db :atom [exp & _]
  (let [[pred & vars] exp]
    (cond-> {}
      true              (assoc :atom/predicate pred)
      (not-empty vars)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v vars]
                                                   {:role/var v :role/pos (swap! cnt inc)}))
                                               vec)))))

(defmethod exp2db :task-list-elem [exp & _]
  (cond (and (= :immediate (first exp))
             (s/valid? ::atom (rest exp)))       (-> (exp2db (rest exp) :atom) (assoc :task/immediate? true))
        (s/valid? ::op-call exp)                 (exp2db exp ::op-call)
        (s/valid? ::op-call-immediate exp)       (-> (exp2db (rest exp) ::op-call) (assoc :task/immediate? true))
        :else                                    (exp2db exp :atom)))

(defmethod exp2db :op-call [exp & _]
  {:op-call/predicate (first exp)
   :op-call/args (-> (let [cnt (atom 0)]
                       (for [arg (rest exp)]
                         (if (s/valid? ::atom arg)
                           (-> (exp2db arg :atom)
                               (assoc :arg/pos (swap! cnt inc)))
                           (-> (:arg/pos (swap! cnt inc)) ; Assumes it is a list of atoms.
                               (assoc :op-call/args (-> (let [ncnt (atom 0)]
                                                          (for [narg arg]
                                                            (-> (exp2db narg :atom)
                                                                (:arg/pos (swap! ncnt inc)))))
                                                        vec)))))))})

;;; Only called explicitly, e.g. (mapv #(exp2db % :logical-exp) pre)), not in dispatch table.
(defmethod exp2db :logical-exp [exp & _]
  (exp2db exp))

;;; Only called explicitly. Note that the things in the task lists are not all task atoms!
(defmethod exp2db :task-list [exp & _]
  (let [explicit (#{:ordered :unordered} (first exp))
        tasks (if explicit (rest exp) exp)
        cnt (atom 0)]
    (cond-> {:method/task-list (for [t tasks]
                                 (-> (exp2db t :task-list-elem)
                                     (assoc :task/pos (swap! cnt inc))))}
      (= explicit :unordered)  (assoc :method/tasks-are-unordered? true))))

;;; Only called explicitly.
(defmethod exp2db :preconds [exp & _]
 (cond (s/valid? ::sort-by-exp exp)          (exp2db exp :sort-by)
       (s/valid? ::first-satisfier-exp exp)  (exp2db exp :first-satisfier)
       :else                                 (let [cnt (atom 0)] ; Ordinary conjunct of :logical-exp
                                               (for [pc exp]
                                                 (-> {:precond/exp (exp2db pc :logical-exp)}
                                                     (assoc :precond/pos (swap! cnt inc)))))))

(defmethod exp2db :sort-by [exp & _]
  (if (== 4 (count exp))
    (let [[_ var  fn-def lexp] exp]
      (-> {:sort-by/var var}
          (assoc :sort-by/fn  (exp2db fn-def :s-exp-extended))
          (assoc :sort-by/exp (exp2db lexp :logical-exp))))
    (let [[_ var lexp] exp]
      (-> {:sort-by/var var}
          (assoc :sort-by/fn  {:fn/ref '<})
          (assoc :sort-by/exp (exp2db lexp :logical-exp))))))

;;; This currently does not handle quoted things.
(defmethod exp2db :s-exp-extended [exp & _]
  (letfn [(box [v] (cond (number? v)  {:box/num v}
                         (string? v)  {:box/str v}
                         (symbol? v) `{:box/sym '~v}))
          (seq2sexp [s]
            (cond-> `{:s-exp/fn-ref '~(first s)}
              (-> s rest not-empty) (assoc :s-exp/args (let [cnt (atom 0)]
                                                         (-> (for [a (rest s)]
                                                               (if (seq? a)
                                                                 (-> (seq2sexp a) (assoc :arg/pos (swap! cnt inc)))
                                                                 {:arg/val (box a) :arg/pos (swap! cnt inc)}))
                                                             vec)))))]
    (if (symbol? exp)
      `{:s-exp/fn-ref '~exp}
      (seq2sexp exp))))

;;; ToDo: task lists can be nested.
;;; (exp2db sample-method)
(defmethod exp2db :method [exp & _]
  (let [[_met head pre tasks] exp]
    (cond-> {}
      true                                         (assoc :method/head (exp2db head :atom))
      (not-empty pre)                              (assoc :method/preconditions (exp2db pre :preconds)) ; ToDo: should be ordered.
      (s/valid? ::unordered-task-list tasks)       (assoc :method/tasks-are-unordered? true)
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
  "Defines schema elements about shop2 constructs.
   This is combined with the project-oriented schema elements to define the schema for the system (as opposed to schema for a project)."
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

   ;; ---------------------- axiom
   :axiom/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
        :doc "a unique name for the axiom; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}

   ;; ---------------------- method
   :method/expression
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
           :doc "an s-expression of the precondition consisting of a logical connectives ('and', 'or'), and quantifiers applied to atoms or sub-expressions."}
   :method/head
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
           :doc "a atom indicating task to be completed."}
   :method/name
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
           :doc "a unique name for the method; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}
   :method/preconditions
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "atoms indicating what must be true in order to apply the method."}
   :method/task-list
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "atoms describing a means to achieve the head literal."}
   :method/tasks-are-unordered?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "when true, indicates that the atoms in task-list can be performed in any order. Ordered is the default."}

   ;; ---------------------- operation
   :operation/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
        :doc "a unique name for the operation; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}

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
