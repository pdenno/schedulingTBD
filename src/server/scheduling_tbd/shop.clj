(ns scheduling-tbd.shop
  "Rewrite SHOP planning domain structures to db structures.
   Rewrite db structures to SHOP planning domain (defdomain) structures."
  (:require
   [clojure.edn          :as edn]
   [clojure.java.io      :as io]
   [clojure.pprint       :refer [cl-format]]
   [clojure.spec.alpha   :as s]
   [datahike.api         :as d]
   ;[datahike.pull-api    :as dp]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.sutil :as sutil :refer [register-db connect-atm not-nothing datahike-schema]]
   [taoensso.timbre      :as log]))

;;; Why so much fuss with planning domain structures?
;;; Answer: It might be temporary, but for the time being, we need lispy structures to run SHOP.
;;;         However, the lispy structures are really difficult to write AND MODIFY; they have "positional semantics" and no keywords.
;;;
;;;  Consequently, there are three forms to the planning domain data:
;;;   edn       - a simple EDN rendering used in exploratory programming.
;;;               The way to get from EDN to a SHOP2 lispy defdomain is (-> edn edn2canonical canon2shop)
;;;
;;;   canonical - a plan domain structured that separates the SHOP2 lispy domain into its elements (operators, methods, and axioms),
;;;               but for those elements just provides :canon/code which holds the corresponding elements SHOP2 lispy structure.
;;;               You can get canonical by calling db2canon. What you get is good for testing translation and not much else.
;;;
;;;  db         - structures conforming to db-schema-shop2+, a datahike schema. These parse the lispy stuff into elementary
;;;               content such as
;;;
;;; Use of SHOP2 might be temporary, but use of planning domains is probably going to stick around.
;;; In our usage, we develop plan structure with EDN and then canonicalize it before storage.
;;; The whole point of storing planning domains (as fine-grained structures) is to allow updating and plan repair.

;;; ToDo: Next line makes a choice that has global consequences, so maybe wrap the SHOP translation code in something that toggles this.
(s/check-asserts true)
(def diag (atom nil))

(defn is-var?
  "Return true if the argument is a symbol beginning with ?"
  [x]
  (and (symbol? x)
       (= \? (-> x name first))))

;;; I don't think this is actually required by parsers, but I could make it so in my work.
(defn is-op?
  "Return true if the argument is a symbol beginning with !"
  [x]
  (and (symbol? x)
       (= \! (-> x name first))))

;;; ======================= SHOP2 Grammar ================================================================
;;;------ toplevel forms ----
(s/def ::domain
  (s/and seq?
         #(= (nth % 0) 'defdomain)
         #(symbol? (nth % 1))
         #(every? (fn [elem] (s/or :method   (s/valid? ::method elem)
                                   :operator (s/valid? ::operator elem)
                                   :axiom    (s/valid? ::axiom elem)))
                  (nth % 2))))

;;; (:method h [n1] C1 T1 [n2] C2 T2 ... [nk] Ck Tk)
(s/def ::method
  (s/and #(reset! diag {:method %})
         seq?
         #(= (nth % 0) :method)
         #(s/valid? ::atom (nth % 1))
         #(s/valid? ::method-rhsides (nthrest % 2))))

(s/def ::operator
  (s/and #(reset! diag {:operator %})
         seq?
         #(or (== (count %) 5) (== (count %) 6))
         #(= (nth % 0) :operator)
         #(s/valid? ::atom (nth % 1))
         #(s/valid? ::operator-preconditions (nth % 2))
         #(s/valid? ::del-list (nth % 3))
         #(s/valid? ::add-list (nth % 4))
         (s/or :no-cost #(== (count %) 5) ; BTW, cost is 1 if not specified.
               :cost #(and (== (count %) 6) (s/valid? ::s-exp (nth % 5))))))

;;; (:- a [name1] E1 [name2] E2 [name3] E3 ... [namen] En)
(s/def ::axiom
  (s/and ;#(reset! diag {:axiom %})
         seq?
         #(= (nth % 0) :-)
         #(s/valid? ::atom (nth % 1))
         #(if (= '(()) (nthrest % 2))
            true
            (loop [still-ok? true
                   terms (subvec (vec %) 2)]
              (cond (empty? terms)  true
                    (not still-ok?) false
                    :else            (if (symbol? (first terms))
                                       (recur (s/valid? ::logical-exp (nth terms 1))
                                              (subvec terms 2))
                                       (recur (s/valid? ::logical-exp (nth terms 0))
                                              (subvec terms 1))))))))

;;;---- toplevel support ----------------------------
;;;      (:method (upper-move-aircraft-no-style ?a ?c)
;;;               Case1 ((at ?a ?c)) ()
;;;               Case2 (:sort-by ?cost <
;;;                               ((at ?a ?somecity)
;;;                                (travel-cost-info ?a ?somecity ?c ?cost ?style)))
;;;               ((move-aircraft ?a ?somecity ?c ?style)))
(s/def ::method-rhsides
  (s/and seq?
         (fn [pairs]
           (letfn [(cond-good? [x]
                     (if (-> x first seq?)
                       (every? #(s/valid? ::precondition %) x)
                       (s/valid? ::precondition x)))]
             (loop [still-ok? true
                    terms (vec pairs)]
               (cond (empty? terms)  true
                     (not still-ok?) false
                     :else           (if (symbol? (first terms))
                                       (recur (and (cond-good? (nth terms 1))
                                                   (s/valid? ::tasks (nth terms 2)))
                                              (subvec terms 3))
                                       (recur (and (cond-good? (nth terms 0))
                                                   (s/valid? ::tasks (nth terms 1)))
                                              (subvec terms 2)))))))))

(s/def ::operator-preconditions
  (s/and seq?
         #(every? (fn [c] (s/valid? ::precondition c)) %)))

(s/def ::precondition
  (s/or :empty                 empty?
        :logical-exp           ::logical-exp
        :first-satisfier-exp   ::first-satisfier-exp
        :sort-by-exp           ::sort-by-exp))

(s/def ::first-satisfier-exp
  (s/and seq?
         #(>= (count %) 3)
         #(= (nth % 0) :first)
         #(every? (fn [l] (s/valid? ::logical-exp l)) (rest %))))

(s/def ::sort-by-exp
  (s/and seq?
         #(>= (count %) 3)
         #(= (nth % 0) :sort-by)
         #(is-var? (nth % 1))
         (s/or :with-exp (s/and
                          #(== (count %) 4)
                          #(s/valid? ::s-exp-extended (nth % 2)) ; This is typically (ToDo: always?) a function symbol.
                          #(s/valid? ::logical-exp (nth % 3)))
               :wo-exp  (s/and
                         #(== (count %) 3)
                         #(s/valid? ::logical-exp (nth % 2))))))

(s/def ::tasks
  (s/and seq?
         (s/or :empty     empty?
               :ordered   ::ordered-task-list
               :unordered ::unordered-task-list)))

(s/def ::unordered-task-list
  (s/and seq?
         #(not-empty %)
         #(= :unordered (nth % 0))
         #(every? (fn [t] (s/valid? ::task-atom-extended t)) (rest %))))

(s/def ::ordered-task-list
  (s/or :empty    empty?
        :explicit (s/and #(= :ordered (nth % 0))
                         #(every? (fn [t] (s/valid? ::task-atom-extended t)) (rest %)))
        :implict #(every? (fn [t] (s/valid? ::task-atom-extended t)) %)))

(s/def ::del-list
  (s/or :list (s/and seq?
                     #(every? (fn [elem]
                                (s/or :atom         (s/valid? ::atom elem)
                                      :protected    (s/valid? ::protected elem)
                                      :op-universal (s/valid? ::op-universal elem))) %))
        :atom #(symbol? %)))

(s/def ::add-list ::del-list)

;;; A little different than the logical-exp version.
(s/def ::op-universal
  (s/and seq?
         #(== (count %) 4)
         #(= (nth % 0) 'forall)
         #(s/valid? ::var-list (nth % 1))
         #(s/valid? ::logical-exp (nth % 2))
         #(every? (fn [elem] (s/valid? ::atom elem)) (nth % 3))))

(s/def ::protected
  (s/and seq?
         #(== (count %) 2)
         #(= (nth % 0) :protection)
         #(s/valid? ::atom (nth % 1))))

;;; 1) Task atom can be preceded by :immediate
;;; 2) The terms of a task atom are those of ordinary atoms plus 'call-terms' (See Section 4.11, third bullet).
(s/def ::task-atom
  (s/and seq?
         (s/or :immediate (s/and #(= :immediate (nth % 0))
                                 #(symbol? (nth % 1))
                                 #(every? (fn [t] (s/valid? ::task-atom-term t)) (nthrest % 2)))
               :ordinary (s/and  #(symbol? (nth % 0))
                                 #(every? (fn [t] (s/valid? ::task-atom-term t)) (nthrest % 1))))))

(s/def ::task-atom-term
  (s/or :term      ::term
        :call-term ::call-term))

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
  (s/and seq?
         (s/or :atom ::atom
               :atom-list #(every? (fn [a] (s/valid? ::atom a)) %))))

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
         #(s/valid? ::s-exp (second %))))

(s/def ::call-term
  (s/and seq?
         #(= (nth % 0) 'call)
         (s/or :sym #(s/valid? ::fn-symbol (nth % 1))
               :fun (s/and seq?
                           #(= 'function (nth (nth % 1) 0))))
         #(every? (fn [t] (s/valid? ::extended-term t)) (-> % rest rest))))

(s/def ::var-list (s/and seq? #(every? is-var? %)))

;;;------------- s-expression -------------------------
(s/def ::s-term (s/or :atomic #(or (number? %) (symbol? %) (string? %))
                      :exp    ::s-exp))

(s/def ::s-exp (s/or
                :atomic #(number? %)
                :form   (s/and seq?
                               #(-> % (nth 0) symbol?)
                               #(every? (fn [t] (s/valid? ::s-term t)) (rest %)))))

;;; ToDo: Maybe just use ::s-exp; study where these are used.
(s/def ::s-exp-extended (s/or :s-exp    ::s-exp
                              :fn-name  symbol?))

;;;----------- logical expression
(s/def ::logical-exp
  (s/or :atom           ::atom
;;        :immediate-atom ::immediate-atom
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

;;; AKA logical-atom, can't have a call-term or a eval-term
(s/def ::atom (s/and seq?
                     #(symbol? (nth % 0))
                     #(every? (fn [t] (s/valid? ::term t)) (rest %))))

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

;;;======================================= Rewriting to DB elements
(def ^:dynamic *debugging?* false)
(def tags   (atom []))
(def locals (atom [{}]))

(defn clear-rewrite!
  "Trouble just passing tags and locals to rewrite.cljc!"
  []
  (reset! tags [:toplevel])
  (reset! locals [{}]))

;;; This is simpler than RM's rewrite, which relied on :typ from parse and called 'rewrite-meth' using it.
;;; This grammar has only a few top-level entry points: defdomain, :method :operator and :axiom.
(defmacro defrew2db [tag [obj & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod rew2db ~tag [~obj & [~@more-args]]
     (when *debugging?* (println (cl-format nil "~A==> ~A" (sutil/nspaces (count @tags)) ~tag)))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [res# (do ~@body)
           result# (if (seq? res#) (doall res#) res#)]
     (swap! tags   #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when *debugging?*
           (println (cl-format nil "~A<-- ~A returns ~S" (sutil/nspaces (count @tags)) ~tag result#)))
         result#))))

(defn rew2db-dispatch
  "Allow calls of two forms: (rew2db exp) (rew2db exp :some-method-key)."
  [exp & [specified]]
  (cond ;; Optional 2nd argument specifies method to call. Order matters!
    (keyword? specified)            specified,
    (s/valid? ::domain   exp)       :domain
    (s/valid? ::method   exp)       :method
    (s/valid? ::operator exp)       :operator
    (s/valid? ::axiom    exp)       :axiom
    (s/valid? ::logical-exp exp)    :logical-exp ; Essentially, has its own similar dispatch function.
    (contains? exp :sys/body)       :body
    :else (throw (ex-info "No dispatch value for exp" {:exp exp}))))

(defmulti rew2db #'rew2db-dispatch)

;;;-------------------------------- Top-level methods
(defrew2db :domain [exp]
  (let [[_ name elems] exp
        cnt (atom 0)]
    (-> {:domain/name (str name) :sys/typ :domain}
        (assoc :domain/elems (vec (for [e elems]
                                    (-> (rew2db e)
                                        (assoc :sys/pos (swap! cnt inc)))))))))

;;; (rew2db '(:method (transport-person ?p ?c) Case1 ((at ?p ?c)) Case2 ((also ?p ?x)) ()) :method)
;;; Method: (:method <head> [label]
(defrew2db :method [body]
  (s/assert ::method body)
  (clear-rewrite!)
  (let [[_ head & pairs] body]
    {:sys/body (-> {:sys/typ :method}
                   (assoc :method/head      (rew2db head :atom))
                   (assoc :method/rhs       (rew2db pairs :method-rhsides)))}))

;;; Operator: (:operator <head> <pre-conds> <d-list> <a-list> [<cost>])
(defrew2db :operator [body]
  (s/assert ::operator body)
  (clear-rewrite!)
  (let [[_ head preconds dlist alist & [cost]] body]
    {:sys/body (cond-> {:sys/typ :operator}
                 true                   (assoc :operator/head (rew2db head :atom))
                 (not-nothing preconds) (assoc :operator/preconds (rew2db preconds :operator-preconds))
                 (not-nothing dlist)    (assoc :operator/d-list   (rew2db dlist :d-list))
                 (not-nothing alist)    (assoc :operator/a-list   (rew2db alist :a-list))
                 cost (assoc :operator/cost (rew2db cost :s-exp-extended)))}))

;;; (:- a [name1] E1 [name2] E2 [name3] E3 ... [namen] En)
;;;   (rew2db '(:- (same ?x ?x) ()) :axiom)
(defrew2db :axiom [body]
  (s/assert ::axiom body)
  (clear-rewrite!)
  (let [[_ head & exps] body
        rhs (loop [exps (vec exps)
                                  pos 1
                                  res []]
                             (cond (empty? exps)              res
                                   (symbol? (nth exps 0))     (recur
                                                               (subvec exps 2)
                                                               (inc pos)
                                                               (conj res (cond-> {:rhs/case-name (nth exps 0)}
                                                                           true                     (assoc :sys/pos pos)
                                                                           (not-empty (nth exps 1)) (assoc :rhs/terms (rew2db (nth exps 1) :logical-exp)))))
                                   :else                      (recur
                                                               (subvec exps 1)
                                                               (inc pos)
                                                               (if (not-empty (nth exps 0))
                                                                 (conj res (-> {:sys/pos pos}
                                                                               (assoc :rhs/terms (rew2db (nth exps 0) :logical-exp))))
                                                                 res))))]
    {:sys/body (cond-> {:sys/typ :axiom
                        :axiom/head (rew2db head :atom)}
                 (not-empty rhs)    (assoc :axiom/rhs rhs))}))


;;;------- supporting methods ------------------------------
(defrew2db :body [exp] (-> exp :sys/body rew2db))

(defrew2db :task-list [exp]
  (let [explicit (#{:ordered :unordered} (first exp))
        tasks (if explicit (rest exp) exp)
        cnt (atom 0)]
    (cond-> {:sys/typ :task-list
             :task-list/elems (-> (for [t tasks]
                                    (-> (rew2db t :task-list-elem)
                                        (assoc :sys/pos (swap! cnt inc))))
                                  vec)}
      (= explicit :unordered)  (assoc :task-list/unordered? true))))

(defrew2db :task-list-elem [exp]
  (let [immediate? (= :immediate (first exp))
        exp (if immediate? (rest exp) exp)]
    (as-> exp ?e
      (cond (s/valid? ::task-atom ?e)  (rew2db ?e :task-atom)
            (s/valid? ::op-call   ?e)  (rew2db ?e :op-call)
            :else (throw (ex-info "Task is neither a task-atom or op-call" {:exp ?e})))
      (if immediate? (assoc ?e :task/immediate? true) ?e))))

(defrew2db :op-call [exp]
  {:sys/typ :op-call
   :op-call/predicate (first exp)
   :op-call/args (let [cnt (atom 0)]
                   (-> (for [arg (rest exp)]
                         (if (s/valid? ::atom arg)
                           (-> (rew2db arg :atom)
                               (assoc :sys/pos (swap! cnt inc)))
                           (-> {:sys/pos (swap! cnt inc)} ; Assumes it is a list of atoms.
                               (assoc :op-call/args (-> (let [ncnt (atom 0)]
                                                          (for [narg arg]
                                                            (-> (rew2db narg :atom)
                                                                (assoc :sys/pos (swap! ncnt inc)))))
                                                        vec)))))
                       vec))})

;;; (:method h [n1] C1 T1 [n2] C2 T2 ... [nk] Ck Tk)
(defrew2db :method-rhsides [rhs] ; this does the [n1] C1 T1 [n2] C2 T2 ... [nk] Ck Tk
  (let [res (loop [res []
                   triples (vec rhs)]
              (if (empty? triples)
                res
                (if (symbol? (nth triples 0))
                  (recur (conj res (cond-> {:method/case-name (nth triples 0)}
                                     (not-empty (nth triples 1)) (assoc :method/preconditions (rew2db (nth triples 1) :method-precond))
                                     (not-empty (nth triples 2)) (assoc :method/task-list     (rew2db (nth triples 2) :task-list))))
                         (subvec triples 3))
                  (recur (conj res (cond-> {}
                                     (not-empty (nth triples 0)) (assoc :method/preconditions (rew2db (nth triples 0) :method-precond))
                                     (not-empty (nth triples 1)) (assoc :method/task-list     (rew2db (nth triples 1) :task-list))))
                         (subvec triples 2)))))]
    (let [cnt (atom 0)]
      (for [r res]
        (assoc r :sys/pos (swap! cnt inc))))))

(defrew2db :method-precond [c]
  (cond (-> c (nth 0) seq?)         (mapv #(rew2db % :atom) c)
        (-> c (nth 0) (= :sort-by)) (rew2db c :sort-by)
        (-> c (nth 0) (= :first))   (rew2db c :first)
        :else (throw (ex-info "Invalid method precondition" {:exp c}))))

(defrew2db :operator-preconds [exp]
  (let [cnt (atom 0)] ; Ordinary conjunct of :logical-exp
    (-> (for [pc exp]
          (-> {:precond/exp (rew2db pc :logical-exp)}
              (assoc :sys/pos (swap! cnt inc))))
        vec)))

(defrew2db :sort-by [exp]
  (if (== 4 (count exp))
    (let [[_ var  fn-def lexp] exp]
      (-> {:sys/typ :sort-by
           :sort-by/var var}
          (assoc :sort-by/fn  (rew2db fn-def :s-exp-extended))
          (assoc :sort-by/exp (rew2db lexp :logical-exp))))
    (let [[_ var lexp] exp]
      (-> {:sort-by/var var}
          (assoc :sort-by/fn {:s-exp/fn-ref '<})
          (assoc :sort-by/exp (rew2db lexp :logical-exp))))))

(defn box [v]
  (cond (number? v)  {:box/num v}
        (string? v)  {:box/str v}
        (symbol? v)  {:box/sym v}
        :else        v))

(defrew2db :d-list [exp]
  (let [exp (if (seq? exp) (vec exp) (vector exp))
        cnt (atom 0)]
    (-> (for [e exp]
          (-> (rew2db e :op-list-elem)
              (assoc :sys/pos (swap! cnt inc))))
        vec)))

(defrew2db :a-list [exp]
  (let [exp (if (seq? exp) (vec exp) (vector exp))
        cnt (atom 0)]
    (-> (for [e exp]
          (-> (rew2db e :op-list-elem)
              (assoc :sys/pos (swap! cnt inc))))
        vec)))

(defrew2db :op-list-elem [exp]
  (cond (symbol? exp)                 {:box/sym exp}
        (s/valid? ::atom exp)         (rew2db exp :atom)
        (s/valid? ::protected exp)    (rew2db exp :protected)
        (s/valid? ::op-universal exp) (rew2db exp :op-universal)
        :else (throw (ex-info "Invalid item in op list:" {:exp exp}))))

(defrew2db ::protected [exp]
  {:protected/atom (rew2db (nth exp 1) :atom)})

(defrew2db ::op-universal [exp]
  (let [[_ vars cond consq] exp]
    (-> {:sys/typ :op-universal
         :forall/vars (vec vars)}
        (assoc :forall/conditions   (rew2db cond :logical-exp))
        (assoc :forall/consequences (mapv #(rew2db % :atom) consq)))))

;;; ---------------- s-expression -----------------
(defn seq2sexp [s]
  (cond (seq? s) (cond-> {:s-exp/fn-ref (first s) :sys/typ :s-exp}
                   (-> s rest not-empty) (assoc :s-exp/args (let [cnt (atom 0)]
                                                              (for [a (rest s)]
                                                                (->  (rew2db a :s-exp-arg)
                                                                     (assoc :sys/pos (swap! cnt inc)))))))
        (or (number? s) (string? s) (symbol? s)) (box s)))

;;; This currently does not handle quoted things.
(defrew2db :s-exp-extended [exp]
  (if (symbol? exp)
    {:s-exp/fn-ref exp :sys/typ :s-exp}
    (seq2sexp exp)))

(defrew2db :s-exp-arg [exp]
  (if (seq? exp)
    (seq2sexp exp)
    {:s-exp/arg-val (box exp)}))
;;;-------------------------------- Logical expressions -------------------
(defrew2db :logical-exp [exp] ; order matters!
  (cond (s/valid? ::implication exp)    (rew2db exp :implication)
        (s/valid? ::negation exp)       (rew2db exp :negation)
        (s/valid? ::disjunct exp)       (rew2db exp :disjunct)
        (s/valid? ::universal exp)      (rew2db exp :universal)
        (s/valid? ::assignment exp)     (rew2db exp :assignment)
        (s/valid? ::eval exp)           (rew2db exp :eval)
        (s/valid? ::call exp)           (rew2db exp :call)
        (s/valid? ::enforce exp)        (rew2db exp :enforce)
        (s/valid? ::setof exp)          (rew2db exp :setof)
        (s/valid? ::atom exp)           (rew2db exp :atom) ; I think other kinds of atoms are handled by :task-list-elem
        (s/valid? ::conjunct exp)       (rew2db exp :conjunct)
        :else (throw (ex-info "Unknown logical exp:" {:exp exp}))))

(defrew2db :implication [exp]
  (let [[_ l1 l2] exp]
    {:sys/typ :implication
     :imply/condition (rew2db l1 :logical-exp)
     :imply/consequence (rew2db l2 :logical-exp)}))

(defrew2db :negation [exp]
  (-> (rew2db (nth exp 1) :logical-exp)
      (assoc :exp/negated? true)))

;;; ToDo: Make the next two ordered
(defrew2db :conjunct [exp]
  (let [res (mapv #(rew2db % :logical-exp) exp)]
    (-> (if (empty? res)
          {:conjunction/shop-empty-list? true}
          {:conjunction/terms res})
        (assoc :sys/typ :conjunction))))

(defrew2db :disjunct [exp]
  (-> {:sys/typ :disjunction}
      (assoc :disjunction/terms (mapv #(rew2db % :logical-exp) (rest exp)))))

;;;    (forall (?c) ((dest ?a ?c)) ((same ?c ?c1)))
(defrew2db :universal [exp]
  (let [[_ vars conds consq] exp]
    (-> {:sys/typ :universal}
        (assoc :forall/vars         (vec vars))
        (assoc :forall/conditions   (mapv #(rew2db % :atom) conds))
        (assoc :forall/consequences (mapv #(rew2db % :atom) consq)))))

(defrew2db :assignment [exp]
  (let [[_ v e] exp]
    (-> {:sys/typ :assignment}
        (assoc :assign/var v)
        (assoc :assign/exp (rew2db e :s-exp-extended)))))

(defrew2db :eval [exp]
  {:sys/typ :eval
   :eval/form (rew2db exp :s-exp-extended)})

(defrew2db :atom [exp]
  (let [[pred & terms] exp]
    (cond-> {:sys/typ :atom}
      true               (assoc :atom/predicate pred)
      (not-empty terms)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v terms]
                                                   {:role/val (box (rew2db v :term))
                                                    :sys/pos (swap! cnt inc)}))
                                                vec)))))

(defrew2db :task-atom [exp]
  (let [[pred & terms] exp]
    (cond-> {:sys/typ :task-atom}
      true               (assoc :atom/predicate pred)
      (not-empty terms)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v terms]
                                                   {:role/val (box (rew2db v :task-term))
                                                    :sys/pos (swap! cnt inc)}))
                                                vec)))))

(defrew2db :term [exp]
  (cond (symbol? exp) exp
        (number? exp) exp
        (seq? exp)    (rew2db exp :list-term)
        :else         (throw (ex-info "Not a valid term:" {:exp exp}))))

(defrew2db :task-term [exp]
  (cond (s/valid? ::term exp)      (rew2db exp :term)
        (s/valid? ::call-term exp) (rew2db exp :call-term)
        (s/valid? ::eval-term exp) (rew2db exp :eval-term)
        :else (throw (ex-info "Not a task-term: " {:exp exp}))))

(defrew2db :call-term [exp]
  (let [[_ fn & args] exp]
    {:sys/typ :call-term
     :call/fn (-> fn (nth 1) (nth 1)) ; It looks something like this (function (quote +)).
     :call/args (let [cnt (atom 0)]
                  (-> (for [a args]
                        (-> (rew2db a :s-exp-arg)
                            (assoc :sys/pos (swap! cnt inc))))
                      vec))}))

(defrew2db :list-term [exp]
  {:sys/typ :list-term
   :list/terms (let [cnt (atom 0)]
                  (-> (for [a exp]
                        (-> {:list/val (box (rew2db a :term))}
                            (assoc :sys/pos (swap! cnt inc))))
                      vec))})

;;;-------------------------------- end of Logical expressions -------------------

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
   This is combined with the project-oriented schema elements to define the schema for the system (as opposed to schema for a project).
   The schema contains three 'name' attributes that are :db.unique/identity unique and not part of the serialization for shop3.
   These are :method/name, :operator/name and :axiom/name. (In contrast, :domain/name IS part of the shop3 serialization.)
   The purpose of these is to allow DB-based management of in plan elements in the context of authoring and refining through the UI.
   A naming convention is used for these three where the name of the domain is prefixed with a dot; thus 'zeno."
  {;; ---------------------- assignment
   :assign/exp
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the expression that evaluate to the value to be assigned."}
   :assign/var
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol,
        :doc "the symbol to which the assignment is assigning."}

   ;; ---------------------- atom
   :atom/predicate
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol,
        :doc "a string naming the proposition inplied by a atom."}
   :atom/roles ; ToDo. Don't need this.
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "a string, typically a ?var, naming the proposition inplied by a atom."}

   ;; ---------------------- axiom
   :axiom/case-name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol
       :doc  "a name for the axiom case; the name is unique only in the context of this axiom."}
   :axiom/head
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the LHS atom of the axiom."}
   :axiom/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string :unique :db.unique/identity,
        :doc "A DB-unique name for the axiom"}
   :axiom/rhs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the RHS of this case of the axiom"}

   ;; ----------------------- box
   :box/num
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/number
        :doc "a construct to wrap primitive types when the type cannot be anticipated and therefore a :db.type/ref is used"}
   :box/str
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a construct to wrap primitive types when the type cannot be anticipated and therefore a :db.type/ref is used"}
   :box/sym
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol
        :doc "a construct to wrap primitive types when the type cannot be anticipated and therefore a :db.type/ref is used"}

   ;; ----------------------- call
   :call/fn
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol
        :doc "The symbol of a call term"}
   :call/args
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "The arguments of th the call term"}

   ;; ----------------------- conjunction
   :conjunction/terms
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the terms that are part of a conjunctive expression."} ; ToDo: need a :sys/pos or something like that.

   ;; ----------------------- domain
   :domain/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
        :doc "a DB-unique name for the domain."}
   :domain/elems
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "The methods, operators and axioms of the domain."}

   ;; ---------------------- logical expression
   :exp/negated?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "expression is negated."}

   ;; ---------------------- eval
   :eval/form
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "a form on which eval is called."}

   ;; ---------------------- forall
   :forall/conditions
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "conditions of the forall statement."}
   :forall/consequences
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "consequences of the forall staement."}
   :forall/vars
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/symbol,
        :doc "variables bound in the forall statement."}

   ;; ---------------------- imply
   :imply/condition
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the LHS of an implication."}
   :imply/consequence
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the RHS of an implication."}

   ;; ---------------------- list
   :list/terms
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "a :list structure."}
   :list/val
      #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the value in the :list structure."}

   ;; ---------------------- method
   :method/head
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
           :doc "a atom indicating task to be completed."}
   :method/case-name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol,
        :doc "a name for the method case; the name is unique only in the context of this method."}
   :method/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
           :doc "a DB-unique name for the method; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}
   :method/preconditions
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "atoms indicating what must be true in order to apply the method."}
   :method/rhs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "Pairs of conditions and action of a method."}
   :method/task-list
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "a single object of :sys/typ = :task-list describing a sequence of tasks to achieve the head literal."}

   ;; ---------------------- operation
   :operator/a-list
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the propositions to add when executing this operation."}
   :operator/cost
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the cost of executing this operation."}
   :operator/d-list
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the propositions to delete when executing this operation."}
   :operator/head
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the head atom of an operation."}
   :operator/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
        :doc "a DB-unique name for the operation; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}
   :operator/preconds
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "preconditions of the operation"}

   ;; ---------------------- preconditions
   :precond/exp
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the expression of a precondition (for a method or operator)."}

   ;; ---------------------- rhs
   :rhs/case-name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol
        :doc "terms of this (possibly named) RHS of the axiom."}
   :rhs/terms
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "terms of this (possibly named) RHS of the axiom."}

   ;; ---------------------- role
   :role/val
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the value of the role."}

   ;; ---------------------- sort-by
   :sort-by/fn
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the optional fn (default is {:s-exp/fn-ref <}) used in a :sort-by."}
   :sort-by/exp
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the logical-expression (typically a conjunct) evaluated to provide a sort var binding."}
   :sort-by/var
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol,
        :doc "the variable bound in a :sort-by."}

   ;; ---------------------- s-expression
   :s-exp/arg-val
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the arguments of the s-expression."}
   :s-exp/args
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "the arguments of the s-expression."}
   :s-exp/fn-ref
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol,
        :doc "the operator of the s-expression."}

   ;; ---------------------- system
   :sys/body
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the body of a method, operator, or axiom, ensuring (by virtue of :db.cardinality/one) that it replaces, rather than adds."}

   :sys/typ
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/keyword,
        :doc "a keyword indicating the type of the object; it is used to select a serialization method."}
   :sys/pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "the position of the element in the paren't attribute's collection."}

   ;; ---------------------- task
   :task/immediate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "indicates that the task is :immediate."}
   :task/atom
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "the task atom."}

   ;; -------------------- task-list
   :task-list/elems
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "when true, indicates that the atoms in task-list can be performed in any order. Ordered is the default."}
   :task-list/unordered?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "when true, indicates that the atoms in task-list can be performed in any order. Ordered is the default."}})

;;;=============================== Serialization (DB structures as SHOP code) ================================================
(defmacro defrew2shop [tag [obj & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod rew2shop ~tag [~obj & [~@more-args]]
     (when *debugging?* (println (cl-format nil "~A==> ~A" (sutil/nspaces (count @tags)) ~tag)))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [res# (do ~@body)
           result# (if (seq? res#) (doall res#) res#)]
     (swap! tags   #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when *debugging?*
           (println (cl-format nil "~A<-- ~A returns ~S" (sutil/nspaces (count @tags)) ~tag result#)))
         result#))))

(defn rew2shop-dispatch
  "Allow calls of two forms: (rew2shop exp) (rew2shop exp :some-method-key).
   Things with :sys/typ:
   :assignment :atom :axiom :call-term :conjunction :disjunction :domain :eval :implication :list-term :method
   :op-call :op-universal :operator :s-exp :sort-by :task-list :universal."
  [exp & [specified]]
  (cond specified                                        specified
        (and (map? exp) (contains? exp :sys/typ))        (:sys/typ exp)
        (or (symbol? exp) (number? exp) (string? exp))   :simple-type
        (contains? exp :sys/body)                        :body
        (contains? exp :box/sym)                         :box
        (contains? exp :box/num)                         :box
        (contains? exp :box/str)                         :box
        (contains? exp :s-exp/arg-val)                   :arg-val
        :else (throw (ex-info "No dispatch value for exp" {:exp exp}))))

(defmulti rew2shop #'rew2shop-dispatch)

(defrew2shop :domain [{:domain/keys [name elems]}]
  `(~'defdomain ~(symbol name)
    ~(for [e (sort-by :sys/pos elems)]
       (rew2shop e))))

;;; (:method h [n1] C1 T1 [n2] C2 T2 ... [nk] Ck Tk)
(defrew2shop :method [{:method/keys [head rhs]}]
  `(:method ~(rew2shop head :atom)
            ~@(mapcat #(rew2shop % :rhs-pair) (sort-by :sys/pos rhs)))) ; Actually rhs=multiple rh sides.

(defrew2shop :axiom [{:axiom/keys [head rhs]}]
  (let [h (rew2shop head :atom)]
    (if rhs
      (let [rew1 (mapcat #(rew2shop % :rhs) (sort-by :sys/pos rhs))]
        (if (-> rew1 first symbol?)
          `(:- ~h ~@rew1)
          `(:- ~h ~rew1)))
      `(:- ~h ()))))

(defrew2shop :operator [{:operator/keys [head preconds d-list a-list cost] :as exp}]
  (let [pre (map rew2shop (->> preconds (sort-by :sys/pos) (map :precond/exp)))
        del (map rew2shop (->> d-list   (sort-by :sys/pos)))
        add (map rew2shop (->> a-list   (sort-by :sys/pos)))
        res `(:operator
              ~(rew2shop head)
              ~pre
              ~(if (-> del first symbol?) (first del) del)
              ~(if (-> add first symbol?) (first add) add))]
    (if cost
      (->> cost rew2shop (conj (vec res)) seq)
      res)))

;;; --------------- Supporting serialization ------------------------
(defrew2shop :body [exp] (rew2shop (:sys/body exp)))

(defrew2shop :simple-type [exp] exp)

(defrew2shop :rhs [{:rhs/keys [terms case-name]}]
  (let [res (rew2shop terms)] ; ToDo: terms is a misnomer. Uses :sys/typ :conjunction.
    (if case-name
      `(~case-name ~res)
      res)))

(defrew2shop :atom [{:atom/keys [predicate roles] :exp/keys [negated?]}]
  (let [atom `(~predicate ~@(map rew2shop (->> roles (sort-by :sys/pos) (map :role/val))))]
    (if negated? `(~'not ~atom) atom)))

(defrew2shop :box [{:box/keys [sym num str]}] (or sym num str))

(defrew2shop :conjunction [{:conjunction/keys [terms]}]
  (map rew2shop terms))

(defrew2shop :eval [{:eval/keys [form]}] (rew2shop form)) ; It is just an s-exp, I think.

(defrew2shop :s-exp [{:s-exp/keys [fn-ref args]}]
  `(~fn-ref ~@(map rew2shop (->> args (sort-by :sys/pos)))))

(defrew2shop :arg-val [{:s-exp/keys [arg-val]}]
  (rew2shop arg-val))

(defrew2shop :assignment [{:assign/keys [var exp]}]
  `(~'assign ~var ~(rew2shop exp)))

(defrew2shop :rhs-pair [{:method/keys [case-name preconditions task-list]}]
  (let [pres (if (-> preconditions vector?)
               (->> preconditions (sort-by :sys/pos) (map rew2shop))
               (if (empty? preconditions) '() (rew2shop preconditions)))
        pres (if (= :sort-by (-> pres first first)) (first pres) pres) ; :sort-by is different!
        res `(~pres ~(rew2shop task-list :task-list))]
    (if case-name
      (conj res case-name)
      res)))

(defrew2shop :task-list [{:task-list/keys [elems]}]
  (->> elems (sort-by :sys/pos) (map rew2shop)))

(defrew2shop :task-atom [{:task/keys [immediate?] :atom/keys [predicate roles]}]
  (let [atom `(~predicate ~@(map rew2shop (->> roles (sort-by :sys/pos) (map :role/val))))]
    (if immediate? `(:immediate ~@atom) atom)))

(defrew2shop :call-term [{:call/keys [fn args]}]
  `(~'call (~'function '~fn) ~@(map rew2shop (sort-by :sys/pos args))))

;;; ToDo: Some of the following have not been tested.
(defrew2shop :disjunction [exp] `(~'or ~(map rew2shop exp)))

(defrew2shop :implication [{:imply/keys[condition consequence]}]
  `(~'imply ~(rew2shop condition) ~(rew2shop consequence)))

(defrew2shop :list-term [{:list/keys [terms]}]
  (->> terms (sort-by :sys/pos) (map :list/val) (map rew2shop)))

(defrew2shop :op-call [exp]
  (rew2shop exp :call-term))

(defrew2shop :op-universal [{:op-forall/keys [vars condition consequences]}]
  `(~'forall (map rew2shop vars)
    ~(rew2shop condition)
    ~(map rew2shop consequences)))

(defrew2shop :sort-by [{:sort-by/keys [var fn exp]}]
  `(:sort-by ~(rew2shop var) ~(-> fn rew2shop first) ~(rew2shop exp)))

(defrew2shop :universal [{:forall/keys [vars conditions consequences]}]
  `(~'forall ~(map rew2shop vars) ~(map rew2shop conditions) ~(map rew2shop consequences)))

;;; ------------------------ Top-level manipulation -----------------------------------
(declare code-type)
(defn canon2db
  "Given a canonical structure, create the corresponding DB structure.
   :canon/pos of each element of :domain/elems is :sys/pos in the DB."
  [{:domain/keys [name elems]}]
    {:domain/name name
     :domain/elems (mapv #(let [name-attr (keyword (code-type %) "name")]
                            (-> (rew2db  (:canon/code %))
                                (assoc :sys/pos (:canon/pos %))
                                (assoc name-attr (get % name-attr))))
                         (sort-by :canon/pos elems))})

(defn canon2shop
  "Rewrite 'canonical' (which is SHOP form embeddded in EDN) as SHOP."
  [{:domain/keys [name elems]}]
  `(~'defdomain ~(symbol name)
    ~(->> elems (sort-by :canon/pos) (map :canon/code))))

(defn db-entry-for
  "Return the named DB structure. name is string and db.unique/identity"
  [name]
  (when-let [eid (d/q '[:find ?e .
                        :in $ ?name
                        :where (or [?e :method/name ?name]
                                   [?e :operator/name ?name]
                                   [?e :axiom/name ?name]
                                   [?e :domain/name ?name])]
                      @(connect-atm :system)
                      name)]
    (sutil/resolve-db-id {:db/id eid} (connect-atm :system) #{:db/id})))

(defn db2canon
  "Rewrite the named DB structure (a :db.unique/identity) to a map of SHOP2 lispy structures called 'canonical'."
  [name]
  (when-let [obj (db-entry-for name)]
    (let [cnt (atom 0)]
      (cond (contains? obj :domain/name) {:domain/name name
                                          :domain/elems (-> (for [e (->> obj :domain/elems (sort-by :sys/pos))]
                                                              (let [name-attr (keyword (code-type e) "name")]
                                                                (-> {:canon/pos (swap! cnt inc)}
                                                                    (assoc name-attr (get e name-attr))
                                                                    (assoc :canon/code (rew2shop e)))))
                                                            vec)}
            ;; This one is just used in debugging, I think.
            (#{"method" "axiom" "operator"} (code-type obj))
                                        {(keyword (code-type obj) "name") name
                                         :canon/code (-> obj :sys/body rew2shop)}))))

(defn db2shop
  "Return the DB structure with the given name (a :domain :method, :operator, or :axiom)."
  [name]
  (when-let [obj (db-entry-for name)]
    (rew2shop obj)))

(defn code-type [exp]
  (cond (contains? exp :method/name)   "method"
        (contains? exp :axiom/name)    "axiom"
        (contains? exp :operator/name) "operator"))

;;; ------------------------ edn2canonical ------------------------------
(def edn-cnt-atm (atom 0))

(defn edn2canon-method
  "Restructure EDN into canonical (which is more lisp-like)."
  [e base-name]
  (-> e
      (assoc :canon/pos (swap! edn-cnt-atm inc))
      (assoc :method/name (str base-name "." (-> e :method/head first)))
      (assoc :canon/code
             `(:method
               ~(:method/head e)
               ~@(mapcat (fn [p]
                           (if-let [cname (:method/case-name p)]
                             `(~(symbol cname)
                               ~(if-let [pc (-> p :method/preconditions seq)] pc ())
                               ~(-> p :method/task-list seq))
                             `(~(if-let [pc (-> p :method/preconditions seq)] pc ())
                               ~(-> p :method/task-list seq))))
                         (:method/rhsides e))))
      (dissoc :method/rhsides)))

;;; ToDo: Implement the next two.
(defn edn2canon-operator
  "Restructure EDN into canonical (which is more lisp-like)."
  [e base-name])

(defn edn2canon-axiom
  "Restructure EDN into canonical (which is more lisp-like)."
  [e base-name])

(defn edn2canonical
  "Rewrite the EDN structure as canonical, which is less lispy."
  [edn]
  (reset! edn-cnt-atm 0)
  (let [base-name (:domain/name edn)]
    (update edn :domain/elems
            #(for [e %]
               (cond (contains? e :method/head)   (edn2canon-method e base-name)
                     (contains? e :operator/head) (edn2canon-operator e base-name)
                     (contains? e :axiom/head)    (edn2canon-axiom e base-name))))))


;;; ------------------------ DB Stuff -------------------------
;;; ToDo: This is probably not necessary! I had :db.unique where it should not be.
(defn collect-lookups
  [obj]
  (let [lookups (atom [])
        id-attr? #{#_:domain/name :method/name :axiom/name :operator/name}]
    (letfn [(co [x]
              (cond (map? x)    (doseq [[k v] (seq x)]
                                  (when (id-attr? k) (swap! lookups conj [:db/add -1 k v]))
                                  (co v))
                    (vector? x) (doseq [e x] (co e))))]
      (co obj)
      @lookups)))

(defn write-obj
  "Create lookup objects write those, then write the whole thing."
  [obj]
  (let [lookup-refs (collect-lookups obj)
        conn (connect-atm :system)]
    (doseq [x lookup-refs] (d/transact conn [x]))
    (d/transact conn [obj])))

;;; Currently this loads zeno, just for testing.
(defn recreate-planning-domains-db!
  "Recreate the plans db from .edn data. It uses the connect-atm, which is already established."
  [config]
  (if (.exists (io/file "data/planning-domains/zeno-travel.edn")) ; <=================== Loading zeno.
    (do (log/info "Recreating the planning domains database.")
        (when (d/database-exists? config)
          (d/delete-database config))
        (d/create-database config)
        (register-db :plans config)
        (let [conn (connect-atm :plans)]
          (d/transact conn (datahike-schema db-schema-shop2+))
          (d/transact conn (-> "data/planning-domains/zeno-travel.edn" ; <============ Loading zeno.
                               slurp
                               edn/read-string
                               rew2db))))
        true)
    (log/error "Not recreating planning domains DB: No backup file."))

;;; -------------------- Starting and stopping -------------------------
(defn init-db-cfg
  "Set sys-db-cfg atoms for system db and the template for the proj-base-cfg (:base-path).
   Recreate the system database if sys-db-cfg.recreate-db? = true."
  []
  (let [base-dir (or (-> (System/getenv) (get "SCHEDULING_TBD_DB")) ; "/opt/scheduling" typically.
                     (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases."
                                          "\nCreate a directory 'planning-domains' under it.") {})))
        ;; https://cljdoc.org/d/io.replikativ/datahike/0.6.1545/doc/datahike-database-configuration
        config {:store {:backend :file :path (str base-dir "/planning-domains")}
                :keep-history? false
                ;;:attribute-refs? true ; With this I can't transact lookup-refs!
                :recreate-db? true ; <=== If true, it will recreate the plans DB.
                :schema-flexibility :write}]
    (when (:recreate-db? config)
      (recreate-planning-domains-db! config)) ; This adds the schema and planning domains.
    {:plan-db-cfg config}))

(defstate plans-db-cfg
  :start (init-db-cfg))
