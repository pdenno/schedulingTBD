(ns scheduling-tbd.shop
  "Database storage and serialization of SHOP planning domain content."
  (:require
   [clojure.pprint       :refer [cl-format]]
   [clojure.spec.alpha   :as s]
   [datahike.api         :as d]
   [scheduling-tbd.sutil :as sutil :refer [register-db connect-atm not-nothing]]
   [taoensso.timbre      :as log]))

;;; ToDo: Factor SHOP2 stuff to its own
;;; ToDo: Next line makes a choice that has global consequences, so maybe wrap the SHOP translation code in something that toggles this.
(s/check-asserts true)
(def diag (atom nil))

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
         #(s/valid? ::method-rhs-pairs (nthrest % 2))))

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
  (s/and #(reset! diag {:axiom %})
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
(s/def ::method-rhs-pairs
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
                          #(s/valid? ::s-exp-extended (nth % 2))
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
         #(s/valid? ::fn-symbol (nth % 1))
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
(def ^:dynamic *debugging?* true)
(def tags   (atom []))
(def locals (atom [{}]))

(defn clear-rewrite!
  "Trouble just passing tags and locals to rewrite.cljc!"
  []
  (reset! tags [:toplevel])
  (reset! locals [{}]))

;;; This is simpler than RM's rewrite, which relied on :typ from parse and called 'rewrite-meth' using it.
;;; This grammar has only a few top-level entry points: defdomain, :method :operator and :axoim.
(defmacro defrewrite [tag [obj & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod rewrite ~tag [~obj & [~@more-args]]
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

(defn rewrite-dispatch
  "Allow calls of two forms: (rewrite exp) (rewrite exp :some-method-key)."
  [exp & [specified]]
  (cond ;; Optional 2nd argument specifies method to call. Order matters!
    (keyword? specified)            specified,
    (s/valid? ::domain exp)         :domain
    (s/valid? ::method exp)         :method
    (s/valid? ::operator exp)       :operator
    (s/valid? ::axiom exp)          :axiom
    (s/valid? ::logical-exp exp)    :logical-exp ; Essentially, has its own similar dispatch function.
    :else (throw (ex-info "No dispatch value for exp" {:exp exp}))))

(defmulti rewrite #'rewrite-dispatch)

;;;-------------------------------- Top-level methods
(defrewrite :domain [exp]
  (let [[_ name elems] exp]
    (-> {:domain/name name}
        (assoc :domain/elems (mapv rewrite elems)))))

;;; (rewrite '(:method (transport-person ?p ?c) Case1 ((at ?p ?c)) Case2 ((also ?p ?x)) ()) :method)
(defrewrite :method [exp]
  (s/assert ::method exp)
  (clear-rewrite!)
  (let [[_ head & pairs] exp]
    (-> {}
        (assoc :method/head      (rewrite head :atom))
        (assoc :method/rhs-pairs (rewrite pairs :method-rhs-pairs)))))

(defrewrite :operator [exp]
  (s/assert ::operator exp)
  (clear-rewrite!)
  (let [[_ head preconds dlist alist & [cost]] exp]
    (cond-> {:op/head (rewrite head :atom)}
      (not-nothing preconds) (assoc :op/preconds (rewrite preconds :operator-preconds))
      (not-nothing dlist)    (assoc :op/d-list   (rewrite dlist :op-list))
      (not-nothing alist)    (assoc :op/a-list   (rewrite alist :op-list))
      cost (assoc :op/cost (rewrite cost :s-exp-extended)))))

;;; (:- a [name1] E1 [name2] E2 [name3] E3 ... [namen] En)
;;;   (rewrite '(:- (same ?x ?x) ()) :axiom)
(defrewrite :axiom [exp]
  (s/assert ::axiom exp)
  (clear-rewrite!)
  (let [[_ head & exps] exp]
    {:axiom/head (rewrite head :atom)
     :axiom/rhs (loop [exps (vec exps)
                       pos 1
                       res []]
                  (cond (empty? exps)              res
                        (symbol? (nth exps 0))     (recur
                                                    (subvec exps 2)
                                                    (inc pos)
                                                    (conj res (cond-> {:axiom/term-name (nth exps 0)}
                                                                true                     (assoc :axiom/term-pos pos)
                                                                (not-empty (nth exps 1)) (assoc :axiom/term (rewrite (nth exps 1) :logical-exp)))))
                        :else                      (recur
                                                    (subvec exps 1)
                                                    (inc pos)
                                                    (conj res (cond-> {:axiom/term-pos pos}
                                                                (not-empty (nth exps 0)) (assoc :axiom/term (rewrite (nth exps 0) :logical-exp)))))))}))

;;;-------------------------------------
;;; Only called explicitly. Note that the things in the task lists are not all task atoms!
(defrewrite :task-list [exp]
  (let [explicit (#{:ordered :unordered} (first exp))
        tasks (if explicit (rest exp) exp)
        cnt (atom 0)]
    (cond-> {:task-list/elems (-> (for [t tasks]
                                    (-> (rewrite t :task-list-elem)
                                        (assoc :task/pos (swap! cnt inc))))
                                  vec)}
      (= explicit :unordered)  (assoc :task-list/unordered? true))))

(defrewrite :task-list-elem [exp]
  (let [immediate? (= :immediate (first exp))
        exp (if immediate? (rest exp) exp)]
    (as-> exp ?e
      (cond (s/valid? ::task-atom ?e)  (rewrite ?e :task-atom)
            (s/valid? ::op-call   ?e)  (rewrite ?e :op-call)
            :else (throw (ex-info "Task is neither a task-atom or op-call" {:exp ?e})))
      (if immediate? (assoc ?e :task/immediate? true) ?e))))

(defrewrite :op-call [exp]
  {:op-call/predicate (first exp)
   :op-call/args (let [cnt (atom 0)]
                   (-> (for [arg (rest exp)]
                         (if (s/valid? ::atom arg)
                           (-> (rewrite arg :atom)
                               (assoc :arg/pos (swap! cnt inc)))
                           (-> (:arg/pos (swap! cnt inc)) ; Assumes it is a list of atoms.
                               (assoc :op-call/args (-> (let [ncnt (atom 0)]
                                                          (for [narg arg]
                                                            (-> (rewrite narg :atom)
                                                                (:arg/pos (swap! ncnt inc)))))
                                                        vec)))))
                       vec))})

;;; (:method h [n1] C1 T1 [n2] C2 T2 ... [nk] Ck Tk)
(defrewrite :method-rhs-pairs [pairs]
  (loop [res []
         terms (vec pairs)]
    (if (empty? terms)
      res
      (if (symbol? (nth terms 0))
        (recur (conj res (cond-> {:method/case-name (nth terms 0)}
                           (not-empty (nth terms 1)) (assoc :method/preconditions (rewrite (nth terms 1) :method-precond))
                           (not-empty (nth terms 2)) (assoc :method/tasks         (rewrite (nth terms 2) :task-list))))
               (subvec terms 3))
        (recur (conj res (cond-> {}
                           (not-empty (nth terms 0)) (assoc :method/preconditions (rewrite (nth terms 0) :method-precond))
                           (not-empty (nth terms 1)) (assoc :method/tasks         (rewrite (nth terms 1) :task-list))))
               (subvec terms 2))))))

(defrewrite :method-precond [c]
  (cond (-> c (nth 0) seq?)         (mapv #(rewrite % :atom) c)
        (-> c (nth 0) (= :sort-by)) (rewrite c :sort-by)
        (-> c (nth 0) (= :first))   (rewrite c :first)
        :else (throw (ex-info "Invalid method precondition" {:exp c}))))

(defrewrite :operator-preconds [exp]
  (let [cnt (atom 0)] ; Ordinary conjunct of :logical-exp
    (-> (for [pc exp]
          (-> {:precond/exp (rewrite pc :logical-exp)}
              (assoc :precond/pos (swap! cnt inc))))
        vec)))

(defrewrite :sort-by [exp]
  (if (== 4 (count exp))
    (let [[_ var  fn-def lexp] exp]
      (-> {:sort-by/var var}
          (assoc :sort-by/fn  (rewrite fn-def :s-exp-extended))
          (assoc :sort-by/exps (mapv #(rewrite % :logical-exp) lexp))))
    (let [[_ var lexp] exp]
      (-> {:sort-by/var var}
          (assoc :sort-by/fn {:fn/ref '<})
          (assoc :sort-by/exps (mapv #(rewrite % :logical-exp) lexp))))))

;;; This currently does not handle quoted things.
(defrewrite :s-exp-extended [exp]
  (letfn [(box [v] (cond (number? v)  {:box/num v}
                         (string? v)  {:box/str v}
                         (symbol? v)  {:box/sym v}))
          (seq2sexp [s]
            (cond (seq? s) (cond-> `{:s-exp/fn-ref '~(first s)}
                             (-> s rest not-empty) (assoc :s-exp/args (let [cnt (atom 0)]
                                                                        (-> (for [a (rest s)]
                                                                              (if (seq? a)
                                                                                (-> (seq2sexp a) (assoc :arg/pos (swap! cnt inc)))
                                                                                {:arg/val (box a) :arg/pos (swap! cnt inc)}))
                                                                            vec))))
                  (or (number? s) (string? s) (symbol? s)) (box s)))]
    (if (symbol? exp)
      {:s-exp/fn-ref exp}
      (seq2sexp exp))))

(defrewrite :op-list [exp]
  (let [exp (if (seq? exp) (vec exp) (vector exp))
        cnt (atom 0)]
    (-> (for [e exp]
          (-> (rewrite e :op-list-elem)
              (assoc :op/pos (swap! cnt inc))))
        vec)))

;;; This delete-list or add-list
(defrewrite :op-list-elem [exp]
  (cond (symbol? exp)                 {:box/sym exp}
        (s/valid? ::atom exp)         (rewrite exp :atom)
        (s/valid? ::protected exp)    (rewrite exp :protected)
        (s/valid? ::op-universal exp) (rewrite exp :op-universal)
        :else (throw (ex-info "Invalid item in op list:" {:exp exp}))))

(defrewrite ::protected [exp]
  {:protected/atom (rewrite (nth exp 1) :atom)})

(defrewrite ::op-universal [exp]
  (let [[_ vars cond consq] exp]
    (-> {:op-forall/vars (vec vars)}
        (assoc :op-forall/condition    (rewrite cond :logical-exp))
        (assoc :op-forall/consequences (mapv #(rewrite % :atom) consq)))))

;;;-------------------------------- Logical expressions -------------------
(defrewrite :logical-exp [exp] ; order matters!
  (cond (s/valid? ::implication exp)    (rewrite exp :implication)
        (s/valid? ::negation exp)       (rewrite exp :negation)
        (s/valid? ::disjunct exp)       (rewrite exp :disjunct)
        (s/valid? ::universal exp)      (rewrite exp :universal)
        (s/valid? ::assignment exp)     (rewrite exp :assignment)
        (s/valid? ::eval exp)           (rewrite exp :eval)
        (s/valid? ::call exp)           (rewrite exp :call)
        (s/valid? ::enforce exp)        (rewrite exp :enforce)
        (s/valid? ::setof exp)          (rewrite exp :setof)
        (s/valid? ::atom exp)           (rewrite exp :atom) ; I think other kinds of atoms are handled by :task-list-elem
        (s/valid? ::conjunct exp)       (rewrite exp :conjunct)
        :else (throw (ex-info "Unknown logical exp:" {:exp exp}))))

(defrewrite :implication [exp]
  (let [[_ l1 l2] exp]
       {:imply/condition (rewrite l1 :logical-exp)
        :imply/implies (rewrite l2 :logical-exp)}))

(defrewrite :negation [exp]
  (-> (rewrite (nth exp 1) :logical-exp)
      (assoc :exp/negated? true)))

;;; ToDo: Make the next two ordered
(defrewrite :conjunct [exp]
  (let [res (mapv #(rewrite % :logical-exp) exp)]
    (if (empty? res)
      {:conjunction/shop-empty-list? true}
      {:conjunction/terms res})))

(defrewrite :disjunct [exp]
  (-> {:disjunction/terms (mapv #(rewrite % :logical-exp) (rest exp))}))

;;;    (forall (?c) ((dest ?a ?c)) ((same ?c ?c1)))
(defrewrite :universal [exp]
  (let [[_ vars conds consq] exp]
    (-> {:forall/vars (vec vars)}
        (assoc :forall/conditions   (mapv #(rewrite % :atom) conds))
        (assoc :forall/consequences (mapv #(rewrite % :atom) consq)))))

(defrewrite :assignment [exp]
  (let [[_ v e] exp]
    {:assign/var v
     :assign/exp (rewrite e :s-exp-extended)}))

(defrewrite :eval [exp]
  {:eval/exp (rewrite exp :s-exp-extended)})

(defrewrite :atom [exp]
  (let [[pred & terms] exp]
    (cond-> {}
      true               (assoc :atom/predicate pred)
      (not-empty terms)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v terms]
                                                   {:role/term (rewrite v :term) :role/pos (swap! cnt inc)}))
                                                vec)))))

(defrewrite :task-atom [exp]
  (reset! diag {:task-atom exp})
  (let [[pred & terms] exp]
    (cond-> {}
      true               (assoc :atom/predicate pred)
      (not-empty terms)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v terms]
                                                   {:role/term (rewrite v :task-term) :role/pos (swap! cnt inc)}))
                                                vec)))))

(defrewrite :term [exp]
  (cond (symbol? exp) exp
        (number? exp) exp
        (seq? exp)    (rewrite exp :list-term)
        :else         (throw (ex-info "Not a valid term:" {:exp exp}))))

(defrewrite :task-term [exp]
  (cond (s/valid? ::term exp)      (rewrite exp :term)
        (s/valid? ::call-term exp) (rewrite exp :call-term)
        (s/valid? ::eval-term exp) (rewrite exp :eval-term)
        :else (throw (ex-info "Not a task-term: " {:exp exp}))))

(defrewrite :call-term [exp]
  (let [[_ fn-sym & args] exp]
    {:call/fn fn-sym
     :call/args (let [cnt (atom 0)]
                  (-> (for [a args]
                        (-> (rewrite a :s-exp-extended)
                            (assoc :arg/pos (swap! cnt inc))))
                      vec))}))

(defrewrite :call-term [exp]
  {:eval/form (rewrite exp :s-exp-extended)})

(defrewrite :list-term [exp]
  {:list/terms (let [cnt (atom 0)]
                  (-> (for [a exp]
                        (-> {:list/val (rewrite a :term)}
                            (assoc :list/pos (swap! cnt inc))))
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
   This is combined with the project-oriented schema elements to define the schema for the system (as opposed to schema for a project)."
  {;; ---------------------- atom
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

   ;; ---------------------- logical expression
   :exp/negated?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "expression is negated."}

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
   :method/tasks
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
