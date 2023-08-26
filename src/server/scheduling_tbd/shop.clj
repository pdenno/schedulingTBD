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
(def ^:dynamic *debugging?* false)
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
                                                    (conj res (cond-> {:axiom/case-name (nth exps 0)}
                                                                true                     (assoc :axiom/case-pos pos)
                                                                (not-empty (nth exps 1)) (assoc :axiom/terms (rewrite (nth exps 1) :logical-exp)))))
                        :else                      (recur
                                                    (subvec exps 1)
                                                    (inc pos)
                                                    (conj res (cond-> {:axiom/case-pos pos}
                                                                (not-empty (nth exps 0)) (assoc :axiom/terms (rewrite (nth exps 0) :logical-exp)))))))}))

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
                               (assoc :atom/role-pos (swap! cnt inc)))
                           (-> (:arg/pos (swap! cnt inc)) ; Assumes it is a list of atoms.
                               (assoc :op-call/args (-> (let [ncnt (atom 0)]
                                                          (for [narg arg]
                                                            (-> (rewrite narg :atom)
                                                                (:atom/role-pos (swap! ncnt inc)))))
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
          (assoc :sort-by/exp (rewrite lexp :logical-exp))))
    (let [[_ var lexp] exp]
      (-> {:sort-by/var var}
          (assoc :sort-by/fn {:s-exp/fn-ref '<})
          (assoc :sort-by/exp (rewrite lexp :logical-exp))))))

(defn box [v]
  (cond (number? v)  {:box/num v}
        (string? v)  {:box/str v}
        (symbol? v)  {:box/sym v}
        :else        v))

;;; This currently does not handle quoted things.
(defrewrite :s-exp-extended [exp]
  (letfn [(seq2sexp [s]
            (cond (seq? s) (cond-> {:s-exp/fn-ref (first s)}
                             (-> s rest not-empty) (assoc :s-exp/args (let [cnt (atom 0)]
                                                                        (-> (for [a (rest s)]
                                                                              (if (seq? a)
                                                                                (-> (seq2sexp a) (assoc :s-exp/arg-pos (swap! cnt inc)))
                                                                                {:s-exp/arg-val (box a) :s-exp/arg-pos (swap! cnt inc)}))
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
        :imply/consequence (rewrite l2 :logical-exp)}))

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
  {:eval/form (rewrite exp :s-exp-extended)})

(defrewrite :atom [exp]
  (let [[pred & terms] exp]
    (cond-> {}
      true               (assoc :atom/predicate pred)
      (not-empty terms)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v terms]
                                                   {:atom/role (box (rewrite v :term)) :atom/role-pos (swap! cnt inc)}))
                                                vec)))))

(defrewrite :task-atom [exp]
  (reset! diag {:task-atom exp})
  (let [[pred & terms] exp]
    (cond-> {}
      true               (assoc :atom/predicate pred)
      (not-empty terms)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v terms]
                                                   {:atom/role (box (rewrite v :task-term)) :atom/role-pos (swap! cnt inc)}))
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
                            (assoc :s-exp/arg-pos (swap! cnt inc))))
                      vec))}))

(defrewrite :call-term [exp]
  {:eval/form (rewrite exp :s-exp-extended)})

(defrewrite :list-term [exp]
  {:list/terms (let [cnt (atom 0)]
                  (-> (for [a exp]
                        (-> {:list/val (box (rewrite a :term))}
                            (assoc :list/val-pos (swap! cnt inc))))
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
   :atom/role
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "a string, typically a ?var, naming the proposition inplied by a atom."}
   :atom/roles ; ToDo. Don't need this.
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "a string, typically a ?var, naming the proposition inplied by a atom."}
   :atom/role-pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "a number used for ordering roles in a predicate."}

   ;; ---------------------- axiom
   :axiom/case-name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol
        :doc "a unique name for the axiom; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}
   :axiom/case-pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "an ordinal number indicating the position of this RHS."}
   :axiom/head
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the LHS atom of the axiom."}
   :axiom/rhs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the RHS of this case of the axiom"}
   :axiom/terms
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "terms of the RHS of the axiom."}

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

   ;; ----------------------- conjunction
   :conjunction/terms
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the terms that are part of a conjunctive expression."} ; ToDo: need a :term/pos or something like that.

   ;; ----------------------- domain
   :domain/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol, :unique :db.unique/identity,
           :doc "a unique name for the domain."}
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
        :doc "the position of the value in the :list structure."}
   :list/val-pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "the position of the value in the :list structure."}

   ;; ---------------------- method
   :method/head
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
           :doc "a atom indicating task to be completed."}
   :method/case-name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol, :unique :db.unique/identity,
        :doc "a unique name for the method case; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}
   :method/name ; ToDo: Not yet used.
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
           :doc "a unique name for the method; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}
   :method/preconditions
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "atoms indicating what must be true in order to apply the method."}
   :method/rhs-pairs
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "Pairs of conditions and action of a method."}
   :method/tasks
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "atoms describing a means to achieve the head literal."}

   ;; ---------------------- operation
   :op/a-list
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the propositions to add when executing this operation."}
   :op/cost
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the cost of executing this operation."}
   :op/d-list
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the propositions to delete when executing this operation."}
   :op/head
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the head atom of an operation."}
   :op/pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc "a position of this operator in the d-list or a-list."}
   :op/name
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity,
        :doc "a unique name for the operation; this isn't part of the SHOP serialization, but rather used for UI manipulation of the object."}
   :op/preconds
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "preconditions of the operation"}

   ;; ---------------------- preconditions
   :precond/exp
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref
        :doc "the expression of a precondition (for a method or operator)."}
   :precond/pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long
        :doc "the position of this precondition in an ordering of preconditions."}

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
   :s-exp/arg-pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "the arguments of the s-expression."}
   :s-exp/arg-val
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/ref,
        :doc "the arguments of the s-expression."}
   :s-exp/args
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "the arguments of the s-expression."}
   :s-exp/fn-ref
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol,
        :doc "the operator of the s-expression."}

   ;; ---------------------- task
   :task/pos
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "indicates the position of the task atom in its task list."}
   :task/immediate?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "indicates the position of the task atom in its task list."}
   :task/atom
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/long,
        :doc "indicates the position of the task atom in its task list."}

   ;; -------------------- task-list
   :task-list/elems
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref,
        :doc "when true, indicates that the atoms in task-list can be performed in any order. Ordered is the default."}
   :task-list/unordered?
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/boolean,
        :doc "when true, indicates that the atoms in task-list can be performed in any order. Ordered is the default."}})

(def small
    '#:domain{:name ZENOTRAVEL,
         :elems
         [#:axiom{:head
                  #:atom{:predicate same, :roles [#:atom{:role #:box{:sym ?x}, :role-pos 1} #:atom{:role #:box{:sym ?x}, :role-pos 2}]},
                  :rhs [#:axiom{:case-pos 1}]}
          #:axiom{:head
                  #:atom{:predicate different,
                         :roles [#:atom{:role #:box{:sym ?x}, :role-pos 1} #:atom{:role #:box{:sym ?y}, :role-pos 2}]},
                  :rhs
                  [#:axiom{:case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [{:atom/predicate same,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?x}, :role-pos 1} #:atom{:role #:box{:sym ?y}, :role-pos 2}],
                                           :exp/negated? true}]}}]}]})

(def zeno-for-db
  '#:domain{:name ZENOTRAVEL,
         :elems
         [#:axiom{:head
                  #:atom{:predicate same, :roles [#:atom{:role #:box{:sym ?x}, :role-pos 1} #:atom{:role #:box{:sym ?x}, :role-pos 2}]},
                  :rhs [#:axiom{:case-pos 1}]}
          #:axiom{:head
                  #:atom{:predicate different,
                         :roles [#:atom{:role #:box{:sym ?x}, :role-pos 1} #:atom{:role #:box{:sym ?y}, :role-pos 2}]},
                  :rhs
                  [#:axiom{:case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [{:atom/predicate same,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?x}, :role-pos 1} #:atom{:role #:box{:sym ?y}, :role-pos 2}],
                                           :exp/negated? true}]}}]}
          #:axiom{:head #:atom{:predicate possible-person-in, :roles [#:atom{:role #:box{:sym ?city}, :role-pos 1}]},
                  :rhs
                  [#:axiom{:case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate person, :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1}]}
                                          #:atom{:predicate at,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?city}, :role-pos 2}]}
                                          #:atom{:predicate goal,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?city2}, :role-pos 2}]}
                                          #:atom{:predicate different,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?city2}, :role-pos 1}
                                                  #:atom{:role #:box{:sym ?city}, :role-pos 2}]}]}}]}
          #:op{:head #:atom{:predicate !!cost, :roles [#:atom{:role #:box{:sym ?end}, :role-pos 1}]},
               :preconds
               [#:precond{:exp #:atom{:predicate maxtime, :roles [#:atom{:role #:box{:sym ?max}, :role-pos 1}]}, :pos 1}
                #:precond{:exp
                          #:assign{:var ?newmax,
                                   :exp
                                   #:s-exp{:fn-ref eval,
                                           :args
                                           [#:s-exp{:fn-ref if,
                                                    :args
                                                    [#:s-exp{:fn-ref <,
                                                             :args
                                                             [#:s-exp{:arg-val #:box{:sym ?max}, :arg-pos 1}
                                                              #:s-exp{:arg-val #:box{:sym ?end}, :arg-pos 2}],
                                                             :arg-pos 1}
                                                     #:s-exp{:arg-val #:box{:sym ?end}, :arg-pos 2}
                                                     #:s-exp{:arg-val #:box{:sym ?max}, :arg-pos 3}],
                                                    :arg-pos 1}]}},
                          :pos 2}],
               :d-list [{:atom/predicate maxtime, :atom/roles [#:atom{:role #:box{:sym ?max}, :role-pos 1}], :op/pos 1}],
               :a-list [{:atom/predicate maxtime, :atom/roles [#:atom{:role #:box{:sym ?newmax}, :role-pos 1}], :op/pos 1}],
               :cost
               #:s-exp{:fn-ref -,
                       :args [#:s-exp{:arg-val #:box{:sym ?newmax}, :arg-pos 1} #:s-exp{:arg-val #:box{:sym ?max}, :arg-pos 2}]}}
          #:method{:head
                   #:atom{:predicate board,
                          :roles
                          [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                           #:atom{:role #:box{:sym ?a}, :role-pos 2}
                           #:atom{:role #:box{:sym ?c}, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?start}, :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !board,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?a}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c}, :role-pos 3}
                                            #:atom{:role #:box{:sym ?start}, :role-pos 4}
                                            #:atom{:role #:box{:num 1}, :role-pos 5}],
                                           :task/pos 1}
                                          {:atom/predicate !!cost,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:eval{:form
                                                          #:s-exp{:fn-ref call,
                                                                  :args
                                                                  [#:s-exp{:arg-val #:box{:sym +}, :arg-pos 1}
                                                                   #:s-exp{:arg-val #:box{:sym ?start}, :arg-pos 2}
                                                                   #:s-exp{:arg-val #:box{:num 1}, :arg-pos 3}]}},
                                                   :role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 2}]}}]}
          #:op{:head
               #:atom{:predicate !board,
                      :roles
                      [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                       #:atom{:role #:box{:sym ?a}, :role-pos 2}
                       #:atom{:role #:box{:sym ?c}, :role-pos 3}
                       #:atom{:role #:box{:sym ?start}, :role-pos 4}
                       #:atom{:role #:box{:sym ?duration}, :role-pos 5}]},
               :preconds
               [#:precond{:exp #:atom{:predicate person, :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role #:box{:sym ?c}, :role-pos 1}]}, :pos 3}
                #:precond{:exp
                          #:atom{:predicate at,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]},
                          :pos 4}
                #:precond{:exp
                          #:atom{:predicate at,
                                 :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]},
                          :pos 5}
                #:precond{:exp
                          #:atom{:predicate onboard,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?num}, :role-pos 2}]},
                          :pos 6}
                #:precond{:exp
                          #:atom{:predicate read-time,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?pmax}, :role-pos 2}]},
                          :pos 7}
                #:precond{:exp
                          #:assign{:var ?new-num,
                                   :exp
                                   #:s-exp{:fn-ref +,
                                           :args
                                           [#:s-exp{:arg-val #:box{:sym ?num}, :arg-pos 1} #:s-exp{:arg-val #:box{:num 1}, :arg-pos 2}]}},
                          :pos 8}
                #:precond{:exp
                          #:assign{:var ?newpmax,
                                   :exp
                                   #:s-exp{:fn-ref max,
                                           :args
                                           [#:s-exp{:arg-val #:box{:sym ?pmax}, :arg-pos 1}
                                            #:s-exp{:fn-ref +,
                                                    :args
                                                    [#:s-exp{:arg-val #:box{:sym ?start}, :arg-pos 1}
                                                     #:s-exp{:arg-val #:box{:sym ?duration}, :arg-pos 2}
                                                     #:s-exp{:arg-val #:box{:num 0.01}, :arg-pos 3}],
                                                    :arg-pos 2}]}},
                          :pos 9}],
               :d-list
               [{:atom/predicate onboard,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?num}, :role-pos 2}],
                 :op/pos 1}
                {:atom/predicate read-time,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?pmax}, :role-pos 2}],
                 :op/pos 2}
                {:atom/predicate at,
                 :atom/roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}],
                 :op/pos 3}
                {:atom/predicate dest,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}],
                 :op/pos 4}],
               :a-list
               [{:atom/predicate onboard,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?new-num}, :role-pos 2}],
                 :op/pos 1}
                {:atom/predicate read-time,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?newpmax}, :role-pos 2}],
                 :op/pos 2}
                {:atom/predicate in,
                 :atom/roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?a}, :role-pos 2}],
                 :op/pos 3}],
               :cost #:box{:num 0.001}}
          #:method{:head
                   #:atom{:predicate debark,
                          :roles
                          [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                           #:atom{:role #:box{:sym ?a}, :role-pos 2}
                           #:atom{:role #:box{:sym ?c}, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?start}, :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !debark,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?a}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c}, :role-pos 3}
                                            #:atom{:role #:box{:sym ?start}, :role-pos 4}
                                            #:atom{:role #:box{:num 1}, :role-pos 5}],
                                           :task/pos 1}
                                          {:atom/predicate !!cost,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:eval{:form
                                                          #:s-exp{:fn-ref call,
                                                                  :args
                                                                  [#:s-exp{:arg-val #:box{:sym +}, :arg-pos 1}
                                                                   #:s-exp{:arg-val #:box{:sym ?start}, :arg-pos 2}
                                                                   #:s-exp{:arg-val #:box{:num 1}, :arg-pos 3}]}},
                                                   :role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 2}]}}]}
          #:op{:head
               #:atom{:predicate !debark,
                      :roles
                      [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                       #:atom{:role #:box{:sym ?a}, :role-pos 2}
                       #:atom{:role #:box{:sym ?c}, :role-pos 3}
                       #:atom{:role #:box{:sym ?start}, :role-pos 4}
                       #:atom{:role #:box{:sym ?duration}, :role-pos 5}]},
               :preconds
               [#:precond{:exp #:atom{:predicate person, :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role #:box{:sym ?c}, :role-pos 1}]}, :pos 3}
                #:precond{:exp
                          #:atom{:predicate at,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]},
                          :pos 4}
                #:precond{:exp
                          #:atom{:predicate in,
                                 :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?a}, :role-pos 2}]},
                          :pos 5}
                #:precond{:exp
                          #:atom{:predicate onboard,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?num}, :role-pos 2}]},
                          :pos 6}
                #:precond{:exp
                          #:atom{:predicate read-time,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?pmax}, :role-pos 2}]},
                          :pos 7}
                #:precond{:exp
                          #:assign{:var ?new-num,
                                   :exp
                                   #:s-exp{:fn-ref -,
                                           :args
                                           [#:s-exp{:arg-val #:box{:sym ?num}, :arg-pos 1} #:s-exp{:arg-val #:box{:num 1}, :arg-pos 2}]}},
                          :pos 8}
                #:precond{:exp
                          #:assign{:var ?newpmax,
                                   :exp
                                   #:s-exp{:fn-ref max,
                                           :args
                                           [#:s-exp{:arg-val #:box{:sym ?pmax}, :arg-pos 1}
                                            #:s-exp{:fn-ref +,
                                                    :args
                                                    [#:s-exp{:arg-val #:box{:sym ?start}, :arg-pos 1}
                                                     #:s-exp{:arg-val #:box{:sym ?duration}, :arg-pos 2}
                                                     #:s-exp{:arg-val #:box{:num 0.01}, :arg-pos 3}],
                                                    :arg-pos 2}]}},
                          :pos 9}],
               :d-list
               [{:atom/predicate onboard,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?num}, :role-pos 2}],
                 :op/pos 1}
                {:atom/predicate read-time,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?pmax}, :role-pos 2}],
                 :op/pos 2}
                {:atom/predicate in,
                 :atom/roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?a}, :role-pos 2}],
                 :op/pos 3}
                {:atom/predicate dest,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}],
                 :op/pos 4}],
               :a-list
               [{:atom/predicate onboard,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?new-num}, :role-pos 2}],
                 :op/pos 1}
                {:atom/predicate read-time,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?newpmax}, :role-pos 2}],
                 :op/pos 2}
                {:atom/predicate at,
                 :atom/roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}],
                 :op/pos 3}],
               :cost #:box{:num 0.001}}
          #:method{:head
                   #:atom{:predicate refuel, :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?start}, :role-pos 2}]}
                              #:atom{:predicate read-time,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?pmax}, :role-pos 2}]}
                              #:atom{:predicate capacity,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?cap}, :role-pos 2}]}
                              #:atom{:predicate fuel,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym >}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?cap}, :val-pos 2}
                                                     #:list{:val #:box{:sym ?fuel}, :val-pos 3}]},
                                             :role-pos 1}]}
                              #:atom{:predicate assign,
                                     :roles [#:atom{:role #:box{:sym ?duration}, :role-pos 1} #:atom{:role #:box{:num 1}, :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role #:box{:sym ?end}, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym +}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?start}, :val-pos 2}
                                                     #:list{:val #:box{:sym ?duration}, :val-pos 3}
                                                     #:list{:val #:box{:num 0.01}, :val-pos 4}]},
                                             :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role #:box{:sym ?newpmax}, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym max}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?pmax}, :val-pos 2}
                                                     #:list{:val #:box{:sym ?end}, :val-pos 3}]},
                                             :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!ra,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym read-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?pmax}, :val-pos 3}]},
                                                                  :val-pos 1}]},
                                                   :role-pos 1}
                                            #:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym read-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?newpmax}, :val-pos 3}]},
                                                                  :val-pos 1}]},
                                                   :role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate !refuel,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?c}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?start}, :role-pos 3}
                                            #:atom{:role #:box{:sym ?duration}, :role-pos 4}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!cost,
                                           :atom/roles [#:atom{:role #:box{:sym ?end}, :role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 3}]}}]}
          #:op{:head
               #:atom{:predicate !refuel,
                      :roles
                      [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                       #:atom{:role #:box{:sym ?c}, :role-pos 2}
                       #:atom{:role #:box{:sym ?start}, :role-pos 3}
                       #:atom{:role #:box{:sym ?duration}, :role-pos 4}]},
               :preconds
               [#:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role #:box{:sym ?c}, :role-pos 1}]}, :pos 2}
                #:precond{:exp
                          #:atom{:predicate at,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]},
                          :pos 3}
                #:precond{:exp
                          #:atom{:predicate fuel,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]},
                          :pos 4}
                #:precond{:exp
                          #:atom{:predicate capacity,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?cap}, :role-pos 2}]},
                          :pos 5}],
               :d-list
               [{:atom/predicate fuel,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}],
                 :op/pos 1}],
               :a-list
               [{:atom/predicate fuel,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?cap}, :role-pos 2}],
                 :op/pos 1}],
               :cost #:box{:num 0.001}}
          #:method{:head
                   #:atom{:predicate zoom,
                          :roles
                          [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                           #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                           #:atom{:role #:box{:sym ?c2}, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?astart}, :role-pos 2}]}
                              #:atom{:predicate read-time,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?pmax}, :role-pos 2}]}
                              #:atom{:predicate distance,
                                     :roles
                                     [#:atom{:role #:box{:sym ?c1}, :role-pos 1}
                                      #:atom{:role #:box{:sym ?c2}, :role-pos 2}
                                      #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                              #:atom{:predicate fuel,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]}
                              #:atom{:predicate fast-burn,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym >=}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?fuel}, :val-pos 2}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val #:box{:sym *}, :val-pos 1}
                                                                    #:list{:val #:box{:sym ?dist}, :val-pos 2}
                                                                    #:list{:val #:box{:sym ?burn}, :val-pos 3}]},
                                                            :val-pos 3}]},
                                             :role-pos 1}]}
                              #:atom{:predicate assign,
                                     :roles [#:atom{:role #:box{:sym ?duration}, :role-pos 1} #:atom{:role #:box{:num 1}, :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role #:box{:sym ?start}, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym max}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?pmax}, :val-pos 2}
                                                     #:list{:val #:box{:sym ?astart}, :val-pos 3}]},
                                             :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role #:box{:sym ?end}, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym +}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?start}, :val-pos 2}
                                                     #:list{:val #:box{:sym ?duration}, :val-pos 3}
                                                     #:list{:val #:box{:num 0.01}, :val-pos 4}]},
                                             :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!ra,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym write-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?astart}, :val-pos 3}]},
                                                                  :val-pos 1}
                                                           #:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym read-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?pmax}, :val-pos 3}]},
                                                                  :val-pos 2}]},
                                                   :role-pos 1}
                                            #:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym read-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:num 0}, :val-pos 3}]},
                                                                  :val-pos 1}
                                                           #:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym write-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?end}, :val-pos 3}]},
                                                                  :val-pos 2}]},
                                                   :role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate !zoom,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c2}, :role-pos 3}
                                            #:atom{:role #:box{:sym ?start}, :role-pos 4}
                                            #:atom{:role #:box{:sym ?duration}, :role-pos 5}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!cost,
                                           :atom/roles [#:atom{:role #:box{:sym ?end}, :role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 3}]}}]}
          #:op{:head
               #:atom{:predicate !zoom,
                      :roles
                      [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                       #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                       #:atom{:role #:box{:sym ?c2}, :role-pos 3}
                       #:atom{:role #:box{:sym ?start}, :role-pos 4}
                       #:atom{:role #:box{:sym ?duration}, :role-pos 5}]},
               :preconds
               [#:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role #:box{:sym ?c1}, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role #:box{:sym ?c2}, :role-pos 1}]}, :pos 3}
                #:precond{:exp
                          #:atom{:predicate onboard,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?num}, :role-pos 2}]},
                          :pos 4}
                #:precond{:exp
                          #:atom{:predicate zoom-limit,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?limit}, :role-pos 2}]},
                          :pos 5}
                #:precond{:exp
                          #:eval{:form
                                 #:s-exp{:fn-ref eval,
                                         :args
                                         [#:s-exp{:fn-ref <=,
                                                  :args
                                                  [#:s-exp{:arg-val #:box{:sym ?num}, :arg-pos 1}
                                                   #:s-exp{:arg-val #:box{:sym ?limit}, :arg-pos 2}],
                                                  :arg-pos 1}]}},
                          :pos 6}
                #:precond{:exp
                          #:atom{:predicate at,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c1}, :role-pos 2}]},
                          :pos 7}
                #:precond{:exp
                          #:atom{:predicate distance,
                                 :roles
                                 [#:atom{:role #:box{:sym ?c1}, :role-pos 1}
                                  #:atom{:role #:box{:sym ?c2}, :role-pos 2}
                                  #:atom{:role #:box{:sym ?dist}, :role-pos 3}]},
                          :pos 8}
                #:precond{:exp
                          #:atom{:predicate fast-burn,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]},
                          :pos 9}
                #:precond{:exp #:atom{:predicate total-fuel-used, :roles [#:atom{:role #:box{:sym ?total-fuel}, :role-pos 1}]}, :pos 10}
                #:precond{:exp
                          #:assign{:var ?new-total,
                                   :exp
                                   #:s-exp{:fn-ref +,
                                           :args
                                           [#:s-exp{:arg-val #:box{:sym ?total-fuel}, :arg-pos 1}
                                            #:s-exp{:fn-ref *,
                                                    :args
                                                    [#:s-exp{:arg-val #:box{:sym ?dist}, :arg-pos 1}
                                                     #:s-exp{:arg-val #:box{:sym ?burn}, :arg-pos 2}],
                                                    :arg-pos 2}]}},
                          :pos 11}
                #:precond{:exp
                          #:atom{:predicate fuel,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]},
                          :pos 12}
                #:precond{:exp
                          #:assign{:var ?new-fuel,
                                   :exp
                                   #:s-exp{:fn-ref -,
                                           :args
                                           [#:s-exp{:arg-val #:box{:sym ?fuel}, :arg-pos 1}
                                            #:s-exp{:fn-ref *,
                                                    :args
                                                    [#:s-exp{:arg-val #:box{:sym ?dist}, :arg-pos 1}
                                                     #:s-exp{:arg-val #:box{:sym ?burn}, :arg-pos 2}],
                                                    :arg-pos 2}]}},
                          :pos 13}],
               :d-list
               [{:atom/predicate at,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c1}, :role-pos 2}],
                 :op/pos 1}
                {:atom/predicate total-fuel-used, :atom/roles [#:atom{:role #:box{:sym ?total-fuel}, :role-pos 1}], :op/pos 2}
                {:atom/predicate fuel,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}],
                 :op/pos 3}],
               :a-list
               [{:atom/predicate at,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c2}, :role-pos 2}],
                 :op/pos 1}
                {:atom/predicate total-fuel-used, :atom/roles [#:atom{:role #:box{:sym ?new-total}, :role-pos 1}], :op/pos 2}
                {:atom/predicate fuel,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?new-fuel}, :role-pos 2}],
                 :op/pos 3}],
               :cost #:box{:num 0.001}}
          #:method{:head
                   #:atom{:predicate fly,
                          :roles
                          [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                           #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                           #:atom{:role #:box{:sym ?c2}, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?astart}, :role-pos 2}]}
                              #:atom{:predicate read-time,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?pmax}, :role-pos 2}]}
                              #:atom{:predicate distance,
                                     :roles
                                     [#:atom{:role #:box{:sym ?c1}, :role-pos 1}
                                      #:atom{:role #:box{:sym ?c2}, :role-pos 2}
                                      #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                              #:atom{:predicate fuel,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]}
                              #:atom{:predicate slow-burn,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym >=}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?fuel}, :val-pos 2}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val #:box{:sym *}, :val-pos 1}
                                                                    #:list{:val #:box{:sym ?dist}, :val-pos 2}
                                                                    #:list{:val #:box{:sym ?burn}, :val-pos 3}]},
                                                            :val-pos 3}]},
                                             :role-pos 1}]}
                              #:atom{:predicate assign,
                                     :roles [#:atom{:role #:box{:sym ?duration}, :role-pos 1} #:atom{:role #:box{:num 1}, :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role #:box{:sym ?start}, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym max}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?pmax}, :val-pos 2}
                                                     #:list{:val #:box{:sym ?astart}, :val-pos 3}]},
                                             :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role #:box{:sym ?end}, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym +}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?start}, :val-pos 2}
                                                     #:list{:val #:box{:sym ?duration}, :val-pos 3}
                                                     #:list{:val #:box{:num 0.01}, :val-pos 4}]},
                                             :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!ra,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym write-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?astart}, :val-pos 3}]},
                                                                  :val-pos 1}
                                                           #:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym read-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?pmax}, :val-pos 3}]},
                                                                  :val-pos 2}]},
                                                   :role-pos 1}
                                            #:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym read-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:num 0}, :val-pos 3}]},
                                                                  :val-pos 1}
                                                           #:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym write-time}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?end}, :val-pos 3}]},
                                                                  :val-pos 2}]},
                                                   :role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate !fly,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c2}, :role-pos 3}
                                            #:atom{:role #:box{:sym ?start}, :role-pos 4}
                                            #:atom{:role #:box{:sym ?duration}, :role-pos 5}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!cost,
                                           :atom/roles [#:atom{:role #:box{:sym ?end}, :role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 3}]}}]}
          #:op{:head
               #:atom{:predicate !fly,
                      :roles
                      [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                       #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                       #:atom{:role #:box{:sym ?c2}, :role-pos 3}
                       #:atom{:role #:box{:sym ?start}, :role-pos 4}
                       #:atom{:role #:box{:sym ?duration}, :role-pos 5}]},
               :preconds
               [#:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role #:box{:sym ?c1}, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role #:box{:sym ?c2}, :role-pos 1}]}, :pos 3}
                #:precond{:exp
                          #:atom{:predicate at,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c1}, :role-pos 2}]},
                          :pos 4}
                #:precond{:exp
                          #:atom{:predicate distance,
                                 :roles
                                 [#:atom{:role #:box{:sym ?c1}, :role-pos 1}
                                  #:atom{:role #:box{:sym ?c2}, :role-pos 2}
                                  #:atom{:role #:box{:sym ?dist}, :role-pos 3}]},
                          :pos 5}
                #:precond{:exp
                          #:atom{:predicate slow-burn,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]},
                          :pos 6}
                #:precond{:exp #:atom{:predicate total-fuel-used, :roles [#:atom{:role #:box{:sym ?total-fuel}, :role-pos 1}]}, :pos 7}
                #:precond{:exp
                          #:assign{:var ?new-total,
                                   :exp
                                   #:s-exp{:fn-ref +,
                                           :args
                                           [#:s-exp{:arg-val #:box{:sym ?total-fuel}, :arg-pos 1}
                                            #:s-exp{:fn-ref *,
                                                    :args
                                                    [#:s-exp{:arg-val #:box{:sym ?dist}, :arg-pos 1}
                                                     #:s-exp{:arg-val #:box{:sym ?burn}, :arg-pos 2}],
                                                    :arg-pos 2}]}},
                          :pos 8}
                #:precond{:exp
                          #:atom{:predicate fuel,
                                 :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]},
                          :pos 9}
                #:precond{:exp
                          #:assign{:var ?new-fuel,
                                   :exp
                                   #:s-exp{:fn-ref -,
                                           :args
                                           [#:s-exp{:arg-val #:box{:sym ?fuel}, :arg-pos 1}
                                            #:s-exp{:fn-ref *,
                                                    :args
                                                    [#:s-exp{:arg-val #:box{:sym ?dist}, :arg-pos 1}
                                                     #:s-exp{:arg-val #:box{:sym ?burn}, :arg-pos 2}],
                                                    :arg-pos 2}]}},
                          :pos 10}],
               :d-list
               [{:atom/predicate at,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c1}, :role-pos 2}],
                 :op/pos 1}
                {:atom/predicate total-fuel-used, :atom/roles [#:atom{:role #:box{:sym ?total-fuel}, :role-pos 1}], :op/pos 2}
                {:atom/predicate fuel,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}],
                 :op/pos 3}],
               :a-list
               [{:atom/predicate at,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c2}, :role-pos 2}],
                 :op/pos 1}
                {:atom/predicate total-fuel-used, :atom/roles [#:atom{:role #:box{:sym ?new-total}, :role-pos 1}], :op/pos 2}
                {:atom/predicate fuel,
                 :atom/roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?new-fuel}, :role-pos 2}],
                 :op/pos 3}],
               :cost #:box{:num 0.001}}
          #:op{:head #:atom{:predicate !!preprocessing, :roles [#:atom{:role #:box{:sym ?problem-name}, :role-pos 1}]},
               :preconds
               [#:precond{:exp #:atom{:predicate totaltime-coeff, :roles [#:atom{:role #:box{:sym ?tc}, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate fuelused-coeff, :roles [#:atom{:role #:box{:sym ?fc}, :role-pos 1}]}, :pos 2}
                #:precond{:exp
                          #:eval{:form
                                 #:s-exp{:fn-ref eval,
                                         :args
                                         [#:s-exp{:fn-ref setf,
                                                  :args
                                                  [#:s-exp{:arg-val #:box{:sym *tc*}, :arg-pos 1}
                                                   #:s-exp{:arg-val #:box{:sym ?tc}, :arg-pos 2}],
                                                  :arg-pos 1}]}},
                          :pos 3}
                #:precond{:exp
                          #:eval{:form
                                 #:s-exp{:fn-ref eval,
                                         :args
                                         [#:s-exp{:fn-ref setf,
                                                  :args
                                                  [#:s-exp{:arg-val #:box{:sym *fc*}, :arg-pos 1}
                                                   #:s-exp{:arg-val #:box{:sym ?fc}, :arg-pos 2}],
                                                  :arg-pos 1}]}},
                          :pos 4}],
               :cost #:box{:num 0}}
          #:op{:head #:atom{:predicate !!assert, :roles [#:atom{:role #:box{:sym ?g}, :role-pos 1}]},
               :a-list [{:box/sym ?g, :op/pos 1}],
               :cost #:box{:num 0}}
          #:op{:head #:atom{:predicate !!ra, :roles [#:atom{:role #:box{:sym ?D}, :role-pos 1} #:atom{:role #:box{:sym ?A}, :role-pos 2}]},
               :d-list [{:box/sym ?D, :op/pos 1}],
               :a-list [{:box/sym ?A, :op/pos 1}],
               :cost #:box{:num 0}}
          #:method{:head
                   #:atom{:predicate transport-person,
                          :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:case-name Case1,
                             :preconditions
                             [#:atom{:predicate at,
                                     :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]}]}]}
          #:method{:head
                   #:atom{:predicate transport-person,
                          :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?c2}, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:case-name Case2,
                             :preconditions
                             #:sort-by{:var ?num,
                                       :fn #:s-exp{:fn-ref >},
                                       :exp
                                       #:conjunction{:terms
                                                     [#:atom{:predicate at,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?c1}, :role-pos 2}]}
                                                      #:atom{:predicate at,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?c1}, :role-pos 2}]}
                                                      #:atom{:predicate aircraft, :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1}]}
                                                      #:atom{:predicate onboard,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?num}, :role-pos 2}]}]}},
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!assert,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym dest}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?c1}, :val-pos 3}]},
                                                                  :val-pos 1}]},
                                                   :role-pos 1}],
                                           :task/pos 1}
                                          {:atom/predicate board,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?a}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!assert,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym dest}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?c2}, :val-pos 3}]},
                                                                  :val-pos 1}]},
                                                   :role-pos 1}],
                                           :task/pos 3}
                                          {:atom/predicate upper-move-aircraft-no-style,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c2}, :role-pos 2}],
                                           :task/immediate? true,
                                           :task/pos 4}
                                          {:atom/predicate debark,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?a}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c2}, :role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 5}]}}]}
          #:method{:head
                   #:atom{:predicate transport-person,
                          :roles [#:atom{:role #:box{:sym ?p}, :role-pos 1} #:atom{:role #:box{:sym ?c2}, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:case-name Case3,
                             :preconditions
                             #:sort-by{:var ?cost,
                                       :fn #:s-exp{:fn-ref <},
                                       :exp
                                       #:conjunction{:terms
                                                     [#:atom{:predicate at,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?c1}, :role-pos 2}]}
                                                      #:atom{:predicate aircraft, :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1}]}
                                                      #:atom{:predicate at,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?c3}, :role-pos 2}]}
                                                      #:atom{:predicate different,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?c1}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?c3}, :role-pos 2}]}
                                                      #:forall{:vars [?c],
                                                               :conditions
                                                               [#:atom{:predicate dest,
                                                                       :roles
                                                                       [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                                                        #:atom{:role #:box{:sym ?c}, :role-pos 2}]}],
                                                               :consequences
                                                               [#:atom{:predicate same,
                                                                       :roles
                                                                       [#:atom{:role #:box{:sym ?c}, :role-pos 1}
                                                                        #:atom{:role #:box{:sym ?c1}, :role-pos 2}]}]}
                                                      #:imply{:condition
                                                              #:conjunction{:terms
                                                                            [#:atom{:predicate different,
                                                                                    :roles
                                                                                    [#:atom{:role #:box{:sym ?c3}, :role-pos 1}
                                                                                     #:atom{:role #:box{:sym ?c1}, :role-pos 2}]}]},
                                                              :consequence
                                                              {:atom/predicate possible-person-in,
                                                               :atom/roles [#:atom{:role #:box{:sym ?c3}, :role-pos 1}],
                                                               :exp/negated? true}}
                                                      #:atom{:predicate travel-cost-info,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?c3}, :role-pos 2}
                                                              #:atom{:role #:box{:sym ?c1}, :role-pos 3}
                                                              #:atom{:role #:box{:sym ?cost}, :role-pos 4}
                                                              #:atom{:role #:box{:sym ?style}, :role-pos 5}]}]}},
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!assert,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym dest}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?c1}, :val-pos 3}]},
                                                                  :val-pos 1}]},
                                                   :role-pos 1}],
                                           :task/pos 1}
                                          {:atom/predicate upper-move-aircraft,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?style}, :role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate board,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?a}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 3}
                                          {:atom/predicate !!assert,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym dest}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}
                                                                          #:list{:val #:box{:sym ?c2}, :val-pos 3}]},
                                                                  :val-pos 1}]},
                                                   :role-pos 1}],
                                           :task/pos 4}
                                          {:atom/predicate upper-move-aircraft-no-style,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c2}, :role-pos 2}],
                                           :task/immediate? true,
                                           :task/pos 5}
                                          {:atom/predicate debark,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?p}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?a}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c2}, :role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 6}]}}]}
          #:method{:head
                   #:atom{:predicate upper-move-aircraft,
                          :roles
                          [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                           #:atom{:role #:box{:sym ?c}, :role-pos 2}
                           #:atom{:role #:box{:sym ?style}, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:case-name Case1,
                             :preconditions
                             [#:atom{:predicate at,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]}]}
                    #:method{:case-name Case2,
                             :preconditions
                             [#:atom{:predicate at,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?somecity}, :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate move-aircraft,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?somecity}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c}, :role-pos 3}
                                            #:atom{:role #:box{:sym ?style}, :role-pos 4}],
                                           :task/pos 1}]}}]}
          #:method{:head
                   #:atom{:predicate upper-move-aircraft-no-style,
                          :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:case-name Case1,
                             :preconditions
                             [#:atom{:predicate at,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]}]}
                    #:method{:case-name Case2,
                             :preconditions
                             #:sort-by{:var ?cost,
                                       :fn #:s-exp{:fn-ref <},
                                       :exp
                                       #:conjunction{:terms
                                                     [#:atom{:predicate at,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?somecity}, :role-pos 2}]}
                                                      #:atom{:predicate travel-cost-info,
                                                             :roles
                                                             [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                                              #:atom{:role #:box{:sym ?somecity}, :role-pos 2}
                                                              #:atom{:role #:box{:sym ?c}, :role-pos 3}
                                                              #:atom{:role #:box{:sym ?cost}, :role-pos 4}
                                                              #:atom{:role #:box{:sym ?style}, :role-pos 5}]}]}},
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate move-aircraft,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?somecity}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c}, :role-pos 3}
                                            #:atom{:role #:box{:sym ?style}, :role-pos 4}],
                                           :task/pos 1}]}}]}
          #:axiom{:head
                  #:atom{:predicate travel-cost-info,
                         :roles
                         [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                          #:atom{:role #:box{:sym ?from}, :role-pos 2}
                          #:atom{:role #:box{:sym ?to}, :role-pos 3}
                          #:atom{:role #:box{:sym ?cost}, :role-pos 4}
                          #:atom{:role #:box{:sym slow}, :role-pos 5}]},
                  :rhs
                  [#:axiom{:case-name CASE1,
                           :case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate capacity,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?cap}, :role-pos 2}]}
                                          #:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?from}, :role-pos 1}
                                                  #:atom{:role #:box{:sym ?to}, :role-pos 2}
                                                  #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                                          #:atom{:predicate slow-burn,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                                          #:eval{:form
                                                 #:s-exp{:fn-ref eval,
                                                         :args
                                                         [#:s-exp{:fn-ref <,
                                                                  :args
                                                                  [#:s-exp{:arg-val #:box{:sym ?cap}, :arg-pos 1}
                                                                   #:s-exp{:fn-ref *,
                                                                           :args
                                                                           [#:s-exp{:arg-val #:box{:sym ?dist}, :arg-pos 1}
                                                                            #:s-exp{:arg-val #:box{:sym ?burn}, :arg-pos 2}],
                                                                           :arg-pos 2}],
                                                                  :arg-pos 1}]}}
                                          #:atom{:predicate assign,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?cost}, :role-pos 1}
                                                  #:atom{:role #:box{:sym most-positive-fixnum}, :role-pos 2}]}]}}
                   #:axiom{:case-name CASE2,
                           :case-pos 2,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?from}, :role-pos 1}
                                                  #:atom{:role #:box{:sym ?to}, :role-pos 2}
                                                  #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                                          #:atom{:predicate fuel,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]}
                                          #:atom{:predicate slow-burn,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                                          #:eval{:form
                                                 #:s-exp{:fn-ref eval,
                                                         :args
                                                         [#:s-exp{:fn-ref >=,
                                                                  :args
                                                                  [#:s-exp{:arg-val #:box{:sym ?fuel}, :arg-pos 1}
                                                                   #:s-exp{:fn-ref *,
                                                                           :args
                                                                           [#:s-exp{:arg-val #:box{:sym ?dist}, :arg-pos 1}
                                                                            #:s-exp{:arg-val #:box{:sym ?burn}, :arg-pos 2}],
                                                                           :arg-pos 2}],
                                                                  :arg-pos 1}]}}
                                          #:assign{:var ?cost,
                                                   :exp
                                                   #:s-exp{:fn-ref float,
                                                           :args
                                                           [#:s-exp{:fn-ref /,
                                                                    :args
                                                                    [#:s-exp{:fn-ref +,
                                                                             :args
                                                                             [#:s-exp{:arg-val #:box{:sym *tc*}, :arg-pos 1}
                                                                              #:s-exp{:fn-ref *,
                                                                                      :args
                                                                                      [#:s-exp{:arg-val #:box{:sym *fc*}, :arg-pos 1}
                                                                                       #:s-exp{:fn-ref *,
                                                                                               :args
                                                                                               [#:s-exp{:arg-val #:box{:sym ?dist},
                                                                                                        :arg-pos 1}
                                                                                                #:s-exp{:arg-val #:box{:sym ?burn},
                                                                                                        :arg-pos 2}],
                                                                                               :arg-pos 2}],
                                                                                      :arg-pos 2}],
                                                                             :arg-pos 1}
                                                                     #:s-exp{:arg-val #:box{:num 1}, :arg-pos 2}],
                                                                    :arg-pos 1}]}}]}}
                   #:axiom{:case-name CASE3,
                           :case-pos 3,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate capacity,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?cap}, :role-pos 2}]}
                                          #:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?from}, :role-pos 1}
                                                  #:atom{:role #:box{:sym ?to}, :role-pos 2}
                                                  #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                                          #:atom{:predicate slow-burn,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                                          #:assign{:var ?cost,
                                                   :exp
                                                   #:s-exp{:fn-ref float,
                                                           :args
                                                           [#:s-exp{:fn-ref /,
                                                                    :args
                                                                    [#:s-exp{:fn-ref +,
                                                                             :args
                                                                             [#:s-exp{:fn-ref *,
                                                                                      :args
                                                                                      [#:s-exp{:arg-val #:box{:sym *tc*}, :arg-pos 1}
                                                                                       #:s-exp{:arg-val #:box{:num 2}, :arg-pos 2}],
                                                                                      :arg-pos 1}
                                                                              #:s-exp{:fn-ref *,
                                                                                      :args
                                                                                      [#:s-exp{:arg-val #:box{:sym *fc*}, :arg-pos 1}
                                                                                       #:s-exp{:fn-ref *,
                                                                                               :args
                                                                                               [#:s-exp{:arg-val #:box{:sym ?dist},
                                                                                                        :arg-pos 1}
                                                                                                #:s-exp{:arg-val #:box{:sym ?burn},
                                                                                                        :arg-pos 2}],
                                                                                               :arg-pos 2}],
                                                                                      :arg-pos 2}],
                                                                             :arg-pos 1}
                                                                     #:s-exp{:arg-val #:box{:num 1}, :arg-pos 2}],
                                                                    :arg-pos 1}]}}]}}]}
          #:axiom{:head
                  #:atom{:predicate travel-cost-info,
                         :roles
                         [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                          #:atom{:role #:box{:sym ?from}, :role-pos 2}
                          #:atom{:role #:box{:sym ?to}, :role-pos 3}
                          #:atom{:role #:box{:sym ?cost}, :role-pos 4}
                          #:atom{:role #:box{:sym fast}, :role-pos 5}]},
                  :rhs
                  [#:axiom{:case-name CASE1,
                           :case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate capacity,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?cap}, :role-pos 2}]}
                                          #:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?from}, :role-pos 1}
                                                  #:atom{:role #:box{:sym ?to}, :role-pos 2}
                                                  #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                                          #:atom{:predicate fast-burn,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                                          #:eval{:form
                                                 #:s-exp{:fn-ref eval,
                                                         :args
                                                         [#:s-exp{:fn-ref <,
                                                                  :args
                                                                  [#:s-exp{:arg-val #:box{:sym ?cap}, :arg-pos 1}
                                                                   #:s-exp{:fn-ref *,
                                                                           :args
                                                                           [#:s-exp{:arg-val #:box{:sym ?dist}, :arg-pos 1}
                                                                            #:s-exp{:arg-val #:box{:sym ?burn}, :arg-pos 2}],
                                                                           :arg-pos 2}],
                                                                  :arg-pos 1}]}}
                                          #:atom{:predicate assign,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?cost}, :role-pos 1}
                                                  #:atom{:role #:box{:sym most-positive-fixnum}, :role-pos 2}]}]}}
                   #:axiom{:case-name CASE2,
                           :case-pos 2,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?from}, :role-pos 1}
                                                  #:atom{:role #:box{:sym ?to}, :role-pos 2}
                                                  #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                                          #:atom{:predicate fuel,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]}
                                          #:atom{:predicate zoom-limit,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?limit}, :role-pos 2}]}
                                          #:atom{:predicate onboard,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?num}, :role-pos 2}]}
                                          #:eval{:form
                                                 #:s-exp{:fn-ref eval,
                                                         :args
                                                         [#:s-exp{:fn-ref <,
                                                                  :args
                                                                  [#:s-exp{:arg-val #:box{:sym ?num}, :arg-pos 1}
                                                                   #:s-exp{:arg-val #:box{:sym ?limit}, :arg-pos 2}],
                                                                  :arg-pos 1}]}}
                                          #:atom{:predicate fast-burn,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                                          #:eval{:form
                                                 #:s-exp{:fn-ref eval,
                                                         :args
                                                         [#:s-exp{:fn-ref >=,
                                                                  :args
                                                                  [#:s-exp{:arg-val #:box{:sym ?fuel}, :arg-pos 1}
                                                                   #:s-exp{:fn-ref *,
                                                                           :args
                                                                           [#:s-exp{:arg-val #:box{:sym ?dist}, :arg-pos 1}
                                                                            #:s-exp{:arg-val #:box{:sym ?burn}, :arg-pos 2}],
                                                                           :arg-pos 2}],
                                                                  :arg-pos 1}]}}
                                          #:assign{:var ?cost,
                                                   :exp
                                                   #:s-exp{:fn-ref float,
                                                           :args
                                                           [#:s-exp{:fn-ref /,
                                                                    :args
                                                                    [#:s-exp{:fn-ref +,
                                                                             :args
                                                                             [#:s-exp{:arg-val #:box{:sym *tc*}, :arg-pos 1}
                                                                              #:s-exp{:fn-ref *,
                                                                                      :args
                                                                                      [#:s-exp{:arg-val #:box{:sym *fc*}, :arg-pos 1}
                                                                                       #:s-exp{:fn-ref *,
                                                                                               :args
                                                                                               [#:s-exp{:arg-val #:box{:sym ?dist},
                                                                                                        :arg-pos 1}
                                                                                                #:s-exp{:arg-val #:box{:sym ?burn},
                                                                                                        :arg-pos 2}],
                                                                                               :arg-pos 2}],
                                                                                      :arg-pos 2}],
                                                                             :arg-pos 1}
                                                                     #:s-exp{:arg-val #:box{:num 1}, :arg-pos 2}],
                                                                    :arg-pos 1}]}}]}}
                   #:axiom{:case-name CASE3,
                           :case-pos 3,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate capacity,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?cap}, :role-pos 2}]}
                                          #:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?from}, :role-pos 1}
                                                  #:atom{:role #:box{:sym ?to}, :role-pos 2}
                                                  #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                                          #:atom{:predicate fast-burn,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                                          #:atom{:predicate zoom-limit,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?limit}, :role-pos 2}]}
                                          #:atom{:predicate onboard,
                                                 :roles
                                                 [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?num}, :role-pos 2}]}
                                          #:eval{:form
                                                 #:s-exp{:fn-ref eval,
                                                         :args
                                                         [#:s-exp{:fn-ref <,
                                                                  :args
                                                                  [#:s-exp{:arg-val #:box{:sym ?num}, :arg-pos 1}
                                                                   #:s-exp{:arg-val #:box{:sym ?limit}, :arg-pos 2}],
                                                                  :arg-pos 1}]}}
                                          #:assign{:var ?cost,
                                                   :exp
                                                   #:s-exp{:fn-ref float,
                                                           :args
                                                           [#:s-exp{:fn-ref /,
                                                                    :args
                                                                    [#:s-exp{:fn-ref +,
                                                                             :args
                                                                             [#:s-exp{:fn-ref *,
                                                                                      :args
                                                                                      [#:s-exp{:arg-val #:box{:sym *tc*}, :arg-pos 1}
                                                                                       #:s-exp{:arg-val #:box{:num 2}, :arg-pos 2}],
                                                                                      :arg-pos 1}
                                                                              #:s-exp{:fn-ref *,
                                                                                      :args
                                                                                      [#:s-exp{:arg-val #:box{:sym *fc*}, :arg-pos 1}
                                                                                       #:s-exp{:fn-ref *,
                                                                                               :args
                                                                                               [#:s-exp{:arg-val #:box{:sym ?dist},
                                                                                                        :arg-pos 1}
                                                                                                #:s-exp{:arg-val #:box{:sym ?burn},
                                                                                                        :arg-pos 2}],
                                                                                               :arg-pos 2}],
                                                                                      :arg-pos 2}],
                                                                             :arg-pos 1}
                                                                     #:s-exp{:arg-val #:box{:num 1}, :arg-pos 2}],
                                                                    :arg-pos 1}]}}]}}]}
          #:method{:head
                   #:atom{:predicate move-aircraft,
                          :roles
                          [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                           #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                           #:atom{:role #:box{:sym ?c2}, :role-pos 3}
                           #:atom{:role #:box{:sym slow}, :role-pos 4}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate fuel,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]}
                              #:atom{:predicate distance,
                                     :roles
                                     [#:atom{:role #:box{:sym ?c1}, :role-pos 1}
                                      #:atom{:role #:box{:sym ?c2}, :role-pos 2}
                                      #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                              #:atom{:predicate slow-burn,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym >}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?fuel}, :val-pos 2}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val #:box{:sym *}, :val-pos 1}
                                                                    #:list{:val #:box{:sym ?dist}, :val-pos 2}
                                                                    #:list{:val #:box{:sym ?burn}, :val-pos 3}]},
                                                            :val-pos 3}]},
                                             :role-pos 1}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate fly,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c2}, :role-pos 3}],
                                           :task/pos 1}]}}
                    #:method{:tasks
                             #:task-list{:elems
                                         [{:atom/predicate refuel,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c1}, :role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate fly,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c2}, :role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 2}]}}]}
          #:method{:head
                   #:atom{:predicate move-aircraft,
                          :roles
                          [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                           #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                           #:atom{:role #:box{:sym ?c2}, :role-pos 3}
                           #:atom{:role #:box{:sym fast}, :role-pos 4}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate fuel,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?fuel}, :role-pos 2}]}
                              #:atom{:predicate distance,
                                     :roles
                                     [#:atom{:role #:box{:sym ?c1}, :role-pos 1}
                                      #:atom{:role #:box{:sym ?c2}, :role-pos 2}
                                      #:atom{:role #:box{:sym ?dist}, :role-pos 3}]}
                              #:atom{:predicate fast-burn,
                                     :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?burn}, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym >}, :val-pos 1}
                                                     #:list{:val #:box{:sym ?fuel}, :val-pos 2}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val #:box{:sym *}, :val-pos 1}
                                                                    #:list{:val #:box{:sym ?dist}, :val-pos 2}
                                                                    #:list{:val #:box{:sym ?burn}, :val-pos 3}]},
                                                            :val-pos 3}]},
                                             :role-pos 1}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate zoom,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c2}, :role-pos 3}],
                                           :task/pos 1}]}}
                    #:method{:tasks
                             #:task-list{:elems
                                         [{:atom/predicate refuel,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c1}, :role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate zoom,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1}
                                            #:atom{:role #:box{:sym ?c1}, :role-pos 2}
                                            #:atom{:role #:box{:sym ?c2}, :role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 2}]}}]}
          #:method{:head
                   #:atom{:predicate transport-aircraft,
                          :roles [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate not,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val #:box{:sym no-use}, :val-pos 1} #:list{:val #:box{:sym ?a}, :val-pos 2}]},
                                             :role-pos 1}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!assert,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym no-use}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}]},
                                                                  :val-pos 1}]},
                                                   :role-pos 1}],
                                           :task/pos 1}
                                          {:atom/predicate upper-move-aircraft-no-style,
                                           :atom/roles
                                           [#:atom{:role #:box{:sym ?a}, :role-pos 1} #:atom{:role #:box{:sym ?c}, :role-pos 2}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!ra,
                                           :atom/roles
                                           [#:atom{:role
                                                   #:list{:terms
                                                          [#:list{:val
                                                                  #:list{:terms
                                                                         [#:list{:val #:box{:sym no-use}, :val-pos 1}
                                                                          #:list{:val #:box{:sym ?a}, :val-pos 2}]},
                                                                  :val-pos 1}]},
                                                   :role-pos 1}
                                            #:atom{:role #:list{:terms []}, :role-pos 2}],
                                           :task/immediate? true,
                                           :task/pos 3}]}}]}]})
