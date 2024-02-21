(ns scheduling-tbd.shop
  "Rewrite SHOP planning domain structures to db structures.
   Rewrite db structures to SHOP planning domain (defdomain) structures."
  (:require
   [clojure.edn          :as edn]
   [clojure.java.io      :as io]
   [clojure.pprint       :refer [cl-format]]
   [clojure.spec.alpha   :as s]
   [datahike.api         :as d] ;[datahike.pull-api    :as dp]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm datahike-schema not-nothing register-db db-cfg-map]]
   [taoensso.timbre      :as log]))

;;; Why so much fuss with planning domain structures?
;;; Answer: The lispy shop structures are really difficult to write AND MODIFY; they have "positional semantics" and no keywords.
;;;
;;;  Consequently, there are four forms to the planning domain data:
;;;   shop      - What the UMd planner SHOP2 takes as input. It is common lisp s-expressions.
;;;               We can read these to the DB with shop2db.
;;;
;;;   canonical - a plan domain structured that separates the SHOP2 lispy domain into its elements (operators, methods, and axioms),
;;;               but for those elements just provides :canon/code which holds the corresponding elements SHOP2 lispy structure.
;;;               You can get canonical by calling db2canon with a unique string (:domain/name, :method/name, :operator/name, or :axiom/name).
;;;               Canonical is good for testing translation because it factors into the individual methods, operators and axioms.
;;;               It doesn't have any other uses, I think.
;;;
;;;  db         - structures conforming to db-schema-shop2+, a datahike schema.
;;;               Since the DB only stores atomic data types and entity references, recovery requires reconstruction of canonical
;;;
;;;   proj      - a simple clojure map rendering used in exploratory programming. (proj = project, the schedulingTBD app).
;;;               The way to get from proj to a SHOP2 lispy defdomain is (-> proj proj2canonical canon2shop)
;;;               Currently, we don't yet have a way to generate proj form other than to write it in an IDE, but that is to be expected,
;;;               it being how we develop plans. Some day maybe we have a GUI interface for something that manipulates these.
;;;
;;; The principal reasons for storing planning domains as fine-grained DB structures is to allow updating and plan repair.
;;;
;;; Use of SHOP2 might be temporary, but use of planning domains in canonical/db/proj is probably going to stick around.
;;; In our usage, we develop plan structure with PROJ and then canonicalize it before storage.

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

(defn method-name
  "SHOP methods AND AXIOMS can overload the predicate.
   Returns a string that concatenates the formal parameters and values."
  [meth-sig]
  (->> (interpose "_" meth-sig) (apply str)))

(defn code-type
  "For use mostly with :domain/elems, return one of 'method', 'axiom' or 'operator' based
   on what the expression contains. The actual :sys/typ of these is in the :sys/body."
  [exp]
  (cond (contains? exp :method/name)   "method"
        (contains? exp :axiom/name)    "axiom"
        (contains? exp :operator/name) "operator"))

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
  (s/and seq?
         #(= (nth % 0) :method)
         #(s/valid? ::atom (nth % 1))
         #(s/valid? ::method-rhsides (nthrest % 2))))

(s/def ::operator
  (s/and seq?
         #(or (== (count %) 5) (== (count %) 6))
         #(= (nth % 0) :operator)
         #(s/valid? ::atom (nth % 1))
         #(s/valid? ::operator-preconds (nth % 2))
         #(s/valid? ::del-list (nth % 3))
         #(s/valid? ::add-list (nth % 4))
         (s/or :no-cost #(== (count %) 5) ; BTW, cost is 1 if not specified.
               :cost #(and (== (count %) 6) (s/valid? ::s-exp (nth % 5))))))

;;; (:- a [name1] E1 [name2] E2 [name3] E3 ... [namen] En)
(s/def ::axiom
  (s/and seq?
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

(s/def ::operator-preconds
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

;;; AKA logical-atom, can't have a call-term or an eval-term
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
(def debugging? (atom false))
(def tags   (atom []))
(def locals (atom [{}]))

(defn clear-rewrite!
  "Trouble just passing tags and locals to rewrite.cljc!"
  []
  (reset! tags [:toplevel])
  (reset! locals [{}]))

;;; This is simpler than RM's rewrite, which relied on :typ from parse and called 'rewrite-meth' using it.
;;; This grammar has only a few top-level entry points: defdomain, :method :operator and :axiom.
(defmacro defshop2db
  "Macro to wrap methods for translating shop to database format."
  {:clj-kondo/lint-as 'clojure.core/fn ; See https://github.com/clj-kondo/clj-kondo/blob/master/doc/config.md#inline-macro-configuration
   :arglists '([tag [obj & more-args] & body])} ; You can put more in :arglists, e.g.  :arglists '([[in out] & body] [[in out err] & body])}
  [tag [obj & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod shop2db ~tag [~obj & [~@more-args]]
     (when @debugging? (println (cl-format nil "~A==> ~A" (sutil/nspaces (count @tags)) ~tag)))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [res# (do ~@body)
           result# (if (seq? res#) (doall res#) res#)]
     (swap! tags   #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when @debugging?
           (println (cl-format nil "~A<-- ~A returns ~S" (sutil/nspaces (count @tags)) ~tag result#)))
         result#))))

(defn shop2db-dispatch
  "Allow calls of two forms: (shop2db exp) (shop2db exp :some-method-key)."
  [exp & [specified]]
  (cond ;; Optional 2nd argument specifies method to call. Order matters!
    (keyword? specified)            specified,
    (s/valid? ::domain   exp)       :domain
    (s/valid? ::method   exp)       :method
    (s/valid? ::operator exp)       :operator
    (s/valid? ::axiom    exp)       :axiom
    (s/valid? ::logical-exp exp)    :logical-exp ; Essentially, has its own similar dispatch function.
    :else (throw (ex-info "shop2db-dispatch: No dispatch value for exp" {:exp exp}))))

(defmulti shop2db #'shop2db-dispatch)

;;;-------------------------------- Top-level methods
(defshop2db :domain [exp]
  (let [[_ name elems] exp
        cnt (atom 0)]
    {:domain/name (str name)
     :sys/body (-> {:sys/typ :domain}
                   (assoc :domain/elems (vec (for [e elems]
                                               (-> (shop2db e {:domain-name (str name)})
                                                   (assoc :sys/pos (swap! cnt inc)))))))}))

;;; (shop2db '(:method (transport-person ?p ?c) Case1 ((at ?p ?c)) Case2 ((also ?p ?x)) ()) :method)
;;; Method: (:method <head> [label]
(defshop2db :method [body & {:keys [domain-name]}]
  (s/assert ::method body)
  (clear-rewrite!)
  (let [[_ head & pairs] body
        res {:method/name (str domain-name "." (method-name head))
             :sys/body (-> {:sys/typ :method}
                           (assoc :method/head      (shop2db head :atom))
                           (assoc :method/rhs       (shop2db pairs :method-rhsides)))}]
    (if-let [case-name (-> res :sys/body :method/rhs first :method/case-name)]
      ;; Uniqueness is not well thought through in SHOP! If there is any case-name whatsoever, add THE FIRST to the name.
      ;; It appears from the zeno example that method signatures might include both formal parameters and sometimes case names.
      ;; I don't think we care, since we are handing the thing to SHOP to deal with, but we need unique for the DB.
      (update res :method/name #(str % "." case-name))
      res)))

;;; Operator: (:operator <head> <pre-conds> <d-list> <a-list> [<cost>])
(defshop2db :operator [body & {:keys [domain-name]}]
  (s/assert ::operator body)
  (clear-rewrite!)
  (let [[_ head preconds dlist alist & [cost]] body]
    {:operator/name (str domain-name "." (first head))
     :sys/body (cond-> {:sys/typ :operator}
                 true                   (assoc :operator/head (shop2db head :atom))
                 (not-nothing preconds) (assoc :operator/preconds (shop2db preconds :operator-preconds))
                 (not-nothing dlist)    (assoc :operator/d-list   (shop2db dlist :d-list))
                 (not-nothing alist)    (assoc :operator/a-list   (shop2db alist :a-list))
                 cost (assoc :operator/cost (shop2db cost :s-exp-extended)))}))

;;; (:- a [name1] E1 [name2] E2 [name3] E3 ... [namen] En)
;;;   (shop2db '(:- (same ?x ?x) ()) :axiom)
(defshop2db :axiom [body & {:keys [domain-name]}]
  (s/assert ::axiom body)
  (clear-rewrite!)
  (let [[_ head & exps] body
        rhs (loop [exps (vec exps)
                   pos 1
                   res []]
              (cond (empty? exps)              res
                    (symbol? (nth exps 0))     (recur ; Has case-name
                                                (subvec exps 2)
                                                (inc pos)
                                                (conj res (cond-> {:rhs/case-name (nth exps 0)}
                                                            true                     (assoc :sys/pos pos)
                                                            (not-empty (nth exps 1)) (assoc :rhs/terms (shop2db (nth exps 1) :logical-exp))
                                                            true                     (assoc :sys/typ :rhs))))
                    :else                      (recur
                                                (subvec exps 1)
                                                (inc pos)
                                                (if (not-empty (nth exps 0))
                                                  (conj res (-> {:sys/pos pos}
                                                                (assoc :rhs/terms (shop2db (nth exps 0) :logical-exp))))
                                                  (conj res {:box/empty-list "empty list"})))))]
    {:axiom/name (if-let [case-name (-> rhs first :rhs/case-name)]
                   (str domain-name "." (method-name head) "." case-name)
                   (str domain-name "." (method-name head)))
     :sys/body (cond-> {:sys/typ :axiom
                        :axiom/head (shop2db head :atom)}
                 (not-empty rhs)    (assoc :axiom/rhs rhs))}))

;;;------- supporting methods ------------------------------
;;; This one is used on methods, operators, and axioms. See schema for :sys/body rationale.
(defshop2db :body [exp] (-> exp :sys/body shop2db))

(defshop2db :task-list [exp]
  (let [explicit (#{:ordered :unordered} (first exp))
        tasks (if explicit (rest exp) exp)
        cnt (atom 0)]
    (cond-> {:sys/typ :task-list
             :task-list/elems (-> (for [t tasks]
                                    (-> (shop2db t :task-list-elem)
                                        (assoc :sys/pos (swap! cnt inc))))
                                  vec)}
      (= explicit :unordered)  (assoc :task-list/unordered? true))))

(defshop2db :task-list-elem [exp]
  (let [immediate? (= :immediate (first exp))
        exp (if immediate? (rest exp) exp)]
    (as-> exp ?e
      (cond (s/valid? ::task-atom ?e)  (shop2db ?e :task-atom)
            (s/valid? ::op-call   ?e)  (shop2db ?e :op-call)
            :else (throw (ex-info "Task is neither a task-atom or op-call" {:exp ?e})))
      (if immediate? (assoc ?e :task/immediate? true) ?e))))

(defshop2db :op-call [exp]
  {:sys/typ :op-call
   :op-call/predicate (first exp)
   :op-call/args (let [cnt (atom 0)]
                   (-> (for [arg (rest exp)]
                         (if (s/valid? ::atom arg)
                           (-> (shop2db arg :atom)
                               (assoc :sys/pos (swap! cnt inc)))
                           (-> {:sys/pos (swap! cnt inc)} ; Assumes it is a list of atoms.
                               (assoc :op-call/args (-> (let [ncnt (atom 0)]
                                                          (for [narg arg]
                                                            (-> (shop2db narg :atom)
                                                                (assoc :sys/pos (swap! ncnt inc)))))
                                                        vec)))))
                       vec))})

;;; (:method h [n1] C1 T1 [n2] C2 T2 ... [nk] Ck Tk)
(defshop2db :method-rhsides [rhs] ; this does the [n1] C1 T1 [n2] C2 T2 ... [nk] Ck Tk
  (let [res (loop [res []
                   triples (vec rhs)]
              (if (empty? triples)
                res
                (if (symbol? (nth triples 0))
                  (recur (conj res (cond-> {:method/case-name (nth triples 0)}
                                     (not-empty (nth triples 1)) (assoc :method/preconds  (shop2db (nth triples 1) :method-precond))
                                     (not-empty (nth triples 2)) (assoc :method/task-list (shop2db (nth triples 2) :task-list))))
                         (subvec triples 3))
                  (recur (conj res (cond-> {}
                                     (not-empty (nth triples 0)) (assoc :method/preconds  (shop2db (nth triples 0) :method-precond))
                                     (not-empty (nth triples 1)) (assoc :method/task-list (shop2db (nth triples 1) :task-list))))
                         (subvec triples 2)))))
        cnt (atom 0)]
    (-> (for [r res]
          (assoc r :sys/pos (swap! cnt inc)))
        vec)))

(defshop2db :method-precond [c]
  (let [cnt (atom 0)]
    (cond (-> c (nth 0) seq?)         (-> (for [x c]
                                            (-> (shop2db x :atom)
                                                (assoc :sys/pos (swap! cnt inc))))
                                          vec)
          (-> c (nth 0) (= :sort-by)) (shop2db c :sort-by)
          (-> c (nth 0) (= :first))   (shop2db c :first)
          :else (throw (ex-info "Invalid method precondition" {:exp c})))))

(defshop2db :operator-preconds [exp]
  (let [cnt (atom 0)] ; Ordinary conjunct of :logical-exp
    (-> (for [pc exp]
          (-> (shop2db pc :logical-exp)
              (assoc :sys/pos (swap! cnt inc))))
        vec)))

(defshop2db :sort-by [exp]
  (if (== 4 (count exp))
    (let [[_ var  fn-def lexp] exp]
      (-> {:sys/typ :sort-by
           :sort-by/var var}
          (assoc :sort-by/fn  (shop2db fn-def :s-exp-extended))
          (assoc :sort-by/exp (shop2db lexp :logical-exp))))
    (let [[_ var lexp] exp]
      (-> {:sort-by/var var}
          (assoc :sort-by/fn {:s-exp/fn-ref '<})
          (assoc :sort-by/exp (shop2db lexp :logical-exp))))))

(defn box [v]
  (cond (number? v)       {:box/num v}
        (string? v)       {:box/str v}
        (symbol? v)       {:box/sym v}
        :else             v))

(defshop2db :d-list [exp]
  (let [exp (if (seq? exp) (vec exp) (vector exp))
        cnt (atom 0)]
    (-> (for [e exp]
          (-> (shop2db e :op-list-elem)
              (assoc :sys/pos (swap! cnt inc))))
        vec)))

(defshop2db :a-list [exp]
  (let [exp (if (seq? exp) (vec exp) (vector exp))
        cnt (atom 0)]
    (-> (for [e exp]
          (-> (shop2db e :op-list-elem)
              (assoc :sys/pos (swap! cnt inc))))
        vec)))

(defshop2db :op-list-elem [exp]
  (cond (symbol? exp)                 {:box/sym exp}
        (s/valid? ::atom exp)         (shop2db exp :atom)
        (s/valid? ::protected exp)    (shop2db exp :protected)
        (s/valid? ::op-universal exp) (shop2db exp :op-universal)
        :else (throw (ex-info "Invalid item in op list:" {:exp exp}))))

(defshop2db ::protected [exp]
  {:protected/atom (shop2db (nth exp 1) :atom)})

(defshop2db ::op-universal [exp]
  (let [[_ vars cond consq] exp]
    (-> {:sys/typ :op-universal
         :forall/vars (vec vars)}
        (assoc :forall/conditions   (shop2db cond :logical-exp))
        (assoc :forall/consequences (mapv #(shop2db % :atom) consq)))))

;;; ---------------- s-expression -----------------
(defn seq2sexp [s]
  (cond (seq? s) (cond-> {:s-exp/fn-ref (first s) :sys/typ :s-exp}
                   (-> s rest not-empty) (assoc :s-exp/args (let [cnt (atom 0)]
                                                              (-> (for [a (rest s)]
                                                                    (->  (shop2db a :s-exp-arg)
                                                                         (assoc :sys/pos (swap! cnt inc))))
                                                                  vec))))
        (or (number? s) (string? s) (symbol? s)) (box s)))

;;; This currently does not handle quoted things.
(defshop2db :s-exp-extended [exp]
  (if (symbol? exp)
    {:s-exp/fn-ref exp :sys/typ :s-exp}
    (seq2sexp exp)))

(defshop2db :s-exp-arg [exp]
  (if (seq? exp)
    (seq2sexp exp)
    {:s-exp/arg-val (box exp)}))

;;;-------------------------------- Logical expressions -------------------
(defshop2db :logical-exp [exp] ; order matters!
  (cond (s/valid? ::implication exp)    (shop2db exp :implication)
        (s/valid? ::negation exp)       (shop2db exp :negation)
        (s/valid? ::disjunct exp)       (shop2db exp :disjunct)
        (s/valid? ::universal exp)      (shop2db exp :universal)
        (s/valid? ::assignment exp)     (shop2db exp :assignment)
        (s/valid? ::eval exp)           (shop2db exp :eval)
        (s/valid? ::call exp)           (shop2db exp :call)
        (s/valid? ::enforce exp)        (shop2db exp :enforce)
        (s/valid? ::setof exp)          (shop2db exp :setof)
        (s/valid? ::atom exp)           (shop2db exp :atom) ; I think other kinds of atoms are handled by :task-list-elem
        (s/valid? ::conjunct exp)       (shop2db exp :conjunct)
        :else (throw (ex-info "Unknown logical exp:" {:exp exp}))))

(defshop2db :implication [exp]
  (let [[_ l1 l2] exp]
    {:sys/typ :implication
     :imply/condition (shop2db l1 :logical-exp)
     :imply/consequence (shop2db l2 :logical-exp)}))

(defshop2db :negation [exp]
  (-> (shop2db (nth exp 1) :logical-exp)
      (assoc :exp/negated? true)))

#_(defshop2db :conjunct [exp]
  (let [res (mapv #(shop2db % :logical-exp) exp)]
    (-> (if (empty? res)
          {:conjunction/shop-empty-list? true} ; ToDo: Don't do it this way. In translation from the DB return a '() if that is what is needed. (shop-empty-list? was NYI anyway...)
          {:conjunction/terms res})
        (assoc :sys/typ :conjunction))))

;;; ToDo: Make the next two ordered
(defshop2db :conjunct [exp]
  (-> {:sys/typ :conjunct}
      (assoc :conjunct/terms (mapv #(shop2db % :logical-exp) exp))))

(defshop2db :disjunct [exp]
  (-> {:sys/typ :disjunct}
      (assoc :disjunct/terms (mapv #(shop2db % :logical-exp) (rest exp)))))

;;;    (forall (?c) ((dest ?a ?c)) ((same ?c ?c1)))
(defshop2db :universal [exp]
  (let [[_ vars conds consq] exp]
    (-> {:sys/typ :universal}
        (assoc :forall/vars         (vec vars))
        (assoc :forall/conditions   (mapv #(shop2db % :atom) conds))
        (assoc :forall/consequences (mapv #(shop2db % :atom) consq)))))

(defshop2db :assignment [exp]
  (let [[_ v e] exp]
    (-> {:sys/typ :assignment}
        (assoc :assign/var v)
        (assoc :assign/exp (shop2db e :s-exp-extended)))))

(defshop2db :eval [exp]
  {:sys/typ :eval
   :eval/form (shop2db exp :s-exp-extended)})

(defshop2db :atom [exp]
  (let [[pred & terms] exp]
    (cond-> {:sys/typ :atom}
      true               (assoc :atom/predicate pred)
      (not-empty terms)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v terms]
                                                   {:role/val (box (shop2db v :term))
                                                    :sys/pos (swap! cnt inc)}))
                                                vec)))))

(defshop2db :task-atom [exp]
  (let [[pred & terms] exp]
    (cond-> {:sys/typ :task-atom}
      true               (assoc :atom/predicate pred)
      (not-empty terms)  (assoc :atom/roles (-> (let [cnt (atom 0)]
                                                 (for [v terms]
                                                   {:role/val (box (shop2db v :task-term))
                                                    :sys/pos (swap! cnt inc)}))
                                                vec)))))

(defshop2db :term [exp]
  (cond (symbol? exp) exp
        (number? exp) exp
        (seq? exp)    (shop2db exp :list-term)
        :else         (throw (ex-info "Not a valid term:" {:exp exp}))))

(defshop2db :task-term [exp]
  (cond (s/valid? ::term exp)      (shop2db exp :term)
        (s/valid? ::call-term exp) (shop2db exp :call-term)
        (s/valid? ::eval-term exp) (shop2db exp :eval-term)
        :else (throw (ex-info "Not a task-term: " {:exp exp}))))

(defshop2db :call-term [exp]
  (let [[_ fn & args] exp]
    {:sys/typ :call-term
     :call/fn (if (symbol? fn) fn (-> fn (nth 1) (nth 1))) ; It looks something like this (function (quote +))... or just a symbol
     :call/args (let [cnt (atom 0)]
                  (-> (for [a args]
                        (-> (shop2db a :s-exp-arg)
                            (assoc :sys/pos (swap! cnt inc))))
                      vec))}))

(defshop2db :list-term [exp]
  {:sys/typ :list-term
   :list/terms (let [cnt (atom 0)]
                  (-> (for [a exp]
                        (-> {:list/val (box (shop2db a :term))}
                            (assoc :sys/pos (swap! cnt inc))))
                      vec))})

;;;-------------------------------- end of Logical expressions -------------------
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
   :box/empty-list
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/string
        :doc "a construct to wrap primitive types when the type cannot be anticipated and therefore a :db.type/ref is used"}

   ;; ----------------------- call
   :call/fn
   #:db{:cardinality :db.cardinality/one, :valueType :db.type/symbol
        :doc "The symbol of a call term"}
   :call/args
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "The arguments of th the call term"}

   ;; ----------------------- conjunction/disjunction
   :conjunct/terms
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the terms that are part of a conjunctive expression."} ; ToDo: need a :sys/pos or something like that.

   :disjunct/terms
   #:db{:cardinality :db.cardinality/many, :valueType :db.type/ref
        :doc "the terms that are part of a disjunctive expression."} ; ToDo: need a :sys/pos or something like that.


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
   :method/preconds
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
        :doc "an unqualified keyword indicating the type of the object; it is used to select a serialization method."}
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

;;;=============================== Serialization (DB structures to SHOP common-lisp s-expressions) ================================================
(defmacro defdb2shop
  "Macro to wrap methods for translating databse to shop format."
  {:clj-kondo/lint-as 'clojure.core/fn
   :arglists '([tag [obj & more-args] & body])} ; You can put more in :arglists, e.g.  :arglists '([[in out] & body] [[in out err] & body])}
  [tag [obj & more-args] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod db2shop ~tag [~obj & [~@more-args]]
     (when @debugging? (println (cl-format nil "~A==> ~A" (sutil/nspaces (count @tags)) ~tag)))
     (swap! tags #(conj % ~tag))
     (swap! locals #(into [{}] %))
     (let [res# (do ~@body)
           result# (if (seq? res#) (doall res#) res#)]
     (swap! tags   #(-> % rest vec))
     (swap! locals #(-> % rest vec))
     (do (when @debugging?
           (println (cl-format nil "~A<-- ~A returns ~S" (sutil/nspaces (count @tags)) ~tag result#)))
         result#))))

(defn db2shop-dispatch
  "Allow calls of two forms: (db2shop exp) (db2shop exp :some-method-key).
   Things with :sys/typ:
   :assignment :atom :axiom :call-term :conjunct :disjunct :domain :eval :implication :list-term :method
   :op-call :op-universal :operator :s-exp :sort-by :task-list :universal."
  [exp & [specified]]
  (cond specified                                        specified
        (and (map? exp) (contains? exp :sys/typ))        (:sys/typ exp)
        (or (symbol? exp) (number? exp) (string? exp))   :simple-type
        (contains? exp :domain/name)                     :domain
        (contains? exp :sys/body)                        :body ; This is used, at least in the shop-round-trip test.
        (contains? exp :box/sym)                         :box
        (contains? exp :box/num)                         :box
        (contains? exp :box/str)                         :box
        (contains? exp :box/empty-list)                  :box
        (contains? exp :s-exp/arg-val)                   :arg-val
        (contains? exp :rhs/terms)                       :rhs-terms
        :else
        (do (when-not (empty? exp) (reset! diag exp))
            (throw (ex-info "db2shop-dispatch: No dispatch value for exp" {:exp exp})))))

(defmulti db2shop #'db2shop-dispatch)

(defdb2shop :domain [{:domain/keys [name] :sys/keys [body]}]
  `(~'defdomain ~(symbol name)
    ~(for [e (sort-by :sys/pos (-> body :domain/elems))]
       (db2shop e))))

;;; (:method h [n1] C1 T1 [n2] C2 T2 ... [nk] Ck Tk)
(defdb2shop :method [{:method/keys [head rhs]}]
  `(:method ~(db2shop head :atom)
            ~@(mapcat #(db2shop % :rhs-pair) (sort-by :sys/pos rhs)))) ; Actually rhs=multiple rh sides.

(defdb2shop :axiom [{:axiom/keys [head rhs]}]
  (let [h (db2shop head :atom)]
    (if rhs
      (let [rew1 (mapcat #(db2shop % :rhs) (sort-by :sys/pos rhs))]
        (if (-> rew1 first symbol?)
          `(:- ~h ~@rew1)
          `(:- ~h ~rew1)))
      `(:- ~h ()))))

(defdb2shop :operator [{:operator/keys [head preconds d-list a-list cost]}]
  (let [pre (map db2shop (->> preconds (sort-by :sys/pos)))
        del (map db2shop (->> d-list   (sort-by :sys/pos)))
        add (map db2shop (->> a-list   (sort-by :sys/pos)))
        res `(:operator
              ~(db2shop head)
              ~pre
              ~(if (-> del first symbol?) (first del) del)
              ~(if (-> add first symbol?) (first add) add))]
    (if cost
      (->> cost db2shop (conj (vec res)) seq)
      res)))

;;; --------------- Supporting serialization ------------------------
(defdb2shop :body [exp] (-> exp :sys/body db2shop)) ; This is used, at least in the shop-round-trip test.

(defdb2shop :simple-type [exp] exp)

(defdb2shop :rhs [{:rhs/keys [terms case-name] :box/keys [empty-list]}]
  (if empty-list
    '()
    (let [res (db2shop terms)] ; ToDo: terms is a misnomer. Uses :sys/typ :conjunction.
      (if case-name
        `(~case-name ~res)
        res))))

(defdb2shop :atom [{:atom/keys [predicate roles] :exp/keys [negated?]}]
  (let [atom `(~predicate ~@(map db2shop (->> roles (sort-by :sys/pos) (map :role/val))))]
    (if negated? `(~'not ~atom) atom)))

(defdb2shop :box [{:box/keys [sym num str empty-list]}]
  (if empty-list '() (or sym num str)))

(defdb2shop :conjunct [{:conjunct/keys [terms]}]
  (map db2shop terms))

(defdb2shop :disjunct [{:disjunct/keys [terms]}]
  `(or ~@(map db2shop terms)))

(defdb2shop :rhs-terms [{:rhs/keys [terms]}]
  (if (and (map? terms) (or (contains? terms :conjunct/terms) (contains? terms :disjunct/terms)))
    (if (contains? terms :conjunct/terms)
      (map db2shop (:conjunct/terms terms))
      `(or ~@(map db2shop (:disjunct/terms terms))))
  (map db2shop terms)))

(defdb2shop :eval [{:eval/keys [form]}] (db2shop form)) ; It is just an s-exp, I think.

(defdb2shop :s-exp [{:s-exp/keys [fn-ref args]}]
  `(~fn-ref ~@(map db2shop (->> args (sort-by :sys/pos)))))

(defdb2shop :arg-val [{:s-exp/keys [arg-val]}]
  (db2shop arg-val))

(defdb2shop :assignment [{:assign/keys [var exp]}]
  `(~'assign ~var ~(db2shop exp)))

(defdb2shop :rhs-pair [{:method/keys [case-name preconds task-list]}]
  (let [pres (if (-> preconds vector?)
               (->> preconds (sort-by :sys/pos) (map db2shop))
               (if (empty? preconds) '() (db2shop preconds)))
        pres (if (= :sort-by (-> pres first first)) (first pres) pres) ; :sort-by is different!
        res `(~pres ~(db2shop task-list :task-list))]
    (if case-name
      (conj res case-name)
      res)))

(defdb2shop :task-list [{:task-list/keys [elems]}]
  (->> elems (sort-by :sys/pos) (map db2shop)))

(defdb2shop :task-atom [{:task/keys [immediate?] :atom/keys [predicate roles]}]
  (let [atom `(~predicate ~@(map db2shop (->> roles (sort-by :sys/pos) (map :role/val))))]
    (if immediate? `(:immediate ~@atom) atom)))

(defdb2shop :call-term [{:call/keys [fn args]}]
  `(~'call (~'function '~fn) ~@(map db2shop (sort-by :sys/pos args))))

;;; ToDo: Some of the following have not been tested.
(defdb2shop :disjunction [exp] `(~'or ~(map db2shop exp)))

(defdb2shop :implication [{:imply/keys[condition consequence]}]
  `(~'imply ~(db2shop condition) ~(db2shop consequence)))

(defdb2shop :list-term [{:list/keys [terms]}]
  (->> terms (sort-by :sys/pos) (map :list/val) (map db2shop)))

(defdb2shop :op-call [exp]
  (db2shop exp :call-term))

(defdb2shop :op-universal [{:op-forall/keys [condition consequences]}]
  `(~'forall (map db2shop vars)
    ~(db2shop condition)
    ~(map db2shop consequences)))

(defdb2shop :sort-by [{:sort-by/keys [var fn exp]}]
  `(:sort-by ~(db2shop var) ~(-> fn db2shop first) ~(db2shop exp)))

(defdb2shop :universal [{:forall/keys [vars conditions consequences]}]
  `(~'forall ~(map db2shop vars) ~(map db2shop conditions) ~(map db2shop consequences)))

;;; ------------------------ Top-level manipulation -----------------------------------
(defn canon2db
  "Given a canonical structure, create the corresponding DB structure.
   :canon/pos of each element of :domain/elems is :sys/pos in the DB."
  [{:domain/keys [name elems]}]
  {:domain/name name
   :sys/body {:sys/typ :domain
              :domain/elems (mapv #(let [name-attr (keyword (code-type %) "name")]
                                     (-> (shop2db  (:canon/code %) {:domain-name name})
                                         (assoc :sys/pos (:canon/pos %))
                                         (assoc name-attr (get % name-attr))))
                                  (sort-by :canon/pos elems))}})

(defn canon2shop
  "Rewrite 'canonical' (which is SHOP form embeddded in EDN) as SHOP."
  [{:domain/keys [name elems]}]
  `(~'defdomain ~(symbol name)
    ~(->> elems (sort-by :canon/pos) (map :canon/code))))

(defn db-entry-for
  "Return the named DB structure. name is string and db.unique/identity"
  [name & {:keys [db-atm] :or {db-atm (connect-atm :planning-domains)}}]
  (when-let [eid (d/q '[:find ?e .
                        :in $ ?name
                        :where (or [?e :method/name ?name]
                                   [?e :operator/name ?name]
                                   [?e :axiom/name ?name]
                                   [?e :domain/name ?name])]
                      @db-atm
                      name)]
    (sutil/resolve-db-id {:db/id eid} db-atm #{:db/id})))

(defn db2canon
  "Use db2shop to create from db-style maps a map structure where the :domain/elems are individual methods, operators and axioms."
  [db-obj]
  (let [cnt (atom 0)]
    (cond (contains? db-obj :domain/name) {:domain/name (:domain/name db-obj)
                                           :domain/elems (-> (for [e (->> db-obj :sys/body :domain/elems (sort-by :sys/pos))]
                                                               (let [name-attr (keyword (code-type e) "name")]
                                                                 (-> {:canon/pos (swap! cnt inc)}
                                                                     (assoc name-attr (get e name-attr))
                                                                     (assoc :canon/code (db2shop e)))))
                                                             vec)}
          ;; This one is just used in debugging, I think.
          (#{"method" "axiom" "operator"} (code-type db-obj))
          (let [name-attr (keyword (code-type db-obj) "name")]
            {name-attr (get db-obj name-attr)
             :canon/code (-> db-obj :sys/body db2shop)}))))

;;; ------------------------ proj2 to/from canonical ------------------------------
(def proj-cnt-atm (atom 0))

(defn proj2canon-method
  "Restructure PROJ into canonical (which is more lisp-like)."
  [e base-name]
  (-> {}
      (assoc :canon/pos (swap! proj-cnt-atm inc))
      (assoc :method/name (cond-> (str base-name "." (-> e :method/head method-name))
                            (-> e :method/rhsides first :method/case-name) (str "." (-> e :method/rhsides first :method/case-name))))
      (assoc :canon/code
             `(:method
               ~(:method/head e)
               ~@(mapcat (fn [p]
                           (if-let [cname (:method/case-name p)]
                             `(~(symbol cname)
                               ~(if-let [pc (-> p :method/preconds seq)] pc ())
                               ~(-> p :method/task-list seq))
                             `(~(if-let [pc (-> p :method/preconds seq)] pc ())
                               ~(-> p :method/task-list seq))))
                         (:method/rhsides e))))
      (dissoc :method/rhsides)))

(defn lisp-seq
  "Return the collection as a seq."
  [x]
  (if (empty? x) '() (seq x)))

(defn proj2canon-operator
  "Restructure PROJ into canonical (which is more lisp-like)."
  [e base-name]
  (-> {}
      (assoc :canon/pos (swap! proj-cnt-atm inc))
      (assoc :operator/name (str base-name "." (-> e :operator/head method-name))) ; Unlike the shop example, we plan to have not naming collisions!
      (assoc :canon/code
             `(:operator
               ~(:operator/head e)
               ~(-> e :operator/preconds lisp-seq)
               ~(-> e :operator/d-list   lisp-seq)
               ~(-> e :operator/a-list   lisp-seq)))))

(defn proj2canon-axiom
  "Restructure PROJ into canonical (which is more lisp-like)."
  [e base-name]
    (-> {}
      (assoc :canon/pos (swap! proj-cnt-atm inc))
      (assoc :axiom/name (str base-name "." (-> e :axiom/head method-name))) ; Unlike the shop example, we plan to have not naming collisions!
      (assoc :canon/code
             `(:-
               ~(:axiom/head e)
               ~@(mapcat (fn [rside]
                           (if-let [cname (:axiom/case-name rside)]
                             `(~(symbol cname)
                               ~(if-let [rhs (-> rside :axiom/rhs seq)] rhs ()))
                             `(~(if-let [rhs (-> rside :axiom/rhs seq)] rhs ()))))
                         (:axiom/rhsides e))))))

(defn proj2canon
  "Rewrite the PROJ structure as canonical, which is less lispy."
  [proj]
  (reset! proj-cnt-atm 0)
  (let [base-name (:domain/name proj)]
    (update proj :domain/elems
            #(for [e %]
               (cond (contains? e :method/head)   (proj2canon-method e base-name)
                     (contains? e :operator/head) (proj2canon-operator e base-name)
                     (contains? e :axiom/head)    (proj2canon-axiom e base-name))))))

(defn db2proj-axiom
  "Rewrite an axiom to proj form."
  [obj]
  (let [{:axiom/keys [head rhs]} (:sys/body obj)]
    (cond-> {:axiom/head      (db2shop head)}
      rhs (assoc :axiom/rhsides
                 (mapv (fn [r]
                         (cond-> {:axiom/rhs (-> r :rhs/terms db2shop)} ; It is a conjunct.
                           (:rhs/case-name r) (assoc :axiom/case-name (-> r :rhs/case-name str))))
                       rhs)))))

(defn db2proj-method
  "Rewrite an method to proj form."
  [obj]
  (-> {:method/head (db2shop (-> obj :sys/body :method/head))}
      (assoc :method/rhsides
             (mapv (fn [rhs]
                     (let [{:method/keys [case-name preconds task-list]} rhs]
                       (cond-> {}
                         case-name  (assoc :method/case-name (str case-name))
                         preconds   (assoc :method/preconds  (mapv db2shop preconds))
                         true       (assoc :method/task-list (db2shop task-list)))))
                     (-> obj :sys/body :method/rhs)))))

(defn db2proj-operator
  "Rewrite an operator to proj form."
  [obj]
  (let [{:operator/keys [head preconds d-list a-list]} (:sys/body obj)]
    (cond-> {:operator/head (db2shop head)}
      preconds     (assoc :operator/preconds (->> preconds (sort-by :sys/pos) (mapv db2shop)))
      d-list       (assoc :operator/d-list   (->> d-list   (sort-by :sys/pos) (mapv db2shop)))
      a-list       (assoc :operator/a-list   (->> a-list   (sort-by :sys/pos) (mapv db2shop))))))

(defn db2proj
  "Rewrite the DB object as a proj object."
  [db-obj]
  `{:domain/name ~(-> db-obj :domain/name str)
    :domain/elems ~(mapv (fn [elem]
                           (let [etyp (-> elem :sys/body :sys/typ)]
                             (cond (= :axiom    etyp)    (db2proj-axiom elem),
                                   (= :method   etyp)    (db2proj-method elem),
                                   (= :operator etyp)    (db2proj-operator elem))))
                         (-> db-obj :sys/body :domain/elems))})

;;; ------------------------ DB Stuff -------------------------
;;; Currently this loads zeno, just for testing.
(defn recreate-planning-domains-db!
  "Recreate the plans db from .edn data. It uses the connect-atm, which is already established."
  [cfg]
  (if (.exists (io/file "data/planning-domains/domains.edn")) ; ToDo: This will need to be better someday soon.
    (do (log/info "Recreating the planning domains database.")
        (when (d/database-exists? cfg)
          (d/delete-database cfg))
        (d/create-database cfg)
        (register-db :planning-domains cfg)
        (let [conn (connect-atm :planning-domains)]
          (d/transact conn (datahike-schema db-schema-shop2+))
          (d/transact conn (->> "data/planning-domains/domains.edn"
                                slurp
                                edn/read-string
                                (mapv proj2canon)
                                (mapv canon2db))))
        true)
    (log/error "Not recreating planning domains DB: No backup file.")))

(def recreate-db? false)

;;; -------------------- Starting and stopping -------------------------
(defn init-db-cfg
  "Set sys-db-cfg atoms for system db and the template for the proj-base-cfg (:base-path).
   Recreate the system database if sys-db-cfg.recreate-db? = true."
  []
  (when recreate-db?
    (let [cfg (db-cfg-map :planning-domains)]
      (recreate-planning-domains-db! cfg)
      {:plan-db-cfg cfg})))

(defstate plans-db-cfg
  :invalid-state :bogus ; Should warn: "lifecycle functions can only contain `:start` and `:stop`. illegal function found: "
  :start (init-db-cfg))
