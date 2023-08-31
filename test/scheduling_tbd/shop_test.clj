(ns scheduling-tbd.shop-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.test :refer [deftest is testing]]
   [datahike.api                 :as d]
   [scheduling-tbd.shop :as shop]
   [scheduling-tbd.llm  :as llm]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm]]
   [scheduling-tbd.util :as util]
   [taoensso.timbre     :as log]))

(def kiwi-domain
  '(defdomain basic-example
     ((:operator (!pickup ?a) () () ((have ?a)))
      (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
      (:method (swap ?x ?y)
               ((have ?x))
               ((!drop ?x) (!pickup ?y))
               ((have ?y))
               ((!drop ?y) (!pickup ?x))))))

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

(def sample-operator
  '(:operator (!!cost ?end)

              ((maxtime ?max) ; Since ?max isn't bound in the head, I supose this is a query.
               (assign ?newmax (eval (if (< ?max ?end) ?end ?max))))

              ((maxtime ?max))
              ((maxtime ?newmax))
              (- ?newmax ?max)))

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


;;; ToDo:  WRONG! Both of them. I don't have time for this now.
(deftest s-expression2db
  (testing "Testing creating db structures for s-expressions"
    (is (= {:s-exp/fn-ref '>} (rewrite '> :s-exp-extended)))
    (is (= #:s-exp{:args
                   [{:arg/pos 1, :s-exp/args [#:arg{:val #:box{:num 1}, :pos 1} #:arg{:val #:box{:sym 'x}, :pos 2}], :s-exp/fn-ref '+}
                    #:arg{:val #:box{:num 3}, :pos 2}
                    #:arg{:val #:box{:sym 'q}, :pos 3}],
                   :fn-ref '*}
           (rewrite '(* (+ 1 x) 3 q) :s-exp-extended)))))

;;; See pg 397 in Dana Nau, et al., "SHOP2: An HTN Planning System" Journal of Artificial Intelligence Research 20 (2003) 379-404
;;; (By convention) names of operators begin with an exclamation mark (!).
;;; This doesn't use:
;;; - and (logical expressions are implicit when nested)
;;; - :task (an optional keyword that can be the first element of a task atom).

(def zeno
  "This is an example in the 'canonical' form of a domain. When a SHOP version is needed, use shop/canon2shop.
   To store this use shop/canon2db."
  {:domain/name "ZENO"
   :domain/elems [{:canon/pos 1
                   :axiom/name "zeno.same"
                   :canon/code '(:- (same ?x ?x) ())}

                  {:canon/pos 2
                   :axiom/name "zeno.different"
                   :canon/code '(:- (different ?x ?y) ((not (same ?x ?y))))}

                  {:canon/pos 3
                   :axiom/name "zeno.possible-person-in"
                   :canon/code '(:- (possible-person-in ?city)
                                    ((person ?p)
                                     (at ?p ?city)
                                     (goal ?p ?city2)
                                     (different ?city2 ?city)))}

                  {:canon/pos 4
                   :operator/name "zeno.!!cost"
                   :canon/code '(:operator (!!cost ?end)
                                              ((maxtime ?max) ; Since ?max isn't bound in the head, I supose this is a query.
                                               (assign ?newmax (eval (if (< ?max ?end) ?end ?max))))

                                              ((maxtime ?max))
                                               ((maxtime ?newmax))
                                              (- ?newmax ?max))}

                  {:canon/pos 5
                   :method/name "zeno.board"
                   :canon/code '(:method (board ?p ?a ?c)
                                          ((write-time ?a ?start))
                                          ((!board ?p ?a ?c ?start 1)
                                           (:immediate !!cost (call (function '+) ?start 1))))}

                  {:canon/pos 6
                   :operator/name "zeno.!board"
                   :canon/code '(:operator (!board ?p ?a ?c ?start ?duration)
                                              ((person ?p)
                                               (aircraft ?a)
                                               (city ?c)
                                               (at ?a ?c)
                                               (at ?p ?c)
                                               (onboard ?a ?num)
                                               (read-time ?a ?pmax)
                                               (assign ?new-num (+ ?num 1))
                                               (assign ?newpmax (max ?pmax (+ ?start ?duration 0.010))))
                                              ((onboard ?a ?num)
                                               (read-time ?a ?pmax)
                                               (at ?p ?c)
                                               (dest ?a ?c))
                                              ((onboard ?a ?new-num)
                                               (read-time ?a ?newpmax)
                                               (in ?p ?a))
                                              0.001)}

                  {:canon/pos 7
                   :method/name "zeno.debark"
                   :canon/code '(:method (debark ?p ?a ?c)
                                          ((write-time ?a ?start))
                                          ((!debark ?p ?a ?c ?start 1)
                                           (:immediate !!cost (call (function '+) ?start 1))))}

                  {:canon/pos 8
                   :operator/name "zeno.!debark"
                   :canon/code '(:operator (!debark ?p ?a ?c ?start ?duration)
                                              ((person ?p)
                                               (aircraft ?a)
                                               (city ?c)
                                               (at ?a ?c)
                                               (in ?p ?a)
                                               (onboard ?a ?num)
                                               (read-time ?a ?pmax)
                                               (assign ?new-num (- ?num 1))
                                               (assign ?newpmax (max ?pmax (+ ?start ?duration 0.010))))
                                              ((onboard ?a ?num)
                                               (read-time ?a ?pmax)
                                               (in ?p ?a) (dest ?a ?c))
                                              ((onboard ?a ?new-num)
                                               (read-time ?a ?newpmax)
                                               (at ?p ?c))
                                              0.001)}

                  {:canon/pos 9
                   :method/name "zeno.refuel"
                   :canon/code '(:method (refuel ?a ?c)
                                          ((write-time ?a ?start)
                                           (read-time ?a ?pmax)
                                           (capacity ?a ?cap)
                                           (fuel ?a ?fuel)
                                           (eval (> ?cap ?fuel))
                                           (assign ?duration 1)
                                           (assign ?end (+ ?start ?duration 0.010))
                                           (assign ?newpmax (max ?pmax ?end)))
                                          ((!!ra ((read-time ?a ?pmax))
                                                 ((read-time ?a ?newpmax)))
                                           (:immediate !refuel ?a ?c ?start ?duration)
                                           (:immediate !!cost ?end)))}

                  {:canon/pos 10
                   :operator/name "zeno.!refuel"
                   :canon/code '(:operator (!refuel ?a ?c ?start ?duration)
                                              ((aircraft ?a)
                                               (city ?c)
                                               (at ?a ?c)
                                               (fuel ?a ?fuel)
                                               (capacity ?a ?cap))
                                              ((fuel ?a ?fuel))
                                              ((fuel ?a ?cap))
                                              0.001)}

                  {:canon/pos 11
                   :method/name "zeno.zoom"
                   :canon/code '(:method (zoom ?a ?c1 ?c2)
                                          ((write-time ?a ?astart)
                                           (read-time ?a ?pmax)
                                           (distance ?c1 ?c2 ?dist)
                                           (fuel ?a ?fuel)
                                           (fast-burn ?a ?burn)
                                           (eval (>= ?fuel (* ?dist ?burn)))
                                           (assign ?duration 1)
                                           (assign ?start (max ?pmax ?astart))
                                           (assign ?end (+ ?start ?duration 0.010)))
                                          ((!!ra ((write-time ?a ?astart)
                                                  (read-time ?a ?pmax))
                                                 ((read-time ?a 0) (write-time ?a ?end)))
                                           (:immediate !zoom ?a ?c1 ?c2 ?start ?duration)
                                           (:immediate !!cost ?end)))}

                  {:canon/pos 12
                   :operator/name "zeno.!zoom"
                   :canon/code '(:operator (!zoom ?a ?c1 ?c2 ?start ?duration)
                                              ((aircraft ?a)
                                               (city ?c1)
                                               (city ?c2)
                                               (onboard ?a ?num)
                                               (zoom-limit ?a ?limit)
                                               (eval (<= ?num ?limit))
                                               (at ?a ?c1)
                                               (distance ?c1 ?c2 ?dist)
                                               (fast-burn ?a ?burn)
                                               (total-fuel-used ?total-fuel)
                                               (assign ?new-total (+ ?total-fuel (* ?dist ?burn)))
                                               (fuel ?a ?fuel)
                                               (assign ?new-fuel (- ?fuel (* ?dist ?burn))))
                                              ((at ?a ?c1)
                                               (total-fuel-used ?total-fuel)
                                               (fuel ?a ?fuel))
                                              ((at ?a ?c2)
                                               (total-fuel-used ?new-total)
                                               (fuel ?a ?new-fuel))
                                              0.001)}

                  {:canon/pos 13
                   :method/name "zeno.fly"
                   :canon/code '(:method (fly ?a ?c1 ?c2)
                                          ((write-time ?a ?astart)
                                           (read-time ?a ?pmax)
                                           (distance ?c1 ?c2 ?dist)
                                           (fuel ?a ?fuel)
                                           (slow-burn ?a ?burn)
                                           (eval (>= ?fuel (* ?dist ?burn)))
                                           (assign ?duration 1)
                                           (assign ?start (max ?pmax ?astart))
                                           (assign ?end (+ ?start ?duration 0.010)))
                                          ((!!ra ((write-time ?a ?astart)
                                                  (read-time ?a ?pmax))
                                                 ((read-time ?a 0)
                                                  (write-time ?a ?end)))
                                           (:immediate !fly ?a ?c1 ?c2 ?start ?duration)
                                           (:immediate !!cost ?end)))}

                  {:canon/pos 14
                   :operator/name "zeno.!fly"
                   :canon/code '(:operator (!fly ?a ?c1 ?c2 ?start ?duration)
                                              ((aircraft ?a)
                                               (city ?c1)
                                               (city ?c2)
                                               (at ?a ?c1)
                                               (distance ?c1 ?c2 ?dist)
                                               (slow-burn ?a ?burn)
                                               (total-fuel-used ?total-fuel)
                                               (assign ?new-total (+ ?total-fuel (* ?dist ?burn)))
                                               (fuel ?a ?fuel)
                                               (assign ?new-fuel (- ?fuel (* ?dist ?burn))))
                                              ((at ?a ?c1)
                                               (total-fuel-used ?total-fuel)
                                               (fuel ?a ?fuel))
                                              ((at ?a ?c2)
                                               (total-fuel-used ?new-total)
                                               (fuel ?a ?new-fuel))
                                              0.001)}

                  {:canon/pos 15
                   :operator/name "zeno.!!preprocessing"
                   :canon/code '(:operator (!!preprocessing ?problem-name)
                                              ((totaltime-coeff ?tc)
                                               (fuelused-coeff ?fc)
                                               (eval (setf *tc* ?tc))
                                               (eval (setf *fc* ?fc)))
                                              ()
                                              ()
                                              0)}

                  {:canon/pos 16
                   :operator/name "zeno.!!assert"
                   :canon/code '(:operator (!!assert ?g)
                                              ()
                                              ()
                                              ?g
                                              0)}

                  {:canon/pos 17
                   :operator/name "zeno.!!ra"
                   :canon/code '(:operator (!!ra ?D ?A)
                                              ()
                                              ?D
                                              ?A
                                              0)}

                  {:canon/pos 18
                   :method/name "zeno.transport-person.Case1"
                   :canon/code '(:method (transport-person ?p ?c)
                                          Case1 ((at ?p ?c)) ())}

                  {:canon/pos 19
                   :method/name "zeno.transport-person.Case2"
                   :canon/code '(:method (transport-person ?p ?c2)
                                          Case2 (:sort-by ?num >
                                                          ((at ?p ?c1)
                                                           (at ?a ?c1)
                                                           (aircraft ?a)
                                                           (onboard ?a ?num)))  ((!!assert ((dest ?a ?c1)))
                                                                                 (:immediate board ?p ?a ?c1)
                                                                                 (!!assert ((dest ?a ?c2)))
                                                                                 (:immediate upper-move-aircraft-no-style ?a ?c2)
                                                                                 (:immediate debark ?p ?a ?c2)))}
                  {:canon/pos 20
                   :method/name "zeno.transport-person.Case3"
                   :canon/code '(:method (transport-person ?p ?c2)
                                          Case3 (:sort-by ?cost <
                                                          ((at ?p ?c1)
                                                           (aircraft ?a)
                                                           (at ?a ?c3)
                                                           (different ?c1 ?c3)
                                                           (forall (?c) ((dest ?a ?c))
                                                                   ((same ?c ?c1)))
                                                           (imply ((different ?c3 ?c1))
                                                                  (not (possible-person-in ?c3)))
                                                           (travel-cost-info ?a ?c3 ?c1 ?cost ?style)))  ((!!assert ((dest ?a ?c1)))
                                                                                                          (:immediate upper-move-aircraft ?a ?c1 ?style)
                                                                                                          (:immediate board ?p ?a ?c1)
                                                                                                          (!!assert ((dest ?a ?c2)))
                                                                                                          (:immediate upper-move-aircraft-no-style ?a ?c2)
                                                                                                          (:immediate debark ?p ?a ?c2)))}

                  {:canon/pos 21
                   :method/name "zeno.upper-move-aircraft"
                   :canon/code '(:method (upper-move-aircraft ?a ?c ?style)
                                          Case1 ((at ?a ?c)) ()
                                          Case2 ((at ?a ?somecity))     ((move-aircraft ?a ?somecity ?c ?style)))}

                  {:canon/pos 22
                   :method/name "zeno.upper-move-aircraft-no-style"
                   :canon/code '(:method (upper-move-aircraft-no-style ?a ?c)
                                          Case1 ((at ?a ?c)) ()
                                          Case2 (:sort-by ?cost <
                                                          ((at ?a ?somecity)
                                                           (travel-cost-info ?a ?somecity ?c ?cost ?style)))   ((move-aircraft ?a ?somecity ?c ?style)))}

                  {:canon/pos 23
                   :axiom/name "zeno.travel-cost-info.slow"
                   :canon/code '(:- (travel-cost-info ?a ?from ?to ?cost slow)
                                    CASE1 ((capacity ?a ?cap)
                                           (distance ?from ?to ?dist)
                                           (slow-burn ?a ?burn)
                                           (eval (< ?cap (* ?dist ?burn)))
                                           (assign ?cost most-positive-fixnum))
                                    CASE2 ((distance ?from ?to ?dist)
                                           (fuel ?a ?fuel)
                                           (slow-burn ?a ?burn)
                                           (eval (>= ?fuel (* ?dist ?burn)))
                                           (assign ?cost (float (/ (+ *tc* (* *fc* (* ?dist ?burn))) 1))))
                                    CASE3 ((capacity ?a ?cap)
                                           (distance ?from ?to ?dist)
                                           (slow-burn ?a ?burn)
                                           (assign ?cost (float (/ (+ (* *tc* 2) (* *fc* (* ?dist ?burn))) 1)))))}

                  {:canon/pos 24
                   :axiom/name "zeno.travel-cost-info.fast"
                   :canon/code '(:- (travel-cost-info ?a ?from ?to ?cost fast)
                                    CASE1 ((capacity ?a ?cap)
                                           (distance ?from ?to ?dist)
                                           (fast-burn ?a ?burn)
                                           (eval (< ?cap (* ?dist ?burn)))
                                           (assign ?cost most-positive-fixnum))
                                    CASE2 ((distance ?from ?to ?dist)
                                           (fuel ?a ?fuel)
                                           (zoom-limit ?a ?limit)
                                           (onboard ?a ?num)
                                           (eval (< ?num ?limit))
                                           (fast-burn ?a ?burn)
                                           (eval (>= ?fuel (* ?dist ?burn)))
                                           (assign ?cost (float (/ (+ *tc* (* *fc* (* ?dist ?burn))) 1))))
                                    CASE3 ((capacity ?a ?cap)
                                           (distance ?from ?to ?dist)
                                           (fast-burn ?a ?burn)
                                           (zoom-limit ?a ?limit)
                                           (onboard ?a ?num)
                                           (eval (< ?num ?limit))
                                           (assign ?cost (float (/ (+ (* *tc* 2) (* *fc* (* ?dist ?burn))) 1)))))}

                  {:canon/pos 25
                   :method/name "zeno.move-aircraft-slow"
                   :canon/code '(:method (move-aircraft ?a ?c1 ?c2 slow)
                                          ((fuel ?a ?fuel)
                                           (distance ?c1 ?c2 ?dist)
                                           (slow-burn ?a ?burn)
                                           (eval (> ?fuel (* ?dist ?burn))))
                                          ;; multiple task lists
                                          ((fly ?a ?c1 ?c2))
                                          ()
                                          ((refuel ?a ?c1)
                                           (:immediate fly ?a ?c1 ?c2)))}

                  {:canon/pos 26
                   :method/name "zeno.move-aircraft-fast"
                   :canon/code '(:method (move-aircraft ?a ?c1 ?c2 fast)
                                          ((fuel ?a ?fuel)
                                           (distance ?c1 ?c2 ?dist)
                                           (fast-burn ?a ?burn)
                                           (eval (> ?fuel (* ?dist ?burn))))
                                          ;; multiple task lists
                                          ((zoom ?a ?c1 ?c2))
                                          ()
                                          ((refuel ?a ?c1)
                                           (:immediate zoom ?a ?c1 ?c2)))}

                  {:canon/pos 27
                   :method/name "zeno.transport-aircraft"
                   :canon/code '(:method (transport-aircraft ?a ?c)
                                          ((not (no-use ?a)))
                                          ((!!assert ((no-use ?a)))
                                           (:immediate upper-move-aircraft-no-style ?a ?c)
                                           (:immediate !!ra ((no-use ?a)) ())))}]})

(defn write-zeno
  "Use this to write the above structure (zeno) to the DB."
  []
  (->> zeno
       shop/canon2db
       shop/write-obj))

(def diag (atom []))

(defn compare-domains
  "Check each of :domain/elems in two domains, ensure each are equal."
  [d1 d2]
  (reset! diag [])
  (letfn [(get-elem [n d] (some #(when (== (:canon/pos %) n) %) (:domain/elems d)))]
    (let [ok? (atom true)]
      (doseq [pos (->> d1 :domain/elems (map :canon/pos) sort)]
        (let [s1  (get-elem pos d1)
              s2  (get-elem pos d2)]
          (when-not (= s1 s2)
            (swap! diag conj [s1 s2])
            (reset! ok? false)
            (log/info "\n\nNot equal: \n " s1 "\n " s2))))
      @ok?)))

(defn tryme []
  (compare-domains zeno (shop/db2canon "ZENO")))

(deftest big-round-trip
  (testing "testing that a SHOP form can be stored and recovered."
    (is
     (do (write-zeno) ; Do it twice to ensure updating, not inserting.
         (write-zeno)
         (compare-domains zeno (shop/db2canon "ZENO"))))))
