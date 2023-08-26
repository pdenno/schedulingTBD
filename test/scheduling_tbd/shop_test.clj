(ns scheduling-tbd.shop-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.test :refer [deftest is testing]]
   [datahike.api                 :as d]
   [scheduling-tbd.shop :as shop :refer [rewrite]]
   [scheduling-tbd.llm  :as llm]
   [scheduling-tbd.util :as util]))

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
  '(defdomain ZENOTRAVEL
     ((:- (same ?x ?x) ())
      (:- (different ?x ?y) ((not (same ?x ?y))))
      (:- (possible-person-in ?city)
          ((person ?p)
           (at ?p ?city)
           (goal ?p ?city2)
           (different ?city2 ?city)))

      (:operator (!!cost ?end)

                 ((maxtime ?max) ; Since ?max isn't bound in the head, I supose this is a query.
                  (assign ?newmax (eval (if (< ?max ?end) ?end ?max))))

                 ((maxtime ?max))
                 ((maxtime ?newmax))
                 (- ?newmax ?max))

      (:method (board ?p ?a ?c)
               ((write-time ?a ?start))
               ((!board ?p ?a ?c ?start 1)
                (:immediate !!cost (call + ?start 1))))

      (:operator (!board ?p ?a ?c ?start ?duration)
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
                 0.001)

      (:method (debark ?p ?a ?c)
               ((write-time ?a ?start))
               ((!debark ?p ?a ?c ?start 1)
                (:immediate !!cost (call + ?start 1))))

      (:operator (!debark ?p ?a ?c ?start ?duration)
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
                 0.001)

      (:method (refuel ?a ?c)
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
                (:immediate !!cost ?end)))

      (:operator (!refuel ?a ?c ?start ?duration)
                 ((aircraft ?a)
                  (city ?c)
                  (at ?a ?c)
                  (fuel ?a ?fuel)
                  (capacity ?a ?cap))
                 ((fuel ?a ?fuel))
                 ((fuel ?a ?cap))
                 0.001)

      (:method (zoom ?a ?c1 ?c2)
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
                (:immediate !!cost ?end)))

      (:operator (!zoom ?a ?c1 ?c2 ?start ?duration)
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
                 0.001)

      (:method (fly ?a ?c1 ?c2)
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
                (:immediate !!cost ?end)))

      (:operator (!fly ?a ?c1 ?c2 ?start ?duration)
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
                 0.001)

      (:operator (!!preprocessing ?problem-name)
                 ((totaltime-coeff ?tc)
                  (fuelused-coeff ?fc)
                  (eval (setf *tc* ?tc))
                  (eval (setf *fc* ?fc)))
                 ()
                 ()
                 0)

      (:operator (!!assert ?g )
                 ()
                 ()
                 ?g
                 0)

      (:operator (!!ra ?D ?A )
                 ()
                 ?D
                 ?A
                 0)

      ;; Main Methods
      (:method (transport-person ?p ?c)
               Case1 ((at ?p ?c)) ())

      (:method (transport-person ?p ?c2)
               Case2 (:sort-by ?num >
                               ((at ?p ?c1)
                                (at ?a ?c1)
                                (aircraft ?a)
                                (onboard ?a ?num)))  ((!!assert ((dest ?a ?c1)))
                                                      (:immediate board ?p ?a ?c1)
                                                      (!!assert ((dest ?a ?c2)))
                                                      (:immediate upper-move-aircraft-no-style ?a ?c2)
                                                      (:immediate debark ?p ?a ?c2)))

      (:method (transport-person ?p ?c2)
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
                                                                               (:immediate debark ?p ?a ?c2)))

      (:method (upper-move-aircraft ?a ?c ?style)
               Case1 ((at ?a ?c)) ()
               Case2 ((at ?a ?somecity))     ((move-aircraft ?a ?somecity ?c ?style)))

      (:method (upper-move-aircraft-no-style ?a ?c)
               Case1 ((at ?a ?c)) ()
               Case2 (:sort-by ?cost <
                               ((at ?a ?somecity)
                                (travel-cost-info ?a ?somecity ?c ?cost ?style)))   ((move-aircraft ?a ?somecity ?c ?style)))

      (:- (travel-cost-info ?a ?from ?to ?cost slow)
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
                 (assign ?cost (float (/ (+ (* *tc* 2) (* *fc* (* ?dist ?burn))) 1)))))

      (:- (travel-cost-info ?a ?from ?to ?cost fast)
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
                 (assign ?cost (float (/ (+ (* *tc* 2) (* *fc* (* ?dist ?burn))) 1)))))

      (:method (move-aircraft ?a ?c1 ?c2 slow)
               ((fuel ?a ?fuel)
                (distance ?c1 ?c2 ?dist)
                (slow-burn ?a ?burn)
                (eval (> ?fuel (* ?dist ?burn))))
               ;; multiple task lists
               ((fly ?a ?c1 ?c2))
               ()
               ((refuel ?a ?c1)
                (:immediate fly ?a ?c1 ?c2)))

      (:method (move-aircraft ?a ?c1 ?c2 fast)
               ((fuel ?a ?fuel)
                (distance ?c1 ?c2 ?dist)
                (fast-burn ?a ?burn)
                (eval (> ?fuel (* ?dist ?burn))))
               ;; multiple task lists
               ((zoom ?a ?c1 ?c2))
               ()
               ((refuel ?a ?c1)
                (:immediate zoom ?a ?c1 ?c2)))

      (:method (transport-aircraft ?a ?c)
               ((not (no-use ?a)))
               ((!!assert ((no-use ?a)))
                (:immediate upper-move-aircraft-no-style ?a ?c)
                (:immediate !!ra ((no-use ?a)) ()))))))

(def zeno-for-db
  '#:domain{:name ZENOTRAVEL,
         :elems
         [#:axiom{:head #:atom{:predicate same, :roles [#:atom{:role ?x, :role-pos 1} #:atom{:role ?x, :role-pos 2}]},
                  :rhs [#:axiom{:case-pos 1}]}
          #:axiom{:head #:atom{:predicate different, :roles [#:atom{:role ?x, :role-pos 1} #:atom{:role ?y, :role-pos 2}]},
                  :rhs
                  [#:axiom{:case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [{:atom/predicate same,
                                           :atom/roles [#:atom{:role ?x, :role-pos 1} #:atom{:role ?y, :role-pos 2}],
                                           :exp/negated? true}]}}]}
          #:axiom{:head #:atom{:predicate possible-person-in, :roles [#:atom{:role ?city, :role-pos 1}]},
                  :rhs
                  [#:axiom{:case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate person, :roles [#:atom{:role ?p, :role-pos 1}]}
                                          #:atom{:predicate at, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?city, :role-pos 2}]}
                                          #:atom{:predicate goal, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?city2, :role-pos 2}]}
                                          #:atom{:predicate different,
                                                 :roles [#:atom{:role ?city2, :role-pos 1} #:atom{:role ?city, :role-pos 2}]}]}}]}
          #:op{:head #:atom{:predicate !!cost, :roles [#:atom{:role ?end, :role-pos 1}]},
               :preconds
               [#:precond{:exp #:atom{:predicate maxtime, :roles [#:atom{:role ?max, :role-pos 1}]}, :pos 1}
                #:precond{:exp
                          #:assign{:var ?newmax,
                                   :exp
                                   #:s-exp{:args
                                           [{:arg/pos 1,
                                             :s-exp/args
                                             [{:arg/pos 1,
                                               :s-exp/args [#:arg{:val #:box{:sym ?max}, :pos 1} #:arg{:val #:box{:sym ?end}, :pos 2}],
                                               :s-exp/fn-ref '<}
                                              #:arg{:val #:box{:sym ?end}, :pos 2}
                                              #:arg{:val #:box{:sym ?max}, :pos 3}],
                                             :s-exp/fn-ref 'if}],
                                           :fn-ref 'eval}},
                          :pos 2}],
               :d-list [{:atom/predicate maxtime, :atom/roles [#:atom{:role ?max, :role-pos 1}], :op/pos 1}],
               :a-list [{:atom/predicate maxtime, :atom/roles [#:atom{:role ?newmax, :role-pos 1}], :op/pos 1}],
               :cost #:s-exp{:args [#:arg{:val #:box{:sym ?newmax}, :pos 1} #:arg{:val #:box{:sym ?max}, :pos 2}], :fn-ref '-}}
          #:method{:head
                   #:atom{:predicate board,
                          :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?a, :role-pos 2} #:atom{:role ?c, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?start, :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !board,
                                           :atom/roles
                                           [{:atom/role ?p, :role/role-pos 1}
                                            {:atom/role ?a, :role/role-pos 2}
                                            {:atom/role ?c, :role/role-pos 3}
                                            {:atom/role ?start, :role/role-pos 4}
                                            {:atom/role 1, :role/role-pos 5}],
                                           :task/pos 1}
                                          {:atom/predicate !!cost,
                                           :atom/roles
                                           [{:atom/role
                                             #:eval{:form
                                                    #:s-exp{:args
                                                            [#:arg{:val #:box{:sym +}, :pos 1}
                                                             #:arg{:val #:box{:sym ?start}, :pos 2}
                                                             #:arg{:val #:box{:num 1}, :pos 3}],
                                                            :fn-ref 'call}},
                                             :role/role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 2}]}}]}
          #:op{:head
               #:atom{:predicate !board,
                      :roles
                      [#:atom{:role ?p, :role-pos 1}
                       #:atom{:role ?a, :role-pos 2}
                       #:atom{:role ?c, :role-pos 3}
                       #:atom{:role ?start, :role-pos 4}
                       #:atom{:role ?duration, :role-pos 5}]},
               :preconds
               [#:precond{:exp #:atom{:predicate person, :roles [#:atom{:role ?p, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role ?a, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role ?c, :role-pos 1}]}, :pos 3}
                #:precond{:exp #:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]}, :pos 4}
                #:precond{:exp #:atom{:predicate at, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c, :role-pos 2}]}, :pos 5}
                #:precond{:exp #:atom{:predicate onboard, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?num, :role-pos 2}]}, :pos 6}
                #:precond{:exp #:atom{:predicate read-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?pmax, :role-pos 2}]},
                          :pos 7}
                #:precond{:exp
                          #:assign{:var ?new-num,
                                   :exp
                                   #:s-exp{:args [#:arg{:val #:box{:sym ?num}, :pos 1} #:arg{:val #:box{:num 1}, :pos 2}], :fn-ref '+}},
                          :pos 8}
                #:precond{:exp
                          #:assign{:var ?newpmax,
                                   :exp
                                   #:s-exp{:args
                                           [#:arg{:val #:box{:sym ?pmax}, :pos 1}
                                            {:arg/pos 2,
                                             :s-exp/args
                                             [#:arg{:val #:box{:sym ?start}, :pos 1}
                                              #:arg{:val #:box{:sym ?duration}, :pos 2}
                                              #:arg{:val #:box{:num 0.01}, :pos 3}],
                                             :s-exp/fn-ref '+}],
                                           :fn-ref 'max}},
                          :pos 9}],
               :d-list
               [{:atom/predicate onboard, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?num, :role-pos 2}], :op/pos 1}
                {:atom/predicate read-time, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?pmax, :role-pos 2}], :op/pos 2}
                {:atom/predicate at, :atom/roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c, :role-pos 2}], :op/pos 3}
                {:atom/predicate dest, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}], :op/pos 4}],
               :a-list
               [{:atom/predicate onboard, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?new-num, :role-pos 2}], :op/pos 1}
                {:atom/predicate read-time, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?newpmax, :role-pos 2}], :op/pos 2}
                {:atom/predicate in, :atom/roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?a, :role-pos 2}], :op/pos 3}],
               :cost #:box{:num 0.001}}
          #:method{:head
                   #:atom{:predicate debark,
                          :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?a, :role-pos 2} #:atom{:role ?c, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?start, :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !debark,
                                           :atom/roles
                                           [{:atom/role ?p, :role/role-pos 1}
                                            {:atom/role ?a, :role/role-pos 2}
                                            {:atom/role ?c, :role/role-pos 3}
                                            {:atom/role ?start, :role/role-pos 4}
                                            {:atom/role 1, :role/role-pos 5}],
                                           :task/pos 1}
                                          {:atom/predicate !!cost,
                                           :atom/roles
                                           [{:atom/role
                                             #:eval{:form
                                                    #:s-exp{:args
                                                            [#:arg{:val #:box{:sym +}, :pos 1}
                                                             #:arg{:val #:box{:sym ?start}, :pos 2}
                                                             #:arg{:val #:box{:num 1}, :pos 3}],
                                                            :fn-ref 'call}},
                                             :role/role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 2}]}}]}
          #:op{:head
               #:atom{:predicate !debark,
                      :roles
                      [#:atom{:role ?p, :role-pos 1}
                       #:atom{:role ?a, :role-pos 2}
                       #:atom{:role ?c, :role-pos 3}
                       #:atom{:role ?start, :role-pos 4}
                       #:atom{:role ?duration, :role-pos 5}]},
               :preconds
               [#:precond{:exp #:atom{:predicate person, :roles [#:atom{:role ?p, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role ?a, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role ?c, :role-pos 1}]}, :pos 3}
                #:precond{:exp #:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]}, :pos 4}
                #:precond{:exp #:atom{:predicate in, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?a, :role-pos 2}]}, :pos 5}
                #:precond{:exp #:atom{:predicate onboard, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?num, :role-pos 2}]}, :pos 6}
                #:precond{:exp #:atom{:predicate read-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?pmax, :role-pos 2}]},
                          :pos 7}
                #:precond{:exp
                          #:assign{:var ?new-num,
                                   :exp
                                   #:s-exp{:args [#:arg{:val #:box{:sym ?num}, :pos 1} #:arg{:val #:box{:num 1}, :pos 2}], :fn-ref '-}},
                          :pos 8}
                #:precond{:exp
                          #:assign{:var ?newpmax,
                                   :exp
                                   #:s-exp{:args
                                           [#:arg{:val #:box{:sym ?pmax}, :pos 1}
                                            {:arg/pos 2,
                                             :s-exp/args
                                             [#:arg{:val #:box{:sym ?start}, :pos 1}
                                              #:arg{:val #:box{:sym ?duration}, :pos 2}
                                              #:arg{:val #:box{:num 0.01}, :pos 3}],
                                             :s-exp/fn-ref '+}],
                                           :fn-ref 'max}},
                          :pos 9}],
               :d-list
               [{:atom/predicate onboard, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?num, :role-pos 2}], :op/pos 1}
                {:atom/predicate read-time, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?pmax, :role-pos 2}], :op/pos 2}
                {:atom/predicate in, :atom/roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?a, :role-pos 2}], :op/pos 3}
                {:atom/predicate dest, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}], :op/pos 4}],
               :a-list
               [{:atom/predicate onboard, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?new-num, :role-pos 2}], :op/pos 1}
                {:atom/predicate read-time, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?newpmax, :role-pos 2}], :op/pos 2}
                {:atom/predicate at, :atom/roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c, :role-pos 2}], :op/pos 3}],
               :cost #:box{:num 0.001}}
          #:method{:head #:atom{:predicate refuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?start, :role-pos 2}]}
                              #:atom{:predicate read-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?pmax, :role-pos 2}]}
                              #:atom{:predicate capacity, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?cap, :role-pos 2}]}
                              #:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms [#:list{:val >, :pos 1} #:list{:val ?cap, :pos 2} #:list{:val ?fuel, :pos 3}]},
                                             :role-pos 1}]}
                              #:atom{:predicate assign, :roles [#:atom{:role ?duration, :role-pos 1} #:atom{:role 1, :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role ?end, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val +, :pos 1}
                                                     #:list{:val ?start, :pos 2}
                                                     #:list{:val ?duration, :pos 3}
                                                     #:list{:val 0.01, :pos 4}]},
                                             :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role ?newpmax, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val max, :pos 1} #:list{:val ?pmax, :pos 2} #:list{:val ?end, :pos 3}]},
                                             :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!ra,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val read-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?pmax, :pos 3}]},
                                                            :pos 1}]},
                                             :role/role-pos 1}
                                            {:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val read-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?newpmax, :pos 3}]},
                                                            :pos 1}]},
                                             :role/role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate !refuel,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?c, :role/role-pos 2}
                                            {:atom/role ?start, :role/role-pos 3}
                                            {:atom/role ?duration, :role/role-pos 4}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!cost,
                                           :atom/roles [{:atom/role ?end, :role/role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 3}]}}]}
          #:op{:head
               #:atom{:predicate !refuel,
                      :roles
                      [#:atom{:role ?a, :role-pos 1}
                       #:atom{:role ?c, :role-pos 2}
                       #:atom{:role ?start, :role-pos 3}
                       #:atom{:role ?duration, :role-pos 4}]},
               :preconds
               [#:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role ?a, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role ?c, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]}, :pos 3}
                #:precond{:exp #:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}, :pos 4}
                #:precond{:exp #:atom{:predicate capacity, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?cap, :role-pos 2}]},
                          :pos 5}],
               :d-list [{:atom/predicate fuel, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}], :op/pos 1}],
               :a-list [{:atom/predicate fuel, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?cap, :role-pos 2}], :op/pos 1}],
               :cost #:box{:num 0.001}}
          #:method{:head
                   #:atom{:predicate zoom,
                          :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c1, :role-pos 2} #:atom{:role ?c2, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?astart, :role-pos 2}]}
                              #:atom{:predicate read-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?pmax, :role-pos 2}]}
                              #:atom{:predicate distance,
                                     :roles
                                     [#:atom{:role ?c1, :role-pos 1} #:atom{:role ?c2, :role-pos 2} #:atom{:role ?dist, :role-pos 3}]}
                              #:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}
                              #:atom{:predicate fast-burn, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val >=, :pos 1}
                                                     #:list{:val ?fuel, :pos 2}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val *, :pos 1}
                                                                    #:list{:val ?dist, :pos 2}
                                                                    #:list{:val ?burn, :pos 3}]},
                                                            :pos 3}]},
                                             :role-pos 1}]}
                              #:atom{:predicate assign, :roles [#:atom{:role ?duration, :role-pos 1} #:atom{:role 1, :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role ?start, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val max, :pos 1} #:list{:val ?pmax, :pos 2} #:list{:val ?astart, :pos 3}]},
                                             :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role ?end, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val +, :pos 1}
                                                     #:list{:val ?start, :pos 2}
                                                     #:list{:val ?duration, :pos 3}
                                                     #:list{:val 0.01, :pos 4}]},
                                             :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!ra,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val write-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?astart, :pos 3}]},
                                                            :pos 1}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val read-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?pmax, :pos 3}]},
                                                            :pos 2}]},
                                             :role/role-pos 1}
                                            {:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val read-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val 0, :pos 3}]},
                                                            :pos 1}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val write-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?end, :pos 3}]},
                                                            :pos 2}]},
                                             :role/role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate !zoom,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?c1, :role/role-pos 2}
                                            {:atom/role ?c2, :role/role-pos 3}
                                            {:atom/role ?start, :role/role-pos 4}
                                            {:atom/role ?duration, :role/role-pos 5}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!cost,
                                           :atom/roles [{:atom/role ?end, :role/role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 3}]}}]}
          #:op{:head
               #:atom{:predicate !zoom,
                      :roles
                      [#:atom{:role ?a, :role-pos 1}
                       #:atom{:role ?c1, :role-pos 2}
                       #:atom{:role ?c2, :role-pos 3}
                       #:atom{:role ?start, :role-pos 4}
                       #:atom{:role ?duration, :role-pos 5}]},
               :preconds
               [#:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role ?a, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role ?c1, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role ?c2, :role-pos 1}]}, :pos 3}
                #:precond{:exp #:atom{:predicate onboard, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?num, :role-pos 2}]}, :pos 4}
                #:precond{:exp #:atom{:predicate zoom-limit, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?limit, :role-pos 2}]},
                          :pos 5}
                #:precond{:exp
                          #:eval{:exp
                                 #:s-exp{:args
                                         [{:arg/pos 1,
                                           :s-exp/args [#:arg{:val #:box{:sym ?num}, :pos 1} #:arg{:val #:box{:sym ?limit}, :pos 2}],
                                           :s-exp/fn-ref '<=}],
                                         :fn-ref 'eval}},
                          :pos 6}
                #:precond{:exp #:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c1, :role-pos 2}]}, :pos 7}
                #:precond{:exp
                          #:atom{:predicate distance,
                                 :roles [#:atom{:role ?c1, :role-pos 1} #:atom{:role ?c2, :role-pos 2} #:atom{:role ?dist, :role-pos 3}]},
                          :pos 8}
                #:precond{:exp #:atom{:predicate fast-burn, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]},
                          :pos 9}
                #:precond{:exp #:atom{:predicate total-fuel-used, :roles [#:atom{:role ?total-fuel, :role-pos 1}]}, :pos 10}
                #:precond{:exp
                          #:assign{:var ?new-total,
                                   :exp
                                   #:s-exp{:args
                                           [#:arg{:val #:box{:sym ?total-fuel}, :pos 1}
                                            {:arg/pos 2,
                                             :s-exp/args [#:arg{:val #:box{:sym ?dist}, :pos 1} #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                             :s-exp/fn-ref '*}],
                                           :fn-ref '+}},
                          :pos 11}
                #:precond{:exp #:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}, :pos 12}
                #:precond{:exp
                          #:assign{:var ?new-fuel,
                                   :exp
                                   #:s-exp{:args
                                           [#:arg{:val #:box{:sym ?fuel}, :pos 1}
                                            {:arg/pos 2,
                                             :s-exp/args [#:arg{:val #:box{:sym ?dist}, :pos 1} #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                             :s-exp/fn-ref '*}],
                                           :fn-ref '-}},
                          :pos 13}],
               :d-list
               [{:atom/predicate at, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c1, :role-pos 2}], :op/pos 1}
                {:atom/predicate total-fuel-used, :atom/roles [#:atom{:role ?total-fuel, :role-pos 1}], :op/pos 2}
                {:atom/predicate fuel, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}], :op/pos 3}],
               :a-list
               [{:atom/predicate at, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c2, :role-pos 2}], :op/pos 1}
                {:atom/predicate total-fuel-used, :atom/roles [#:atom{:role ?new-total, :role-pos 1}], :op/pos 2}
                {:atom/predicate fuel, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?new-fuel, :role-pos 2}], :op/pos 3}],
               :cost #:box{:num 0.001}}
          #:method{:head
                   #:atom{:predicate fly,
                          :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c1, :role-pos 2} #:atom{:role ?c2, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate write-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?astart, :role-pos 2}]}
                              #:atom{:predicate read-time, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?pmax, :role-pos 2}]}
                              #:atom{:predicate distance,
                                     :roles
                                     [#:atom{:role ?c1, :role-pos 1} #:atom{:role ?c2, :role-pos 2} #:atom{:role ?dist, :role-pos 3}]}
                              #:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}
                              #:atom{:predicate slow-burn, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val >=, :pos 1}
                                                     #:list{:val ?fuel, :pos 2}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val *, :pos 1}
                                                                    #:list{:val ?dist, :pos 2}
                                                                    #:list{:val ?burn, :pos 3}]},
                                                            :pos 3}]},
                                             :role-pos 1}]}
                              #:atom{:predicate assign, :roles [#:atom{:role ?duration, :role-pos 1} #:atom{:role 1, :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role ?start, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val max, :pos 1} #:list{:val ?pmax, :pos 2} #:list{:val ?astart, :pos 3}]},
                                             :role-pos 2}]}
                              #:atom{:predicate assign,
                                     :roles
                                     [#:atom{:role ?end, :role-pos 1}
                                      #:atom{:role
                                             #:list{:terms
                                                    [#:list{:val +, :pos 1}
                                                     #:list{:val ?start, :pos 2}
                                                     #:list{:val ?duration, :pos 3}
                                                     #:list{:val 0.01, :pos 4}]},
                                             :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!ra,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val write-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?astart, :pos 3}]},
                                                            :pos 1}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val read-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?pmax, :pos 3}]},
                                                            :pos 2}]},
                                             :role/role-pos 1}
                                            {:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val read-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val 0, :pos 3}]},
                                                            :pos 1}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val write-time, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?end, :pos 3}]},
                                                            :pos 2}]},
                                             :role/role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate !fly,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?c1, :role/role-pos 2}
                                            {:atom/role ?c2, :role/role-pos 3}
                                            {:atom/role ?start, :role/role-pos 4}
                                            {:atom/role ?duration, :role/role-pos 5}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!cost,
                                           :atom/roles [{:atom/role ?end, :role/role-pos 1}],
                                           :task/immediate? true,
                                           :task/pos 3}]}}]}
          #:op{:head
               #:atom{:predicate !fly,
                      :roles
                      [#:atom{:role ?a, :role-pos 1}
                       #:atom{:role ?c1, :role-pos 2}
                       #:atom{:role ?c2, :role-pos 3}
                       #:atom{:role ?start, :role-pos 4}
                       #:atom{:role ?duration, :role-pos 5}]},
               :preconds
               [#:precond{:exp #:atom{:predicate aircraft, :roles [#:atom{:role ?a, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role ?c1, :role-pos 1}]}, :pos 2}
                #:precond{:exp #:atom{:predicate city, :roles [#:atom{:role ?c2, :role-pos 1}]}, :pos 3}
                #:precond{:exp #:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c1, :role-pos 2}]}, :pos 4}
                #:precond{:exp
                          #:atom{:predicate distance,
                                 :roles [#:atom{:role ?c1, :role-pos 1} #:atom{:role ?c2, :role-pos 2} #:atom{:role ?dist, :role-pos 3}]},
                          :pos 5}
                #:precond{:exp #:atom{:predicate slow-burn, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]},
                          :pos 6}
                #:precond{:exp #:atom{:predicate total-fuel-used, :roles [#:atom{:role ?total-fuel, :role-pos 1}]}, :pos 7}
                #:precond{:exp
                          #:assign{:var ?new-total,
                                   :exp
                                   #:s-exp{:args
                                           [#:arg{:val #:box{:sym ?total-fuel}, :pos 1}
                                            {:arg/pos 2,
                                             :s-exp/args [#:arg{:val #:box{:sym ?dist}, :pos 1} #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                             :s-exp/fn-ref '*}],
                                           :fn-ref '+}},
                          :pos 8}
                #:precond{:exp #:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}, :pos 9}
                #:precond{:exp
                          #:assign{:var ?new-fuel,
                                   :exp
                                   #:s-exp{:args
                                           [#:arg{:val #:box{:sym ?fuel}, :pos 1}
                                            {:arg/pos 2,
                                             :s-exp/args [#:arg{:val #:box{:sym ?dist}, :pos 1} #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                             :s-exp/fn-ref '*}],
                                           :fn-ref '-}},
                          :pos 10}],
               :d-list
               [{:atom/predicate at, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c1, :role-pos 2}], :op/pos 1}
                {:atom/predicate total-fuel-used, :atom/roles [#:atom{:role ?total-fuel, :role-pos 1}], :op/pos 2}
                {:atom/predicate fuel, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}], :op/pos 3}],
               :a-list
               [{:atom/predicate at, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c2, :role-pos 2}], :op/pos 1}
                {:atom/predicate total-fuel-used, :atom/roles [#:atom{:role ?new-total, :role-pos 1}], :op/pos 2}
                {:atom/predicate fuel, :atom/roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?new-fuel, :role-pos 2}], :op/pos 3}],
               :cost #:box{:num 0.001}}
          #:op{:head #:atom{:predicate !!preprocessing, :roles [#:atom{:role ?problem-name, :role-pos 1}]},
               :preconds
               [#:precond{:exp #:atom{:predicate totaltime-coeff, :roles [#:atom{:role ?tc, :role-pos 1}]}, :pos 1}
                #:precond{:exp #:atom{:predicate fuelused-coeff, :roles [#:atom{:role ?fc, :role-pos 1}]}, :pos 2}
                #:precond{:exp
                          #:eval{:exp
                                 #:s-exp{:args
                                         [{:arg/pos 1,
                                           :s-exp/args [#:arg{:val #:box{:sym *tc*}, :pos 1} #:arg{:val #:box{:sym ?tc}, :pos 2}],
                                           :s-exp/fn-ref 'setf}],
                                         :fn-ref 'eval}},
                          :pos 3}
                #:precond{:exp
                          #:eval{:exp
                                 #:s-exp{:args
                                         [{:arg/pos 1,
                                           :s-exp/args [#:arg{:val #:box{:sym *fc*}, :pos 1} #:arg{:val #:box{:sym ?fc}, :pos 2}],
                                           :s-exp/fn-ref 'setf}],
                                         :fn-ref 'eval}},
                          :pos 4}],
               :cost #:box{:num 0}}
          #:op{:head #:atom{:predicate !!assert, :roles [#:atom{:role ?g, :role-pos 1}]},
               :a-list [{:box/sym ?g, :op/pos 1}],
               :cost #:box{:num 0}}
          #:op{:head #:atom{:predicate !!ra, :roles [#:atom{:role ?D, :role-pos 1} #:atom{:role ?A, :role-pos 2}]},
               :d-list [{:box/sym ?D, :op/pos 1}],
               :a-list [{:box/sym ?A, :op/pos 1}],
               :cost #:box{:num 0}}
          #:method{:head #:atom{:predicate transport-person, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:case-name Case1,
                             :preconditions
                             [#:atom{:predicate at, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c, :role-pos 2}]}]}]}
          #:method{:head #:atom{:predicate transport-person, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c2, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:case-name Case2,
                             :preconditions
                             #:sort-by{:var ?num,
                                       :fn #:s-exp{:fn-ref >},
                                       :exps
                                       [#:atom{:predicate at, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c1, :role-pos 2}]}
                                        #:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c1, :role-pos 2}]}
                                        #:atom{:predicate aircraft, :roles [#:atom{:role ?a, :role-pos 1}]}
                                        #:atom{:predicate onboard,
                                               :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?num, :role-pos 2}]}]},
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!assert,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val dest, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?c1, :pos 3}]},
                                                            :pos 1}]},
                                             :role/role-pos 1}],
                                           :task/pos 1}
                                          {:atom/predicate board,
                                           :atom/roles
                                           [{:atom/role ?p, :role/role-pos 1}
                                            {:atom/role ?a, :role/role-pos 2}
                                            {:atom/role ?c1, :role/role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!assert,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val dest, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?c2, :pos 3}]},
                                                            :pos 1}]},
                                             :role/role-pos 1}],
                                           :task/pos 3}
                                          {:atom/predicate upper-move-aircraft-no-style,
                                           :atom/roles [{:atom/role ?a, :role/role-pos 1} {:atom/role ?c2, :role/role-pos 2}],
                                           :task/immediate? true,
                                           :task/pos 4}
                                          {:atom/predicate debark,
                                           :atom/roles
                                           [{:atom/role ?p, :role/role-pos 1}
                                            {:atom/role ?a, :role/role-pos 2}
                                            {:atom/role ?c2, :role/role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 5}]}}]}
          #:method{:head #:atom{:predicate transport-person, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c2, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:case-name Case3,
                             :preconditions
                             #:sort-by{:var ?cost,
                                       :fn #:s-exp{:fn-ref <},
                                       :exps
                                       [#:atom{:predicate at, :roles [#:atom{:role ?p, :role-pos 1} #:atom{:role ?c1, :role-pos 2}]}
                                        #:atom{:predicate aircraft, :roles [#:atom{:role ?a, :role-pos 1}]}
                                        #:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c3, :role-pos 2}]}
                                        #:atom{:predicate different,
                                               :roles [#:atom{:role ?c1, :role-pos 1} #:atom{:role ?c3, :role-pos 2}]}
                                        #:forall{:vars [?c],
                                                 :conditions
                                                 [#:atom{:predicate dest,
                                                         :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]}],
                                                 :consequences
                                                 [#:atom{:predicate same,
                                                         :roles [#:atom{:role ?c, :role-pos 1} #:atom{:role ?c1, :role-pos 2}]}]}
                                        #:imply{:condition
                                                #:conjunction{:terms
                                                              [#:atom{:predicate different,
                                                                      :roles
                                                                      [#:atom{:role ?c3, :role-pos 1} #:atom{:role ?c1, :role-pos 2}]}]},
                                                :implies
                                                {:atom/predicate possible-person-in,
                                                 :atom/roles [#:atom{:role ?c3, :role-pos 1}],
                                                 :exp/negated? true}}
                                        #:atom{:predicate travel-cost-info,
                                               :roles
                                               [#:atom{:role ?a, :role-pos 1}
                                                #:atom{:role ?c3, :role-pos 2}
                                                #:atom{:role ?c1, :role-pos 3}
                                                #:atom{:role ?cost, :role-pos 4}
                                                #:atom{:role ?style, :role-pos 5}]}]},
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!assert,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val dest, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?c1, :pos 3}]},
                                                            :pos 1}]},
                                             :role/role-pos 1}],
                                           :task/pos 1}
                                          {:atom/predicate upper-move-aircraft,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?c1, :role/role-pos 2}
                                            {:atom/role ?style, :role/role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate board,
                                           :atom/roles
                                           [{:atom/role ?p, :role/role-pos 1}
                                            {:atom/role ?a, :role/role-pos 2}
                                            {:atom/role ?c1, :role/role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 3}
                                          {:atom/predicate !!assert,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val dest, :pos 1}
                                                                    #:list{:val ?a, :pos 2}
                                                                    #:list{:val ?c2, :pos 3}]},
                                                            :pos 1}]},
                                             :role/role-pos 1}],
                                           :task/pos 4}
                                          {:atom/predicate upper-move-aircraft-no-style,
                                           :atom/roles [{:atom/role ?a, :role/role-pos 1} {:atom/role ?c2, :role/role-pos 2}],
                                           :task/immediate? true,
                                           :task/pos 5}
                                          {:atom/predicate debark,
                                           :atom/roles
                                           [{:atom/role ?p, :role/role-pos 1}
                                            {:atom/role ?a, :role/role-pos 2}
                                            {:atom/role ?c2, :role/role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 6}]}}]}
          #:method{:head
                   #:atom{:predicate upper-move-aircraft,
                          :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2} #:atom{:role ?style, :role-pos 3}]},
                   :rhs-pairs
                   [#:method{:case-name Case1,
                             :preconditions [#:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]}]}
                    #:method{:case-name Case2,
                             :preconditions
                             [#:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?somecity, :role-pos 2}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate move-aircraft,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?somecity, :role/role-pos 2}
                                            {:atom/role ?c, :role/role-pos 3}
                                            {:atom/role ?style, :role/role-pos 4}],
                                           :task/pos 1}]}}]}
          #:method{:head
                   #:atom{:predicate upper-move-aircraft-no-style, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:case-name Case1,
                             :preconditions [#:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]}]}
                    #:method{:case-name Case2,
                             :preconditions
                             #:sort-by{:var ?cost,
                                       :fn #:s-exp{:fn-ref <},
                                       :exps
                                       [#:atom{:predicate at, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?somecity, :role-pos 2}]}
                                        #:atom{:predicate travel-cost-info,
                                               :roles
                                               [#:atom{:role ?a, :role-pos 1}
                                                #:atom{:role ?somecity, :role-pos 2}
                                                #:atom{:role ?c, :role-pos 3}
                                                #:atom{:role ?cost, :role-pos 4}
                                                #:atom{:role ?style, :role-pos 5}]}]},
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate move-aircraft,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?somecity, :role/role-pos 2}
                                            {:atom/role ?c, :role/role-pos 3}
                                            {:atom/role ?style, :role/role-pos 4}],
                                           :task/pos 1}]}}]}
          #:axiom{:head
                  #:atom{:predicate travel-cost-info,
                         :roles
                         [#:atom{:role ?a, :role-pos 1}
                          #:atom{:role ?from, :role-pos 2}
                          #:atom{:role ?to, :role-pos 3}
                          #:atom{:role ?cost, :role-pos 4}
                          #:atom{:role slow, :role-pos 5}]},
                  :rhs
                  [#:axiom{:case-name CASE1,
                           :case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate capacity,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?cap, :role-pos 2}]}
                                          #:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role ?from, :role-pos 1}
                                                  #:atom{:role ?to, :role-pos 2}
                                                  #:atom{:role ?dist, :role-pos 3}]}
                                          #:atom{:predicate slow-burn,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                                          #:eval{:exp
                                                 #:s-exp{:args
                                                         [{:arg/pos 1,
                                                           :s-exp/args
                                                           [#:arg{:val #:box{:sym ?cap}, :pos 1}
                                                            {:arg/pos 2,
                                                             :s-exp/args
                                                             [#:arg{:val #:box{:sym ?dist}, :pos 1} #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                                             :s-exp/fn-ref '*}],
                                                           :s-exp/fn-ref '<}],
                                                         :fn-ref 'eval}}
                                          #:atom{:predicate assign,
                                                 :roles
                                                 [#:atom{:role ?cost, :role-pos 1} #:atom{:role most-positive-fixnum, :role-pos 2}]}]}}
                   #:axiom{:case-name CASE2,
                           :case-pos 2,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role ?from, :role-pos 1}
                                                  #:atom{:role ?to, :role-pos 2}
                                                  #:atom{:role ?dist, :role-pos 3}]}
                                          #:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}
                                          #:atom{:predicate slow-burn,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                                          #:eval{:exp
                                                 #:s-exp{:args
                                                         [{:arg/pos 1,
                                                           :s-exp/args
                                                           [#:arg{:val #:box{:sym ?fuel}, :pos 1}
                                                            {:arg/pos 2,
                                                             :s-exp/args
                                                             [#:arg{:val #:box{:sym ?dist}, :pos 1} #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                                             :s-exp/fn-ref '*}],
                                                           :s-exp/fn-ref '>=}],
                                                         :fn-ref 'eval}}
                                          #:assign{:var ?cost,
                                                   :exp
                                                   #:s-exp{:args
                                                           [{:arg/pos 1,
                                                             :s-exp/args
                                                             [{:arg/pos 1,
                                                               :s-exp/args
                                                               [#:arg{:val #:box{:sym *tc*}, :pos 1}
                                                                {:arg/pos 2,
                                                                 :s-exp/args
                                                                 [#:arg{:val #:box{:sym *fc*}, :pos 1}
                                                                  {:arg/pos 2,
                                                                   :s-exp/args
                                                                   [#:arg{:val #:box{:sym ?dist}, :pos 1}
                                                                    #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                                                   :s-exp/fn-ref '*}],
                                                                 :s-exp/fn-ref '*}],
                                                               :s-exp/fn-ref '+}
                                                              #:arg{:val #:box{:num 1}, :pos 2}],
                                                             :s-exp/fn-ref '/}],
                                                           :fn-ref 'float}}]}}
                   #:axiom{:case-name CASE3,
                           :case-pos 3,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate capacity,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?cap, :role-pos 2}]}
                                          #:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role ?from, :role-pos 1}
                                                  #:atom{:role ?to, :role-pos 2}
                                                  #:atom{:role ?dist, :role-pos 3}]}
                                          #:atom{:predicate slow-burn,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                                          #:assign{:var ?cost,
                                                   :exp
                                                   #:s-exp{:args
                                                           [{:arg/pos 1,
                                                             :s-exp/args
                                                             [{:arg/pos 1,
                                                               :s-exp/args
                                                               [{:arg/pos 1,
                                                                 :s-exp/args
                                                                 [#:arg{:val #:box{:sym *tc*}, :pos 1} #:arg{:val #:box{:num 2}, :pos 2}],
                                                                 :s-exp/fn-ref '*}
                                                                {:arg/pos 2,
                                                                 :s-exp/args
                                                                 [#:arg{:val #:box{:sym *fc*}, :pos 1}
                                                                  {:arg/pos 2,
                                                                   :s-exp/args
                                                                   [#:arg{:val #:box{:sym ?dist}, :pos 1}
                                                                    #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                                                   :s-exp/fn-ref '*}],
                                                                 :s-exp/fn-ref '*}],
                                                               :s-exp/fn-ref '+}
                                                              #:arg{:val #:box{:num 1}, :pos 2}],
                                                             :s-exp/fn-ref '/}],
                                                           :fn-ref 'float}}]}}]}
          #:axiom{:head
                  #:atom{:predicate travel-cost-info,
                         :roles
                         [#:atom{:role ?a, :role-pos 1}
                          #:atom{:role ?from, :role-pos 2}
                          #:atom{:role ?to, :role-pos 3}
                          #:atom{:role ?cost, :role-pos 4}
                          #:atom{:role fast, :role-pos 5}]},
                  :rhs
                  [#:axiom{:case-name CASE1,
                           :case-pos 1,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate capacity,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?cap, :role-pos 2}]}
                                          #:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role ?from, :role-pos 1}
                                                  #:atom{:role ?to, :role-pos 2}
                                                  #:atom{:role ?dist, :role-pos 3}]}
                                          #:atom{:predicate fast-burn,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                                          #:eval{:exp
                                                 #:s-exp{:args
                                                         [{:arg/pos 1,
                                                           :s-exp/args
                                                           [#:arg{:val #:box{:sym ?cap}, :pos 1}
                                                            {:arg/pos 2,
                                                             :s-exp/args
                                                             [#:arg{:val #:box{:sym ?dist}, :pos 1} #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                                             :s-exp/fn-ref '*}],
                                                           :s-exp/fn-ref '<}],
                                                         :fn-ref 'eval}}
                                          #:atom{:predicate assign,
                                                 :roles
                                                 [#:atom{:role ?cost, :role-pos 1} #:atom{:role most-positive-fixnum, :role-pos 2}]}]}}
                   #:axiom{:case-name CASE2,
                           :case-pos 2,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role ?from, :role-pos 1}
                                                  #:atom{:role ?to, :role-pos 2}
                                                  #:atom{:role ?dist, :role-pos 3}]}
                                          #:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}
                                          #:atom{:predicate zoom-limit,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?limit, :role-pos 2}]}
                                          #:atom{:predicate onboard,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?num, :role-pos 2}]}
                                          #:eval{:exp
                                                 #:s-exp{:args
                                                         [{:arg/pos 1,
                                                           :s-exp/args
                                                           [#:arg{:val #:box{:sym ?num}, :pos 1} #:arg{:val #:box{:sym ?limit}, :pos 2}],
                                                           :s-exp/fn-ref '<}],
                                                         :fn-ref 'eval}}
                                          #:atom{:predicate fast-burn,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                                          #:eval{:exp
                                                 #:s-exp{:args
                                                         [{:arg/pos 1,
                                                           :s-exp/args
                                                           [#:arg{:val #:box{:sym ?fuel}, :pos 1}
                                                            {:arg/pos 2,
                                                             :s-exp/args
                                                             [#:arg{:val #:box{:sym ?dist}, :pos 1} #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                                             :s-exp/fn-ref '*}],
                                                           :s-exp/fn-ref '>=}],
                                                         :fn-ref 'eval}}
                                          #:assign{:var ?cost,
                                                   :exp
                                                   #:s-exp{:args
                                                           [{:arg/pos 1,
                                                             :s-exp/args
                                                             [{:arg/pos 1,
                                                               :s-exp/args
                                                               [#:arg{:val #:box{:sym *tc*}, :pos 1}
                                                                {:arg/pos 2,
                                                                 :s-exp/args
                                                                 [#:arg{:val #:box{:sym *fc*}, :pos 1}
                                                                  {:arg/pos 2,
                                                                   :s-exp/args
                                                                   [#:arg{:val #:box{:sym ?dist}, :pos 1}
                                                                    #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                                                   :s-exp/fn-ref '*}],
                                                                 :s-exp/fn-ref '*}],
                                                               :s-exp/fn-ref '+}
                                                              #:arg{:val #:box{:num 1}, :pos 2}],
                                                             :s-exp/fn-ref '/}],
                                                           :fn-ref 'float}}]}}
                   #:axiom{:case-name CASE3,
                           :case-pos 3,
                           :terms
                           #:conjunction{:terms
                                         [#:atom{:predicate capacity,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?cap, :role-pos 2}]}
                                          #:atom{:predicate distance,
                                                 :roles
                                                 [#:atom{:role ?from, :role-pos 1}
                                                  #:atom{:role ?to, :role-pos 2}
                                                  #:atom{:role ?dist, :role-pos 3}]}
                                          #:atom{:predicate fast-burn,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                                          #:atom{:predicate zoom-limit,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?limit, :role-pos 2}]}
                                          #:atom{:predicate onboard,
                                                 :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?num, :role-pos 2}]}
                                          #:eval{:exp
                                                 #:s-exp{:args
                                                         [{:arg/pos 1,
                                                           :s-exp/args
                                                           [#:arg{:val #:box{:sym ?num}, :pos 1} #:arg{:val #:box{:sym ?limit}, :pos 2}],
                                                           :s-exp/fn-ref '<}],
                                                         :fn-ref 'eval}}
                                          #:assign{:var ?cost,
                                                   :exp
                                                   #:s-exp{:args
                                                           [{:arg/pos 1,
                                                             :s-exp/args
                                                             [{:arg/pos 1,
                                                               :s-exp/args
                                                               [{:arg/pos 1,
                                                                 :s-exp/args
                                                                 [#:arg{:val #:box{:sym *tc*}, :pos 1} #:arg{:val #:box{:num 2}, :pos 2}],
                                                                 :s-exp/fn-ref '*}
                                                                {:arg/pos 2,
                                                                 :s-exp/args
                                                                 [#:arg{:val #:box{:sym *fc*}, :pos 1}
                                                                  {:arg/pos 2,
                                                                   :s-exp/args
                                                                   [#:arg{:val #:box{:sym ?dist}, :pos 1}
                                                                    #:arg{:val #:box{:sym ?burn}, :pos 2}],
                                                                   :s-exp/fn-ref '*}],
                                                                 :s-exp/fn-ref '*}],
                                                               :s-exp/fn-ref '+}
                                                              #:arg{:val #:box{:num 1}, :pos 2}],
                                                             :s-exp/fn-ref '/}],
                                                           :fn-ref 'float}}]}}]}
          #:method{:head
                   #:atom{:predicate move-aircraft,
                          :roles
                          [#:atom{:role ?a, :role-pos 1}
                           #:atom{:role ?c1, :role-pos 2}
                           #:atom{:role ?c2, :role-pos 3}
                           #:atom{:role slow, :role-pos 4}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}
                              #:atom{:predicate distance,
                                     :roles
                                     [#:atom{:role ?c1, :role-pos 1} #:atom{:role ?c2, :role-pos 2} #:atom{:role ?dist, :role-pos 3}]}
                              #:atom{:predicate slow-burn, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val >, :pos 1}
                                                     #:list{:val ?fuel, :pos 2}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val *, :pos 1}
                                                                    #:list{:val ?dist, :pos 2}
                                                                    #:list{:val ?burn, :pos 3}]},
                                                            :pos 3}]},
                                             :role-pos 1}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate fly,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?c1, :role/role-pos 2}
                                            {:atom/role ?c2, :role/role-pos 3}],
                                           :task/pos 1}]}}
                    #:method{:tasks
                             #:task-list{:elems
                                         [{:atom/predicate refuel,
                                           :atom/roles [{:atom/role ?a, :role/role-pos 1} {:atom/role ?c1, :role/role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate fly,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?c1, :role/role-pos 2}
                                            {:atom/role ?c2, :role/role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 2}]}}]}
          #:method{:head
                   #:atom{:predicate move-aircraft,
                          :roles
                          [#:atom{:role ?a, :role-pos 1}
                           #:atom{:role ?c1, :role-pos 2}
                           #:atom{:role ?c2, :role-pos 3}
                           #:atom{:role fast, :role-pos 4}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate fuel, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?fuel, :role-pos 2}]}
                              #:atom{:predicate distance,
                                     :roles
                                     [#:atom{:role ?c1, :role-pos 1} #:atom{:role ?c2, :role-pos 2} #:atom{:role ?dist, :role-pos 3}]}
                              #:atom{:predicate fast-burn, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?burn, :role-pos 2}]}
                              #:atom{:predicate eval,
                                     :roles
                                     [#:atom{:role
                                             #:list{:terms
                                                    [#:list{:val >, :pos 1}
                                                     #:list{:val ?fuel, :pos 2}
                                                     #:list{:val
                                                            #:list{:terms
                                                                   [#:list{:val *, :pos 1}
                                                                    #:list{:val ?dist, :pos 2}
                                                                    #:list{:val ?burn, :pos 3}]},
                                                            :pos 3}]},
                                             :role-pos 1}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate zoom,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?c1, :role/role-pos 2}
                                            {:atom/role ?c2, :role/role-pos 3}],
                                           :task/pos 1}]}}
                    #:method{:tasks
                             #:task-list{:elems
                                         [{:atom/predicate refuel,
                                           :atom/roles [{:atom/role ?a, :role/role-pos 1} {:atom/role ?c1, :role/role-pos 2}],
                                           :task/pos 1}
                                          {:atom/predicate zoom,
                                           :atom/roles
                                           [{:atom/role ?a, :role/role-pos 1}
                                            {:atom/role ?c1, :role/role-pos 2}
                                            {:atom/role ?c2, :role/role-pos 3}],
                                           :task/immediate? true,
                                           :task/pos 2}]}}]}
          #:method{:head #:atom{:predicate transport-aircraft, :roles [#:atom{:role ?a, :role-pos 1} #:atom{:role ?c, :role-pos 2}]},
                   :rhs-pairs
                   [#:method{:preconditions
                             [#:atom{:predicate not,
                                     :roles
                                     [#:atom{:role #:list{:terms [#:list{:val no-use, :pos 1} #:list{:val ?a, :pos 2}]}, :role-pos 1}]}],
                             :tasks
                             #:task-list{:elems
                                         [{:atom/predicate !!assert,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val #:list{:terms [#:list{:val no-use, :pos 1} #:list{:val ?a, :pos 2}]},
                                                            :pos 1}]},
                                             :role/role-pos 1}],
                                           :task/pos 1}
                                          {:atom/predicate upper-move-aircraft-no-style,
                                           :atom/roles [{:atom/role ?a, :role/role-pos 1} {:atom/role ?c, :role/role-pos 2}],
                                           :task/immediate? true,
                                           :task/pos 2}
                                          {:atom/predicate !!ra,
                                           :atom/roles
                                           [{:atom/role
                                             #:list{:terms
                                                    [#:list{:val #:list{:terms [#:list{:val no-use, :pos 1} #:list{:val ?a, :pos 2}]},
                                                            :pos 1}]},
                                             :role/role-pos 1}
                                            {:atom/role #:list{:terms []}, :role/role-pos 2}],
                                           :task/immediate? true,
                                           :task/pos 3}]}}]}]})
