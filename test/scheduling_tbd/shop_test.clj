(ns scheduling-tbd.shop-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.test :refer [deftest is testing]]
   [datahike.api                 :as d]
   [scheduling-tbd.shop :as shop :refer [exp2db]]
   [scheduling-tbd.llm  :as llm]
   [scheduling-tbd.util :as util]))

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
    (is (= {:s-exp/fn-ref '>} (exp2db '> :s-exp-extended)))
    (is (= #:s-exp{:args
                   [{:arg/pos 1, :s-exp/args [#:arg{:val #:box{:num 1}, :pos 1} #:arg{:val #:box{:sym 'x}, :pos 2}], :s-exp/fn-ref '+}
                    #:arg{:val #:box{:num 3}, :pos 2}
                    #:arg{:val #:box{:sym 'q}, :pos 3}],
                   :fn-ref '*}
           (exp2db '(* (+ 1 x) 3 q) :s-exp-extended)))))

;;; See pg 397 in Dana Nau, et al., "SHOP2: An HTN Planning System" Journal of Artificial Intelligence Research 20 (2003) 379-404
;;; (By convention) names of operators begin with an exclamation mark (!).
;;; This doesn't use:
;;; - and (logical expressions are implicit when nested)
;;; - :task (an optional keyword that can be the first element of a task atom).
(def zeno
  '(defdomain ZENOTRAVEL
     (
      (:- (same ?x ?x) ())
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
               Case2 (:sort-by ?num #’>
                               ((at ?p ?c1)
                                (at ?a ?c1)
                                (aircraft ?a)
                                (onboard ?a ?num)))
               ((!!assert ((dest ?a ?c1)))
                (:immediate board ?p ?a ?c1)
                (!!assert ((dest ?a ?c2)))
                (:immediate upper-move-aircraft-no-style ?a ?c2)
                (:immediate debark ?p ?a ?c2)))

      (:method (transport-person ?p ?c2)
               Case3 (:sort-by ?cost #’<
                               ((at ?p ?c1)
                                (aircraft ?a)
                                (at ?a ?c3)
                                (different ?c1 ?c3)
                                (forall (?c) ((dest ?a ?c))
                                        ((same ?c ?c1)))
                                (imply ((different ?c3 ?c1))
                                       (not (possible-person-in ?c3)))
                                (travel-cost-info ?a ?c3 ?c1 ?cost ?style)))
               ((!!assert ((dest ?a ?c1)))
                (:immediate upper-move-aircraft ?a ?c1 ?style)
                (:immediate board ?p ?a ?c1)
                (!!assert ((dest ?a ?c2)))
                (:immediate upper-move-aircraft-no-style ?a ?c2)
                (:immediate debark ?p ?a ?c2)))

      (:method (upper-move-aircraft ?a ?c ?style)
               Case1 ((at ?a ?c)) ()
               Case2 ((at ?a ?somecity))
               ((move-aircraft ?a ?somecity ?c ?style)))

      (:method (upper-move-aircraft-no-style ?a ?c)
               Case1 ((at ?a ?c)) ()
               Case2 (:sort-by ?cost #’<
                               ((at ?a ?somecity)
                                (travel-cost-info ?a ?somecity ?c ?cost ?style)))
               ((move-aircraft ?a ?somecity ?c ?style)))

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


(def craft-brewing-desc
  "In medium-scale craft beer brewing, a significant challenge arises in the form of production scheduling. Craft breweries often produce a diverse range of beer styles with varying ingredients, fermentation times, and packaging requirements. Coordinating the brewing process to meet customer demands while optimizing resources can be complex. The production scheduling problem entails determining the most efficient sequence and timing of brewing batches, taking into account factors like ingredient availability, tank capacities, yeast propagation, and production deadlines. Balancing these variables is crucial to ensure optimal utilization of equipment, minimize idle time, reduce inventory holding costs, and meet customer expectations. Effective production scheduling plays a vital role in maintaining consistent beer quality, managing production costs, and maximizing overall brewery efficiency.")

(db/create-proj-db!
 (let [{:keys [summary industry]} (llm/project-name craft-brewing-desc)
       {:keys [decision-objective probability]} (llm/find-objective-sentence craft-brewing-desc)
       id (-> summary str/lower-case (str/replace #"\s+" "-") keyword)]
   {:project/id id
    :project/name summary
    :project/desc craft-brewing-desc
    :project/industry industry}))

(def proj-obj
  (util/resolve-db-id
   {:db/id (d/q '[:find ?e . :where [?e :project/id _]] @(db/connect-proj))}
   (db/connect-proj)
   #{:db/id}))

(def sys-obj
  (util/resolve-db-id
   {:db/id (d/q '[:find ?e . :in $ ?name :where [?e :project/id ?name]]
                @(db/connect-sys)
                (:project/id proj-obj))}
   (db/connect-sys)
   #{:db/id}))

(deftest start-a-project
  (testing "Testing the creation of a project."
    (is (and (keyword? (:project/id proj-obj))
             (= (:project/id proj-obj) (:project/id sys-obj))
             (every? #(contains? proj-obj %) [:project/desc :project/id :project/name])
             (every? #(contains? sys-obj %) [:project/id :project/dir :project/name])))))

;;; See pg 397 in Dana Nau, et al., "SHOP2: An HTN Planning System" Journal of Artificial Intelligence Research 20 (2003) 379-404
;;; This doesn't use:
;;; - and (logical expressions are implicit when nested)
;;; - :task (an optional keyword that can be the first element of a task atom).
(def zeno
  '(defdomain ZENOTRAVEL
     (
      (:- (same ?x ?x) ())
      (:- (different ?x ?y) ((not (same ?x ?y))))
      (:- (possible-person-in ?city)
          ((person ?p)
           (at ?p ?city)
           (goal ?p ?city2)
           (different ?city2 ?city)))

      (:operator (!!cost ?end)

                 ((maxtime ?max)
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
               Case2 (:sort-by ?num #’>
                               ((at ?p ?c1)
                                (at ?a ?c1)
                                (aircraft ?a)
                                (onboard ?a ?num)))
               ((!!assert ((dest ?a ?c1)))
                (:immediate board ?p ?a ?c1)
                (!!assert ((dest ?a ?c2)))
                (:immediate upper-move-aircraft-no-style ?a ?c2)
                (:immediate debark ?p ?a ?c2)))

      (:method (transport-person ?p ?c2)
               Case3 (:sort-by ?cost #’<
                               ((at ?p ?c1)
                                (aircraft ?a)
                                (at ?a ?c3)
                                (different ?c1 ?c3)
                                (forall (?c) ((dest ?a ?c))
                                        ((same ?c ?c1)))
                                (imply ((different ?c3 ?c1))
                                       (not (possible-person-in ?c3)))
                                (travel-cost-info ?a ?c3 ?c1 ?cost ?style)))
               ((!!assert ((dest ?a ?c1)))
                (:immediate upper-move-aircraft ?a ?c1 ?style)
                (:immediate board ?p ?a ?c1)
                (!!assert ((dest ?a ?c2)))
                (:immediate upper-move-aircraft-no-style ?a ?c2)
                (:immediate debark ?p ?a ?c2)))

      (:method (upper-move-aircraft ?a ?c ?style)
               Case1 ((at ?a ?c)) ()
               Case2 ((at ?a ?somecity))
               ((move-aircraft ?a ?somecity ?c ?style)))

      (:method (upper-move-aircraft-no-style ?a ?c)
               Case1 ((at ?a ?c)) ()
               Case2 (:sort-by ?cost #’<
                               ((at ?a ?somecity)
                                (travel-cost-info ?a ?somecity ?c ?cost ?style)))
               ((move-aircraft ?a ?somecity ?c ?style)))

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
