(ns scheduling-tbd.shop-test
  (:require
   [clojure.edn             :as edn]
   [clojure.spec.alpha   :as s]  ; Keep around for debugging
   [clojure.test            :refer [deftest is testing]]
   [datahike.api            :as d]
   ;[datahike.pull-api       :as dp] ; Keep around for debugging
   [scheduling-tbd.shop     :as shop :refer [shop2db db-schema-shop2+]]
   [scheduling-tbd.sutil    :as sutil :refer [datahike-schema]]
   [taoensso.timbre         :as log]))

(def test-cfg {:store {:backend :mem :keep-history? false :schema-flexibility :write}})
(def ^:my-diag diag (atom []))

(defn make-test-db!
  "In memory DB for testing planning domain management. Preloads the schema."
  [config]
  (when (d/database-exists? config) (d/delete-database config))
  (d/create-database config)
  (d/transact (d/connect config) (datahike-schema db-schema-shop2+)))

(deftest axiom-round-trip
  (testing "Testing whether axioms can be round tripped."
    (testing "Testing starting with shop object."
      (let [axiom-shop '(:- (same ?x ?x) ())]
        (is (= axiom-shop (-> axiom-shop shop/shop2db shop/db2shop)))))
    (testing "Testing starting with db object."
      (let [axiom-db '{:axiom/name ".same_?x_?x",
                       :sys/body
                       {:sys/typ :axiom,
                        :axiom/head
                        {:sys/typ :atom, :atom/predicate same, :atom/roles [{:role/val #:box{:sym ?x}, :sys/pos 1} {:role/val #:box{:sym ?x}, :sys/pos 2}]},
                        :axiom/rhs [#:box{:empty-list "empty list"}]}}]
        (is (= axiom-db (-> axiom-db shop/db2shop shop/shop2db)))))))

(deftest method-round-trip
  (testing "Testing whether method can be round tripped."
    (testing "Testing starting with shop object."
      (let [method-shop '(:method (transport-aircraft ?a ?c)
                                  ((not (no-use ?a)))
                                  ((!!assert ((no-use ?a)))
                                   (:immediate upper-move-aircraft-no-style ?a ?c)
                                   (:immediate !!ra ((no-use ?a)) ())))]
        (is (= method-shop (-> method-shop shop/shop2db shop/db2shop)))))
    (testing "Testing starting with db object."
      (let [method-db (-> "test/data/method-db-example-1.edn" slurp read-string)]
        (is (= method-db (-> method-db shop/db2shop shop/shop2db)))))))

(deftest operator-round-trip
  (testing "Testing whether operator can be round tripped."
    (testing "Testing starting with shop object."
      (let [operator-shop '(:operator (!!cost ?end)
                                      ((maxtime ?max)
                                       (assign ?newmax (eval (if (< ?max ?end) ?end ?max))))
                                      ((maxtime ?max))
                                      ((maxtime ?newmax))
                                      (- ?newmax ?max))]
        (is (= operator-shop (-> operator-shop shop/shop2db shop/db2shop)))))
    (testing "Testing starting with db object."
      (let [operator-db (-> "test/data/operator-db-example-1.edn" slurp read-string)]
        (is (= operator-db (-> operator-db shop/db2shop shop/shop2db)))))))

(deftest domain-round-trip
  (let [domain-shop '(defdomain kiwi-example
                       ((:operator (!pickup ?a) () () ((have ?a)))
                        (:operator (!drop ?a) ((have ?a)) ((have ?a)) ())
                        (:method (swap ?x ?y)
                                 ((have ?x))
                                 ((!drop ?x) (!pickup ?y))
                                 ((have ?y))
                                 ((!drop ?y) (!pickup ?x)))))
        domain-db (-> "test/data/domain-db-example-1.edn" slurp read-string)]
    (testing "Testing whether a domain in proj format can be round tripped."
      (testing "Testing starting with a shop object."
        (is (= domain-shop (-> domain-shop shop/shop2db shop/db2shop))))
      (testing "Testing starting with a db object."
        (is (= domain-db (-> domain-db shop/db2shop shop/shop2db))))
      (testing "Testing with a :mem DB; starting with a db object."
        (make-test-db! test-cfg)
        (->> domain-db vector (d/transact (d/connect test-cfg)))
        (let [dom-id (d/q '[:find ?eid . :where [?eid :domain/name "kiwi-example"]]
                          @(d/connect test-cfg))
              db-obj (sutil/resolve-db-id {:db/id dom-id} (d/connect test-cfg) #{:db/id})
              res (shop/db2shop db-obj)]
          (is (= domain-shop res))))
      (testing "Testing with a :mem DB; starting with a shop object."
        (make-test-db! test-cfg)
        (->> domain-shop shop2db vector (d/transact (d/connect test-cfg)))
        (let [dom-id (d/q '[:find ?eid . :where [?eid :domain/name "kiwi-example"]]
                          @(d/connect test-cfg))
              db-obj (sutil/resolve-db-id {:db/id dom-id} (d/connect test-cfg) #{:db/id})
              res (shop/db2shop db-obj)]
          (is (= domain-shop res)))))))

(def zeno-canonical
  "This is an example in the 'canonical' form of a domain. When a SHOP version is needed, use shop/canon2shop.
   To store this use shop/canon2db."
  {:domain/name "zenotravel"
   :domain/elems [{:canon/pos 1
                   :axiom/name "zenotravel.same_?x_?x"
                   :canon/code '(:- (same ?x ?x) ())}

                  {:canon/pos 2
                   :axiom/name "zenotravel.different_?x_?y"
                   :canon/code '(:- (different ?x ?y) ((not (same ?x ?y))))}

                  {:canon/pos 3
                   :axiom/name "zenotravel.possible-person-in_?city"
                   :canon/code '(:- (possible-person-in ?city)
                                    ((person ?p)
                                     (at ?p ?city)
                                     (goal ?p ?city2)
                                     (different ?city2 ?city)))}

                  {:canon/pos 4
                   :operator/name "zenotravel.!!cost"
                   :canon/code '(:operator (!!cost ?end)
                                              ((maxtime ?max) ; Since ?max isn't bound in the head, I supose this is a query.
                                               (assign ?newmax (eval (if (< ?max ?end) ?end ?max))))

                                              ((maxtime ?max))
                                               ((maxtime ?newmax))
                                              (- ?newmax ?max))}

                  {:canon/pos 5
                   :method/name "zenotravel.board_?p_?a_?c"
                   :canon/code '(:method (board ?p ?a ?c)
                                          ((write-time ?a ?start))
                                          ((!board ?p ?a ?c ?start 1)
                                           (:immediate !!cost (call (function '+) ?start 1))))}

                  {:canon/pos 6
                   :operator/name "zenotravel.!board"
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
                   :method/name "zenotravel.debark_?p_?a_?c"
                   :canon/code '(:method (debark ?p ?a ?c)
                                          ((write-time ?a ?start))
                                          ((!debark ?p ?a ?c ?start 1)
                                           (:immediate !!cost (call (function '+) ?start 1))))}

                  {:canon/pos 8
                   :operator/name "zenotravel.!debark"
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
                   :method/name "zenotravel.refuel_?a_?c"
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
                   :operator/name "zenotravel.!refuel"
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
                   :method/name "zenotravel.zoom_?a_?c1_?c2"
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
                   :operator/name "zenotravel.!zoom"
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
                   :method/name "zenotravel.fly_?a_?c1_?c2"
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
                   :operator/name "zenotravel.!fly"
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
                   :operator/name "zenotravel.!!preprocessing"
                   :canon/code '(:operator (!!preprocessing ?problem-name)
                                              ((totaltime-coeff ?tc)
                                               (fuelused-coeff ?fc)
                                               (eval (setf *tc* ?tc))
                                               (eval (setf *fc* ?fc)))
                                              ()
                                              ()
                                              0)}

                  {:canon/pos 16
                   :operator/name "zenotravel.!!assert"
                   :canon/code '(:operator (!!assert ?g)
                                              ()
                                              ()
                                              ?g
                                              0)}

                  {:canon/pos 17
                   :operator/name "zenotravel.!!ra"
                   :canon/code '(:operator (!!ra ?D ?A)
                                              ()
                                              ?D
                                              ?A
                                              0)}

                  {:canon/pos 18
                   :method/name "zenotravel.transport-person_?p_?c.Case1"
                   :canon/code '(:method (transport-person ?p ?c)
                                          Case1 ((at ?p ?c)) ())}

                  {:canon/pos 19
                   :method/name "zenotravel.transport-person_?p_?c2.Case2"
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
                   :method/name "zenotravel.transport-person_?p_?c2.Case3"
                   :canon/code '(:method (transport-person ?p ?c2)  ; Note that won't get a unique name here if just use shop/method-name; same as above.
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
                   :method/name "zenotravel.upper-move-aircraft_?a_?c_?style.Case1"
                   :canon/code '(:method (upper-move-aircraft ?a ?c ?style)
                                          Case1 ((at ?a ?c)) ()
                                          Case2 ((at ?a ?somecity))     ((move-aircraft ?a ?somecity ?c ?style)))}

                  {:canon/pos 22
                   :method/name "zenotravel.upper-move-aircraft-no-style_?a_?c.Case1"
                   :canon/code '(:method (upper-move-aircraft-no-style ?a ?c)
                                          Case1 ((at ?a ?c)) ()
                                          Case2 (:sort-by ?cost <
                                                          ((at ?a ?somecity)
                                                           (travel-cost-info ?a ?somecity ?c ?cost ?style)))   ((move-aircraft ?a ?somecity ?c ?style)))}

                  {:canon/pos 23
                   :axiom/name "zenotravel.travel-cost-info_?a_?from_?to_?cost_slow.CASE1"
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
                   :axiom/name "zenotravel.travel-cost-info_?a_?from_?to_?cost_fast.CASE1"
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
                   :method/name "zenotravel.move-aircraft_?a_?c1_?c2_slow"
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
                   :method/name "zenotravel.move-aircraft_?a_?c1_?c2_fast"
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
                   :method/name "zenotravel.transport-aircraft_?a_?c"
                   :canon/code '(:method (transport-aircraft ?a ?c)
                                          ((not (no-use ?a)))
                                          ((!!assert ((no-use ?a)))
                                           (:immediate upper-move-aircraft-no-style ?a ?c)
                                           (:immediate !!ra ((no-use ?a)) ())))}]})

(defn compare-domains
  "Check each of :domain/elems in two domains, ensure each are equal.
   This operates on canonical, of course."
  [d1 d2]
  (letfn [(get-elem [n d] (some #(when (== (:canon/pos %) n) %) (:domain/elems d)))]
    (let [ok? (atom true)]
      (doseq [pos (->> d1 :domain/elems (map :canon/pos) sort)]
        (let [s1  (get-elem pos d1)
              s2  (get-elem pos d2)]
          (when-not (= s1 s2)
            (reset! ok? false)
            (log/info "\n\nNot equal: \n " s1 "\n " s2))))
      @ok?)))

(deftest canonical-round-trip
  (testing "Testing that canonical can be stored and recovered."
    (make-test-db! test-cfg)
    (is
     (let [db-atm (d/connect test-cfg)]
       (do ; Write it twice to ensure updating, not inserting.
         (->> zeno-canonical shop/canon2db vector (d/transact db-atm))
         (->> zeno-canonical shop/canon2db vector (d/transact db-atm))
         (compare-domains zeno-canonical
                          (-> "zenotravel" (shop/db-entry-for {:db-atm (d/connect test-cfg)}) shop/db2canon)))))))

(deftest shop-round-trip
  (testing "Testing that canonical can be stored from shop syntax and recovered."
    (make-test-db! test-cfg)
    (->> "test/data/zeno-travel-shop.edn" ; This is common-lisp syntax, despite the name.
         slurp
         edn/read-string
         shop/shop2db
         vector
         (d/transact (d/connect test-cfg)))
    (let [db-obj (shop/db-entry-for "zenotravel" {:db-atm (d/connect test-cfg)})]
      (reset! diag db-obj)
      (is (compare-domains
           zeno-canonical
           (shop/db2canon db-obj))))))

(deftest proj-round-trip []
  (testing "Testing whether proj data is handled correctly."
    (make-test-db! test-cfg)
    (let [proj-obj (-> "data/planning-domains/test-domain.edn" slurp edn/read-string)
          db-obj (-> proj-obj
                     shop/proj2canon
                     shop/canon2db
                     vector)]
      (d/transact (d/connect test-cfg) db-obj)
      (let [db-obj (shop/db-entry-for "test-pi" {:db-atm (d/connect test-cfg)})]
        (reset! diag {:db-obj db-obj
                      :proj-obj-golden proj-obj
                      :proj-obj-computed (shop/db2proj db-obj)})
        (is (= proj-obj (shop/db2proj db-obj)))))))

(defn ttt []
  (make-test-db! test-cfg)
  (->> "data/planning-domains/zeno-travel-shop.edn" ; This is common-lisp syntax, despite the name.
       slurp
       edn/read-string
       shop/shop2db
       vector
       (d/transact (d/connect test-cfg)))
  (let [db-obj (shop/db-entry-for "zenotravel" {:db-atm (d/connect test-cfg)})]
    db-obj
    #_(shop/db2canon db-obj)))

;;; Specifically, I think it sometimes fails to report a test had been run when that test fails.
(defn all-tests
  "cider-test-run-ns-tests, C-c C-t n, isn't recognizing all the tests!"
  []
  (log/info "axiom round trip"       (axiom-round-trip))
  (log/info "method round trip"      (method-round-trip))
  (log/info "operator round trip"    (operator-round-trip))
  (log/info "domain round trip"      (domain-round-trip))
  (log/info "shop round trip"        (shop-round-trip))
  (log/info "canonical round trip"   (canonical-round-trip)))
