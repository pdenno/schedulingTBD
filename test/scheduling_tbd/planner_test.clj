(ns scheduling-tbd.planner-test
  (:require
   [clojure.spec.alpha      :as s]
   [clojure.test            :refer [deftest is testing]]
   [mznp.rewrite            :as rw]
   [scheduling-tbd.planner  :as plan]
   [scheduling-tbd.specs    :as specs]
   [scheduling-tbd.sutil    :as sutil]))

(defn ^:diag test-mznp []
  (rw/rewrite*
   :mznp/gen-call-expr
   "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)"))

;;; ============================================= plan9 =================================================================
(def travel-domain '{:domain/id :travel-plan
                     :domain/description "Testing stepping through simple sequence. BTW, these aren't good plans; state is too sparse/vague, etc."
                     :domain/elems [{:method/head (be-at-home ?person)
                                     :method/rhsides [{:method/case-name "take car"
                                                       :method/preconds [(have-car ?person)]
                                                       :method/task-list [(drive-home ?person)]}

                                                      {:method/case-name "take bus"
                                                       :method/task-list [(bus-to-home ?person)]}]}

                                    {:method/head (drive-home ?person)
                                     :method/rhsides [{:method/case-name "drive home"
                                                       :method/task-list [(!walk-to-car ?person)
                                                                          (!drive-car ?person)
                                                                          (!walk-garage-to-home ?person)]}]}

                                    {:method/head (bus-to-home ?person)
                                     :method/rhsides [{:method/case-name "take bus home"
                                                       :method/task-list [(!walk-to-bus-stop ?person)
                                                                          (!board-bus ?person)
                                                                          (!exit-bus ?person)]}]}

                                    {:operator/head (!walk-to-car ?person)}

                                    {:operator/head  (!drive-car ?person)
                                     :operator/d-list    [(at-work ?person)]
                                     :operator/a-list    [(at-garage ?person)]}

                                    {:operator/head (!walk-garage-to-home ?person)
                                     :operator/preconds [(at-garage ?person)]
                                     :operator/d-list   [(at-garage ?person)]
                                     :operator/a-list   [(at-home ?person)]}

                                    {:operator/head (!walk-to-bus-stop ?person)
                                     :operator/d-list  [(at-work ?person)]}

                                    {:operator/head (!board-bus ?person)}

                                    {:operator/head (!exit-bus ?person)
                                     :operator/a-list  [(at-home ?person)]}]})

(def travel-problem '{:problem/domain :travel-plan})
;                      :goal   (be-at-home me)
;                      :state #{(at-work me) (have-car me)}})

(deftest valid-problem
  (testing "That the spec for planning problems works."
    (is (s/valid? ::specs/domain-problem travel-problem))))

;;; Typically, the id will be a client-id (a UUID string), but in planner_test, at least, this is not the case.
(defmacro with-planning-domain [[id domain] & body]
  `(try (sutil/register-planning-domain ~id ~domain)
        ~@body
        (finally
          (sutil/deregister-planning-domain ~id))))

(defn tryme []
  (with-planning-domain [:travel-domain travel-domain]
    (plan/plan9
     '#{(at-work me) (have-car me)}
     '[(be-at-home me)]
     {:domain-id :travel-domain :shop2? true}))) ; <================== These are all broken because I removed :shop2?

(deftest simple-plans ; <================== These are all broken because I removed :shop2?
  (testing "Testing a simple plan."
    (is (= '{:result :success,
             :plan-info {:plan [(!walk-to-car me) (!drive-car me) (!walk-garage-to-home me)],
                         :new-tasks [],
                         :state #{(have-car me) (at-home me)}}}
           (with-planning-domain [:travel-domain travel-domain]
             (plan/plan9
              '#{(at-work me) (have-car me)}
              '[(be-at-home me)]
              {:domain-id :travel-domain :shop2? true})))))

  (testing "Testing an alternative plan."
    (is (= '{:result :success,
             :plan-info {:plan [(!walk-to-bus-stop me) (!board-bus me) (!exit-bus me)],
                         :new-tasks [],
                         :state #{(at-home me)}}}
             (with-planning-domain [:travel-domain travel-domain]
               (plan/plan9
                '#{(at-work me)}
                '[(be-at-home me)]
                {:domain-id :travel-domain :shop2? true})))))

  (testing "Testing failure of the one plan, success of an alternative."
    (is (= '{:result :success,
             :plan-info {:plan [(!walk-to-bus-stop me) (!board-bus me) (!exit-bus me)],
                         :new-tasks [],
                         :state #{(have-car me) (at-home me)}}}
           (with-planning-domain [:travel-domain travel-domain]
             (plan/plan9
              :travel-domain
              '#{(at-work me) (have-car me)}
              '[(be-at-home me)]
              {:shop2? true
               :inject-failures '[(!drive-car me)]})))))

  (testing "Testing failure of all plans."
    (is (= '{:result :failure, :reason :no-successful-plans}
           (with-planning-domain [:travel-domain travel-domain]
             (plan/plan9
              '#{(at-work me) (have-car me)}
              '[(be-at-home me)]
              {:domain-id :travel-domain :shop2? true
               :inject-failures '[(!drive-car me) (!board-bus me)]}))))))
