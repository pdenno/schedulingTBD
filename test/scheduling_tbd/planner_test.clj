(ns scheduling-tbd.planner-test
  (:require
   [clojure.edn             :as edn]
   [clojure.pprint          :refer [cl-format]]
   [clojure.spec.alpha      :as s]
   [clojure.test            :refer [deftest is testing]]
   [mznp.rewrite            :as rw]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.planner  :as plan]
   [scheduling-tbd.specs    :as specs]
   [scheduling-tbd.sutil    :as sutil]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.timbre          :as log]))

(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (alias 'uni    'clojure.core.unify)
  (alias 'str    'clojure.string)
  (alias 'd      'datahike.api)
  (alias 'dp     'datahike.pull-api)
  (alias 'mount  'mount.core)
  (alias 'p      'promesa.core)
  (alias 'px     'promesa.exec)
  (alias 'core   'scheduling-tbd.core)
  (alias 'dom    'scheduling-tbd.domain)
  ;(alias 'domt    'scheduling-tbd.domain-test)
  (alias 'how    'scheduling-tbd.how-made)
  (alias 'llm    'scheduling-tbd.llm)
  ;(alias 'llmt   'scheduling-tbd.llm-test)
  (alias 'op     'scheduling-tbd.operators)
  (alias 'opt    'scheduling-tbd.operators-test)
  (alias 'resp   'scheduling-tbd.web.controllers.respond)
  (alias 'spec   'scheduling-tbd.specs)
  (alias 'sutil  'scheduling-tbd.sutil)
  (alias 'sur    'scheduling-tbd.surrogate)
  ;(alias 'surt   'scheduling-tbd.surrogate-test)
  (alias 'util   'scheduling-tbd.util)
  (alias 'ws     'scheduling-tbd.web.websockets)
  (alias 'openai 'wkok.openai-clojure.api))

(defn aaa []
  (println "We are a medium-sized craft beer brewery. We produce about 100,000 barrels/year.\n   We run several products simultaneously and simply would like to be able to have the beer bottled and ready\n   to ship as near as possible to the dates defined in our sales plan."))

(defn tryme []
  (rw/rewrite*
   :mznp/gen-call-expr
   "sum (j in Jobs) (if (LineOfJob[j] == lin) then WorkersOnJob[j,w1] else 0 endif)"))

;;; ============================================= plan9 =================================================================
(def travel-domain '{:domain/id :travel-plan
                     :domain/description "Testing stepping through simple sequence. BTW, these aren't good plans; state is too sparse/vague, etc."
                     :domain/elems [{:method/head (go-home ?person)
                                     :method/rhsides [{:method/case-name "take car"
                                                       :method/preconds [(have-car ?person)]
                                                       :method/task-list [(drive-home ?person)]}

                                                      {:method/case-name "take bus"
                                                       :method/preconds [(not (have-car ?person))]
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

(def travel-problem '{:problem/domain :travel-plan
                      :goal   (go-home me)
                      :state #{(at-work me) (have-car me)}})

(deftest valid-problem
  (testing "That the spec for planning problems works."
    (is (s/valid? ::specs/domain-problem travel-problem))))

;;; Typically, the id will be a client-id (a UUID string), but in planner_test, at least, this is not the case.
(defmacro with-planning-domain [[id domain] & body]
  `(try (sutil/register-planning-domain ~id ~domain)
        ~@body
        (finally
          (sutil/deregister-planning-domain ~id))))

(deftest simple-plans
  (testing "Testing a simple sequential plan."
    (is (= '{:result :success,
             :plan-info {:plan [(!walk-to-car me) (!drive-car me) (!walk-garage-to-home me)],
                         :new-tasks [],
                         :state #{(have-car me) (at-home me)}}}
           (with-planning-domain [:travel-domain travel-domain]
             (plan/plan9 :travel-domain travel-problem)))))

  (testing "Testing a simple sequential plan, negated pre-condition."
    (is (= '{:result :success,
             :plan-info {:plan
                         [(!walk-to-car me) (!drive-car me) (!walk-garage-to-home me)],
                         :new-tasks [],
                         :state #{(have-car me) (at-home me)}}}
           (let [no-car (assoc-in travel-domain [:domain/problem :problem/state] '#{(at-work me)})]
             (with-planning-domain [:no-car no-car]
               (plan/plan9 :no-car travel-problem))))))

  (testing "Testing failure of the only possible plan."
    (is (= '{:result :failure, :reason :no-successful-plans}
           (with-planning-domain [:travel-domain travel-domain]
             (plan/plan9 :travel-domain travel-problem {:inject-failures '[(!drive-car ?person)]}))))))
