(ns scheduling-tbd.interviewing.domain.process.scheduling-problem-type
  "Define a EADS to elicit general information about the scheduling problem the interviewees are interested in solving."
  (:require
   [clojure.data.json]
   [clojure.spec.alpha :as s]
   [datahike.api          :as d]
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm]]))

;;; ToDo: Because we use a central spec registry, the specs defined with short namespaces (e.g. :problem-type/val) might collide with specs from other domains.
;;;       The best solution might be not to use a central repository. These things won't be needed outside this file.

(s/def :scheduling-problem-type/EADS-message (s/keys :req-un [::message-type ::interview-objective ::EADS]))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::comment string?)

(s/def ::EADS (s/keys :req-un [::EADS-id ::problem-type ::continuous? ::cyclical?]))
(s/def ::EADS-id #(= % :process/scheduling-problem-type))

;;; We use the 'trick' that :<some-property>/val can be used to signify a non-namespaced attribute 'val' and a reference to a spec for value of 'val'.
(s/def ::problem-type (s/or :normal :problem-type/val :annotated ::annotated-ptype))
(s/def :problem-type/val #(#{:FLOW-SHOP-SCHEDULING-PROBLEM
                             :RESOURCE-ASSIGNMENT-PROBLEM
                             :PROJECT-SCHEDULING-PROBLEM
                             :JOB-SHOP-SCHEDULING-PROBLEM
                             :SINGLE-MACHINE-SCHEDULING-PROBLEM} %))
(s/def ::annotated-ptype (s/keys :req-un [:problem-type/val ::comment]))

(s/def ::continuous? (s/or :normal :continuous?/val :annotated ::annotated-continuous?))
(s/def :continuous?/val boolean?)
(s/def ::annotated-continuous? (s/keys :req-un [:continuous?/val ::comment]))

(s/def ::cyclical? (s/or :normal :cyclical?/val :annotated ::annotated-cyclical?))
(s/def :cyclical?/val boolean?)
(s/def ::annotated-cyclical? (s/keys :req-un [:cyclical?/val ::comment]))

(def scheduling-problem-type
  {:message-type :EADS-INSTRUCTIONS,
   :interview-objective (str "Using the three property types 'problem-type', 'continuous?', and 'cyclical?', determine the kind of production system and scheduling problem for which the interviewees seek a solution.\n"
                             "For 'problem-type' you may only choose one of the following:\n"
                             "   1) FLOW-SHOP-SCHEDULING-PROBLEM: the problem of defining start times for jobs that all execute the same sequence of production steps across multiple machines or workstations.\n"
                             "   2) RESOURCE-ASSIGNMENT-PROBLEM: [not exactly a scheduling problem, but it might come up] the problem of assigning work to resources without necessarily specifying the time at which the work is to occur.\n"
                             "   3) PROJECT-SCHEDULING-PROBLEM:  the problem of defining start and finish dates to all activities, deliverables, and milestones within an undertaking.\n"
                             "   4) JOB-SHOP-SCHEDULING-PROBLEM: the problem of scheduling jobs where the order in which the jobs visit machines or workstations may vary as determined by the job type.\n"
                             "   5) SINGLE-MACHINE-SCHEDULING-PROBLEM: the problem of choosing the sequence by which each of several jobs use the same resource or set of resources.")
   :EADS {:EADS-id :process/scheduling-problem-type
          :problem-type {:val :FLOW-SHOP-SCHEDULING-PROBLEM ; On receiving this from interviewers, we'll make it a keyword using ru/ds2clj.
                         :comment "We asked interviewees a few questions about their operations (not shown here) and inferred that they operate a flow shop."},
          :continuous? {:val false,
                        :comment (str "continuous? refers to whether or not product flows continuously from one process to the next, as it does in, for example, production of many petroleum products.\n"
                                        "This might be apparent from the answers that determined the value of problem-type. If not, then you can ask.")},
          :cyclical? {:val false,
                      :comment  (str "cyclical? refers to whether or not they seek a system that creates schedules that can be repeated in a pattern.\n"
                                     "For example, if the made the same collection of products in the same order each week, cylical? would be true.")}}})

(if (s/valid? :scheduling-problem-type/EADS-message scheduling-problem-type)
  ;; Write the EADS to data/EADS/process
  (let [db-obj {:EADS/id :process/scheduling-problem-type
                :EADS/cid :process
                :EADS/specs #:spec{:full :scheduling-problem-type/EADS-message}
                :EADS/msg-str (str scheduling-problem-type)}
        conn (connect-atm :system)
        eid (d/q '[:find ?e . :where [?e :system/name "SYSTEM"]] @conn)]
    (d/transact conn {:tx-data [{:db/id eid :system/EADS db-obj}]})
    ;; Write the EADS JSON to resources/EADS/process so it can be placed in ork's vector store.
    (->> (with-out-str (clojure.data.json/pprint scheduling-problem-type)) (spit "resources/EADS/process/scheduling-problem-type.json")))
  (throw (ex-info "Invalid EADS message (scheduling-problem-type)." {})))
