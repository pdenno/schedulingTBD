(ns scheduling-tbd.iviewr.domain.process.scheduling-problem-type
  "Define a EADS to elicit general information about the scheduling problem the interviewees are interested in solving."
  (:require
   [clojure.spec.alpha    :as s]
   [mount.core :as mount  :refer [defstate]]
   [scheduling-tbd.db     :as db]
   [scheduling-tbd.iviewr.eads-util :refer [ds-complete?]]
   [scheduling-tbd.sutil  :as sutil]
   [taoensso.telemere     :refer [log!]]))

;;; ToDo: Because we use a central spec registry, the specs defined with short namespaces (e.g. :problem-type/val) might collide with specs from other domains.
;;;       The best solution might be not to use a central repository. These things won't be needed outside this file.

(s/def :scheduling-problem-type/EADS-message (s/keys :req-un [::message-type ::interview-objective ::EADS]))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::comment string?)

(s/def ::EADS (s/keys :req-un [::EADS-id ::principal-problem-type ::problem-components ::continuous? ::cyclical?]))
(s/def ::EADS-id #(= % :process/scheduling-problem-type))

;;; We use the 'trick' that :<some-property>/val can be used to signify a non-namespaced attribute 'val' and a reference to a spec for value of 'val'.
(s/def ::principal-problem-type (s/or :normal :principal-problem-type/val :annotated ::annotated-ptype))
(s/def :principal-problem-type/val #(#{:FLOW-SHOP-SCHEDULING-PROBLEM
                                       :TIMETABLING-PROBLEM
                                       :PROJECT-SCHEDULING-PROBLEM
                                       :JOB-SHOP-SCHEDULING-PROBLEM
                                       :SINGLE-MACHINE-SCHEDULING-PROBLEM} %))
(s/def ::annotated-ptype (s/keys :req-un [:principal-problem-type/val ::comment]))

(s/def ::problem-components (s/or :normal :problem-components/val :annotated ::annotated-pcomponents))
(s/def :problem-components/val (s/coll-of :principal-problem-type/val))
(s/def ::annotated-pcomponents (s/keys :req-un [:problem-components/val ::comment]))

(s/def ::continuous? (s/or :normal :continuous?/val :annotated ::annotated-continuous?))
(s/def :continuous?/val boolean?)
(s/def ::annotated-continuous? (s/keys :req-un [:continuous?/val ::comment]))

(s/def ::cyclical? (s/or :normal :cyclical?/val :annotated ::annotated-cyclical?))
(s/def :cyclical?/val boolean?)
(s/def ::annotated-cyclical? (s/keys :req-un [:cyclical?/val ::comment]))

(def scheduling-problem-type
  {:message-type :EADS-INSTRUCTIONS,
   :interview-objective (str "Using the four property types 'problem-type', 'problem-components', 'continuous?', and 'cyclical?', determine the kind of production system and scheduling problem for which the interviewees seek a solution.\n"
                             "For 'problem-type' you may only choose one of the following:\n"
                             "   1) FLOW-SHOP-SCHEDULING-PROBLEM: the problem of defining start times for jobs that all execute the same sequence of production steps across multiple machines or workstations.\n"
                             "   2) TIMETABLING-PROBLEM: the problem of assigning collections of resources to event time slots, ensuring that all the resources required for each event are available in its time slot.\n"
                             "   3) PROJECT-SCHEDULING-PROBLEM:  the problem of defining start and finish dates to all activities, deliverables, and milestones within an undertaking.\n"
                             "   4) JOB-SHOP-SCHEDULING-PROBLEM: the problem of scheduling jobs where the order in which the jobs visit machines or workstations may vary as determined by the job type.\n"
                             "   5) SINGLE-MACHINE-SCHEDULING-PROBLEM: the problem of choosing the sequence by which each of several jobs use the same resource or set of resources.")
   :EADS {:EADS-id :process/scheduling-problem-type
          :principal-problem-type {:val :FLOW-SHOP-SCHEDULING-PROBLEM ; On receiving this from interviewers, we'll make it a keyword using ru/ds2clj.
                                   :comment (str "The value here should be the problem-type that best characterizes the problem and system architecture.\n"
                                                 "We asked interviewees a few questions about their operations (not shown here) and inferred that they operate a flow shop.")},
          :problem-components {:val [:FLOW-SHOP-SCHEDULING-PROBLEM :TIMETABLING-PROBLEM]
                               :comment (str "This should be a list of problem-type values that include all the problem types that seem relevant given your conversation so far.\n"
                                             "For example, whereas in this example the principal-problem-type is a flow-shop, conversation suggests that work might queue for a task the presents a timetabling problem.")}
          :continuous? {:val false,
                        :comment (str "continuous? refers to whether or not, in the principle-problem-type, product flows continuously from one process to the next, as it does in, for example, production of many petroleum products.\n"
                                        "This might be apparent from the answers that determined the value of problem-type. If not, then you can ask.")},
          :cyclical? {:val false,
                      :comment  (str "cyclical? refers to whether or not they seek a system that creates schedules that can be repeated in a pattern.\n"
                                     "For example, if the made the same collection of products in the same order each week, cylical? would be true.")}}})

;;; ------------------------------- checking for completeness ---------------
(defmethod ds-complete? :process/scheduling-problem-type
  [eads-id ds]
  (log! :info (str "This is the ds-complete for " eads-id ". ds = " ds))
  true)

;;; ------------------------------- starting and stopping ---------------
(defn init-scheduling-problem-type
  []
  (if (s/valid? :scheduling-problem-type/EADS-message scheduling-problem-type)
    (when-not (db/same-EADS-instructions? scheduling-problem-type)
      (sutil/update-resources-EADS-json! scheduling-problem-type)
      (db/put-EADS-instructions! scheduling-problem-type))
    (throw (ex-info "Invalid EADS message (scheduling-problem-type)." {}))))

(defstate init-scheduling-problem-type-eads
  :start (init-scheduling-problem-type))
