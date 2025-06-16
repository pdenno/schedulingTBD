(ns scheduling-tbd.iviewr.domain.process.scheduling-problem-type
  "Define a EADS to determine the scheduling problem type."
  (:require
   [clojure.pprint                  :refer [pprint]]
   [clojure.spec.alpha              :as s]
   [mount.core                      :as mount :refer [defstate]]
   [scheduling-tbd.agent-db         :refer [agent-log]]
   [scheduling-tbd.db               :as db]
   [scheduling-tbd.iviewr.eads-util :as eads-util :refer [ds-complete? combine-ds!]]
   [scheduling-tbd.sutil            :as sutil]))

;;; ToDo: Because we use a central spec registry, the specs defined with short namespaces (e.g. :problem-type/val) might collide with specs from other domains.
;;;       The best solution might be not to use a central repository. These things won't be needed outside this file.

(def ^:diag diag (atom nil))

(s/def :scheduling-problem-type/EADS-message (s/keys :req-un [::message-type ::interview-objective ::EADS]))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::comment string?)

(s/def ::EADS (s/keys :req-un [::principal-problem-type ::problem-components ::continuous? ::cyclical?]
                      :opt-un [::msg-id ::EADS-ref ::EADS-id])) ; ToDo: EADS-id should never be in here (as a matter of style).
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
   :interview-objective
   (str
    "This is typically the EADS to use directly after 'process/warm-up-with-challenges';\n"
    "it continues to discover the basic characteristics of the interviewees' scheduling problem.\n"
    "More specifically, the objective of this interview segment is to \n"
    "   1) classify the principal method of scheduling that is most appropriate for the work they do, and"
    "   2) make some basic observations about ancillary aspects of scheduling.\n")
   :EADS {:EADS-id :process/scheduling-problem-type

          :principal-problem-type
          {:val :FLOW-SHOP-SCHEDULING-PROBLEM ; On receiving this from interviewers, we'll make it a keyword using ru/ds2clj.
           :comment
           (str "The value here should be the problem-type that best characterizes the problem and production system architecture.\n"
                "If the industrial domain is well-known, use your background knowledge to help make this choice.\n"
                "The  value you provide here must be one of the following:\n"
                "\n"
                "   1) FLOW-SHOP-SCHEDULING-PROBLEM: the problem of defining start times for jobs that, roughly speaking, all visit production resources in the same order.\n"
                "      Choose flow shop whenever the process suggests that production resources are laid out to facilitate movement to the next resource.\n"
                "   2) TIMETABLING-PROBLEM: the problem of assigning collections of resources to event time slots, ensuring that all the resources required for each event are available in its time slot.\n"
                "   3) PROJECT-SCHEDULING-PROBLEM:  the problem of defining start and finish dates to all activities, deliverables, and milestones within an undertaking.\n"
                "   4) JOB-SHOP-SCHEDULING-PROBLEM: the problem of scheduling jobs where the order in which the jobs visit machines or workstations may vary as determined by the job type.\n"
                "   5) SINGLE-MACHINE-SCHEDULING-PROBLEM: the problem of choosing the sequence by which each of several jobs use the same resource or set of resources.")}

          :problem-components
          {:val [:FLOW-SHOP-SCHEDULING-PROBLEM :JOB-SHOP-SCHEDULING-PROBLEM]
           :comment
           (str "This should be a list of problem-type values that include all the problem types that seem relevant given your conversation so far.\n"
                "For example, whereas in this example the principal-problem-type is a flow-shop, conversation suggests that work might queue for a task the presents a timetabling problem.")}
          :continuous?
          {:val false,
           :comment
           (str "continuous? refers to whether or not, in the principle-problem-type, product flows continuously from one process to the next, as it does in, for example, production of many petroleum products.\n"
                "This might be apparent from the answers that determined the value of problem-type. If not, then you can ask.")},
          :cyclical?
          {:val false,
           :comment
           (str "cyclical? refers to whether or not they seek a system that creates schedules that can be repeated in a pattern.\n"
                "For example, if the made the same collection of products in the same order each week, cylical? would be true.")}}})

;;; See if it compiles.
(when-not (s/valid? :scheduling-problem-type/EADS-message scheduling-problem-type)
  (throw (ex-info "Invalid EADS (scheduling problem type)" {})))

;;; ------------------------------- checking for completeness ---------------
;;; Collect and combine :process/scheduling-problem-type ds refinements, favoring recent over earlier versions.
(defmethod combine-ds! :process/scheduling-problem-type
  [tag pid]
  (let [merged (->> (db/get-msg-dstructs pid tag)
                    (sort-by :msg-id)
                    (reduce (fn [r m] (merge r m)) {})
                    eads-util/strip-annotations)
        merged (-> merged
                   (update :principal-problem-type keyword)
                   (update :problem-components (fn [pcomps] (mapv #(keyword %) pcomps))))]
    (db/put-summary-ds! pid tag merged)))

(defmethod ds-complete? :process/scheduling-problem-type
  [tag pid]
  (let [ds (-> (db/get-summary-ds pid tag) eads-util/strip-annotations)
    complete? (s/valid? ::EADS ds)]
    (agent-log (str "This is the stripped DS for problem type (complete? = " complete? "):\n" (with-out-str (pprint ds)))
               {:console? true #_#_:elide-console 130})
    complete?))

;;; (sptype/init-scheduling-problem-type)
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
