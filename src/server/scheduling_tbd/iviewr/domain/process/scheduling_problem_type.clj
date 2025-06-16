(ns scheduling-tbd.iviewr.domain.process.scheduling-problem-type
  "Define a EADS to elicit general information about the scheduling problem the interviewees are interested in solving."
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

(s/def ::EADS (s/keys :req-un [::principal-problem-type ::problem-components ::continuous? ::cyclical? ::scheduling-challenges ::one-more-thing ::product-or-service-name]
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

(s/def ::scheduling-challenges (s/or :normal :scheduling-challenges/val :annotated ::annotated-scheduling-challenges))
(s/def :scheduling-challenges/val (s/coll-of string? :kind vector?))
(s/def ::annotated-scheduling-challenges (s/keys :req-un [:scheduling-challenges/val ::comment]))

(s/def ::one-more-thing (s/or :normal :one-more-thing/val :annotated ::annotated-one-more-thing))
(s/def :one-more-thing/val  string?)
(s/def ::annotated-one-more-thing (s/keys :req-un [:one-more-thing/val ::comment]))

(s/def ::product-or-service-name (s/or :normal :product-or-service-name/val :annotated ::annotated-product-or-service-name))
(s/def :product-or-service-name/val  string?)
(s/def ::annotated-product-or-service-name (s/keys :req-un [:product-or-service-name/val ::comment]))


(def scheduling-problem-type
  {:message-type :EADS-INSTRUCTIONS,
   :interview-objective
   (str
    "The objective of this interview segment is to \n"
    "   1) get the interviewees started in discussing their scheduling problem, and,\n"
    "   2) make some basic observations about the scheduling challenges they face.\n"
    "The first question to ask, which we call the 'warm up question' is simply this:\n"
    "\n"
    " 'What are the products you make or the services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences.' \n"
    "\n"
    "But remember, you don't supply this question until you receive a SUPPLY-QUESTION message!\n"
    "The correct response to an EADS-INSTRUCTIONS message such as this one is always, {\"message-type\": \"STATUS\", \"status\": \"OK\"}.\n"
    "Completing the EADS (responding with a complete DATA-STRUCTURE-REFINEMENT message) may require more than just that one question, however.\n"
    "Examine the EADS to determine what other questions you may wish to ask.\n")
   :EADS {:EADS-id :process/scheduling-problem-type

          :principal-problem-type
          {:val :JOB-SHOP-SCHEDULING-PROBLEM ; On receiving this from interviewers, we'll make it a keyword using ru/ds2clj.
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
          {:val [:JOB-SHOP-SCHEDULING-PROBLEM :FLOW-SHOP-SCHEDULING-PROBLEM]
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
                "For example, if the made the same collection of products in the same order each week, cylical? would be true.")}

          :scheduling-challenges
          {:comment
           (str "The value here should be an enumeration of any of the 15 keywords provided below that characterize the kinds of problems they face.\n"
                "DO NOT ask questions specifically about these.\n"
                "Instead, use the responses you got back from determining 'principal-problem-type', 'continuous?' and 'cyclical?' to determine the value of this property.\n"
                "The reason that we ask that you do not ask specifically about these is that we do not want to 'put words in their mouthes'.\n"
                "We want to know what is on their mind without prompting them with suggestions.\n"
                "\n"
                "Here are the 15 keywords you can use, and their definitions:\n"
                "\n"
                "  1) raw-material-uncertainty : They sometimes don't have the raw material they need to make what they want to make.\n"
                "  2) demand-uncertainty : They are uncertain what to make because they are uncertain what customers need.\n"
                "  3) delivery-schedules : They are having problems meeting delivery promise dates.\n"
                "  4) variation-in-demand : They have slow periods and very busy periods. This is often the case, for example, when demand has seasonality.\n"
                "  5) planned-maintenance : They need to accommodate equipment maintenance schedules in their production schedules.\n"
                "  6) resource-assignment : They need to reserve several resources for simultaneous use.\n"
                "  7) equipment-changeover : The time it takes to change equipment setting and tooling is considerable.\n"
                "  8) equipment-availability : They struggle with equipment breakdowns.\n"
                "  9) equipment-utilization : They have expensive equipment that they would like to be able to use more.\n"
                "  10) worker-availability : They struggle with shortages of workers.\n"
                "  11) skilled-worker-availability : This is a specific subtype of 'worker-availability' where they suggest that matching worker skills and/or "
                "certification to the process is the challenge.\n"
                "  12) bottleneck-processes : The pace of their production is throttled by just a few processes.\n"
                "  13) process-variation : They have many different processes for making roughly the same class of products.\n"
                "  14) product-variation : They have many different products to make.\n"
                "  15) meeting-KPIs : They mention key performance indicators (KPIs) or difficulty performing to them.\n"
                "\n"
                "Suppose they answered the warm-up question with the following:\n"

                "    'We primarily produce various types of plate glass, including clear, tinted, and tempered glass, used in construction, "
                "     automotive, and architectural applications. Our main scheduling challenge is efficiently coordinating production runs to "
                "     match supply with fluctuating demand, minimize downtime during equipment changeovers for different glass types, and manage "
                "     the lead times associated with raw material sourcing and delivery. Additionally, we must balance production with the availability "
                "     of skilled labor and maintenance schedules to ensure optimal use of resources.'\n"
                "\n"
                "Then a reasonable response to for scheduling challenges would be\n"
                "  [\"process-variation\", \"demand-uncertainty\", \"equipment-changeover\", \"skilled-worker-availability\", \"planned-maintenance\", \"product-variation\"].\n")
           :val
           ["process-variation", "demand-uncertainty", "equipment-changeover", "skilled-worker-availability", "planned-maintenance", "product-variation"]}

          :one-more-thing
          {:comment
           (str "This is an opportunity to make one more observation (beyond those in 'scheduling-challenges') about their challenges.\n"
                "Again, like 'scheduling-challenges', formulate your value for this property without asking additional questions.\n"
                "Your response should be a single sentence.")
           :val "They are probably talking about scheduling production on multiple lines."}

          :product-or-service-name
          {:comment "Based only on the interviewees' response to prior questions, provide a name for the product they produce or service they deliver."
           :val "plate glass"}}})

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
