(ns scheduling-tbd.iviewr.domain.process.warm-up-with-challenges
  "Define a EADS to elicit general information about the scheduling problem the interviewees are interested in solving."
  (:require
   [clojure.pprint                  :refer [pprint]]
   [clojure.spec.alpha              :as s]
   [mount.core                      :as mount :refer [defstate]]
   [scheduling-tbd.agent-db         :refer [agent-log]]
   [scheduling-tbd.db               :as db]
   [scheduling-tbd.iviewr.eads-util :as eads-util :refer [ds-complete? combine-ds!]]
   [scheduling-tbd.sutil            :as sutil]))

(s/def :warm-up-with-challenges/EADS-message (s/keys :req-un [::message-type ::interview-objective ::EADS]))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::comment string?)

(s/def ::EADS (s/keys :req-un [::scheduling-challenges ::one-more-thing ::product-or-service-name]
                      :opt-un [::msg-id ::EADS-ref]))
(s/def ::scheduling-challenges (s/or :normal :scheduling-challenges/val :annotated ::annotated-scheduling-challenges))
(s/def :scheduling-challenges/val (s/coll-of string? :kind vector?))
(s/def ::annotated-scheduling-challenges (s/keys :req-un [:scheduling-challenges/val ::comment]))

(s/def ::one-more-thing (s/or :normal :one-more-thing/val :annotated ::annotated-one-more-thing))
(s/def :one-more-thing/val  string?)
(s/def ::annotated-one-more-thing (s/keys :req-un [:one-more-thing/val ::comment]))

(s/def ::product-or-service-name (s/or :normal :product-or-service-name/val :annotated ::annotated-product-or-service-name))
(s/def :product-or-service-name/val  string?)
(s/def ::annotated-product-or-service-name (s/keys :req-un [:product-or-service-name/val ::comment]))

(def ^:diag diag (atom nil))

(def warm-up-with-challenges
  {:message-type :EADS-INSTRUCTIONS,
   :interview-objective
   (str
    "This is typically the EADS the orchestrator will use first.\n"
    "Use it in cases where there has been very little conversation so far, where the activities property of CONVERSATION-HISTORY is empty or nearly so.\n"
    "The objective of this interview segment is to \n"
    "   1) get the interviewees started in discussing their scheduling problem, and,\n"
    "   2) make some basic observations about the scheduling challenges they face.\n"
    "The first question to ask in this interview segment, which we call the 'warm up question' is simply this:\n"
    "\n"
    " 'What are the products you make or the services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences.' \n"
    "\n"
    "But remember, you don't ask that question until you receive a SUPPLY-QUESTION message!\n"
    "The correct response to an EADS-INSTRUCTIONS message such as this one is always, {\"message-type\": \"STATUS\", \"status\": \"OK\"}.\n"
    "Completing the EADS (responding with a complete DATA-STRUCTURE-REFINEMENT message) may require more than just that one question, however.\n"
    "Examine the EADS to determine what other questions you may wish to ask.\n")
   :EADS {:EADS-id :process/warm-up-with-challenges
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
(when-not (s/valid? :warm-up-with-challenges/EADS-message warm-up-with-challenges)
  (throw (ex-info "Invalid EADS (scheduling problem type)" {})))

;;; ------------------------------- checking for completeness ---------------
;;; Collect and combine :process/warm-up-with-challenges ds refinements, favoring recent over earlier versions.
(defmethod combine-ds! :process/warm-up-with-challenges
  [tag pid]
  (let [merged (->> (db/get-msg-dstructs pid tag)
                    (sort-by :msg-id)
                    (reduce (fn [r m] (merge r m)) {})
                    eads-util/strip-annotations)
        merged (-> merged
                   (update :principal-problem-type keyword)
                   (update :problem-components (fn [pcomps] (mapv #(keyword %) pcomps))))]
    (db/put-summary-ds! pid tag merged)))

(defmethod ds-complete? :process/warm-up-with-challenges
  [tag pid]
  (let [ds (-> (db/get-summary-ds pid tag) eads-util/strip-annotations)
    complete? (s/valid? ::EADS ds)]
    (agent-log (str "This is the stripped DS for problem type (complete? = " complete? "):\n" (with-out-str (pprint ds)))
               {:console? true #_#_:elide-console 130})
    complete?))

;;; (warm/init-warm-up-with-challenges)
;;; ------------------------------- starting and stopping ---------------
(defn init-warm-up-with-challenges
  []
  (if (s/valid? :warm-up-with-challenges/EADS-message warm-up-with-challenges)
    (when-not (db/same-EADS-instructions? warm-up-with-challenges)
      (sutil/update-resources-EADS-json! warm-up-with-challenges)
      (db/put-EADS-instructions! warm-up-with-challenges))
    (throw (ex-info "Invalid EADS message (warm-up-with-challenges)." {}))))

(defstate init-warm-up-eads
  :start (init-warm-up-with-challenges))
