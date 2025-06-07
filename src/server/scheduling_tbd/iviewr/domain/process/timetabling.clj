(ns scheduling-tbd.iviewr.domain.process.timetabling
  (:require
   [clojure.edn              :as edn]
   [clojure.pprint           :refer [pprint]]
   [clojure.set              :as set]
   [clojure.spec.alpha       :as s]
   [datahike.api             :as d]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.agent-db  :refer [agent-log]]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm]]
   [scheduling-tbd.iviewr.eads-util :as eu :refer [ds-complete? combine-ds!]]
   [taoensso.telemere        :refer [log!]]))

(def ^:diag diag (atom nil))

;;; ToDo. We need a separate spec repository for each EADS, or better naming in order to avoid name collisions.
;;;       Better naming: for example, preface all namespaces like 'units' with 'tt'. 'tt-units'.

(s/def :timetabling/EADS-message (s/keys :req-un [::interview-objective ::interviewer-agent ::EADS ::message-type]))
(s/def ::EADS (s/keys :req-un [::event-types ::timeslots]
                      :opt-un [::msg-id ::EADS-ref ::EADS-id]))
(s/def ::interview-objective string?)
(s/def ::interviewer-agent keyword?)
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::EADS-id keyword?)

;;; (dutil/make-specs ttable/timetabling "timetabling")
;;; Created Sat May 17 18:55:10 EDT 2025 using develop.dutil/make-spec."
(s/def ::event-types (s/or :normal :event-types/val :annotated ::annotated-event-types))
(s/def :event-types/val (s/coll-of ::event-type :kind vector?))
(s/def ::event-type (s/keys :req-un [::occurrence-assignment ::event-type-id] :opt-un [::periodicity ::event-constituents]))
(s/def ::annotated-event-types (s/keys :req-un [::comment :event-types/val]))
(s/def ::timeslots (s/or :normal :timeslots/val :annotated ::annotated-timeslots))
(s/def :timeslots/val (s/coll-of ::timeslot :kind vector?))
(s/def ::timeslot (s/keys :req-un [::spans ::ts-type-id]))
(s/def ::annotated-timeslots (s/keys :req-un [::comment :timeslots/val]))

;;; We make event-constituents mandatory, and hope the answer is 'none'.
(s/def ::event-constituents (s/or :normal :event-constituents/val :annotated ::annotated-event-constituents))
(s/def :event-constituents/val (s/or :normal (s/coll-of ::event-constituent :kind vector?) :none string?))
(s/def ::event-constituent (s/keys :req-un [::base-type ::constituent-type] :opt-un [::quantity]))
(s/def ::annotated-event-constituents (s/keys :req-un [::comment :event-constituents/val]))
(s/def ::event-type-id (s/or :normal :event-type-id/val :annotated ::annotated-event-type-id))
(s/def :event-type-id/val string?)
(s/def ::annotated-event-type-id (s/keys :req-un [::comment :event-type-id/val]))
(s/def ::occurrence-assignment (s/or :normal :occurrence-assignment/val :annotated ::annotated-occurrence-assignment))
(s/def :occurrence-assignment/val (s/keys :req-un [::ts-type-refs]))
(s/def ::annotated-occurrence-assignment (s/keys :req-un [::comment :occurrence-assignment/val]))
(s/def ::periodicity (s/or :normal :periodicity/val :annotated ::annotated-periodicity))

;;; We make periodicity mandatory, and hope the answer is 'none'.
(s/def :periodicity/val (s/or :normal (s/keys :req-un [::occurrences ::interval]) :none string?))
(s/def ::annotated-periodicity (s/keys :req-un [::comment :periodicity/val]))
(s/def ::spans (s/or :normal :spans/val :annotated ::annotated-spans))
(s/def :spans/val (s/coll-of ::span :kind vector?))
(s/def ::span (s/keys :req-un [::periods ::span-id]))
(s/def ::annotated-spans (s/keys :req-un [::comment :spans/val]))
(s/def ::ts-type-id (s/or :normal :ts-type-id/val :annotated ::annotated-ts-type-id))
(s/def :ts-type-id/val string?)
(s/def ::annotated-ts-type-id (s/keys :req-un [::comment :ts-type-id/val]))
(s/def ::base-type (s/or :normal :base-type/val :annotated ::annotated-base-type))
(s/def :base-type/val string?)
(s/def ::annotated-base-type (s/keys :req-un [::comment :base-type/val]))
(s/def ::quantity (s/or :normal :quantity/val :annotated ::annotated-quantity))
(s/def :quantity/val (s/keys :req-un [::value-string ::units] :opt-un [::modifier]))
(s/def ::annotated-quantity (s/keys :req-un [::comment :quantity/val]))
(s/def ::constituent-type (s/or :normal :constituent-type/val :annotated ::annotated-constituent-type))
(s/def ::modifier (s/or :normal :modifier/val :annotated ::annotated-modifier))
(s/def :modifier/val string?)
(s/def ::annotated-modifier (s/keys :req-un [::comment :modifier/val]))
(s/def :constituent-type/val string?)
(s/def ::annotated-constituent-type (s/keys :req-un [::comment :constituent-type/val]))
(s/def ::ts-type-refs (s/or :normal :ts-type-refs/val :annotated ::annotated-ts-type-refs))
(s/def :ts-type-refs/val (s/coll-of ::ts-type-ref :kind vector?))
(s/def ::ts-type-ref string?)
(s/def ::annotated-ts-type-refs (s/keys :req-un [::comment :ts-type-refs/val]))
(s/def ::interval (s/or :normal :interval/val :annotated ::annotated-interval))
(s/def :interval/val (s/keys :req-un [::value-string ::units]))
(s/def ::annotated-interval (s/keys :req-un [::comment :interval/val]))
(s/def ::occurrences (s/or :normal :occurrences/val :annotated ::annotated-occurrences))
(s/def :occurrences/val (s/keys :req-un [::value-string]))
(s/def ::annotated-occurrences (s/keys :req-un [::comment :occurrences/val]))
(s/def ::periods (s/or :normal :periods/val :annotated ::annotated-periods))
(s/def :periods/val (s/coll-of ::period :kind vector?))
(s/def ::period string?)
(s/def ::annotated-periods (s/keys :req-un [::comment :periods/val]))
(s/def ::span-id (s/or :normal :span-id/val :annotated ::annotated-span-id))
(s/def :span-id/val string?)
(s/def ::annotated-span-id (s/keys :req-un [::comment :span-id/val]))
(s/def ::units (s/or :normal :units/val :annotated ::annotated-units))
(s/def :units/val string?)
(s/def ::annotated-units (s/keys :req-un [::comment :units/val]))
(s/def ::value-string (s/or :normal :value-string/val :annotated ::annotated-value-string))
(s/def :value-string/val string?)
(s/def ::annotated-value-string (s/keys :req-un [::comment :value-string/val]))
(s/def ::units (s/or :normal :units/val :annotated ::annotated-units))
(s/def :units/val string?)
(s/def ::annotated-units (s/keys :req-un [::comment :units/val]))
(s/def ::value-string (s/or :normal :value-string/val :annotated ::annotated-value-string))
(s/def :value-string/val string?)
(s/def ::annotated-value-string (s/keys :req-un [::comment :value-string/val]))
(s/def ::value-string (s/or :normal :value-string/val :annotated ::annotated-value-string))
(s/def :value-string/val string?)
(s/def ::annotated-value-string (s/keys :req-un [::comment :value-string/val]))

;;; ToDo: add a timetabled? property to flow and job shop EADS processes ????
;;; ToDo: Define the enumerations that are being used!
;;; (s/explain :timetabling/EADS-message ttable/timetabling)
(def timetabling
    {:message-type :EADS-INSTRUCTIONS
     :interviewer-agent :process
     :interview-objective
     (str "Whereas most other EADS-INSTRUCTIONS in the process interview focus on characterizing the processes by which the interviewees' enterprise produces product or delivers a service,\n"
          "process/timetabling does not. Timetabling is about assigning limited resources, such as classrooms, teachers, or machinery, to events types that will occur in timeslots.\n"
          "Timetabling interviews are about characterizing the constituents (role players), event types, and timeslots (ts), not how enterprise makes product or delivers a service.\n"
          "A timetabling discussion mediated by these EADS-INSTRUCTIONS might be tackling the interviewees principal scheduling problem, or the problem of some subprocess of it.\n"
          "For example, you might have pursued the process/flow-shop EADS, and learned that a particular subprocess in the flow uses timetabling.\n"
          "\n"
          "The specifics of timetablings problems can vary widely depending on the enterprises's goals and circumstances.\n"
          "For example, in some cases, the time at which the event occurs can be chosen in light of opportunity.\n"
          "For example, one could timetable the use of a heat-treat oven based on having available a sufficient number of parts needing the same heat treat process.\n"
          "Likewise one might timetable equipment maintenance opportunistically.\n"
          "Another opportunistic timetabling problem might involve perishable raw materials, such as food ingredients.\n"
          "Many timetabling problems are not opportunistic, scheduling classes in a university, for example.\n"
          "\n"
          "There are three kinds of event types in our formulation of timetabling:\n"
          "    (1) regularly scheduled: they have property named 'periodicity',\n"
          "    (2) one-time: their 'occurrence-assignment' property defines when they occur using a date, and,\n"
          "    (3) opportunistic: their 'occurrence-assignment' property defines 'opportunistic? = true' and conditions (possibly including periodicity) in which they occur.\n"
          "\n"
          "You can think of the information you are trying to capture in the interview as including descriptions of\n"
          "   (1) a Cartesian product of constituents needed for the event type to occur,\n"
          "   (2) rules for what constituents can, must, or must not occur together in instances of the Cartesian product, and\n"
          "   (3) how instances of the Cartesian product can be assigned to timeslots.\n"
          "\n"
          "Be careful in how you define the quantity properties of event-constituent (event-consituent.quantity). These are not about how many of the constituant-type the inteviewees have.\n"
          "The Resource Interviewer is responsible for making that determination, not you.\n"
          "Instead, event-constituent.quantity is about how many of the constituent-type might take part in the event. That quantity might signify the capacity of another constituent of the event type.\n"
          "For example, if a lecture room has capacity for 30 students. The event-constituent.quantity of students could be up to 30, but there is only one lecture room involved in the event type.\n"
          "\n"
          "You have a choice as to how you communicate back to us in DATA-STRUCTURE-REFINEMENT (DSR) messages: you can accummulate information from interviewing about 'event-types' and 'timeslots'\n"
          "into a single structure, such as shown in the EADS example below.\n"
          "Alternatively -- because we have code to combine the several DSRs you return in response to INTERVIEWEES-RESPOND -- you can send back just what you learned from the last few questions.\n"
          "In this arrangement you would minimally be returning an object for which we can navigate event-types.event-type-id or timeslots.ts-type-id and what you provide will overwrite any\n"
          "existing event-type object or timeslot-type object with that identifier."
          "The challenge remains, however, that the event-type's occurrence-assignment.ts-type-refs refers to a timeslot's ts-type-id.\n")

     ;"Finally, we must confess to not knowing too much about what to expected from timetabling interviews; we haven't studied this area as much as some of the others.\n"      ; <=== Probably temporary.
     ;"With this in mind, feel free to use the 'invented' property discussed in the interviewer instructions whenever you think the EADS isn't capturing something important." ; <=== Probably temporary.
     :EADS
     {:EADS-id :process/timetabling
      :event-types {:comment (str "The event-types property is a list of objects that capture\n"
                                  "   *  'event-constituents'        : a Cartesian product of constituents in the sense described in the interview objectives, and\n"
                                  "   *  'periodicity'            : (optional) the interval and number of instances in which periodic events of this event type occur.\n"
                                  "   *  'occurrence-assignment'  : the timeslots in which this event type are allowed to occur.\n"
                                  "This example is about timetabling classes at a community college for one semester.")
                    :val [{:event-type-id {:val "lecture-class-30-90min-type"
                                             :comment (str "Note that we used the suffix '-type' in the name to emphasize that this defines the general form for instances of this class, not an instance occurrence.\n"
                                                           "Example lecture-class-types include chemistry lectures and physics lectures.\n"
                                                           "You, the interviewer, decided on this naming convention in light of the conversation you had with interviewees.")}
                           :event-constituents {:comment "These are the elements of the Cartesian product. When no quantity is specified, we assume exactly one is required."
                                             :val [{:constituent-type {:val "room-type-30"
                                                                    :comment (str "Note the -30 suffix here is because (as shown below) the room should have capacity for 30 people and the event is '-30-90min-type'.\n"
                                                                                  "Similar to 'event-type-id', you devised this naming convention.\n"
                                                                                  "The meaning of room-type-30 will be made clear in the constituents interview, not here.")}
                                                    :base-type {:val "place" :comment "Possible values for this property are 'human', 'place', and 'equipment'."}}
                                                   {:constituent-type "student-type"    :base-type "human" :quantity {:units "person" :value-string "30" :modifier "up to"}}
                                                   {:constituent-type "chalk-board" :base-type "equipment"}
                                                   {:constituent-type "instructor-type" :base-type "human"}]}
                           :periodicity {:val {:interval {:units "week" :value-string "1"}
                                               :occurrences {:value-string "2"}}
                                         :comment (str "This periodicity object describes the event type as (being periodic and) having two occurrence per week.\n"
                                                       "For example, in this case, the interviewees suggested that many courses are taught as two 90-minute lectures per week.")}
                           :occurrence-assignment {:val {:ts-type-refs ["Tu-Th-90min"]
                                                          :constraints  ["same-time", "different-days"]}
                                                    :comment (str "'occurrence-assignment objects define a disjunction of possibilities of where in time the event can be assigned.\n"
                                                                  "The 'ts-type-refs' property is a list referring to timeslots. (These are defined below.)\n"
                                                                  "The 'constraints' property is a list of enumeration values. These define where in time these events can be assigned to occur.\n"
                                                                  "same-time indicates that events must be in the same periodicity interval must occur at the same time of day.\n"
                                                                  "different-days indicates that events in the same periodicity interval must be assigned to different days.\n"
                                                                  "Because there are only two days, Tuesday and Thursday, in the 'Tu-Th-90min' timeslot class, and the periodicity is 2 occurences per week,\n"
                                                                  "it must be the case that one event occurs on Tuesday and the other on Thursday each week (and at the same time of day).")}}

                          {:event-type-id "lecture-class-30-60min-type"
                           :event-constituents [{:constituent-type "room-type-30",   :base-type "place"}
                                             {:constituent-type "student-type",   :base-type "human"}
                                             {:constituent-type "instructor-type" :base-type "human"}]
                           :periodicity {:interval {:units "week" :value-string "1"} :occurrences {:value-string "3"}}
                           :occurrence-assignment {:ts-type-refs ["Mon-Wed-Fri-90min"]
                                                   :constraints  ["same-time", "different-days"]}}

                          {:event-type-id "chem-lab-class"
                           :event-constituents {:val [{:constituent-type "room-type-chem-lab" :base-type "place"}
                                                   {:constituent-type "fume hood"          :base-type "equipment" :quantity {:value-string "10" :units "instances"}}
                                                   {:constituent-type "student-type"       :base-type "human"}
                                                   {:constituent-type "student-type"       :base-type "human"}
                                                   {:constituent-type "lab-assistant-type" :base-type "human" :quantity {:units "person" :value-string "2"}}]
                                             :comment (str "There is certainly more equipment required in a chemistry lab than just 'fume hood', but that constituent alone should be sufficient\n"
                                                           "to distinguish chemistry lab events from other kinds of other class events.")}
                           :periodicity {:interval {:units "week" :value-string "1"} :occurrences {:value-string "1"}}
                           :occurrence-assignment {:ts-type-refs ["Three-hour-lab"]}}

                          {:event-type-id {:val "instructor-break" :comment "This is an example of an opportunistic event with periodicity."}
                           :event-constituents [{:constituent-type "instructor-type" :base-type "human"}]
                           :periodicity {:interval {:units "day" :value-string "1"} :occurrences {:value-string "1"}}
                           :occurrence-assignment {:opportunistic? true,
                                                  :constraints {:val ["every-day", "once"]
                                                                :comment (str "The purpose of this event type is to ensure the no instructor has to work every timeslot.\n"
                                                                              "Interviewees stipulated this in the conversation.")}
                                                  :ts-type-refs ["Mon-Wed-Fri-60min", "Tu-Th-90min", "Three-hour-lab"]}}

                          {:event-type-id {:val "spring-break" :comment "This is an example of a one-time event. It has no 'event-constituents'; in this sense, it is a non-event!"}
                           :event-constituents {:val "none"
                                             :comment (str "By the end of your interview, all event type objects should have 'event-type-id', 'event-constituents', 'periodicity', and\n"
                                                           "'occurrence-assignment'. Spring break does not require any constituents, so we'll just say 'none' here.")}
                           :occurrence-assignment {:ts-type-refs {:val ["2025-03-17" "2025-03-18" "2025-03-19" "2025-03-20" "2025-03-21"]
                                                                  :comment "Instead of enumeration values, we put dates here."}}
                           :periodicity {:val "none"
                                         :comment (str "By the end of your interview, all event type objects should have 'event-type-id', 'event-constituents', 'periodicity', and\n"
                                                       "'occurrence-assignment'. Since in the context of one semester, spring break only occurs once, we'll just say 'none' here.\n"
                                                       "By the way, we make things mandatory and allow 'none' so that we can check the data structure for completeness more thoroughly.")}}]}

      :timeslots [{:ts-type-id "Mon-Wed-Fri-60min"
                   :spans [{:span-id "Monday"    :periods  ["9:00-9:50" "10:00-10:50" "11:00-11:50" "13:00-13:50" "14:00-14:50" "15:00-15:50" "16:00-16:50"]}
                           {:span-id "Wednesday" :periods  ["9:00-9:50" "10:00-10:50" "11:00-11:50" "13:00-13:50" "14:00-14:50" "15:00-15:50" "16:00-16:50"]}
                           {:span-id "Friday"    :periods  ["9:00-9:50" "10:00-10:50" "11:00-11:50" "13:00-13:50" "14:00-14:50" "15:00-15:50" "16:00-16:50"]}]}

                  {:ts-type-id "Tu-Th-90min"
                   :spans [{:span-id "Tuesday"   :periods {:val ["9:00-10:20" "10:30-11:50" "13:00-14:20" "14:30-15:50" "16:00-17:20"]
                                                           :comment (str "We asked the interviewees about the regularity of class events and came up with this formulation.\n"
                                                                         "Try to stick to a common syntax for 'period' for all values in the 'period-values' property.\n"
                                                                         "Note that our '90 minute' periods actually run 80 minutes to give people time to get to their next class, etc.\n")}}
                           {:span-id "Thursday" :periods  ["9:00-10:20" "10:30-11:50" "13:00-14:20" "14:30-15:50" "16:00-17:20"]}]}

                  {:ts-type-id "Three-hour-lab"
                   :spans [{:span-id "Monday"    :periods  ["9:00-11:50" "13:00-15:50"]}
                           {:span-id "Tuesday"   :periods  ["9:00-11:50" "13:00-15:50"]}
                           {:span-id "Wednesday" :periods  ["9:00-11:50" "13:00-15:50"]}
                           {:span-id "Thursday"  :periods  ["9:00-11:50" "13:00-15:50"]}
                           {:span-id "Friday"    :periods  ["9:00-11:50" "13:00-15:50"]}]}]}})

;;; ------------------------------- checking for completeness ---------------
;;; Collect and combine :process/timetabling ds refinements, favoring recent over earlier versions.
;;; Interviewer has been instructed that it is okay to do :event-types separate from :timeslots, and in fact, it
;;; is currently giving things *very piecemeal*, suggesting that I use :ts-type-id and :event-type-id to stitch together the result.
(defn combine
  "Accommodate information from the second argument map into the first argument map,
   returning the modified first argument. We don't do upsert, just insert and replace
   on timeslot-types (by :ts-type-id) and event-types (by :event-type-id)."
  [best more]
  (let [best-ts-type-ids    (eu/collect-keys-vals best :ts-type-id)
        more-ts-type-ids    (eu/collect-keys-vals more :ts-type-id)
        best-event-type-ids (eu/collect-keys-vals more :event-type-id)
        more-event-type-ids (eu/collect-keys-vals more :event-type-id)
        ts-inserts   (set/difference more-ts-type-ids best-ts-type-ids)
        ts-replaces  (set/intersection best-ts-type-ids more-ts-type-ids)
        ev-inserts   (set/difference more-event-type-ids best-event-type-ids)
        ev-replaces  (set/intersection best-event-type-ids more-event-type-ids)]
    ;; <============================================================================================= ToDo: (1) Make sure best has :event-types and :timeslots.
    ;; <=============================================================================================       (2) Remove :event-types and :timeslots if they are empty when done.
    (as-> best ?b
      (reduce (fn [r new-id] (eu/insert-by-id r :timeslots   (eu/get-object more new-id))) ?b ts-inserts)
      (reduce (fn [r new-id] (eu/insert-by-id r :event-types (eu/get-object more new-id))) ?b ev-inserts)
      (reduce (fn [r new-id] (eu/replace-by-id r :timeslots   :ts-type-id    (eu/get-object more new-id))) ?b ts-replaces)
      (reduce (fn [r new-id] (eu/replace-by-id r :event-types :event-type-id (eu/get-object more new-id))) ?b ev-replaces))))


(defmethod combine-ds! :process/timetabling
  [tag pid]
  (let [msg-ds (->> (db/get-msg-dstructs pid tag)
                    (mapv eu/strip-annotations))
        merged (reduce (fn [r ds] (combine r ds))
                       (first msg-ds) ; We start with earliest instance of the DS. <==== NOT GOOD: Maybe start with a full merge.
                       (rest msg-ds))]
    (db/put-summary-ds! pid
                        :process/timetabling
                        (assoc merged :EADS-id tag))))

;;; (-> ttable/timetabling :EADS eu/strip-annotations ttable/completeness-test)
(defn completeness-test
  "A timetabling DS is ds-complete? when (conjunctively):
      - there is at least one event-type and all the event types have event-type-id, event-constituents, periodicity, and occurrence-assignment,
      - there is at least one value in timeslots and it has ts-type-id and and spans."
  [eads]
  (and (contains? eads :event-types)
       (s/valid? ::event-types (:event-types eads))
       (contains? eads :timeslots)
       (s/valid? ::timeslots (:timeslots eads))))

(defmethod ds-complete? :process/timetabling
  [tag pid]
  (let [ds (-> (db/get-summary-ds pid tag) eu/strip-annotations)
    complete? (completeness-test ds)]
    (agent-log (str "This is the stripped DS for timetabling (complete? = " complete? "):\n" (with-out-str (pprint ds)))
               {:console? true :elide-console 130})
    complete?))

;;; -------------------- Starting and stopping -------------------------
(defn init-timetabling
  []
  (if (s/valid? :timetabling/EADS-message timetabling)
    (when-not (db/same-EADS-instructions? timetabling)
      (log! :info "Updating timetabling in resources and system DB.")
      (sutil/update-resources-EADS-json! timetabling)
      (db/put-EADS-instructions! timetabling))
    (throw (ex-info "Invalid EADS message (timetabling)." {}))))

(defstate timetabling-eads
  :start (init-timetabling))
