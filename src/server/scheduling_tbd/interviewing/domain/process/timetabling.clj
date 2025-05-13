(ns scheduling-tbd.interviewing.domain.process.timetabling
  (:require
   [clojure.spec.alpha    :as s]
   [datahike.api          :as d]
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm clj2json-pretty]]))

(s/def :timetabling/EADS-message (s/keys :req-un [::message-type ::interview-objective ::interviewer-agent ::EADS]))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::interviewer-agent #(= % :process))

(s/def ::comment string?) ; About annotations

(s/def ::EADS (s/keys :req-un [::EADS-id ::event-types ::timeslots]))
(s/def ::EADS-id #(= % :process/timetabling))

(s/def ::event-types (s/or :normal :event-types/val :annotated ::annotated-event-types))
(s/def :event-types/val (s/coll-of ::event-type :kind vector?))
(s/def ::annotated-event-types (s/keys :req-un [::comment :event-types/val]))
(s/def ::event-type (s/or :normal :event-type/val :annotated ::annotated-event-type))
(s/def :event-type/val (s/keys :req-un [::event-type-name ::occurrence-assignment] :opt-un [::periodicity ::event-resources]))
(s/def ::annotated-event-type (s/keys :req-un [::comment :event-type/val]))

(s/def ::event-type-name (s/or :normal :event-type-name/val :annotated ::annotated-event-type-name))
(s/def :event-type-name/val string?)
(s/def ::annotated-event-type-name (s/keys :req-un [::comment :event-name/val]))

(s/def ::occurrence-assignment (s/or :normal :occurrence-assignment/val :annotated ::annotated-occurrence-assignment))
(s/def :occurrence-assignment/val (s/keys :opt-un [::timeslot-refs ::constraints ::opportunistic?]))
(s/def ::annotated-occurrence-assignment (s/keys :req-un [::comment :occurrence-assignment/val]))
(s/def ::timeslot-refs (s/or :normal :timeslot-refs/val :annotated ::annotated-timeslots-ref))
(s/def :timeslot-refs/val (s/coll-of ::timeslot-ref :kind vector?))
(s/def ::timeslot-ref string?)
(s/def ::annotated-timeslots-ref (s/keys :opt-un [::comment :timeslots-ref/val]))
(s/def ::constraints (s/or :normal :constraints/val :annotated ::annotated-constraints))
(s/def :constraints/val (s/coll-of ::constraint :kind vector?))
(s/def ::annotated-constraints (s/keys :req-un [::comment :constraints/val]))

(s/def ::constraint (s/or :normal :constraint/val :annotated ::annotated-constraint))
(s/def :constraint/val string?)
(s/def ::annotated-constraint (s/keys :req-un [::comment :constraint/val]))

(s/def ::opportunistic? (s/or :normal :opportunistic/val :annotated ::annotated-opportunistic?))
(s/def :opportunistic?/val  boolean?)
(s/def ::annotated-opportunistic? (s/keys :req-un [::comment :opportunistic?/val]))

(s/def ::periodicity (s/or :normal :periodicity/val :annotated ::annotated-periodicity))
(s/def :periodicity/val (s/keys :req-un [::interval ::occurrences]))
(s/def ::annotated-periodicity (s/keys :req-un [::comment :periodicity/val]))
(s/def ::interval (s/or :normal :interval/val :annotated ::annotated-interval))
(s/def :interval/val (s/keys :req-un [::units ::value-string]))
(s/def ::annotated-interval (s/keys :req-un [::comment :interval/val]))
(s/def ::occurrences (s/or :normal :occurrences/val :annotated ::annotated-occurrences))
(s/def :occurrences/val (s/keys :req-un [::value-string]))
(s/def ::annotated-occurrences (s/keys :req-un [::comment :occurrences/val]))
(s/def ::units (s/or :normal :units/val :annotated ::annotated-units))
(s/def :units/val string?)
(s/def ::annotated-units (s/keys :req-un [::comment :units/val]))
(s/def ::value-string (s/or :normal :value-string/val :annotated ::annotated-value-string))
(s/def :value-string/val string?)
(s/def ::annotated-value-string (s/keys :req-un [::comment :value-string/val]))


;;; Promises and Pitfalls: Using LLMs to Generate Visualization Items (Fumeng Yang et al. (fy@umg.edu)
;;; How High School Teachers Develop Tests and How AI Could Help.    (Fumeng Yang et al.) (Maybe not published yet.)
;;; There was 1 quick slide on decision-making; she has one grad student working on it.
;;; https://news.northwestern.edu/stories/2020/10/presidential-plinko-visualizes-data-uncertainty/


;;; ToDo: add a timetabled? property to flow and job shop EADS processes ????
;;; ToDo: Define the enumerations that are being used!
;;; (s/explain :timetabling/EADS-message ttable/timetabling)
(def timetabling
    {:message-type :EADS-INSTRUCTIONS
     :interviewer-agent :process
     :interview-objective
     (str "Whereas most other EADS-INSTRUCTIONS in the process interview focus on characterizing the processes by which the interviewees' enterprise produces product or delivers a service,\n"
          "process/timetabling does not. Timetabling is about assigning limited resources, such as classrooms, teachers, or machinery, to events types that will occur in timeslots.\n"
          "Timetabling interviews are about characterizing the resources, event types, and timeslots (ts), not how enterprise makes product or delivers a service.\n"
          "A timetabling discussion mediated by these EADS-INSTRUCTIONS can occur as a focused examination of some subprocess of a larger process for which you have already had some discussion.\n"
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
          "    (2) one-time: their 'occurrence-assigment' property defines when they occur using a date, and,\n"
          "    (3) opportunistic: their 'occurrence-assigment' property defines 'opportunistic? = true' and conditions (possibly including periodicity) in which they occur.\n"
          "\n"
          "You can think of the information you are trying to capture in the interview as including descriptions of\n"
          "   (1) a Cartesian product of resources needed for the event type to occur,\n"
          "   (2) rules for what resources can, must, or must not occur together in instances of the Cartesian product, and\n"
          "   (3) how instances of the Cartesian product can be assigned to timeslots.\n"
          "\n"
          "Finally, we must confess to not knowing too much about what to expected from timetabling interviews; we haven't studied this area as much as some of the others.\n"      ; <=== Probably temporary.
          "With this in mind, feel free to use the 'invented' property discussed in the interviewer instructions whenever you think the EADS isn't capturing something important.") ; <=== Probably temporary.
     :EADS
     {:EADS-id :process/timetabling
      :event-types {:comment (str "The event-types property is a list of objects that capture\n"
                                  "   *  'event-resources'        : a Cartesian product of resources in the sense described in the interview objectives, and\n"
                                  "   *  'periodicity'            : (optional) the interval and number of instances in which periodic events of this event type occur.\n"
                                  "   *  'occurrence-assignment'  : the timeslots in which this event type are allowed to occur.\n"
                                  "This example is about timetabling classes at a community college for one semester.")
                    :val [{:event-type-name {:val "lecture-class-30-90min-type"
                                             :comment (str "Note that we used the suffix '-type' in the name to emphasize that this defines the general form for instances of this class, not an instance occurrence.\n"
                                                           "Example lecture-class-types include chemistry lectures and physics lectures.\n"
                                                           "You, the interviewer, decided on this naming convention in light of the conversation you had with interviewees.")}
                           :event-resources {:comment "These are the elements of the Cartesian product. When no quantity is specified, we assume exactly one is required."
                                             :val [{:resource-type {:val "room-type-30"
                                                                    :comment (str "Note the -30 suffix here is because (as shown below) the room should have capacity for 30 people and the event is '-30-90min-type'.\n"
                                                                                  "Similar to 'event-type-name', you devised this naming convention.\n"
                                                                                  "The meaning of room-type-30 will be made clear in the resources interview, not here.")}
                                                    :base-type {:val "place" :comment "Possible values for this property are 'human', 'place', and 'equipment'."}}
                                                   {:resource-type "student-type"    :base-type "human" :quantity {:units "person" :value-string "30" :modifier "up to"}}
                                                   {:resource-type "instructor-type" :base-type "human"}]}
                           :periodicity {:val {:interval {:units "week" :value-string "1"}
                                               :occurrences {:value-string "2"}}
                                         :comment (str "This periodicity object describes the event type as (being periodic and) having two occurrence per week.\n"
                                                       "For example, in this case, the interviewees suggested that many courses are taught as two 90-minute lectures per week.")}
                           :occurrence-assignment {:val {:ts-type-ref ["Tu-Th-90min"]
                                                          :constraints  ["same-time", "different-days"]}
                                                    :comment (str "'occurrence-assignment objects define a disjunction of possibilities of where in time the event can be assigned.\n"
                                                                  "The 'ts-type-ref' property is a list referring to timeslots. (These are defined below.)\n"
                                                                  "The 'constraints' property is a list of enumeration values. These define where in time these events can be assigned to occur.\n"
                                                                  "same-time indicates that events must be in the same periodicity interval must occur at the same time of day.\n"
                                                                  "different-days indicates that events in the same periodicity interval must be assigned to different days.\n"
                                                                  "Because there are only two days, Tuesday and Thursday, in the 'Tu-Th-90min' timeslot class, and the periodicity is 2 occurences per week,\n"
                                                                  "it must be the case that one event occurs on Tuesday and the other on Thursday each week (and at the same time of day).")}}

                          {:event-type-name "lecture-class-30-60min-type"
                           :event-resources [{:resource-type "room-type-30",   :base-type "place"}
                                             {:resource-type "student-type",   :base-type "human"}
                                             {:resource-type "instructor-type" :base-type "human"}]
                           :periodicity {:interval {:units "week" :value-string "1"} :occurrences {:value-string "3"}}
                           :occurrence-assignment {:ts-type-ref ["Mon-Wed-Fri-90min"]
                                                   :constraints  ["same-time", "different-days"]}}

                          {:event-type-name "lab-class"
                           :event-resources [{:resource-type "room-type-lab"      :base-type "place"}
                                             {:resource-type "student-type"       :base-type "human"}
                                             {:resource-type "lab-assistant-type" :base-type "human" :quantity {:units "person" :value-string "2"}}]
                           :periodicity {:interval {:units "week" :value-string "1"} :occurrences {:value-string "1"}}
                           :occurrence-assignment {:ts-type-ref ["Three-hour-lab"]}}

                          {:event-type-name {:val "instructor-break" :comment "This is an example of an opportunistic event with periodicity."}
                           :event-resources [{:resource-type "instructor-type" :base-type "human"}]
                           :periodicity {:interval {:units "day" :value-string "1"} :occurrences {:value-string "1"}}
                           :occurrence-assigment {:opportunistic? true,
                                                  :constraints {:val ["every-day", "once"]
                                                                :comment (str "The purpose of this event type is to ensure the no instructor has to work every timeslot.\n"
                                                                              "Interviewees stipulated this in the conversation.")}
                                                  :ts-type-ref ["Mon-Wed-Fri-60min", "Tu-Th-90min", "Three-hour-lab"]}}

                          {:event-type-name {:val "spring-break" :comment "This is an example of a one-time event. It has no 'event-resources'; in this sense, it is a non-event!"}
                           :occurrence-assigment {:ts-type-ref {:val ["2025-03-17" "2025-03-18" "2025-03-19" "2025-03-20" "2025-03-21"]
                                                                :comment "Instead of enumeration values, we put dates here."}}}]}

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

(if (s/valid? :timetabling/EADS-message timetabling)
  (let [db-obj {:EADS/id :process/timetabling
                :EADS/cid :process
                :EADS/specs #:spec{:full :timetabling/EADS-message}
                :EADS/msg-str (str timetabling)}
        conn (connect-atm :system)
        eid (d/q '[:find ?e . :where [?e :system/name "SYSTEM"]] @conn)]
    (d/transact conn {:tx-data [{:db/id eid :system/EADS db-obj}]})
    ;; Write the EADS JSON to resources/EADS/process so it can be placed in ork's vector store.
    (->> timetabling clj2json-pretty (spit "resources/EADS/process/timetabling.json")))
  (throw (ex-info "Invalid EADS message (timetabling)." {})))
