(ns scheduling-tbd.iviewr.ork-test
  (:require
   [clojure.data.json                        :as cdjson]
   [clojure.test                             :refer [deftest is testing]]
   [scheduling-tbd.agent-db                  :as adb]
   [scheduling-tbd.db                        :as db]
   [scheduling-tbd.iviewr.interviewers :as inv]
   [scheduling-tbd.iviewr.ork          :as ork]
   [scheduling-tbd.surrogate                 :as sur]
   [scheduling-tbd.web.websockets            :as ws]
   [taoensso.telemere                        :as tel :refer [log!]]))

(defonce pid nil #_(db/create-proj-db! {:project/id :orch-test :project/name "orch-test"} {} {:force-this-name? true}))

#_(def ork
  (if (some #(= % :plate-glass-ork) (db/list-projects))
    (ork/ensure-ork! :plate-glass-ork)
    (log! :error "Project :plate-glass-ork doesn't exist. Most projects should work for this test; change it.")))

(def ork nil)

(defn music-school
  "Run a surrogate with special instructions (using start-surrogate+)."
  []
  (sur/start-surrogate+
   {:client-id :console
    :map-str (str {:pid :sur-music-school
                   :pname "Music School"
                   :sur-instructions (str "You run a private music school. Mostly students of your school are from local middle schools and high schools.\n"
                                          "Your task is to answer questions about scheduling lessons at this school, committing to realistic details you make up rather than speaking about music schools in general.\n"
                                          "Keep your answers short, typically less than 10 sentences; the interviewer will get the whole story by asking additional question that follow from your reply.\n"
                                          "You only schedule two kinds of things: lessons for individual students, and explicit reservations for rooms by instructors for various reasons we don't care about.\n" ; This for the weekend, at least.
                                          "Until today you've been scheduling student lessons by fiddling with a spreadsheet.\n"
                                          "But that is error-prone and time consuming. The problem is that you need to bring each student together with their instructor\n"
                                          "in a room having the right equipment. (Only some practice rooms have pianos or drums.) Of course, instructors don't want\n"
                                          "to come in to teach just one student. Most instructors would like to do a block of a few hours of lessons on days they come in.\n"
                                          "Lessons are either 30 min, 45, or an hour.")
                   :warm-up-response "Thanks! Yes, we run a music school and we'd like a system that helps us schedule use of our practice rooms with students and instructors."
                   :expertise "running a music school"})}))

#_(defn ^:diag check-ork-knowledge-of-eads
  []
  (-> (adb/query-agent
       ork
       (sutil/clj2json-pretty
        {:message-type "BACKCHANNEL-COMMUNICATION",
         :question  "What EADS-instructions are you aware of? Respond with a JSON list of their names."}))
      sutil/output-struct2clj))

(defn tryme []
  (let [ctx {:pid :plate-glass-ork}]))

;;; These are defined in the order they are used in exercising the orchestrator.
(def ch-1 {:message-type "CONVERSATION-HISTORY",
           :interviewer-type "process",
           :activity [{:question "What are the products you make or the services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences."
                       :answer  (str "We make aluminum foil, both for sale direct to consumers and for customers with special requirements.\n"
                                     "Our principal scheduling problem revolves around coordinating raw material procurement with production schedules and customer delivery deadlines.\n"
                                     "We often face the challenge of ensuring a continuous supply of high-quality raw aluminium, which is subject to market availability and price fluctuations.\n"
                                     "The energy-intensive nature of aluminium production further mandates us to abide by specific utility schedules, failing which could lead to costly disruptions.\n"
                                     "Finally, synchronization of our manufacturing processes and customer demands require us to adeptly balance production rates with varying lead times and order sizes.")}]})

(def sup-eads-1 {:message-type "SUPPLY-EADS" :interviewer "process"})

;;; ToDo: Though I think it is okay to continue with "FLOW-SHOP-SCHEDULING-PROBLEM" just to capture the process,
;;;       I think also that I'm going to need something new for flow-shop/continuous because we probably want single-machine.
(def ch-2 {:message-type "CONVERSATION-HISTORY",
           :interviewer-type "process",
           :activity [{:question "Does production of your various products generally follow the same process, or does it vary by product?"
                       :answer   "It follows the same process. We change machine settings to accommodate differ dimensions of the products."}
                      {:data-structure {:data-structure {:EADS-id "scheduling-problem-type",
                                                         :problem-type "FLOW-SHOP-SCHEDULING-PROBLEM",
                                                         :continuous? true,
                                                         :cyclical? false}}}
                      {:scheduling-challenges ["raw-material-uncertainty", "demand-uncertainty" "variation-in-demand"]}]})

(def sup-eads-2 {:message-type "SUPPLY-EADS" :interviewer "process"})

(defn msg2json-str [msg] (with-out-str (-> msg cdjson/pprint str)))

(defn json-str2msg [json-str] (-> json-str cdjson/read-str (update-keys keyword)))

(defn status-ok? [{:keys [message-type status]}]
  (or (and (= message-type "STATUS")
           (= status "OK"))
      (log! :warn "Status not okay.")))

(defn ^:diag try-ork
  []
  (let [ork (adb/ensure-agent! {:base-type :orchestrator-agent :pid :plate-glass-ork})]
    (doseq [msg [ch-1 sup-eads-1 ch-2 sup-eads-2]]
      (let [resp (->> msg msg2json-str (adb/query-agent ork) json-str2msg)]
        (log! :info (str "Response: " resp))))))


;;;============================================================ Scratch space for timetabling

(def tslots
{:timeslots
 [{:ts-type-id "weekday-after-school",
   :spans
   [{:span-id "weekday",
     :periods
     ["3:30-4:00"
      "4:00-4:30"
      "4:30-5:00"
      "5:00-5:30"
      "5:30-6:00"
      "6:00-6:30"
      "6:30-7:00"
      "7:00-7:30"
      "7:30-8:00"
      "8:00-8:30"]}],
   :constraints
   {:buffer-duration
    {:units "minutes",
     :value-string "5",
     :comment
     "Mandatory 5-minute gaps added to improve room transitions and reduce delays."},
    :no-overlap true,
    :max-continuous-lessons
    {:value-string "5",
     :comment
     "Instructors have been asked to align their schedules to meet peak demand, but this limit ensures they are not overworked."},
    :prioritization-rules
    {:details
     [{:group "younger-students",
       :priority-time "3:30-5:00 PM",
       :comment
       "Younger students are prioritized for early time slots due to their limited availability."}
      {:group "older-students",
       :priority-time "5:00-8:30 PM",
       :comment
       "Older or more flexible students are asked to take later times."}],
     :comment
     "Time slots have been adjusted to prioritize students based on their needs and availability."}},
   :comment
   "Weekday after-school hours have been extended to 8:30 PM, and strategies like mandatory buffers, prioritization, and instructor scheduling adjustments aim to alleviate peak-hour challenges."}
  {:ts-type-id "weekend-morning",
   :spans
   [{:span-id "weekend",
     :periods
     ["9:00-9:30"
      "9:30-10:00"
      "10:00-10:30"
      "10:30-11:00"
      "11:00-11:30"
      "11:30-12:00"]}],
   :constraints
   {:buffer-duration {:units "minutes", :value-string "5"},
    :max-continuous-lessons {:value-string "4"},
    :discounted-timeslots
    {:details
     "Discounts or priority scheduling offered to students willing to shift lessons to weekends.",
     :comment
     "Encouraging weekend lessons helps to redistribute peak-hour demand from weekday timeslots."}},
   :comment
   "Weekend mornings are actively promoted with discounts to reduce weekday bottlenecks."}
  {:ts-type-id "weekend-afternoon",
   :spans
   [{:span-id "weekend",
     :periods
     ["1:00-1:30"
      "1:30-2:00"
      "2:00-2:30"
      "2:30-3:00"
      "3:00-3:30"
      "3:30-4:00"
      "4:00-4:30"
      "4:30-5:00"]}],
   :constraints
   {:buffer-duration {:units "minutes", :value-string "5"},
    :max-continuous-lessons {:value-string "4"},
    :discounted-timeslots
    {:details
     "Discounted weekend afternoons are leveraged to ease weekday scheduling constraints.",
     :comment
     "Instructors willing to open extra weekend hours are helping accommodate higher demand."}},
   :comment
   "Weekend afternoons are less utilized but promoted for students and instructors as an alternative to oversubscribed weekday slots."}
  {:ts-type-id "recital-block",
   :spans
   [{:span-id "weekend",
     :periods ["9:00-11:00" "1:00-3:00" "3:00-5:00"]}],
   :constraints
   {:no-overlap true,
    :comment
    "Non-overlapping recital blocks continue to be a priority to ensure smooth scheduling for longer events."},
   :comment
   "Recital blocks remain unchanged but are scheduled with extended advance notice to avoid resource conflicts."}],
 :invented
 {:strategies
  [{:strategy "Encouraging weekend participation",
    :details
    "Discounts and priority scheduling incentivize students and instructors to book weekend timeslots."}
   {:strategy "Extending weekday hours",
    :details
    "Weekday hours extended to 8:30 PM to create additional flexibility."}
   {:strategy "Grouping students",
    :details
    "Shared group sessions (e.g., beginner theory) reduce individual lesson demand and optimize room use."}
   {:strategy "Instructor onboarding",
    :details
    "New instructors have been added to cover high-demand weekday peak times."}],
  :comment
  "List of implemented strategies to redistribute peak demand and improve overall scheduling efficiency."}})

(def etypes
{:event-types
 [{:event-type-name "individual-instrument-lesson",
   :event-resources
   [{:resource-type "piano-room",
     :base-type "place",
     :quantity {:value-string "3", :units "rooms"},
     :comment
     "Dedicated rooms equipped with acoustic or digital pianos."}
    {:resource-type "drum-room",
     :base-type "place",
     :quantity {:value-string "1", :units "room"},
     :comment "Soundproofed room with a drum kit."}
    {:resource-type "general-practice-room",
     :base-type "place",
     :comment "Rooms for guitar, violin, or voice lessons."}
    {:resource-type "instructor-type", :base-type "human"}
    {:resource-type "student-type", :base-type "human"}],
   :occurrence-assignment
   {:ts-type-refs ["After-School-Timeslot" "Weekend-Timeslot"],
    :constraints ["no-overlap"]}}
  {:event-type-name "group-music-theory-lesson",
   :event-resources
   [{:resource-type "music-classroom",
     :base-type "place",
     :quantity {:value-string "2", :units "classrooms"},
     :comment "Classroom setup with desks, chairs, and a whiteboard."}
    {:resource-type "instructor-type", :base-type "human"}
    {:resource-type "student-type",
     :base-type "human",
     :quantity
     {:units "person", :value-string "10", :modifier "up to"}}],
   :occurrence-assignment
   {:ts-type-refs ["After-School-Timeslot" "Weekend-Timeslot"],
    :constraints ["no-conflict"]}}
  {:event-type-name "group-ensemble-session",
   :event-resources
   [{:resource-type "recital-room",
     :base-type "place",
     :quantity {:value-string "1", :units "room"},
     :comment
     "Large space equipped with chairs, music stands, and areas for multiple instruments."}
    {:resource-type "instructor-type", :base-type "human"}
    {:resource-type "student-type",
     :base-type "human",
     :quantity {:units "person", :value-string "4-8", :modifier nil}}],
   :occurrence-assignment
   {:ts-type-refs ["Weekend-Timeslot"], :constraints ["no-conflict"]}}
  {:event-type-name "recital-performance",
   :event-resources
   [{:resource-type "recital-room",
     :base-type "place",
     :quantity {:value-string "1", :units "room"}}
    {:resource-type "piano",
     :base-type "equipment",
     :quantity {:value-string "1", :units "instrument"}}
    {:resource-type "audio-equipment", :base-type "equipment"}
    {:resource-type "instructor-type", :base-type "human"}
    {:resource-type "student-type",
     :base-type "human",
     :quantity {:value-string "20-30", :units "people"}}],
   :occurrence-assignment
   {:ts-type-refs ["Quarterly-Performance-Slot"]}}
  {:event-type-name "workshop-or-masterclass",
   :event-resources
   [{:resource-type "recital-room", :base-type "place"}
    {:resource-type "music-classroom", :base-type "place"}
    {:resource-type "guest-instructor", :base-type "human"}
    {:resource-type "student-type",
     :base-type "human",
     :quantity {:value-string "15-20", :units "person"}}],
   :occurrence-assignment
   {:opportunistic? true,
    :constraints ["align-with-guest-availability"]}}]})
