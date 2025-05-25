(ns scheduling-tbd.interviewing.ork-test
  (:require
   [clojure.data.json                        :as cdjson]
   [clojure.test                             :refer [deftest is testing]]
   [scheduling-tbd.agent-db                  :as adb]
   [scheduling-tbd.db                        :as db]
   [scheduling-tbd.interviewing.interviewers :as inv]
   [scheduling-tbd.interviewing.ork          :as ork]
   [scheduling-tbd.surrogate                 :as sur]
   [scheduling-tbd.web.websockets            :as ws]
   [taoensso.telemere                        :as tel :refer [log!]]))

(defonce pid nil #_(db/create-proj-db! {:project/id :orch-test :project/name "orch-test"} {} {:force-this-name? true}))

(def ork
  (if (some #(= % :plate-glass-ork) (db/list-projects))
    (ork/ensure-ork :plate-glass-ork)
    (log! :error "Project :plate-glass-ork doesn't exist. Most projects should work for this test; change it.")))

(defn music-school
  "Run a surrogate with special instructions (using start-surrogate+)."
  []
  (sur/start-surrogate+
   {:client-id (ws/recent-client!)
    :map-str (str {:pid :sur-music-school
                   :pname "Music School"
                   :sur-instructions (str "You run a music school. Until today you've been scheduling student lessons by fiddling with a spreadsheet.\n"
                                          "But that is error-prone and time consuming. The problem is that you need to bring each student together with their instructor\n"
                                          "in a room having the right equipment. (Only some practice rooms have pianos or drums.) Of course, instructors don't want\n"
                                          "to come in to teach just one student. Most instructors would like to do a block of a few hours of lessons on days they come in.\n"
                                          "Lessons are either 30 min, 45, or an hour.")
                   :warm-up-response "Thanks! Yes, we run a music school and we'd like a system that helps us schedule use of our practice rooms with students and instructors."
                   :expertise "running a music school"})}))

(defn ^:diag check-ork-knowledge-of-eads
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
  (let [ork (adb/ensure-agent! (-> (get @adb/agent-infos :orchestrator-agent) (assoc :pid :sur-plate-glass)))]
    (doseq [msg [ch-1 sup-eads-1 ch-2 sup-eads-2]]
      (let [resp (->> msg msg2json-str (adb/query-agent ork) json-str2msg)]
        (log! :info (str "Response: " resp))))))
