(ns scheduling-tbd.interviewing.ork-test
  (:require
   [clojure.data.json             :as cdjson]
   [clojure.pprint                :refer [pprint]]
   [clojure.spec.alpha            :as s]
   [clojure.test                  :refer [deftest is testing]]
   [datahike.api                  :as d]
   [scheduling-tbd.agent-db       :as adb]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.llm            :as llm]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm]]
   [scheduling-tbd.surrogate      :as sur]
   [taoensso.telemere             :as tel :refer [log!]]))

;;; ============================= Orchestrator =========================================
(defonce pid nil #_(db/create-proj-db! {:project/id :orch-test :project/name "orch-test"} {} {:force-this-name? true}))

(when-not (some #(= % :sur-plate-glass) (db/list-projects))
  (log! :error "Project :sur-plate-glass doesn't exist. Most should work for this test."))

(def ork (adb/ensure-agent! (-> (get @adb/agent-infos :orchestrator-agent)
                                    (assoc :pid :sur-plate-glass)
                                    (assoc :force-new? true))))



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
  (doseq [msg [ch-1 sup-eads-1 ch-2 sup-eads-2]]
    (let [resp (->> msg msg2json-str (adb/query-agent ork) json-str2msg)]
      (log! :info (str "Response: " resp)))))
