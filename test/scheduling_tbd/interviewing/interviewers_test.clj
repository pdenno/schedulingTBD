(ns scheduling-tbd.interviewing.interviewers-test
  (:require
   [clojure.pprint                :refer [pprint]]
   [clojure.spec.alpha            :as s]
   [clojure.test                  :refer [deftest is testing]]
   [datahike.api                  :as d]
   [scheduling-tbd.agent-db       :as adb]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.interviewing.interviewers   :as inv]
   [scheduling-tbd.llm            :as llm]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm]]
   [scheduling-tbd.surrogate      :as sur]
   [taoensso.telemere             :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

;; THIS is 2 (the other namespace I am hanging out in recently).
;;; Remember to db/backup-system-db once you get things straight.

;;; (invt/recreate-agent! :response-analysis-agent)
;;; (invt/recreate-agent! :process-interview-agent)
;;; (invt/recreate-agent! :scheduling-challenges-agent)
(defn ^:diag recreate-agent!
  "This is used to experiment with different LLMs and value of flow-charts.
   Use, for example process-read-the-flowchart.txt rather than the full instructions."
  ([] (recreate-agent! :production-challenges-agent))
  ([id]
   (if-let [info (some #(when (= (:id %) id) %) adb/agent-infos)]
     (adb/ensure-agent! info)
     (log! :error (str "No such agent: " id)))))

;;; Used to query the flowchart using the process-interview-agent.
(defonce process-interview-system-agent (atom {}))

(defn ^:diag setup-flowchart-with-agent! []
  (let [agent (adb/ensure-agent! :base-type :process-interview-agent)
        thread (llm/make-thread :assistant-id (:aid agent))]
    (reset! process-interview-system-agent {:aid (:aid agent) :tid (:id thread)})))

;;; ToDo: Consider asking these two questions first on every thread!
;;; (ask-about-flowchart "Summarize what you see in the uploaded file 'Process Interview Flowchart'."  @process-interview-system-agent)
;;; (ask-about-flowchart "Suppose the production-system-type was FLOW-SHOP. In the order in which they are to be asked, what are all the question-types you would ask?" @process-interview-system-agent)
;;; Typical response to the query directly above this line:

;;; If the production-system-type is FLOW-SHOP, the questions would be asked in the following order:
;;;
;;; 1. warm-up
;;; 2. work-type
;;; 3. production-location
;;; 4. production-motivation
;;; 5. production-system-type
;;; 6. process-steps
;;; 7. process-durations
;;; 8. batch-size
;;; 9. process-ordering
;;;
;;; These question-types guide the interview from understanding what the entity does to detailed aspects of their production process,
;;; ultimately helping in designing an effective scheduling system for their FLOW-SHOP production system.
(defn ^diag ask-about-flowchart
  [query {:keys [aid tid]}]
  (println (adb/query-on-thread :aid aid :tid tid :query-text query)))

(defn ^:diag ask-about-flowchart:inverviewer [])

#_(defn tell-one
  "Diagnostic for one interaction with interviewer."
  [cmd {:keys [pid cid] :as ctx}]
  (inv/tell-interviewer cmd
                        (merge ctx (inv/ensure-interview-agent! pid cid))))

#_(deftest finished-process-test
  (testing "Testing that :sur-ice-cream has finished all process questions."
    (is (= {:status "DONE"}
           (do (tell-one (inv/conversation-history :sur-ice-cream :process) {:pid :sur-ice-cream :cid :process})
               (tell-one {:command "SUPPLY-QUESTION"} {:pid :sur-ice-cream :cid :process}))))))

(deftest test-vector-stores
  (let [{:keys [id object] :as obj}
        (llm/make-vector-store
         {:name "Process Interview flowchart"
          :file_ids [(llm/upload-file {:fname "data/instructions/interviewers/process-interview-flowchart.pdf"})]})]
    (reset! diag obj)
    (is (and (string? id) (= object "vector_store")))))

(defn ^:diag check-instructions
  "It might be the case that the system instructions were too long. This asks what it knows about."
  []
  (let [{:keys [aid tid base-type]} (-> (adb/ensure-agent! :base-type :process-dur-agent) adb/agent-db2proj)]
    (adb/query-on-thread
     {:aid aid :tid tid :role "user" :base-type base-type
      :query-text (str "I provided instructions to perform a number of transformation we call 'REVs', "
                       "REV-1, REV-2, etc. What are the REVs that you know about, and what do they do?")})))

;;; ToDo: BTW, "consulting" was thrown in there to test "DONE" in interviewing. Consulting is a kind of service; maybe it is an okay scheduling domain.
(def q-work-type
  "This is typical of what you get back from the interviewer when you do SUPPLY-QUESTION."
  {:question-type :work-type,
   :question "Would you characterize your company's work as primarily providing a product, a service, or consulting? Respond respectively with the single word PRODUCT, SERVICE, or CONSULTING."
   :status "OK"})

#_(invt/ask-one-question {:question "What are the products you make or the services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences."
                          :question-type :process/warm-up
                          :responder-role :surrogate
                          :pid :sur-ice-cream})

#_(defn ask-one-question
  "Use inv/chat-pair to get back an answer"
  [{:keys [question question-type pid responder-role client-id]
    :or {client-id (ws/recent-client!)}
    :as ctx}]
  (assert (#{:human :surrogate} responder-role))
  (assert (string? question))
  (assert (keyword? question-type))
   (let [ctx (assoc ctx :client-id client-id)]
     (when (= responder-role :surrogate) (assert pid))
     (if (= responder-role :surrogate)
       (let [{:surrogate/keys [assistant-id thread-id]} (db/get-surrogate-agent-info pid)]
         (inv/get-an-answer
          (merge ctx {:sur-aid assistant-id
                      :sur-tid thread-id})))
       (inv/get-an-answer ctx)))) ; ToDo: Have special behavior for a client-id="dev-null".

#_(deftest test-get-an-answer
  (testing "get-an-answer for surrogates"
    (if (db/project-exists? :sur-ice-cream)
      (let [result (ask-one-question {:question "What are the products you make or the services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences."
                                      :question-type :process/warm-up
                                      :responder-role :surrogate
                                      :pid :sur-ice-cream})]
        (is (and (vector? result)
                 (== 2 (count result))
                 (= :query (-> result first :tags first))
                 (= :response (-> result second :tags first)))))
      (log! :warn "Need project :sur-ice-cream to run this test."))))


#_(deftest question-asking
  (testing "Testing question asking capability through loop-for-answer."
    (when (or (nil? (db/get-project :sur-fountain-pens :error? false))
              (nil? (ws/recent-client!)))
      (log! :warn "No project or client to run interview_test/question-asking."))
    (is (or (nil? (db/get-project :sur-fountain-pens :error? false))
            (nil? (ws/recent-client!))
            (let [pid :sur-fountain-pens]
              (= {:command "INTERVIEWEES-RESPONDS", :response "PRODUCT", :question-type :work-type}
                 (ask-one-question pid q-work-type)))))))
;;; ================================================= Throw-away-able updating of project.edn =====================================
(def ^:diag tag2qname
  {:!describe-challenge :initial-question
   :query-product-or-service :work-type
   :!query-product-or-service :work-type
   :query-production-mode :production-motivation
   :!query-production-mode :production-motivation
   :query-activity-location-type :production-location
   :!query-activity-location-type :production-location
   :query-shop-type :production-system-type
   :!query-shop-type :production-system-type
   :query-process-steps :process-steps
   :!query-process-steps :process-steps
   :query-process-durs :process-durations
   :!remark-raw-material-challenge :remark-raw-material-challenge
   :!query-process-durs :process-durations})

(def qname? (-> tag2qname vals set))

(defn ^:diag rename-tag-to-qname
  "If a tag matches any tag2qname, replace it with that qname."
  [project-map]
  (letfn [(rename-tag [obj]
            (cond (and (map? obj) (contains? obj :message/id))  (reduce-kv (fn [m k v]
                                                                             (if (= k :message/tags)
                                                                               (assoc m k (mapv #(if-let [new-tag (get tag2qname %)] new-tag %) v))
                                                                               (assoc m k v)))
                                                                           {}
                                                                           obj)
                  (map? obj)                                     (reduce-kv (fn [m k v] (assoc m k (rename-tag v))) {} obj)
                  (vector? obj)                                  (mapv rename-tag obj)
                  :else                                          obj))]
    (rename-tag project-map)))

(defn ^:diag add-qname
  [project-map]
  (letfn [(add-qn [obj]
            (cond (and (map? obj) (contains? obj :message/id))  (if-let [qname (some #(when (qname? %) %) (:message/tags obj))]
                                                                  (assoc obj :message/question-name qname)
                                                                  obj)
                  (map? obj)                                     (reduce-kv (fn [m k v] (assoc m k (add-qn v))) {} obj)
                  (vector? obj)                                  (mapv add-qn obj)
                  :else                                          obj))]
    (add-qn project-map)))

(defn update-project [pid]
  (as-> pid ?o
    (db/get-project ?o)
    ;;(rename-tag-to-qname ?o)
    ;;(add-qname ?o)
    (vector ?o)
    (with-out-str (pprint ?o))
    (spit (str "data/new-projects/" (name pid) ".edn") ?o)))

(defn add-state-vector [pid]
  (let [conn (connect-atm pid)
        eid (d/q '[:find ?eid . :where [?eid :conversation/id :process]] @conn)
        new-ones [:initial-question
                  :work-type
                  :production-motivation
                  :production-system-type
                  :process-steps
                  :process-durations
                  :batch-size
                  :process-ordering]]
    (if eid
      (d/transact conn {:tx-data (vec (for [n new-ones] [:db/add eid :conversation/state-vector n]))})
      (log! :error "No eid"))))

(deftest interviewer-commands
  (testing "that the specs for interviewer commands are correct."
    (is (s/valid? ::inv/interviewer-cmd
                  {:command "CONVERSATION-HISTORY",
                   :claims  "[(project-id :sur-ice-cream)]"
                   :already-answered [:warm-up],
                   :responses
                   [{:question-type :warm-up
                     :answer "Our most significant scheduling problem is..."}]}))))


 #_{:command "CONVERSATION-HISTORY",
  :claims
  "[(process-step :sur-ice-cream 10 \"Label and Inspect\") (process-step :sur-ice-cream 9 \"Package\") (production-mode :sur-ice-cream make-to-stock) (production-location :sur-ice-cream factory) (flow-shop :sur-ice-cream) (process-step :sur-ice-cream 2 \"Pasteurize Mixture\") (process-step :sur-ice-cream 1 \"Mix Ingredients\") (surrogate :sur-ice-cream) (have-process-durs :sur-ice-cream) (provides-product :sur-ice-cream) (process-step :sur-ice-cream 6 \"Freeze Mixture\") (process-step :sur-ice-cream 3 \"Homogenize Mixture\") (process-step :sur-ice-cream 7 \"Fill Containers\") (process-step :sur-ice-cream 5 \"Add Flavors and Inclusions\") (process-step :sur-ice-cream 8 \"Harden Ice Cream\") (process-step :sur-ice-cream 4 \"Age Mixture\") (project-id :sur-ice-cream) (project-name :sur-ice-cream \"SUR Ice Cream\") (cites-raw-material-challenge nil)]",
  :already-answered
  [:production-motivation
  :production-system-type
  :process-steps
  :process-ordering
  :warm-up
  :production-location
  :process-durations
  :work-type
  :batch-size],
 :responses
 [{:answer
   "We produce a variety of ice cream flavors, including traditional favorites and seasonal specials, in different packaging options like pints, quarts, and bulk containers for food service. Our scheduling challenge involves balancing the production schedule to meet fluctuating demand, especially during peak seasons, while managing supply chain constraints such as ingredient availability and production line capacities. Additionally, coordinating delivery schedules to ensure timely distribution without overstocking or understocking our retailers is crucial.",
   :question-type "warm-up"}
  {:answer "PRODUCT", :question-type "work-type"}
  {:answer "OUR-FACILITY", :question-type "production-location"}
  {:answer "MAKE-TO-STOCK", :question-type "production-motivation"}
  {:answer "FLOW-SHOP", :question-type "production-system-type"}
  {:answer
   "1. Mix Ingredients\n2. Pasteurize Mixture\n3. Homogenize Mixture\n4. Age Mixture\n5. Add Flavors and Inclusions\n6. Freeze Mixture\n7. Fill Containers\n8. Harden Ice Cream\n9. Package\n10. Label and Inspect",
   :question-type "process-steps"}
  {:answer
   "1. Mix Ingredients (30 minutes)\n2. Pasteurize Mixture (15 minutes)\n3. Homogenize Mixture (10 minutes)\n4. Age Mixture (4 hours)\n5. Add Flavors and Inclusions (20 minutes)\n6. Freeze Mixture (30 minutes)\n7. Fill Containers (20 minutes)\n8. Harden Ice Cream (3 hours)\n9. Package (15 minutes)\n10. Label and Inspect (10 minutes)",
   :question-type "process-durations"}
  {:answer
   "1. Mix Ingredients (500 gallons)\n2. Pasteurize Mixture (500 gallons)\n3. Homogenize Mixture (500 gallons)\n4. Age Mixture (500 gallons)\n5. Add Flavors and Inclusions (500 gallons)\n6. Freeze Mixture (500 gallons)\n7. Fill Containers (1000 pints)\n8. Harden Ice Cream (1000 pints)\n9. Package (1000 pints)\n10. Label and Inspect (1000 pints)",
   :question-type "batch-size"}
  {:answer
   "1. Mix Ingredients (milk, cream, sugar, stabilizers, emulsifiers)\n2. Pasteurize Mixture (use mixture from Mix Ingredients)\n3. Homogenize Mixture (use mixture from Pasteurize Mixture)\n4. Age Mixture (use mixture from Homogenize Mixture)\n5. Add Flavors and Inclusions (use aged mixture, flavorings, inclusions such as fruits, nuts, chocolate chips)\n6. Freeze Mixture (use flavored mixture from Add Flavors and Inclusions)\n7. Fill Containers (use frozen mixture from Freeze Mixture)\n8. Harden Ice Cream (use filled containers from Fill Containers)\n9. Package (use hardened ice cream from Harden Ice Cream)\n10. Label and Inspect (use packaged ice cream from Package)",
   :question-type "process-ordering"}]}

(def warm-up-text
  "We produce a variety of ice cream flavors, including traditional favorites and seasonal specials, in different packaging options like pints, quarts, and bulk containers for food service. Our scheduling challenge involves balancing the production schedule to meet fluctuating demand, especially during peak seasons, while managing supply chain constraints such as ingredient availability and production line capacities. Additionally, coordinating delivery schedules to ensure timely distribution without overstocking or understocking our retailers is crucial.")

(deftest test-start-human-project!
  (testing "that you can make a human project from an answer to warm-up text."
    (inv/start-human-project! {:use-this-answer warm-up-text})))

(deftest response-analysis-agent-test
  (testing "that the response-analysis-agent does the right thing."
    (let [{:keys [answers-the-question? raises-a-question? wants-a-break?] :as res}
          (inv/response-analysis
           "Would you characterize some process as being a relatively persistent bottleneck?"
           "Yes, sewing is typically the bottleneck.")]
      (is (= #{:answers-the-question? :raises-a-question? :wants-a-break?}
             (-> res keys set)))
      (is (string? answers-the-question?))
      (is (false? raises-a-question?))
      (is (false? wants-a-break?)))))

(defn  ^:diag migrate-project!
  "Add a :conversation/interviewer-budget value to every conversation; 0.5 for :process 1.0 for others."
  [pid]
  (letfn [(update-proj [obj]
            (cond (and (map? obj) (= :process    (:conversation/id obj)))  (assoc obj :conversation/interviewer-budget 0.5)
                  (and (map? obj) (= :data       (:conversation/id obj)))  (assoc obj :conversation/interviewer-budget 1.0)
                  (and (map? obj) (= :resources  (:conversation/id obj)))  (assoc obj :conversation/interviewer-budget 1.0)
                  (and (map? obj) (= :optimality (:conversation/id obj)))  (assoc obj :conversation/interviewer-budget 1.0)
                  (map? obj)      (reduce-kv (fn [m k v] (assoc m k (update-proj v))) {} obj)
                  (vector? obj)   (mapv update-proj obj)
                  :else           obj))]
    (let [new-proj (-> (db/get-project pid) update-proj db/clean-project-for-schema)
          proj-string (with-out-str (pprint new-proj))]
      (spit (str "data/projects/" (name pid) ".edn") (format "[\n%s\n]" proj-string)))))

(defn  ^:diag migrate-projects!
  []
  (doseq [pid (db/list-projects)]
    (log! :info (str "Doing project " pid))
    (migrate-project! pid)
    (db/recreate-project-db! pid)))


(defn  ^:diag new-backup-files!
    []
  (doseq [pid (db/list-projects)]
    (log! :info (str "Doing project " pid))
    (db/backup-proj-db pid)))

"We run a fitness center. We have two small areas for one-on-one sessions between trainers and their clients.
 It only fits one pair at a time, so we'd like to schedule things so trainers can meet (typically once a week)
 with their clients in the one-on-one areas. Ideally, the trainers would like to have back-to-back lessons;
 they don't like to drive in for just one client."

"We run a fitness center. We have two small areas for one-on-one sessions between trainers and their clients.
 Each area only fits one pair at a time, so we'd like to schedule things so trainers can meet  with their clients in the one-on-one areas (typically once a week, same time each week to make it easier on everyone).
 Ideally, the trainers would like to have back-to-back lessons; they don't like to drive in for just one client."
