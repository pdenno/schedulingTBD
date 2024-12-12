(ns scheduling-tbd.domain.process-analysis-test
  "Currently these are more about exploring prompts than they are about test the code."
  (:require
   [clojure.core.unify                     :as uni]
   [clojure.test                           :refer [deftest is testing]]
   [clojure.spec.alpha                     :as s]
   [datahike.api                           :as d]
   [datahike.pull-api                      :as dp]
   [jsonista.core                          :as json]
   [scheduling-tbd.agent-db                :as adb]
   [scheduling-tbd.db                      :as db]
   [scheduling-tbd.domain.process-analysis :as pan]
   [scheduling-tbd.interviewers            :as inv :refer [tell-interviewer]]
   [scheduling-tbd.llm                     :as llm :refer [query-llm]]
   [scheduling-tbd.response-utils          :as ru]
   [scheduling-tbd.util                    :as util]
   [scheduling-tbd.sutil                   :as sutil]
   [scheduling-tbd.web.websockets          :as ws]
   [taoensso.telemere                      :as tel :refer [log!]]))

;;; THIS is the namespace I am hanging out in recently.
(def ^:diag diag (atom nil))

(def alias? (atom (-> (ns-aliases *ns*) keys set)))

(defn ^:diag ns-start-over!
  "This one has been useful. If you get an error evaluate this ns, (the declaration above) run this and try again."
  []
  (map (partial ns-unalias *ns*) (keys (ns-aliases *ns*))))

(defn ^:diag remove-alias
  "This one has NOT been useful!"
  [al ns-sym]
  (swap! alias? (fn [val] (->> val (remove #(= % al)) set)))
  (ns-unalias (find-ns ns-sym) al))

(defn safe-alias
  [al ns-sym]
  (when (and (not (@alias? al))
             (find-ns ns-sym))
    (alias al ns-sym)))

(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (ns-start-over!)
  (reset! alias? (-> (ns-aliases *ns*) keys set))
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 's      'clojure.spec.alpha)
  (safe-alias 'uni    'clojure.core.unify)
  (safe-alias 'edn    'clojure.edn)
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 'str    'clojure.string)
  (safe-alias 'd      'datahike.api)
  (safe-alias 'dp     'datahike.pull-api)
  (safe-alias 'jt     'java-time.api)
  (safe-alias 'json   'jsonista.core)
  (safe-alias 'mount  'mount.core)
  (safe-alias 'p      'promesa.core)
  (safe-alias 'px     'promesa.exec)
  (safe-alias 'adb    'scheduling-tbd.agent-db)
  (safe-alias 'core   'scheduling-tbd.core)
  (safe-alias 'pan    'scheduling-tbd.domain.process-analysis)
  (safe-alias 'pant   'scheduling-tbd.domain.process-analysis-test)
  (safe-alias 'db     'scheduling-tbd.db)
  (safe-alias 'dbt    'scheduling-tbd.db-test)
  (safe-alias 'how    'scheduling-tbd.how-made)
  (safe-alias 'invt   'scheduling-tbd.interviewers-test)
  (safe-alias 'llm    'scheduling-tbd.llm)
  (safe-alias 'llmt   'scheduling-tbd.llm-test)
  (safe-alias 'mzn    'scheduling-tbd.minizinc)
  (safe-alias 'mznt   'scheduling-tbd.minizinc-test)
  (safe-alias 'ou     'scheduling-tbd.op-utils)
  (safe-alias 'opt    'scheduling-tbd.operators-test)
  (safe-alias 'ru     'scheduling-tbd.response-utils)
  (safe-alias 'spec   'scheduling-tbd.specs)
  (safe-alias 'sutil  'scheduling-tbd.sutil)
  (safe-alias 'sur    'scheduling-tbd.surrogate)
  (safe-alias 'surt   'scheduling-tbd.surrogate-test)
  (safe-alias 'util   'scheduling-tbd.util)
  (safe-alias 'resp   'scheduling-tbd.web.controllers.respond)
  (safe-alias 'ws     'scheduling-tbd.web.websockets)
  (safe-alias 'tel    'taoensso.telemere)
  (safe-alias 'openai 'wkok.openai-clojure.api))

(deftest interviewer-question-ordering
  (testing "that interviews for flow-shop manufacturing follow the correct processes."
    (let [aid (:aid (adb/get-agent :base-type :process-interview-agent))
          tid (:id (llm/make-thread {:assistant-id aid
                                     :llm-provider :openai
                                     :metadata {:usage :project-agent}}))
          ctx {:iview-aid aid :iview-tid tid}
          result (atom [])]
      (letfn [(tell-inv [cmd] (swap! result conj (-> (tell-interviewer cmd ctx) (dissoc :question :iview-aid :iview-tid))))]
        (tell-inv {:command  "ANALYSIS-CONCLUDES", :conclusions "1) You are talking to surrogate humans (machine agents)."})
        (tell-inv {:command  "CONVERSATION-HISTORY"  :already-answered [] :responses []})
        (tell-inv {:command  "SUPPLY-QUESTION"})
        (tell-inv {:command  "INTERVIEWEES-RESPONDS"                            ;; Response to :warm-up-question
                   :response
                   "We make plate glass. One of our most significant scheduling problems involves coordinating the production schedule with the supply of raw materials.
 Fluctuations in raw material deliveries can disrupt planned production runs, leading to delays and inefficiencies.
 Additionally, we need to balance these inconsistencies with varying demand from customers, ensuring that we meet order deadlines without overproducing and holding excess inventory."})
        (tell-inv {:command "SUPPLY-QUESTION"})
        (tell-inv {:command "INTERVIEWEES-RESPONDS" :response "PRODUCT"})       ;; Response to :work-type
        (tell-inv {:command "SUPPLY-QUESTION"})
        (tell-inv {:command "INTERVIEWEES-RESPONDS" :response "MAKE-TO-STOCK"}) ;; Response to :production-motivation
        (tell-inv {:command "SUPPLY-QUESTION"})
        (tell-inv {:command "INTERVIEWEES-RESPONDS" :response "FLOW-SHOP"})     ;; Response to :production-system-type
        (tell-inv {:command "SUPPLY-QUESTION"})
        (tell-inv {:command "INTERVIEWEES-RESPONDS"                             ;; Response to process-steps
                   :response
                   "Our production process for plate glass involves several key stages.
 It starts with raw material preparation, where materials like silica sand, soda ash, and limestone are accurately measured and mixed.
 This mixture is then fed into a furnace and melted at very high temperatures to form molten glass.
 The molten glass is then formed into sheets using the float glass process, where it is floated on a bed of molten tin.
 After forming, the glass is slowly cooled to prevent stress in a process called annealing.
 Finally, the glass is cut to size, inspected for quality, and prepared for shipment."})
        (tell-inv {:command "SUPPLY-QUESTION"}))
      (is (=  [{:status "OK"}                                            ;; ANALYSIS-CONCLUDES
               {:status "OK"}                                            ;; CONVERSATION-HISTORY
               {:question-type :warm-up-question, :status "OK"}          ;; SUPPLY-QUESTION
               {:status "OK"}                                            ;; INTERVIEWEES-RESPONDS
               {:question-type :work-type, :status "OK"}                 ;; SUPPLY-QUESTION
               {:status "OK"}                                            ;; INTERVIEWEES-RESPONDS
               {:question-type :production-motivation, :status "OK"}     ;; SUPPLY-QUESTION
               {:status "OK"}                                            ;; INTERVIEWEES-RESPONDS
               {:question-type :production-system-type, :status "OK"}    ;; SUPPLY-QUESTION
               {:status "OK"}                                            ;; INTERVIEWEES-RESPONDS
               {:question-type :process-steps, :status "OK"}]            ;; SUPPLY-QUESTION
              @result)))))

(def response-durs-ice-cream {:client-id (ws/recent-client!)
               :response "1. Mix Ingredients (30 minutes)
2. Pasteurize Mixture (45 minutes)
3. Homogenize Mixture (20 minutes)
4. Age Mixture (4 hours)
5. Flavor Addition (15 minutes)
6. Freeze Mixture (30 minutes)
7. Add Inclusions (10 minutes)
8. Fill Containers (20 minutes)
9. Harden Ice Cream (4 hours)
10. Package (40 minutes)
11. Store in Cold Storage (ongoing)"
               :pid :sur-ice-cream})

(defn  ^:diag analyze-durs-ice-cream-1 []
  (pan/analyze-process-durs-response response-durs-ice-cream))

(defn ^:diag analyze-durs-ice-cream-2 []
  (ru/analyze-response-meth (assoc response-durs-ice-cream :question-type :process-durations)))


(def response-ordering-ice-cream
  "This is entirely serial."
  {:client-id (ws/recent-client!)
   :response "1. Mix Ingredients (cream, milk, sugar, stabilizers, emulsifiers)
2. Pasteurize Mixture (use mixture from Mix Ingredients)
3. Homogenize Mixture (use mixture from Pasteurize Mixture)
4. Age Mixture (use mixture from Homogenize Mixture)
5. Add Flavors and Colors (use aged mixture, flavor extracts, color additives)
6. Freeze Mixture (use flavored mixture)
7. Add Inclusions (use partially frozen mixture, chocolate chips, nuts, fruits)
8. Package Ice Cream (use ice cream from Add Inclusions)
9. Store Finished Product (use packaged ice cream)"})

(def response-ordering-cookies
  " 1. Make Cookie Dough (flour, water, eggs, sugar, chocolate chips)
 2. Make Filling (sugar, water vanilla flavoring)
 3. Bake Wafers (use dough from Make Cookie Dough)
 4. Assemble Cookies (use wafers from Bake Wafers, use filling from Make Filling)
 5. Package (use cookies from Assemble Cookies)")

(deftest test-process-ordering
  (testing "Testing process ordering"
    (testing "Testing the same example as I gave the agent")
    (pan/analyze-process-ordering-response {:response response-ordering-cookies})),,,,) ; <=============================

(defn ^:diag tpo []
  (pan/analyze-process-ordering-response {:response response-ordering-cookies}))

(def warm-up-fountain-pens
  "The original of this makes it clear that we need update the warm-up question."
  (str "We make fountain pens. "
       "Our most significant scheduling problem is coordinating production runs with the arrival of raw materials. "
       "Due to variability in lead times from suppliers, sometimes we face delays or shortages that disrupt our production schedule. "
       "This, in turn, leads to challenges in meeting customer demand and fulfilling orders on time, "
       "resulting in potential bottlenecks in various stages of the production process."))

(deftest test-warm-up-question
  (testing "Testing warm-up-question with no current project."
    (let [claims (pan/analyze-warm-up-response  warm-up-fountain-pens '#{(project-id :START-A-NEW-PROJECT)})
          [_ ?pid] (ru/find-claim '(project-id ?x) claims)]
      (is (and (ru/find-claim (list 'project-id ?pid) claims)
               (ru/find-claim (list 'cites-raw-material-challenge ?pid) claims)
               (ru/find-claim '(project-name ?x) claims))))))

(defn ^:diag ask-about-ice-cream-agent
  ([] (ask-about-ice-cream-agent "What are your instructions?"))
  ([q-txt]
     (adb/query-agent :sur-ice-cream q-txt)))

(def domain-problems ; I have removed from these descriptions text that gives away too much (e.g. 'we plan projects' for scheduling/project planning." I switched "scheduling problem" to "production problem"
  {:snack-food "We make snack food.
 The primary challenge in our production process involves effectively coordinating all the steps in our supply chain, starting from raw material procurement to the final delivery of our snack foods to grocery chains.
 We aim to maintain an optimal inventory level which involves proper timing of production runs to minimize stockouts and excess storage costs.
 Seasonal fluctuations in demand, delays from suppliers, equipment breakdowns, and transportation delays pose consistent scheduling problems.
 Additionally, the scheduling process needs to account for shelf-life of our products to prevent wastage.
 Lastly, integrating all the processes within the firm and communicating the schedule effectively to all the stakeholders is a significant problem.",

   :paving "We pave roads.
 The principal problem we face is coordinating the availability of our clients, skilled work crew, and the delivery of material supplies.
 Unpredictable weather conditions often lead to sudden schedule changes.
 Also, delays in supply chain due to various reasons can significantly affect the timeline of the project.
 Balancing multiple projects simultaneously without over-committing our resources is also a challenge.
 Lastly, unanticipated repairs and maintenance of our paving equipment affects our work schedule.",

   :injection-molds "We make injection molds.
 Our principal production problem in running an injection mold facility is coordinating the different production orders in a manner that optimizes our machine utilization and minimizes
 production time without leading to bottlenecks.
 We must effectively manage the flow of materials from suppliers, ensuring that they're available exactly when needed to avoid delays.
 It's also crucial to ensure our labor force is appropriately assigned to different tasks in the shop to maximize efficiency.
 Lastly, unexpected maintenance or breakdowns of machinery can throw our schedule off and present a major challenge.
 Central to all this is the need for high precision and quality in molds, which can potentially impact scheduling if reworks or corrections are required due to errors.",

   :brewery "As the manager of a craft brewery, the principal production challenge is in coordinating processes to maintain a consistent supply of diverse beers without overproduction or storage issues.
 This spans from the initial scheduling of raw materials procurement, to the fermenting process which can take weeks, and finally to the bottling and distribution procedures.
 Additionally, managing seasonal demand fluctuations and accommodating special edition or experimental brews without disrupting the core product line adds complexity.
 Timely maintenance of brewing equipment and quality check is also crucial to the schedule.
 Lastly, ensuring synergies with marketing timelines and release dates is necessary to avoid any mismatch between supply and market launch."

  :music-lessons "We have a music shop offering instruments and lessons. Our biggest challenging scheduling lessons is in matching the availability of students, instructors,
 and rehersal rooms with the correct equipment (some have pianos, some have guitar amplifiers, etc.)."

 :general-contractor "We are prime contractors in construction specializing in office and laboratory buildings. We manage subcontractors, permits, quality, the site, and the budget.
  Most importantly, we are the prinicpal contact between the client, subs, vendors, and regulatory authorities. Our principal challenge is in providing a plan and schedule for the project that meets the client's budget."})

(deftest product-or-service?
  (testing "Testing whether are zero-shot prompt on a given LLM is sufficient on various service vs. product intro paragraphs."
    (let [answer-key {:snack-food         "product"
                      :paving             "service"
                      :injection-molds    "product"
                      :brewery            "product"
                      :music-lessons      "service"
                      :general-contractor "service"}]
      (is (= answer-key
             (reduce (fn [res k]
                       (assoc res k
                              (let [dialog [{:role "system"
                                             :content "You are a helpful assistant."}
                                            {:role "user"
                                             :content (format (str "The following paragraph describes an activity that either provides a service or produces a product.\n"
                                                                   "Reply with respectively with the single word 'service' or 'product' depending on which most closely describes the paragraph.\n\n %s")
                                                              (get domain-problems k))}]]
                                (query-llm dialog {:model-class :gpt #_:gpt-3.5}))))
                     {}
                     (keys answer-key)))))))

;;; GPT-3.5 has been slower than GPT-4 on 2024-04-28.
;;;
;;; Regarding :general-contractor, I changed the wording from
;;;    "like cutting down a tree, it has to be done where the customer designates" to
;;;    "it concerns activity that will be performed at a place the customer designates.
;;;
;;; Also was better off with full sentences; sometimes I got back "Our facility" when I asked for 'our facility'.

;;; This is NOT working out well! The LLM is concluding that everything except :general-contractor is a scheduling problem. <===========================================
;;; Maybe we are going to have to narrow it down to just those two, and then recognize resource-assignment as a sub-problem that can occur in either case:
;;; major-challenge - Just one of scheduling or project-management
;;; minor-challenges - resource-allocation, forecasting,
#_(deftest challenge-type-test-3-way-rejected
    (testing "Testing whether a few-shop prompt can classify the principal challenge being discussed."
    (let [answer-key {:snack-food         "scheduling"
                      :paving             "project-management"
                      :injection-molds    "scheduling"
                      :brewery            "scheduling"
                      :music-lessons      "resource-assignment"
                      :general-contractor "project-management"}]
      (is (= answer-key
             (reset! diag
              (reduce (fn [res k]
                       (assoc res k
                              (let [dialog [{:role "system"
                                             :content "You are a helpful assistant that knows operations management."}
                                            {:role "user"
                                             :content (str "The following paragraph describes an activity that produces a product or delivers a service.\n"
                                                           "You will respond with one of the following three words: 'scheduling', 'resource-assignment' or 'project-management'.\n"
                                                           "Respond with 'scheduling' if the challenge is to decide what work to run when.\n"
                                                           "Respond with 'resource-assignment' if the challenge is finding the right resource to do the job.\n"
                                                           "Respond with 'project-management' if the challenge is to orchestrate complex tasks towards a desired result.\n\n\n %s")}
                                            {:role "user" :content (str "We make radiators for automobiles and truck. Demand for our various products is uncertain.\n"
                                                                        "Our challenge is in switching over the production line at the right time for product changes.")}
                                            {:role "assistant" :content "scheduling"}
                                            {:role "user" :content (str "We are an accounting firm that works with clients in many sectors, manufacturing, IT, medical services, etc.\n"
                                                                        "Our challenge is in ensuring that our clients are matched with accountants who understand the client's business.")}
                                            {:role "assistant" :content "resource-assignment"}
                                            {:role "user" :content (str "We do event planning for popular musical groups. We coordinate with venues and vendors, organize transportation, "
                                                                        "and handle on-site coordination.")}
                                            {:role "assistant" :content "project-management"}
                                            {:role "user" :content "We remanufacture automotive alternators. Our challenge is in deciding what work is required on a given used alternator and routing it through the shop appropriately."}
                                            {:role "assistant" :content "You are describing a 'scheduling' situation."}
                                            {:role "user" :content "WRONG! The answer must be just the string 'scheduling'."}
                                            {:role "user" :content (get domain-problems k)}]]
                                (query-llm dialog {:model-class :gpt}))))
                      {}
                      (keys answer-key))))))))

(deftest challenge-type-test
    (testing "Testing whether a few-shop prompt can classify the principal challenge being discussed."
    (let [answer-key {:snack-food         "scheduling"
                      :paving             "project-management"
                      :injection-molds    "scheduling"
                      :brewery            "scheduling"
                      :music-lessons      "scheduling"
                      :general-contractor "project-management"}]
      (is (= answer-key
             (reset! diag
              (reduce (fn [res k]
                       (assoc res k
                              (let [dialog [{:role "system"
                                             :content "You are a helpful assistant that knows operations management."}
                                            {:role "user"
                                             :content (str "The following paragraph describes an activity that produces a product or delivers a service.\n"
                                                           "You will respond with one of the following two terms: 'scheduling',  or 'project-management'.\n"
                                                           "Respond with 'scheduling' if the challenge is to decide what work to run when.\n"
                                                           "Respond with 'project-management' if the challenge is to orchestrate complex tasks towards a desired result.\n\n\n %s")}
                                            {:role "user" :content (str "We make radiators for automobiles and truck. Demand for our various products is uncertain.\n"
                                                                        "Our challenge is in switching over the production line at the right time for product changes.")}
                                            {:role "assistant" :content "scheduling"}
                                            {:role "user" :content (str "We are an accounting firm that works with clients in many sectors, manufacturing, IT, medical services, etc.\n"
                                                                        "Our challenge is in ensuring that our clients are matched with accountants who understand the client's business.")}
                                            {:role "assistant" :content "resource-assignment"}
                                            {:role "user" :content (str "We do event planning for popular musical groups. We coordinate with venues and vendors, organize transportation, "
                                                                        "and handle on-site coordination.")}
                                            {:role "assistant" :content "project-management"}
                                            {:role "user" :content "We remanufacture automotive alternators. Our challenge is in deciding what work is required on a given used alternator and routing it through the shop appropriately."}
                                            {:role "assistant" :content "You are describing a 'scheduling' situation."}
                                            {:role "user" :content "WRONG! The answer must be just the string 'scheduling'."}
                                            {:role "user" :content (get domain-problems k)}]]
                                (query-llm dialog {:model-class :gpt}))))
                      {}
                      (keys answer-key))))))))

(def process-order-resp
"1. Raw Material Inspection (sand, soda ash, limestone, dolomite)
2. Glass Melting (use inspected raw materials from Raw Material Inspection)
3. Forming Glass Sheets (use molten glass from Glass Melting)
4. Annealing Process (use glass sheets from Forming Glass Sheets)
5. Cutting Glass (use annealed glass from Annealing Process)
6. Edge Finishing (use cut glass from Cutting Glass)
7. Coating or Tinting (if required) (use finished glass from Edge Finishing, apply coatings or tints)
8. Quality Inspection (use coated or non-coated glass from Coating or Tinting)
9. Packaging (use inspected glass from Quality Inspection)
10. Shipping (use packaged glass from Packaging)")

(defn tryme-po []
  (pan/analyze-process-ordering-response {:response process-order-resp}))

(deftest process-ordering
  (testing "whether the process-ordering system agent works okay."))

(def ice-cream-answer-warm-up
  (str "We produce a variety of ice cream flavors, including traditional favorites and seasonal specials, in different packaging options like "
       "pints, quarts, and bulk containers for food service. "
       "Our scheduling challenge involves balancing the production schedule to meet fluctuating demand, especially during peak seasons, "
       "while managing supply chain constraints such as ingredient availability and production line capacities. "
       "Additionally, coordinating delivery schedules to ensure timely distribution without overstocking or understocking our retailers is crucial."))

; We produce a variety of ice cream flavors, including traditional favorites and seasonal specials, in different packaging options like pints, quarts, and bulk containers for food service. Our scheduling challenge involves balancing the production schedule to meet fluctuating demand, especially during peak seasons, while managing supply chain constraints such as ingredient availability and production line capacities. Additionally, coordinating delivery schedules to ensure timely distribution without overstocking or understocking our retailers is crucial.

(deftest scheduling-challenges-agent
  (testing "the scheduling-challenges agent"
    (let [result (adb/query-agent :scheduling-challenges-agent ice-cream-answer-warm-up)
          {:keys [challenges]} (-> result
                                   json/read-value
                                   (update-keys keyword)
                                   (update :challenges #(->> % (map keyword) set)))]
      (is (= #{:raw-material-uncertainty :product-variation :variation-in-demand :delivery-schedules :demand-uncertainty},
             challenges)))))

#_(defn migrate-project
  "Translate :message/content to html."
  [pid]
  (let [new-proj (-> (db/get-project pid)
                     (update :project/messages (fn [msgs] (-> (reduce (fn [res msg] (conj res (msg-vec2html msg))) [] msgs) vec))))
        proj-string (with-out-str (pprint new-proj))]
    (spit (str "data/projects/" (name pid) ".edn") (format "[\n%s\n]" proj-string))))

#_(defn migrate-project
  "Translate :message/content to html."
  [pid]
  (let [new-proj (as-> (db/get-project pid) ?x
                   (assoc ?x :project/conversations
                          [{:conversation/name :data}
                           {:conversation/name :resource}
                           {:conversation/name :process
                            :conversation/messages (:project/messages ?x)}])
                   (assoc ?x :project/current-conversation :process)
                   (dissoc ?x :project/messages))
        proj-string (with-out-str (pprint new-proj))]
    (spit (str "data/projects/" (name pid) ".edn") (format "[\n%s\n]" proj-string))))

#_(defn migrate-project
  "Translate :message/content to html."
  [pid]
  (letfn [(name2id [obj]
            (cond (map? obj)    (reduce-kv (fn [m k v] (if (= k :conversation/name)
                                                         (assoc m :conversation/id v)
                                                         (assoc m k (name2id v)))) {} obj)
                  (vector? obj) (mapv name2id obj)
                  :else         obj))]
    (let [proj (db/get-project pid)
          proj-string (with-out-str (-> proj name2id pprint))]
      (spit (str "data/projects/" (name pid) ".edn") (format "[\n%s\n]" proj-string)))))




;;; THIS ONE NOT WORKING CORRECTLY!
#_(defn ^:diag fix-the-text
  "Text didn't have spacing and hrefs correct."
  []
  (letfn [(get-eid [topic] (d/q '[:find ?eid .
                                  :in $ ?topic
                                  :where
                                  [?c :conversation/id ?topic]
                                  [?c :conversation/messages ?eid]
                                  [?eid :message/tags :conversation-intro]]
                                @(sutil/connect-atm :sur-canned-corn)
                                topic))]
    (doseq [pid [:sur-ice-cream] #_(db/list-projects)]
      (doseq [topic [:data :resources :optimality]]
        (if-let [eid (get-eid topic)]
          (do (log! :info (str "Doing " pid " " topic))
              (d/transact (sutil/connect-atm pid)
                          ;; Need retract too??? <===================================
                          {:tx-data [[:db/add eid :message/content (get db/conversation-intros topic)]]}))
          (log! :warn (str "No eid: pid = " pid)))))))

(defn ^:diag delete-agent-without-id
  "Bug in code caused creation of process interview agents where they aren't needed.
   Who knows what was being used!"
  [pid]
  (letfn [(bad-threads [pid]
            (d/q '[:find [?eid ...]
                   :where
                   [?eid :agent/thread-id]
                   (not [?eid :agent/id])]
                 @(sutil/connect-atm pid)))]
    (doseq [eid (bad-threads pid)]
      (log! :warn (str "Removing thread for pid = " pid " eid = " eid))
      (d/transact (sutil/connect-atm pid) {:tx-data [[:db/retract eid :agent/thread-id]]}))))

(defn ^:diag fix-project-generally!
  "(1) Add intro messages to all conversations except :process.
   (2) Mark :process conversation as done.
   (3) Delete agents that don't have :agent/id (a bug that was fixed long ago?)
   This is written so that you can run it multiply without messing up project.
   It writes a new backup file in addition to updating the DB."
  [pid]
  (log! :info (str "***Doing project " pid))
  (delete-agent-without-id pid)
  (let [resource-eid (db/conversation-exists? pid :resource)]
    (db/assert-conversation-done! pid :process)
    (when resource-eid
      (d/transact (sutil/connect-atm pid) {:tx-data [[:db/retract resource-eid :conversation/id :resource]]}))
    (d/transact (sutil/connect-atm pid) {:tx-data [{:project/id pid
                                                    :project/conversations [{:conversation/id :resources}]}]})
    (d/transact (sutil/connect-atm pid) {:tx-data [{:project/id pid
                                                    :project/conversations [{:conversation/id :optimality}]}]})
    (doseq [cid [:data :resources :optimality]]
      (when (empty? (db/get-conversation pid cid))
        (db/add-msg {:pid pid :cid cid :from :system :text (get db/conversation-intros cid) :tags [:conversation-intro]})))
    (db/backup-proj-db pid)
    (db/recreate-project-db! pid)))

(defn ^:diag fix-projects-generally! []
  (doseq [pid (db/list-projects)]
    (fix-project-generally! pid)))


(defn ^:diag filter-claims
  "Remove claims you don't want, fix arity on others."
  [claims pid]
  (as-> claims ?c
    (remove #(uni/unify '(process-step ?x ?y ?z) %) ?c)
    (remove #(uni/unify '(have-process-durs ?x) %) ?c)
    (if (ru/find-claim '(cites-raw-material-challenge ?x) ?c)
      (as-> ?c ?c1
        (remove #(uni/unify '(cites-raw-material-challenge ?x) %) ?c1)
        (conj ?c1 (list 'scheduling-challenge pid :raw-material-uncertainty)))
      ?c)))

(defn claim-question-type
  [[pred & _]]
  (case pred
    scheduling-challenge :warm-up
    (provides-product provides-service consulting) :work-type
    production-location  :production-location
    (flow-shop job-shop) :production-system-type
    nil))

(defn ^:diag migrate-claims!
  "Rewrite the project (and make a backup) so that
    1) claims have :claim/conversation and :claim/question-type.
    2) claims have correct arity.
    3) process-step claims are eliminated."
  [pid]
  (db/backup-proj-db pid)
  (db/recreate-project-db! pid)
  (letfn [(claim-obj [c]
            (let [q-type (claim-question-type c)]
              (cond-> {:claim/string (str c)
                       :claim/conversation-id :process}
                q-type (assoc :claim/question-type q-type))))]
    (let [new-claims (as-> (db/get-claims pid) ?c
                       (filter-claims ?c pid)
                       (mapv claim-obj ?c))
          eids (d/q '[:find [?eid ...]
                      :where [?eid :claim/string]]
                    @(sutil/connect-atm pid))]
      (doseq [eid eids]
        (d/transact (sutil/connect-atm pid) {:tx-data [[:db/retract eid :claim/string]]}))
      (doseq [c new-claims]
        (db/add-claim! pid c))))
  (db/backup-proj-db pid))

(defn ^:diag oops-conversation-id!
  "I was calling it :claim/conversation in some places."
  [pid]
  (let [eids (d/q '[:find [?eid ...]
                    :where [?eid :claim/string]]
                  @(sutil/connect-atm pid))]
    (doseq [eid eids]
      (d/transact (sutil/connect-atm pid) {:tx-data [[:db/add eid :claim/conversation-id :process]]}))
    (db/backup-proj-db pid)))

(def surs [:sur-aluminum-cans
           :sur-canned-corn
           :sur-canned-vegetables
           :sur-carpet
           :sur-craft-beer
           :sur-fountain-pens
           :sur-garage-doors
           :sur-ice-cream
           :sur-ice-hockey-sticks
           :sur-injection-molds
           :sur-inline-skates
           :sur-key-blanks
           :sur-plate-glass
           :sur-remanufactured-alternators
           :sur-stoneware])

(defn migrate-surrogate!
  "Rewrite the project with surrogates as agent, rather than project/surrogate."
  [pid]
  (let [conn-atm (sutil/connect-atm pid)
        eid (d/q '[:find ?eid . :where [?eid :surrogate/id]] @conn-atm)
        {:surrogate/keys [assistant-id thread-id subject-of-expertise system-instruction] :as sur-obj} (dp/pull @conn-atm '[*] eid)
        ;sur-obj (dissoc sur-obj :db/id)
        eid-agents (d/q '[:find ?eid . :where [?eid :project/agents]] @conn-atm)
        sur-agent #:agent{:id (-> (str (name pid) "-openai") keyword)
                          :base-type pid
                          :surrogate? true
                          :expertise subject-of-expertise
                          :system-instruction system-instruction
                          :llm-provider :openai
                          :assistant-id assistant-id
                          :thread-id thread-id}
        eid-project (d/q '[:find ?eid . :where [?eid :project/id]] @conn-atm)]
    (d/transact conn-atm {:tx-data [{:db/id eid-agents :project/agents sur-agent}]})
    (d/transact conn-atm {:tx-data [[:db/retract eid-project :project/surrogate (:db/id sur-obj)]]})))


(defn oops-add-timestamp! []
  (doseq [pid surs]
    (let [eid (d/q '[:find ?eid . :where [?eid :agent/surrogate? true]] @(sutil/connect-atm pid))]
      (d/transact (sutil/connect-atm pid) {:tx-data [[:db/add eid :agent/timestamp (util/now)]]}))))


(defn migrate-surrogates!
  "Rewrite the project with surrogates as agent, rather than project/surrogate."
  []
  (doseq [pid surs] (migrate-surrogate! pid)))
