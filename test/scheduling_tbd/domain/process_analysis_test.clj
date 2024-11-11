(ns scheduling-tbd.domain.process-analysis-test
  "Currently these are more about exploring prompts than they are about test the code."
  (:require
   [clojure.test                           :refer [deftest is testing]]
   [scheduling-tbd.domain.process-analysis :as pan]
   [scheduling-tbd.db                      :as db]
   [scheduling-tbd.interviewers            :as inv :refer [tell-interviewer]]
   [scheduling-tbd.llm                     :as llm :refer [query-llm]]
   [scheduling-tbd.response-utils          :as ru]
   [scheduling-tbd.web.websockets          :as ws]))

;;; THIS is the namespace I am hanging out in recently.
(def ^:diag diag (atom nil))

(def alias? (atom (-> (ns-aliases *ns*) keys set)))

(defn safe-alias
  [al ns-sym]
  (when (and (not (@alias? al))
             (find-ns ns-sym))
    (alias al ns-sym)))

(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (reset! alias? (-> (ns-aliases *ns*) keys set))
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 's      'clojure.spec.alpha)
  (safe-alias 'uni    'clojure.core.unify)
  (safe-alias 'edn    'clojure.edn)
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 'str    'clojure.string)
  (safe-alias 'd      'datahike.api)
  (safe-alias 'dp     'datahike.pull-api)
  (safe-alias 'json   'jsonista.core)
  (safe-alias 'mount  'mount.core)
  (safe-alias 'p      'promesa.core)
  (safe-alias 'px     'promesa.exec)
  (safe-alias 'core   'scheduling-tbd.core)
  (safe-alias 'pan    'scheduling-tbd.domain.process-analysis)
  (safe-alias 'db     'scheduling-tbd.db)
  (safe-alias 'how    'scheduling-tbd.how-made)
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
    (let [aid (:aid (db/get-agent :base-type :process-interview-agent))
          tid (:id (llm/make-thread {:assistant-id aid
                                     :llm-provider :openai
                                     :metadata {:usage :project-agent}}))
          ctx {:iview-aid aid :iview-tid tid}
          result (atom [])]
      (letfn [(tell-inv [cmd] (swap! result conj (-> (tell-interviewer cmd ctx) (dissoc :question :iview-aid :iview-tid))))]
        (tell-inv {:command  "ANALYSIS-CONCLUDES", :conclusions "1) You are talking to surrogate humans (machine agents)."})
        (tell-inv {:command  "PRIOR-RESPONSES"  :already-answered [] :responses []})
        (tell-inv {:command  "SUPPLY-QUESTION"})
        (tell-inv {:command  "HUMAN-RESPONDS"                            ;; Response to :warm-up-question
                   :response
                   "We make plate glass. One of our most significant scheduling problems involves coordinating the production schedule with the supply of raw materials.
 Fluctuations in raw material deliveries can disrupt planned production runs, leading to delays and inefficiencies.
 Additionally, we need to balance these inconsistencies with varying demand from customers, ensuring that we meet order deadlines without overproducing and holding excess inventory."})
        (tell-inv {:command "SUPPLY-QUESTION"})
        (tell-inv {:command "HUMAN-RESPONDS" :response "PRODUCT"})       ;; Response to :work-type
        (tell-inv {:command "SUPPLY-QUESTION"})
        (tell-inv {:command "HUMAN-RESPONDS" :response "MAKE-TO-STOCK"}) ;; Response to :production-motivation
        (tell-inv {:command "SUPPLY-QUESTION"})
        (tell-inv {:command "HUMAN-RESPONDS" :response "FLOW-SHOP"})     ;; Response to :production-system-type
        (tell-inv {:command "SUPPLY-QUESTION"})
        (tell-inv {:command "HUMAN-RESPONDS"                             ;; Response to process-steps
                   :response
                   "Our production process for plate glass involves several key stages.
 It starts with raw material preparation, where materials like silica sand, soda ash, and limestone are accurately measured and mixed.
 This mixture is then fed into a furnace and melted at very high temperatures to form molten glass.
 The molten glass is then formed into sheets using the float glass process, where it is floated on a bed of molten tin.
 After forming, the glass is slowly cooled to prevent stress in a process called annealing.
 Finally, the glass is cut to size, inspected for quality, and prepared for shipment."})
        (tell-inv {:command "SUPPLY-QUESTION"}))
      (is (=  [{:status "OK"}                                            ;; ANALYSIS-CONCLUDES
               {:status "OK"}                                            ;; PRIOR-RESPONSES
               {:question-type :warm-up-question, :status "OK"}          ;; SUPPLY-QUESTION
               {:status "OK"}                                            ;; HUMAN-RESPONDS
               {:question-type :work-type, :status "OK"}                 ;; SUPPLY-QUESTION
               {:status "OK"}                                            ;; HUMAN-RESPONDS
               {:question-type :production-motivation, :status "OK"}     ;; SUPPLY-QUESTION
               {:status "OK"}                                            ;; HUMAN-RESPONDS
               {:question-type :production-system-type, :status "OK"}    ;; SUPPLY-QUESTION
               {:status "OK"}                                            ;; HUMAN-RESPONDS
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
    (let [claims (pan/analyze-intro-response  warm-up-fountain-pens '#{(proj-id :START-A-NEW-PROJECT)})
          [_ ?pid] (ru/find-claim '(proj-id ?x) claims)]
      (is (and (ru/find-claim (list 'proj-id ?pid) claims)
               (ru/find-claim (list 'cites-raw-material-challenge ?pid) claims)
               (ru/find-claim '(proj-name ?x) claims))))))

(defn ^:diag ask-about-ice-cream-agent
  ([] (ask-about-ice-cream-agent "What are your instructions?"))
  ([q-txt]
   (let [{:keys [iview-aid iview-tid]} (inv/interview-agent :process :sur-ice-cream)]
     (llm/query-on-thread {:aid iview-aid :tid iview-tid :query-text q-txt}))))

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

;;; Is job-shop vs. flow-shop too nebulous? Yes, based on the intro paragraph there is not likely to be sufficient information.
;;; For that reason, I think I need to pose a question to a surrogate expert / parallel expert:

;;;     (scheduling-problem ?x),        ; As opposed to planning-problem (Could have lower priority for (planning-problem ?x)).
;;;     (has-production-facility ?x)    ; As opposed to performed at customer site.
;;;     (product-focus ?x)              ; As opposed to (service-focus ?x)
;;;
;;; ====> SUR?: Could you provide a few short sentences about your production processes? <======

(def production-processes
  {:canned-vegetables "Our production process starts with the receipt of fresh vegetables from our suppliers, which are then cleaned, sorted, and prepped for processing. The vegetables undergo blanching to preserve color and nutritional value, followed by a filling process where they are packed into cans with brine or sauce. The cans are then sealed, sterilized through a high-temperature process to eliminate bacteria and extend shelf life, and finally labeled and packaged for distribution. We operate multiple production lines, each tailored to handle specific types of vegetables, with a focus on efficiency and quality control throughout the process."

   :plate-glass "Our plate glass production process begins at the Batch House, where raw materials are accurately mixed to create the glass batch. The batch is then transferred to the Melting Furnace, where it's melted at high temperatures to form molten glass. Following this, the molten glass proceeds to the Float Bath, where it's floated on molten tin to create a flat surface. After forming, the glass ribbon enters the Annealing Lehr, where it is gradually cooled to remove internal stresses and ensure the glass's structural integrity. The annealed glass is then Quality Inspected for any defects, Cut into the required sizes, and finally, if necessary, undergoes a Coating process to meet specific product requirements before Packing and Shipping to customers. Each of these steps must be carefully scheduled and managed to ensure a seamless operation and meet customer demands efficiently."

   :remanufactured-alternators "Our production process begins with the reception of core units, which are then inspected for suitability. Following inspection, suitable units are disassembled, and their components are cleaned. Each part is then inspected and sorted, with some being reconditioned or replaced as necessary. Reassembly is the next step, during which the alternators are put back together using both the reconditioned and new parts. The final step involves testing the finished alternators to ensure they meet our quality standards. Once they pass the testing phase, the alternators are packaged and stored, ready for distribution."

   :injection-molds "Our production process for injection molds starts with understanding the customer's requirements and translating that into a functional design. Once the design is approved, we move onto procuring the necessary materials, primarily high-grade metals. The bulk of the mold is then formed using CNC machining for precise cuts and shapes, followed by EDM for intricate details and geometries. Post machining, the mold undergoes polishing and finishing to achieve the required surface quality. If the mold consists of multiple components, they are assembled together. Before shipping, the mold is subjected to rigorous testing and quality control checks to ensure it meets the specified standards. This comprehensive process ensures our molds meet the high expectations of durability and precision our customers have come to expect."

   :craft-beer "Our production processes begin with the brewing phase, where we mix the ingredients and boil them to create the wort. Following brewing, the wort is cooled and transferred to fermentation tanks, where yeast is added and the fermentation process begins. This phase is crucial as it converts sugars into alcohol and carbon dioxide. After fermentation, the beer undergoes conditioning, a phase that helps develop flavors and removes unwanted compounds. Finally, the beer is filtered (if necessary) and then bottled, making it ready for distribution. Each phase has its specific duration and conditions, requiring precise scheduling and monitoring to ensure the quality of the final product."

   ;; On this one I asked SUR?: Could you briefly describe your production processes?
   :ice-hockey-sticks "Certainly, our production processes for making ice hockey sticks can be summarized as follows: 1. **Raw Material Procurement:** We source high-quality carbon fiber and resin, essential materials for making durable and lightweight ice hockey sticks. 2. **Material Preparation:** The carbon fiber is cut and prepared according to the specifications for different types of hockey sticks. The resin is also prepared to bond the carbon fibers effectively. 3. **Molding and Shaping:** Prepared materials are placed into molds to form the shape of the hockey stick. This process involves layering the carbon fiber and applying resin to bond these layers together. 4. **Curing and Cooling:** Once the hockey sticks are shaped, they undergo a curing process where they are heated in an oven to harden and solidify the resin. Following this, they are cooled to room temperature to set the final shape. 5. **Quality Inspection:** After cooling, each hockey stick is inspected for defects and to ensure they meet our strict quality standards. This includes checking the stick for durability, weight, and balance. 6. **Packaging and Shipping:** The approved hockey sticks are then packaged and prepared for shipping to various customers and retailers. This streamlined process allows us to maintain a high level of quality and meet the demands of our customers effectively."})

;;; This will be part of prelim-analysis on a parallel or surrogate expert. Here I'm just testing it alone.
(deftest flow-v-job
  (testing "Testing whether a zero-shop prompt on a given LLM is sufficient on 'done at my place or yours' intro paragraphs."
    (let [answer-key {:canned-vegetables            "FIXED"
                      :plate-glass                  "FIXED"
                      :remanufactured-alternators   "FIXED"      ; <=================== Hmmm... I'll see if it changes is a parallel/surrogate expert
                      :injection-molds              "VARIABLE"
                      :ice-hockey-sticks            "FIXED"}]
      (is (= answer-key
             (reset! diag
                     (reduce (fn [res k]
                               (assoc res k
                                      (let [dialog [{:role "system"
                                                     :content "You are a helpful assistant that knows operations research."}
                                                    {:role "user"
                                                     :content (format (str "The following paragraph describes manufacturing production processes for a certain product.\n"
                                                                           "Respond with either 'FIXED' if, like a flow shop in operations research, the order of production operations is apt to be identical for all product types,\n"
                                                                           "or 'VARIABLE' if, like a job shop in operations reasearch, the order of production operations is apt to vary depending on what is being produced.\n\n\n%s")
                                                                      (get production-processes k))}]]
                                        (query-llm dialog {:model-class :gpt #_:gpt-3.5}))))
                             {}
                             (keys answer-key))))))))


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
