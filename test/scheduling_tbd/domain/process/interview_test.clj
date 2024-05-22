(ns scheduling-tbd.domain.process.interview-test
  "Currently these are more about exploring prompts than they are about test the code."
  (:require
   [clojure.pprint        :refer [cl-format pprint]]
   [clojure.string        :as str]
   [clojure.test          :refer [deftest is testing]]
   [promesa.core          :as p]
   [scheduling-tbd.domain.process.interview :as inv]
   [scheduling-tbd.db    :as db]
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm]]
   [scheduling-tbd.llm    :as llm :refer [query-llm]]
   [taoensso.timbre          :as log]))

;;; (ns-unalias (find-ns 'scheduling-tbd.domain.process.interview-test) 'inv)
;;; (ns-unmap  'scheduling-tbd.domain.process.interview-test 'inv)

(defn huh?
  []
  (ns-unalias (find-ns 'scheduling-tbd.domain.process.interview-test) 'db)
  (ns-unalias (find-ns 'scheduling-tbd.domain.process.interview-test) 'sutil)
  (ns-unalias (find-ns 'scheduling-tbd.domain.process.interview-test) 'llm)
  (ns-unalias (find-ns 'scheduling-tbd.domain.process.interview-test) 'log)
  (ns-unmap  'scheduling-tbd.domain.process.interview-test 'inv)
  (ns-unmap  'scheduling-tbd.domain.process.interview-test 'db)
  (ns-unmap  'scheduling-tbd.domain.process.interview-test 'sutil)
  (ns-unmap  'scheduling-tbd.domain.process.interview-test 'llm)
  (ns-unmap  'scheduling-tbd.domain.process.interview-test 'log))


(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (alias 's      'clojure.spec.alpha)
  (alias 'uni    'clojure.core.unify)
  (alias 'edn    'clojure.edn)
  (alias 'str    'clojure.string)
  (alias 'd      'datahike.api)
  (alias 'dp     'datahike.pull-api)
  (alias 'mount  'mount.core)
  (alias 'p      'promesa.core)
  (alias 'px     'promesa.exec)
  ;(alias 'core   'scheduling-tbd.core)
  (alias 'inv    'scheduling-tbd.domain.process.interview)
  (alias 'db     'scheduling-tbd.db)
  (alias 'how    'scheduling-tbd.how-made)
  (alias 'llm    'scheduling-tbd.llm)
  ;(alias 'llmt   'scheduling-tbd.llm-test)
  (alias 'op     'scheduling-tbd.operators)
;  (alias 'opt    'scheduling-tbd.operators-test)
  (alias 'plan   'scheduling-tbd.planner)
  (alias 'resp   'scheduling-tbd.web.controllers.respond)
  (alias 'spec   'scheduling-tbd.specs)
; (alias 'sutil  'scheduling-tbd.sutil)
  (alias 'sur    'scheduling-tbd.surrogate)
  ;(alias 'surt   'scheduling-tbd.surrogate-test)
  (alias 'util   'scheduling-tbd.util)
  (alias 'ws     'scheduling-tbd.web.websockets)
  (alias 'openai 'wkok.openai-clojure.api))

(def diag (atom nil))


(def proj-objective-prompt
  (conj inv/project-objective-partial
        {:role "user"
         :content "[We bake cookies and sell them through grocery chains. Our challenge is to supply fresh cookies and never stock-out.
 We want a schedule that will ensure we meet demand and have the ingredients we need.]"}))


(def domain-problems ; I have removed from these descriptions text that gives away too much (e.g. 'we plan projects' for scheduling/project planning." I switched "scheduling problem" to "production problem"
  {:snack-food "The primary challenge in our production process involves effectively coordinating all the steps in our supply chain, starting from raw material procurement to the final delivery of our snack foods to grocery chains.
 We aim to maintain an optimal inventory level which involves proper timing of production runs to minimize stockouts and excess storage costs.
 Seasonal fluctuations in demand, delays from suppliers, equipment breakdowns, and transportation delays pose consistent scheduling problems.
 Additionally, the scheduling process needs to account for shelf-life of our products to prevent wastage.
 Lastly, integrating all the processes within the firm and communicating the schedule effectively to all the stakeholders is a significant problem.",

   :paving "The principal problem we face is coordinating the availability of our clients, skilled work crew, and the delivery of material supplies.
 Unpredictable weather conditions often lead to sudden schedule changes.
 Also, delays in supply chain due to various reasons can significantly affect the timeline of the project.
 Balancing multiple projects simultaneously without over-committing our resources is also a challenge.
 Lastly, unanticipated repairs and maintenance of our paving equipment affects our work schedule.",

   :injection-molds "Our principal production problem in running an injection mold facility is coordinating the different production orders in a manner that optimizes our machine utilization and minimizes
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
                                (query-llm dialog {:model-class :gpt-4 #_:gpt-3.5}))))
                     {}
                     (keys answer-key)))))))

;;; GPT-3.5 has been slower than GPT-4 on 2024-04-28.
;;;
;;; Regarding :general-contractor, I changed the wording from
;;;    "like cutting down a tree, it has to be done where the customer designates" to
;;;    "it concerns activity that will be performed at a place the customer designates.
;;;
;;; Also was better off with full sentences; sometimes I got back "Our facility" when I asked for 'our facility'.
(deftest my-place-or-yours?
  (testing "Testing whether a zero-shop prompt on a given LLM is sufficient on 'done at my place or yours' intro paragraphs."
    (let [answer-key {:snack-food         "Our facility."
                      :paving             "Customer site."
                      :injection-molds    "Our facility."
                      :brewery            "Our facility."
                      :music-lessons      "Our facility."
                      :general-contractor "Customer site."}]  ; <===== a hard one, where the LLMs disagree and aren't consistent across runs. See remarks above.
      (is (= answer-key
             (reduce (fn [res k]
                       (assoc res k
                              (let [dialog [{:role "system"
                                             :content "You are a helpful assistant."}
                                            {:role "user"
                                             :content (format (str "The following paragraph describes an activity that can be performed in only one of two ways.\n"
                                                                   "Respond with either 'Our facility.' if, like factory work, it is done in a place other than where the customer will use it,"
                                                                   "or 'Customer site.' if it concerns work that will be performed at a place the customer designates.\n\n %s")
                                                              (get domain-problems k))}]]
                                (query-llm dialog {:model-class :gpt-4 #_gpt-3.5}))))
                     {}
                     (keys answer-key)))))))


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
                                (query-llm dialog {:model-class :gpt-4}))))
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
                                (query-llm dialog {:model-class :gpt-4}))))
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
                                        (query-llm dialog {:model-class :gpt-4 #_gpt-3.5}))))
                             {}
                             (keys answer-key))))))))


(deftest project-objective-test
  (testing "Testing that project objective prompt is okay. Really this only cares that it returns a clojure map with the correct keys.")
  (let [res (-> (query-llm proj-objective-prompt {:model-class :gpt-4 :raw-text? false}) (p/await))]
    (is (= #{:objective :probability} (-> res keys set))))

  ;; This one is interesting; in some sense better than GPT-4. It sometimes returns two sentences.
  (let [res (-> (query-llm proj-objective-prompt {:model-class :gpt-3.5 :raw-text? false}) (p/await))]
    (is (= #{:objective :probability} (-> res keys set)))))

(defn msg-vec2html
  [msg]
  (if (some #(contains? % :msg-link/text) (:message/content msg))
    (assoc msg :message/content "Describe your most significant scheduling problem in a few sentences or <a href=\"http://localhost:3300/learn-more\">learn more about how this works</a>.")
    (assoc msg :message/content (apply str (map #(:msg-text/string %) (:message/content msg))))))

(defn migrate-project
  "Translate :message/content to html."
  [pid]
  (let [new-proj (-> (db/get-project pid)
                     (update :project/messages (fn [msgs] (-> (reduce (fn [res msg] (conj res (msg-vec2html msg))) [] msgs) vec))))
        proj-string (with-out-str (pprint new-proj))]
    (spit (str "data/projects/" (name pid) ".edn") (format "[\n%s\n]" proj-string))))


(def beer-steps
"Our production process involves several key steps:

1. Mashing: Mixing milled grains (usually malted barley) with water and heating the mixture. This step extracts sugars from the grains, creating a sugary liquid known as \"wort.\"

2. Boiling: The wort is boiled, and hops are added for flavor, aroma, and bitterness. This step also sterilizes the wort.

3. Cooling: After boiling, the wort is rapidly cooled down to a temperature suitable for fermentation.

4. Fermentation: The cooled wort is transferred to fermentation tanks, where yeast is added. The yeast ferments the sugars in the wort, producing alcohol and carbon dioxide. This can take from a few days to several weeks, depending on the beer type.

5. Conditioning: After fermentation, the beer is conditioned to develop its full flavor profile. This can happen in the same tank or by transferring the beer to a new tank.

6. Packaging: The final step is packaging the beer into cans, bottles, or kegs for distribution.

Each of these steps requires specific time intervals and conditions (e.g., temperature) to ensure the production of high-quality beer.")
