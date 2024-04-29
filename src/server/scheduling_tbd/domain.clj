(ns scheduling-tbd.domain
  "Scheduling domain prompts."
  (:require
   [clojure.core.unify            :as uni]
   [clojure.string                :as str]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.llm            :as llm :refer [query-llm]]
   [scheduling-tbd.sutil          :as sutil :refer [find-fact yes-no-unknown str2msg-vec]]
   [taoensso.timbre               :as log]))

(def ^:diag diag (atom nil))

;;; ------------------------------- project name --------------------------------------
;;; The user would be prompted: "Tell us what business you are in and what your scheduling problem is."
;;; Also might want a prompt for what business they are in.
(def project-name-partial
  "This is used to name the project.  Wrap the user's paragraph in square brackets."
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "Produce a Clojure map containing 1 key, :project-name, the value of which is a string of 3 words or less describing the scheduling project being discussed in the text in square brackets.
                                If the text says something like 'We make <x>', <x> ought to be part of your answer."}

   {:role "user"      :content "[We produce clothes for firefighting (PPE). Our most significant scheduling problem is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "{:project-name \"PPE production scheduling\"}"}

   {:role "user"      :content "[We do road construction and repaving. We find coping with our limited resources (trucks, workers etc) a challenge.]"}
   {:role "assistant" :content "{:project-name \"road construction and repaving scheduling\"}"}
   {:role "user"      :content "WRONG. That is more than 3 words."} ; This doesn't help, except with GPT-4!

   {:role "user"      :content "[We do road construction and repaving. We find coping with our limited resources (trucks, workers etc) a challenge.]"}
   {:role "assistant" :content "{:project-name \"supply chain scheduling\"}"}
   {:role "user"      :content "WRONG. Be more specific!"} ; This doesn't help, except with GPT-4!

   {:role "user"      :content "[Acme Machining is a job shop producing mostly injection molds. We want to keep our most important customers happy, but we also want to be responsive to new customers.]"}
   {:role "assistant" :content "{:project-name \"job shop scheduling\"}"}])

(def project-objective-partial
  "This is used to scan text for an objective. Wrap the user's paragraph in square brackets."
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "You will be given a paragraph enclosed in square brackets.
From it, produce a Clojure map containing 2 keys, :decision-objective and :probability.
  The value of :objective is a string that is one of the sentences in the input, the sentence that most obviously expresses what needs to be decided in planning work.
  The value of :probability is the likelihood that the sentence really is the one that best expresses an objective."}

   {:role "user"      :content "[We are a construction company contracting for publics works projects, such as bridges and roads.
Our challenge is to complete our work while minimizing inconvenience to commuters, businesses, and residents in the affected areas.]"}
   {:role "assistant" :content "{:objective \"Our challenge is to complete our work while minimizing inconvenience to commuters, businesses, and residents in the affected areas.\" :probability 0.9}"}


   {:role "user"      :content "[We install HVAC systems. Our scheduling problem is to efficiently organize and manage our workforce and resources to meet the demands and deadlines of multiple installation projects.
 We must allocate technicians and equipment to various job sites while considering factors such as project complexity, location, customer preferences, and availability of resources.
 The scheduling problem involves balancing the workload, optimizing travel time between sites, minimizing delays and ensuring customer satisfaction, by completing installations within an agreed upon time.]"}
   {:role "assistant" :content "{:objective \"The scheduling problem involves balancing the workload, optimizing travel time between sites, minimizing delays and ensuring customer satisfaction,
 by completing installations within an agreed upon time.\"
   :probability 0.6}"}
   {:role "user"      :content "Correct. In this one, the probability is lower because the sentence beginning with \"Our scheduling problem is to efficiently organize...\" also seems like an objective."}

   {:role "user"      :content "[We produce clothes for firefighting. It is fun to do. Our most significant scheduling challenge is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "{:objective \"Our most significant scheduling challenge is about deciding how many workers to assign to each product.\" :probability 0.9}"}])

;;; ToDo: Not sure that "disjoint and covering...therefore" is logically correct here!
(def service-vs-artifact-partial
  "This provides probabilities for whether the scheduling concerns a service vs artifact. We assume those two classes are disjoint and covering."
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "The text in square brackets describes a scheduling problem.
 Classify it using probabilities expressing how well it describes each of two classes;
 the classes are assumed to be disjoint and covering so the sum of the two probabilties should be 1.0.
 The classes are:
   :service - the text describes the scheduling of a service to be rendered.
   :artifact - the text describes the scheduling of the production of physical artifacts.
 Express your result as a Clojure map where the keys are the keywords :service and :artifact."}
   {:role "user"      :content "[We provide music lessons. Our scheduling challenges is finding each week a practice room and time at which students and instructors can meet.]"}
   {:role "assistant" :content "{:service 1.0, :artifact 0.0}"}
   {:role "user"      :content "[We produce and repair clothes for firefighting. Our most significant scheduling challenge is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "{:service 0.2, :artifact 0.8}"}
   {:role "user"      :content "[We repair traffic signal and street lights, replacing bulbs, etc. Our scheduling challenge is in reaching and repairing the most important jobs quickly.]"}
   {:role "assistant" :content "{:service 0.9, :artifact 0.2}"}
   {:role "user"      :content "WRONG: The sum of 0.9 and 0.2 is not 1.0."}])

;;; ToDo: Seeing how we are getting so much text about supply chain/inventory management, maybe not mention supply chain.
;;;       I'd like to say "manufacturing processes" rather than "business's processes" (where appropriate) so maybe run this after mfg/service query.
;;;       In the mfg case, the user would ask about manufacturing scheduling problems.
;;;       Thus have two versions of this, one for manufacturing and one for services.
(defn pretend-you-manage-prompt
  [manage-what]
  [{:role "system"    :content (format "Pretend you manage %s. You have good knowledge of the business's processes and supply chain." manage-what)}
   {:role "user"      :content "In no more than 5 sentences, describe your principal scheduling problem."}])

(defn project-name-llm-query
  "Wrap the user-text in square brackets and send it to an LLM to suggest a project name.
   Returns a string consisting of just a few words."
  [user-text]
  (reset! diag user-text)
  (as-> (conj project-name-partial
              {:role "user" :content (str "[ " user-text " ]")}) ?r
    (query-llm ?r {:model-class :gpt-4 :raw-text? false}) ; 2024-03-23 I was getting bad results with :gpt-3.5. This is too important!
    (:project-name ?r)
    (if (string? ?r) ?r (throw (ex-info "Could not obtain a project name suggestion from and LLM." {:user-text user-text})))))

#_(defn pretend-you-manage-interview [what-you-manage & desc]
  (let [high-level-desc (or (first desc)
                            (-> what-you-manage
                                pretend-you-manage-prompt
                                (query-llm {:model-class :gpt-4 :raw-text? true})))
        user-text {:role "user" :content (format "[%s]" high-level-desc)}
        project-name (-> (conj project-name-partial user-text)
                         (query-llm {:model-class :gpt-3.5})
                         :summary
                         (str/replace #"\s+" "-"))]
    {:high-level    high-level-desc
     :activity-name (-> (re-matches #"(.*)\s+scheduling" project-name) (nth 1))
     :project-name  project-name
     :objective     (-> (conj project-objective-partial user-text)
                        (query-llm {:model-class :gpt-4})
                        :objective)
     :service?      (-> (conj service-vs-artifact-partial user-text)
                        (query-llm {:model-class :gpt-4}))}))


;;; ToDo: I've seen :snack-food going from {:service 0.5, :artifact 0.5}, {:service 0.0, :artifact 1.0}. I suppose delivering to a supply-chain partner....
#_(defn test-service-vs-artifact
  []
  (reduce-kv (fn [m k v]
               (try
                 (let [user-text {:role "user" :content (format "[%s]" v)}]
                   (assoc m k (-> (conj service-vs-artifact-partial user-text) (query-llm {:model-class :gpt-4}))))
                 (catch Throwable e
                   (assoc m k (format "Failed: %s" e)))))
             {}
             user-problems))

;(reduce-kv (fn [m k v] (assoc m k (test-service-vs-artifact k))) {} user-problems)

;;; (pretend-you-manage-interview "a company that sells snack foods to grocery chains")
#_{:high-level
     "The primary challenge in our scheduling process involves effectively coordinating all the steps in our supply chain, starting from raw material procurement to the final delivery of our snack foods to grocery chains.
      We aim to maintain an optimal inventory level which involves proper timing of production runs to minimize stockouts and excess storage costs.
      Seasonal fluctuations in demand, delays from suppliers, equipment breakdowns, and transportation delays pose consistent scheduling problems.
      Additionally, the scheduling process needs to account for shelf-life of our products to prevent wastage.
      Lastly, integrating all the processes within the firm and communicating the schedule effectively to all the stakeholders is a significant problem.",
   :project-name "supply chain scheduling",
   :objective "We aim to maintain an optimal inventory level which involves proper timing of production runs to minimize stockouts and excess storage costs."}

;;; Thoughts:
;;;   - That's a great description. Thank you GPT-4!
;;;   - To set inventory levels, we will need to get a handle on demand AND supply fluctuation and what level of stockouts they are willing to tolerate. We need to see some demand data.
;;;     There are a few common methods for calculating the level of inventory they need to maintain, some handle seasonality. We'd drop one of these into the MiniZinc and talk about it.
;;;   - We need to see current inventory levels too.
;;;   - There is a tradeoff between stockouts and storage cost. What they are talking about as storage cost is probably most about shelf-life,
;;;     though we should ask whether sometimes finding sufficient room in the warehouse is a problem.
;;;   - We'd work backwards from demand, associating products with production lines.
;;;   - It isn't clear whether we are solving a problem for a single facility or not. We'd need to ask about that, and if multiple facilities, what runs where, what can run where, etc.

;;; (pretend-you-manage-interview "a contracting company for paving")
#_ {:high-level
      "The principal scheduling problem faced is coordinating the availability of our clients, skilled work crew, and the delivery of material supplies.
       Unpredictable weather conditions often lead to sudden schedule changes.
       Also, delays in supply chain due to various reasons can significantly affect the timeline of the project.
       Balancing multiple projects simultaneously without over-committing our resources is also a challenge.
       Lastly, unanticipated repairs and maintenance of our paving equipment affects our work schedule.",
    :project-name "construction project scheduling",
    :objective "The principal scheduling problem faced is coordinating the availability of our clients, skilled work crew, and the delivery of material supplies."}

;;; Thoughts:
;;;   - This is project scheduling. I'll extend these examples with a request to classify, based just on the title, pick one of
;;;     ["project scheduling", "fixed process scheduling", "service scheduling"].
;;;     (next iteration of this exercise).
;;;   - The sentence "Also, delays in supply chain due to various reasons can significantly affect the timeline of the project." seemed dubious to me.
;;;    If the text makes vague mention of supply chain like this, we'd have to seek an elaboration.
;;;    So offline I asked: "Enumerate the specific supply chain issues expressed in the following text (in double quotes): [the :high-level text above]"
;;;    Result: "Based on the text provided, the specific supply chain issues faced in this scenario are:
;;;
;;;         1. Coordinating the availability of clients, skilled work crew, and material supplies.
;;;         2. Unpredictable weather conditions leading to sudden schedule changes.
;;;         3. Delays in supply chain affecting the timeline of the project.
;;;         4. Balancing multiple projects simultaneously without over-committing resources.
;;;         5. Unanticipated repairs and maintenance of paving equipment affecting the work schedule."
;;;   - Generally these would make good constraints, but
;;;            * "clients" in (1) is a problem
;;;            * (3) just restates the question.
;;;     Next iteration I'll ask for the supply chain issues of every problem.
;;;   - I think project scheduling users ultimately want a Gantt that is reflective of the constraints on resource utilization.
;;;     They want to be able to update the schedules every day with progress or lack thereof.
;;;     I've done something like that before with Jupyter notebooks integrated with Mzn. But, of course, we'll focus on just getting the basic problem done in Mzn first.

;;; (pretend-you-manage-interview "a job shop that makes injection molds")
#_{:high-level
   "The principal scheduling problem in running an injection mold job shop is coordinating the different job orders in a manner that optimizes our machine utilization and minimizes production time without leading to bottlenecks.
    We must effectively manage the flow of materials from suppliers, ensuring that they're available exactly when needed to avoid delays.
    It's also crucial to ensure our labor force is appropriately assigned to different tasks in the shop to maximize efficiency.
    Lastly, unexpected maintenance or breakdowns of machinery can throw our schedule off and present a major challenge.
    Central to all this is the need for high precision and quality in molds, which can potentially impact scheduling if reworks or corrections are required due to errors.",
   :project-name "injection mold job shop scheduling", ; Note to self: GPT-3.5 can't count. I asked for 3 words or less.
   :objective  "The principal scheduling problem in running an injection mold job shop is coordinating the different job orders in a manner that optimizes our machine utilization and minimizes production time without leading to bottlenecks."}

;;; Thoughts:
;;;    - The remark about high precision is useless, a red herring. Also "flow of materials from suppliers" isn't such a problem here. This one doesn't reflect reality too well.
;;;      Compared to what we got for snack foods and paving, this doesn't seem very realistic to me.
;;;    - Any time the user talks about resource utilization we need to ask if there is a consistent bottleneck resource.
;;;      If there is, typically a model that schedules that resource works best.
;;;      All the tasks leading up to the bottleneck can be "back scheduled" so that they start with sufficient time to be ready for the bottleneck resource when it is their turn.
;;;    - Here we'd want to know the scope of what is scheduled. Do they have mold designs? Designs implicitly define process plans.
;;;      I think deeper classification of "project scheduling" would help here. The title has "job shop scheduling" that gets us pretty far, generally speaking


;;; (pretend-you-manage-interview "a craft brewery")
#_{:high-level
   "As the manager of a craft brewery, the principal scheduling problem is coordinating the production process to maintain a consistent supply of diverse beers without overproduction or storage issues.
    This spans from the initial scheduling of raw materials procurement, to the fermenting process which can take weeks, and finally to the bottling and distribution procedures.
    Additionally, managing seasonal demand fluctuations and accommodating special edition or experimental brews without disrupting the core product line adds complexity.
    Timely maintenance of brewing equipment and quality check is also crucial to the schedule.
    Lastly, ensuring synergies with marketing timelines and release dates is necessary to avoid any mismatch between supply and market launch.",
   :project-name "craft brewery production scheduling",
   :objective "the principal scheduling problem is coordinating the production process to maintain a consistent supply of diverse beers without overproduction or storage issues."}

;;; Thoughts:
;;;    - Unlike GPT-3.5-turbo, GPT-4 can count and I think 5 sentences is a good start. We might ask real users for 3-6 sentences.
;;;    - A few us visited Flying Dog Brewery (when it was in Frederick) a while back. This seems to capture what they were saying pretty well!
;;;    - Similar to what I said about "deeper classification" above, I think deeper on "fixed process scheduling" would help here.
;;;      This is batch manufacturing without a place to put WIP. That makes it something like synchronous line discrete scheduling, but the products all follow the same path.
;;;    - Whenever the problem classifies as "fixed process scheduling" we'll ask (either the LLM or the human) what the steps are. In the next iteration, I'll ask the LLM.

;;; ------------------- Continuing...

;;; Frequently, users (in the real world as well as the test data) will allude to inventory challenges.
;;; There are two principal types: raw material and finished goods stockout.
;;; Two things we'll need to do to help these people are:
;;;   (1) Obviously don't schedule a task that quite likely won't have materials needed to carry it out, and
;;;   (2) Offer to analyze their inventory management processes.
;;; (1) is something we'll do soon, (2) quite a bit later. But the important point at the introduction is to steer the conversation away from the inventory problem.
;;; We should try to characterize (mechanistically) the manufacturing problem first if at all possible.
;;; Generally speaking, finished goods stock outs take things in different directions (e.g. demand forecasting) than raw materials.

;;; ToDo:
;;;   (1) Write the finished good version of the following, which handles raw-material shortages.
;;;   (2) Get these into responses in the conversation.

(def raw-material-challenge-partial
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "Respond with either 'yes' or 'no' to whether the text in square brackets alludes to a raw-materials shortage."}

   {:role "user"      :content "[We produce clothes for firefighting. Our most significant scheduling problem is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "no"}

   {:role "user"      :content "[We do road construction and repaving. We find coping with our limited resources (trucks, workers, aggregate, etc.) a challenge.]"}
   {:role "assistant" :content "yes"}

   {:role "user"      :content "[Our principal scheduling problem revolves around coordinating raw material procurement with production schedules and customer delivery deadlines.
 We often face the challenge of ensuring a continuous supply of high-quality raw aluminium, which is subject to market availability and price fluctuations.]"}
   {:role "assistant" :content "yes"}

   {:role "user"      :content "[We run several products simultaneously and simply would like to be able to have the beer bottled and ready to ship as near as possible to
 the dates defined in our sales plan.]"}
   {:role "assistant" :content "no"}])

(defn text-cites-raw-material-challenge?
  "Return :yes, :no, or :unknown depending on whether the text cites an inventory challenge."
  [text]
  (-> (conj raw-material-challenge-partial
            {:role "user" :content (format "[%s]" text)})
      (query-llm {:model-class :gpt-3.5})
      yes-no-unknown))

;;; ToDo: This was written before I decided that domain.clj ought to require db and use it. Refactor this and its caller, !initial-question ?
(defn project-name-analysis
  "Analyze the response to the initial question, adding to the init-state vector."
  [response init-state]
  (log/info "prelim-analysis: init-state =" init-state "response =" response)
  (let [[_ pid] (sutil/find-fact '(proj-id ?x) init-state)
        proj-state (if (= pid :START-A-NEW-PROJECT)
                     (let [proj-name (as-> (project-name-llm-query response) ?s
                                       (str/trim ?s)
                                       (str/split ?s #"\s+")
                                       (map str/capitalize ?s)
                                       (interpose " " ?s)
                                       (apply str ?s))
                           proj-id (as-> proj-name ?s (str/lower-case ?s) (str/replace ?s #"\s+" "-") (symbol ?s))]
                       (into (filterv #(not= % '(proj-id START-A-NEW-PROJECT)) init-state)
                             `[(~'proj-id ~proj-id)
                               (~'proj-name ~proj-name)]))
                     ;; Otherwise, it is a surrogate; proj-id and proj-name are already known.
                     init-state)]
    (if (= :yes (text-cites-raw-material-challenge? response))
        (conj proj-state (list 'cites-raw-material-challenge (-> pid name symbol)))
        proj-state)))

;;; ----------------------- parallel expert preliminary analysis  ---------------------------------------------------------
(defn ask-about-process-on-thread!
  "Ask the surrogate to describe production processes. We don't care what is returned, we just
   are developing context to do some downstream preliminary analysis.
   We put these into the conversation under surrogate."
  [aid tid]
  (let [msg "Please briefly describe your production processes?"]
    {:query msg
     :answer (llm/query-on-thread :aid aid :tid tid :query-text msg)}))

(defn product-vs-service
  "Return a vector of ground predicates (so far just either [(provides-product ?x)] or [(provides-service ?x)],
   depending on whether the project describes respectively work to provide a product or work to provide a service."
  [aid tid]
  (let [query (str "Would you characterize your company's work as primarily providing a product or a service?\n" ; ToDo: "firm's work" in all of these not good?
                   "Respond respectively with either the single word PRODUCT or SERVICE.")
        answer (llm/query-on-thread {:aid aid :tid tid :query-text query})
        preds (cond (re-matches #".*(?i)PRODUCT.*" answer) '[(provides-product ?x)]
                    (re-matches #".*(?i)SERVICE.*" answer) '[(provides-service ?x)]
                    :else                                  '[(fails-query product-vs-service ?x)])]
    {:query query :answer answer :preds preds}))

;;; ToDo: Maybe what I really want is to split scheduling vs some notion of constraint satisfaction (which would include project management and cyclical scheduling)
;;;       But for the time meaning, it was this that came to mind.
(defn production-mode
  "Return a vector of ground predicates (so far just either [(is-product ?x)] or [(is-service ?x)],
   depending on whether the project describes respectively work to provide a product or work to provide a service."
  [aid tid]
  (let [query (str "Three commonly recognized ways of production are termed MAKE-TO-STOCK, MAKE-TO-ORDER, and ENGINEER-TO-ORDER.\n"
                   "In MAKE-TO-STOCK you make product to replenish inventory based on forecasted demand.\n"
                   "In MAKE-TO-ORDER you make product because a customer has specifically asked you to, and the customer has described characteristics of the product in your own terminology, perhaps using your catalog of offerings.\n"
                   "ENGINEER-TO-ORDER is something like MAKE-TO-ORDER but here the customer also expects you to do some creative problem solving to meet their need.\n"
                   "For example, a commercial aircraft might be ENGINEER-TO-ORDER because though the customer may have specified the engine type and seating capacity it wants,\n"
                   "it is relying on you to determine how to best accommodate the engine and arrange the seats.\n"
                   "Other examples of ENGINEER-TO-ORDER include general contracting for building construction, film production, event planning, and 3rd party logisistics.\n"
                   "Respond with just one of the terms MAKE-TO-STOCK, MAKE-TO-ORDER or ENGINEER-TO-ORDER according to which most accurately describes your mode of production.\n")
        answer (llm/query-on-thread {:aid aid :tid tid :query-text query})
        preds (cond (re-matches #".*(?i)MAKE-TO-STOCK.*" answer)        '[(production-mode ?x make-to-stock)]
                    (re-matches #".*(?i)MAKE-TO-ORDER.*" answer)        '[(production-mode ?x make-to-order)]
                    (re-matches #".*(?i)ENGINEER-TO-ORDER.*" answer)    '[(production-mode ?x engineer-to-order)]
                    :else                                               '[(fails-query production-mode ?x)])]
    {:query query :answer answer :preds preds}))

(defn facility-vs-site
  "Return a vector of ground predicates (so far just either [(is-product ?x)] or [(is-service ?x)],
   depending on whether the project describes respectively work to provide a product or work to provide a service."
  [aid tid]
  (let [query (str "Some work, for example factory work, must be performed in a specially designed facility.\n"
                   "Other work, like cutting down a tree, can only be performed at a location designated by the customer.\n"
                   "Are the processes you describe things that must be performed at your facility, or are they things that must be done at the customer's site?\n"
                   "Respond respectively with either the single term OUR-FACILITY or CUSTOMER-SITE.")
        answer (llm/query-on-thread {:aid aid :tid tid :query-text query})
        preds (cond (re-matches #".*(?i)OUR-FACILITY.*" answer)  '[(has-production-facility ?x)]
                    (re-matches #".*(?i)CUSTOMER-SITE.*" answer) '[(performed-at-customer-site ?x)]
                    :else                                        '[(fails-query facility-vs-site ?x)])]
    {:query query :answer answer :preds preds}))

(defn flow-vs-job
  "Return a vector of ground predicates (so far just either [(is-flow-shop ?x)] or [(is-job-shop ?x)] or [])
   depending on whether the project describes respectively work to provide a product or work to provide a service.
   Note that this question is only applied where (provides-product ?x) (scheduling-problem ?x) (has-production-facility ?x)."
  [aid tid]
  (let [query (str "A flow-shop is a production system designed so that all jobs follows the same sequence of steps through production resources.\n"
                   "A job-shop is a production system where each job might follow its own route, depending on its unique requirements.\n"
                   "Is the process you described more like a flow-shop or a job-shop?\n"
                   "Respond respectively with either the single term OUR-FACILITY or CUSTOMER-SITE.")
        answer (llm/query-on-thread {:aid aid :tid tid :query-text query})
        preds (cond (re-matches #".*(?i)FLOW-SHOP.*" answer) '[(flow-shop ?x)]
                    (re-matches #".*(?i)JOB-SHOP.*" answer)  '[(job-shop ?x)]
                    :else                                    '[(fails-query flow-vs-job ?x)])]
    {:query query :answer answer :preds preds}))

;;; ToDo: Define attributes in DB and test starting a parallel expert.
(defn parallel-expert-prelim-analysis
  "Do preliminary characterization of the scheduling problem including:
      - product vs. service
      - production facility vs. customer site,
      - scheduling vs. project management, and
      - flow-shop vs. job-shop   (of course, this one only for product/production facility/scheduling).
   If the project is a surrogate expert, this work is done in the surrogate thread,
   otherwise a new surrogate (called a 'parallel expert') is started and its
   Assistant ID and Thread ID are stored. (This part not done yet.)
   Return a vector of new propositions."
  ([pid] (parallel-expert-prelim-analysis pid true))
  ([pid write?]
   (when-not (find-fact '(surrogate ?proj) (db/get-planning-state pid)) ; ToDo: Complete for human expert.
     (throw (ex-info "Parallel expert not yet implemented." {})))
   (let [proj-sym (-> pid name symbol)
         proj-bind {'?x proj-sym}
         aid (db/get-assistant-id pid)
         tid (db/get-thread-id pid)
         new-props (atom [])
         {:keys [query answer]} (ask-about-process-on-thread! aid tid)] ; This one asks a question that sets up context.
     (if (string? answer)
       (do (when write?
             (db/add-msg pid :system query)
             (db/add-msg pid :surrogate answer))
           (doseq [f [product-vs-service production-mode facility-vs-site]]
             (let [{:keys [query answer preds]} (f aid tid)]
               (swap! new-props into (map #(uni/subst % proj-bind) preds))
               (when write?
                 (db/add-msg pid :system query)
                 (db/add-msg pid :surrogate answer))))
           (when (and (find-fact '(provides-product ?x) @new-props)
                      (find-fact '(has-production-facility ?x) @new-props))
             (swap! new-props into (->> (flow-vs-job aid tid) :preds (map #(uni/subst % proj-bind)))))
           @new-props)
       [(list 'fails-query 'process-description proj-sym)]))))

;;; --------------------------------------- unimplemented (from the plan) -----------------------------
(defn yes-no-process-steps
  "Return state addition for analyzing a Y/N user response about process steps."
  [_response]
  [])

(defn query-process-durs
  "Return state addition for analyzing a query to user about process durations."
  [_response]
  [])

(defn yes-no-process-ordering
  "Return state addition for analyzing a Y/N user response about process ordering."
  [_response]
  [])
