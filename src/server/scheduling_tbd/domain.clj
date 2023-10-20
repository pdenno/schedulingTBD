(ns scheduling-tbd.domain
  "Scheduling domain prompts."
  (:require
   [scheduling-tbd.llm           :as llm :refer [query-llm]]
   [scheduling-tbd.sutil         :refer [connect-atm get-api-key]]
   [clojure.pprint               :refer [cl-format pprint]]
   [clojure.string               :as str]
   [taoensso.timbre              :as log]))

(def diag (atom nil))

;;; ------------------------------- project name --------------------------------------
;;; The user would be prompted: "Tell us what business you are in and what your scheduling problem is."
;;; Also might want a prompt for what business they are in.
(def project-name-partial
  "This is used to name the project.  Wrap the user's paragraph in square brackets."
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "Produce a Clojure map containing 1 key, :summary, the value of which is a string of 3 words or less describing the scheduling task being discussed in the text in square brackets."}

   {:role "user"      :content "[We produce clothes for firefighting. Our most significant scheduling problem is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "{:summary \"PPE production scheduling\"}"}

   {:role "user"      :content "[We do road construction and repaving. We find coping with our limited resources (trucks, workers etc) a challenge.]"}
   {:role "assistant" :content "{:summary \"road construction and repaving scheduling\"}"}
   {:role "user"      :content "WRONG. That is more than 3 words."} ; This doesn't help, except with GPT-4!

   {:role "user"      :content "[Acme Machining is a job shop producing mostly injection molds. We want to keep our most important customers happy, but we also want to be responsive to new customers.]"}
   {:role "assistant" :content "{:summary \"job shop scheduling\"}"}])

(def project-objective-partial
  "This is used to scan text for an objective. Wrap the user's paragraph in square brackets."
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "You will be given a paragraph enclosed in square brackets.
From it, produce a Clojure map containing 2 keys, :decision-objective and :probability.
  The value of :objective is a string that is one of the sentences in the input, the sentence that most obviously expresses what needs to be decided in planning work.
  The value of :probability is the likelihood that the sentence really is the one that best expresses an objective."}

   {:role "user"      :content "[We are a construction company contracting for publics works projects, such as bridges and roads. Our challenge is to complete our work while minimizing inconvenience to commuters, businesses, and residents in the affected areas.]"}
   {:role "assistant" :content "{:objective \"Our challenge is to complete our work while minimizing inconvenience to commuters, businesses, and residents in the affected areas.\" :probability 0.9}"}


   {:role "user"      :content "[We install HVAC systems. Our scheduling problem is to efficiently organize and manage our workforce and resources to meet the demands and deadlines of multiple installation projects.
 We must allocate technicians and equipment to various job sites while considering factors such as project complexity, location, customer preferences, and availability of resources.
 The scheduling problem involves balancing the workload, optimizing travel time between sites, minimizing delays and ensuring customer satisfaction, by completing installations within an agreed upon time.]"}
   {:role "assistant" :content "{:objective \"The scheduling problem involves balancing the workload, optimizing travel time between sites, minimizing delays and ensuring customer satisfaction, by completing installations within an agreed upon time.\"
   :probability 0.6}"}
   {:role "user"      :content "Correct. In this one, the probability is lower because the sentence beginning with \"Our scheduling problem is to efficiently organize...\" also seems like an objective."}

   {:role "user"      :content "[We produce clothes for firefighting. It is fun to do. Our most significant scheduling challenge is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "{:objective \"Our most significant scheduling challenge is about deciding how many workers to assign to each product.\" :probability 0.9}"}])


(defn pretend-you-manage-prompt
  [manage-what]
  [{:role "system"    :content (cl-format nil "Pretend you manage ~A. You have good knowledge of the business's processes and supply chain." manage-what)}
   {:role "user"      :content "In no more than 5 sentences, describe your principal scheduling problem."}])

(defn run-interview [what-you-manage]
  (let [high-level-desc (-> what-you-manage
                            pretend-you-manage-prompt
                            (query-llm {:model "gpt-4" :raw-text? true}))
        user-text       {:role "user" :content (cl-format nil "[~A]" high-level-desc)}
        project-name    (-> (conj project-name-partial      user-text) (query-llm {:model "gpt-3.5-turbo"}) :summary)
        objective       (-> (conj project-objective-partial user-text) (query-llm {:model "gpt-4"})         :objective)]
    {:high-level   high-level-desc
     :project-name project-name
     :objective    objective}))

;;; (run-interview "a company that sells snack foods to grocery chains")
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

;;; (run-interview "a contracting company for paving")
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

;;; (run-interview "a job shop that makes injection molds")
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


;;; (run-interview "a craft brewery")
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