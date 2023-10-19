(ns scheduling-tbd.domain
  "Scheduling domain prompts."
  (:require
   [scheduling-tbd.llm           :as llm :refer [chat2clj]]
   [scheduling-tbd.sutil         :refer [connect-atm get-api-key]]
   [clojure.pprint               :refer [cl-format pprint]]
   [clojure.string               :as str]
   [taoensso.timbre              :as log]))

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
