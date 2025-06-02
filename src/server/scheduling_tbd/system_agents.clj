(ns scheduling-tbd.system-agents
  (:require
   [clojure.java.io         :as io]
   [clojure.pprint          :refer [cl-format]]
   [clojure.set             :as set]
   [mount.core              :as mount :refer [defstate]]
   [scheduling-tbd.agent-db :as adb]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.sutil    :as sutil]))

(def system-agents-and-bases

  {;; ---------- project agents base-types (interviewers, orchestrator)  -------------------------
   ;; (ToDo: This will eventually be :agent/agent-type :base-type.)
   :orchestrator-agent,
   {:base-type :orchestrator-agent,
    :model-class :gpt,
    :instruction-path "agents/orchestrator.txt",
    ;; The vector store includes all the EADS instructions for interviewers.
    :tools "[{:type \"file_search\"}]"
    :vector-store-paths (->> (.list (io/file "resources/agents/iviewrs/EADS/")) (mapv #(str "agents/iviewrs/EADS/" %)))
    :agent-type :shared-assistant}

   :process-interview-agent,
   {:base-type :process-interview-agent,
    :cid :process
    :iviewr-name "Process Interviewer"
    :focus "the processes used in their enterprise and the challenges they face in executing those processes."
    :warm-up-question "What are the products you make or the services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences."
    :model-class :gpt,
    :agent-type :shared-assistant}

   :data-interview-agent
   {:base-type  :data-interview-agent
    :cid :data
    :iviewr-name "Data Interviewer"
    :focus "data they use in their work."
    :warm-up-question (str "To get started, please provide a short overview (a few sentences) about the data you use to do your work.\n"
                           "Typically, and especially in small businesses, information about customer orders, inventory, resources used to do the work, etc. are maintained in spreadsheets.\n"
                           "It is okay if you don't use spreadsheets for these purposes; we can make what we need with a little more discussion.\n"
                           "Whatever the case, to get started, please write a few sentences about the information you use.\n"
                           "Once you provide this overview, we'll have more detailed discussions on each data source you mentioned.")
    :model-class :gpt,
    :tools "[{:type \"file_search\"}]"
    :vector-store-paths ["resources/agents/iviewrs/papers/object-role-modeling--an-overview.pdf"],
    :agent-type :shared-assistant}

   :resources-interview-agent,
   {:base-type :resources-interview-agent,
    :cid :resources
    :iviewr-name "Resources Interviewer"
    :focus "the actual physical resources that they use to do their work, their numbers, and capabilities. For example, the number of workers with particular skills."
    :warm-up-question (str "To get started, please provide a short overview (a few sentences) about the resources your company use to get work done.\n"
                           "This might include, for example, machines and workers. Once you've provided this overview, we'll dig into the details of the quantities, capabilities,\n"
                           "and skills classifications etc. for each type of resource you mentioned.")
    :model-class :gpt,
    :agent-type :shared-assistant}

   :optimality-interview-agent,
   {:base-type :optimality-interview-agent,
    :cid :optimality
    :iviewr-name "Optimality Interviewer"
    :focus "what good schedules in the their enterprise achieve or avoid. For example, a schedule might seek to maximize utilization of expensive equipment or minimize late delivery of product."
    :warm-up-question (str "To get started, please provide a short overview (a few sentences) about what you expect of good schedules.\n"
                           "In some domains, for example, it is important to maximize the utilization of an expensive resource, in others it is to minimize a certain kind of waste.\n"
                           "Often, it is a combination of such things. After you've provided a short summary, we'll probably have some follow-up questions.")
    :model-class :gpt,
    :agent-type :shared-assistant}

   ;; -----------------  system agents
   :response-analysis-agent
   {:base-type :response-analysis-agent
    :agent-type :system
    :instruction-path "agents/response-analysis-agent.txt"
    :response-format-path "agents/response-analysis-format.edn"}

   :scheduling-challenges-agent
   {:base-type :scheduling-challenges-agent
    :agent-type :system
    :instruction-path "agents/scheduling-challenges.txt"
    :response-format-path "agents/scheduling-challenges-response-format.edn"}

   :text-to-var
   {:base-type :text-to-var
    :agent-type :system
    :model-class :mini
    :response-format-path "agents/text-to-var-response-format.edn"
    :instruction-path "agents/text-to-var.txt"}

   :text-function-agent
   {:base-type :text-function-agent
    :agent-type :system
    :instruction-path "agents/text-function-agent.txt"}

   :trivial-agent
   {:base-type :trivial-agent
    :agent-type :system} })

(def sys-agent? (-> system-agents-and-bases keys set))

(def ^:diag diag (atom nil))

(defn iviewr-instructions
  "Write the agent instructions to resources/agents/iviewrs/<cid>-iviewr-instructions.txt."
  [{:keys [iviewr-name cid focus]}]
  (let [others (set/difference #{:process :data :resources :optimality} #{cid})]
     (str "You are one of four interviewers engaging humans in conversations to elicit from them requirements for a scheduling system we humans and AI agents will be creating together using MiniZinc.\n"
          (format "You are the %s Agent; you ask questions about %s\n" iviewr-name focus)
          "The other three agents are:"
          (cl-format nil "~{~%     - a ~A Agent: that interviews about ~A~}" others)
          "\nIn as far as it is practical, you should avoid asking questions in the areas that are the responsibility of these other agents.\n\n"
          (-> "agents/iviewrs/base-iviewr-instructions.txt" io/resource slurp))))

(defn update-iviewers
  "Return a version of system-agents-and-bases with :instruction-string set to
   the concatenation of things defined by make-iviewr-instructions (a string)."
  [agent-maps]
  (reduce-kv  (fn [m k v]
                (if (:cid v)
                  (assoc m k (assoc v :system-instructions (iviewr-instructions v)))
                  (assoc m k v)))
              []
              agent-maps))

(defn ^:admin force-new-system-agents!
  "This is to update the aid."
  []
  (doseq [agent-id (keys system-agents-and-bases)]
    (adb/ensure-agent! agent-id))) ; <======================================================================================== Finish this.



;;; ------------------------------- starting and stopping ---------------------------------
(defn ensure-system-agent-basics
  "Make sure the agents in the system db have the above basics. This is necessary for adb/ensure-agent! to work.
   The argument is the :base-type name."
  ([agent-id] (ensure-system-agent-basics agent-id @sutil/default-llm-provider))
  ([agent-id llm-provider]
   (assert (sys-agent? agent-id))
   (let [{:keys [base-type] :as agent-map} (get system-agents-and-bases agent-id)
         agent-map (-> agent-map
                       (assoc :agent-id (-> base-type name (str "-" (name llm-provider)) keyword))
                       (update-keys #(keyword "agent" (name %))))]
     (db/put-agent! agent-id agent-map))))

(defn make-agents-and-bases!
  "Iterate through all agents in the systems-agents-and-bases map, ensuring the information
   in those maps is also in the DB."
  []
  (doseq [[k _v] system-agents-and-bases]
    (ensure-system-agent-basics k)))

(defn init-system-agents!
  []
  (make-agents-and-bases!)
  :ok)

(defstate system-agents
  :start init-system-agents!)
