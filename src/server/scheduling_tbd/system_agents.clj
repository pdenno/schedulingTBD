(ns scheduling-tbd.system-agents
  (:require
   [clojure.java.io         :as io]
   [mount.core              :as mount :refer [defstate]]
   [scheduling-tbd.db       :as db]
   [scheduling-tbd.sutil    :as sutil]))

(def system-agents-and-bases
  { ;; ---------- project agents base-types (ToDo: This will eventually be :agent/agent-type :base-type.)  -------------------------
   :orchestrator-agent,
   {:base-type :orchestrator-agent,
    :tools "[{:type \"file_search\"}]"
    :model-class :gpt,
    :instruction-path "agents/orchestrator.txt",
    ;; The vector store includes all the EADS instructions for interviewers.
    :vector-store-paths (->> (.list (io/file "resources/agents/iviewrs/EADS/")) (mapv #(str "agents/iviewrs/EADS/" %)))
    :agent-type :shared-assistant}

   :process-interview-agent,
   {:base-type :process-interview-agent,
    :model-class :gpt,
    :instruction-path "agents/iviewrs/process-iviewr-instructions.txt",
    :agent-type :shared-assistant}

   :data-interview-agent
   {:base-type  :data-interview-agent
    :model-class :gpt,
    :instruction-path "agents/iviewrs/data-iviewr-instructions.txt",
    :vector-store-paths ["resources/agents/iviewrs/papers/object-role-modeling--an-overview.pdf"],
    :agent-type :shared-assistant}

   :resources-interview-agent,
   {:base-type :resources-interview-agent,
    :model-class :gpt,
    :instruction-path "agents/iviewrs/resources-iviewr-instructions.txt",
    :agent-type :shared-assistant}

   :optimality-interview-agent,
   {:base-type :optimality-interview-agent,
    :model-class :gpt,
    :instruction-path "agents/iviewrs/optimality-iviewr-instructions.txt",
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
     (if (db/agent-exists? agent-id llm-provider)
       (db/update-agent! agent-id agent-map)
       (db/put-agent! agent-id agent-map)))))

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
