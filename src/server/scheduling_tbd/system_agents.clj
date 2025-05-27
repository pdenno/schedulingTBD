(ns scheduling-tbd.system-agents
  (:require
   [clojure.java.io         :as io]
   [datahike.core           :as db]
   [mount.core              :as mount :refer [defstate]]
   [scheduling-tbd.agent-db :as adb]
   [scheduling-tbd.sutil    :as sutil]))

(def system-agents-and-bases
  { ;; ---------- project agents base-types -------------------------
   :orchestrator-agent,
   {:base-type :orchestrator-agent,
    :tools [{:type "file_search"}],
    :model-class :gpt,
    :instruction-path "agents/orchestrator.txt",
    ;; The vector store includes all the EADS instructions for interviewers.
    :vector-store-paths (->> (.list (io/file "resources/agents/iviewrs/EADS/")) (mapv #(str "agents/iviewrs/EADS/" %)))
    :agent-type :project}

   :process-interview-agent,
   {:base-type :process-interview-agent,
    :tools [{:type "file_search"}],
    :model-class :gpt,
    :instruction-path "agents/iviewrs/process-iviewr-instructions.txt",
    :agent-type :shared-assistant}

   :data-interview-agent
   {:base-type  :data-interview-agent
    :tools [{:type "file_search"}],
    :model-class :gpt,
    :instruction-path "agents/iviewrs/data-iviewr-instructions.txt",
    :vector-store-paths ["resources/agents/iviewrs/papers/object-role-modeling--an-overview.pdf"],
    :agent-type :shared-assistant}

   :resources-interview-agent,
   {:base-type :resources-interview-agent,
    :tools [{:type "file_search"}],
    :model-class :gpt,
    :instruction-path "agents/iviewrs/resources-iviewr-instructions.txt",
    :agent-type :shared-assistant}

   :optimality-interview-agent,
   {:base-type :optimality-interview-agent,
    :tools [{:type "file_search"}],
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
    :instruction-path "agents/text-function-agent.txt"}})

(def sys-agent? (-> system-agents-and-bases keys set))

          #:agent{:base-type :response-analysis-agent,
                  :agent-type :system,
                  :thread-id "thread_wDcvDiGb8ZbRrVANUuTLLCmk",
                  :model-class :gpt,
                  :llm-provider :openai,
                  :assistant-id "asst_VaeePwn9LHgSwQsh1gDgoNB4",
                  :id :response-analysis-agent-openai,
                  :timestamp #inst "2024-12-13T21:09:08.701-00:00"}


(defn ensure-system-agent-basics
  "Make sure the agents in the system db have the above basics. This is necessary for adb/ensure-agent! to work."
  [agent-id]
  (let [eid (d/q '[:find ?eid .
                   :where [?eid :system/agents]]
                 @(sutil/connect-atm :system))])) ;<==========================================================================



(defn make-agent-db-info
  "Make one agent using keyword agent-id and opts."
  ([agent-id] (make-agent agent-id nil))
  ([agent-id force-new?]
   (assert (sys-agent? agent-id))
   (adb/ensure-agent! agent-id
                      (assoc (get system-agents-and-bases agent-id) :force-new? force-new?))))

;;; ------------------------------- starting and stopping ---------------------------------
(defn make-agents-and-bases!
  "You can pick up :force-new? from the agent map above, or use force it on everything."
  []
  (doseq [[k v] system-agents-and-bases]
    (make-agent-db-info k v)))

(defn init-system-agents!
  []
  (make-agents-and-bases!)
  :ok)

(defstate system-agents
  :start init-system-agents!)
