(ns scheduling-tbd.agent-db
  "These agent-related utilities rely on system and project DBs, unlike llm.clj."
  (:require
   [clojure.edn          :as edn]
   [clojure.instant      :as inst]
   [clojure.java.io      :as io]
   [clojure.pprint       :refer [pprint]]
   [clojure.set          :as set]
   [clojure.spec.alpha   :as s]
   [datahike.api         :as d]
   [datahike.pull-api    :as dp]
   [mount.core           :as mount :refer [defstate]]
   [scheduling-tbd.llm   :as llm]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm default-llm-provider]]
   [scheduling-tbd.util  :as util]
   [taoensso.telemere    :as tel :refer [log!]])
  (:import java.time.Instant))

(def ^:diag diag (atom nil))

(s/def ::base-type keyword?)
(s/def ::instruction-path string?)
(s/def ::response-format-path string?) ;; ToDo: Valid JSON schema.
(s/def ::llm-provider #{:openai :azure})
(s/def ::shared-assistant? boolean?)
(s/def ::vector-store-paths (s/coll-of string?))
(s/def ::tools (s/and vector? (s/coll-of ::tool)))
(s/def ::tool (s/keys :req-un [::type]))
(s/def ::agent-info (s/keys :req-un [::base-type ::instruction-path]
                            :opt-un [::response-format-path ::llm-provider ::model-class ::shared-assistant?
                                     ::tools ::tool-resources ::vector-store-paths]))

(def agent-infos
  (-> (io/resource "agents/agent-infos.edn")
      slurp
      edn/read-string))

(when-not (every? #(s/valid? ::agent-info %) agent-infos)
  (log! :error "Invalid agent infos.")
  (throw (ex-info "Invalid agent infos." {})))

(defn agent-db2proj
  "Return a map of agent info translated to project attributes (aid, tid, role, expertise, surrogate?)."
  [agent]
  (reduce-kv (fn [m k v]
               (cond (= k :agent/assistant-id)         (assoc m :aid v)
                     (= k :agent/thread-id)            (assoc m :tid v)
                     (= k :agent/base-type)            (assoc m :role v)
                     (= k :agent/subject-of-expertise) (assoc m :expertise v)
                     (= k :agent/surrogate?)           (assoc m :surrogate? true)
                     :else m))
             {}
             agent))

(defn agent-eid
  "Return the entity-id of the agent of given base-type in the db at conn-atm (a connection to the system db, or a project db)."
  [base-type llm-provider conn-atm]
  ;;{:post [(-> % :aid string?) (-> % :tid string?)]}
  (d/q '[:find ?e .
         :in $ ?base-type ?llm-provider
         :where
         [?e :agent/base-type ?base-type]
         [?e :agent/llm-provider ?llm-provider]]
       @conn-atm base-type llm-provider))


;;; ToDo: In ensure-agent, use agent-info to determine what needs to checked as up-to-date and where to collect data.
;;;       Likewise, have add-agent use this information to decide where to put things.
;;;       I think the project needs both the assistant-id and the thread-id, otherwise the assistant might be destroyed and we won't know that the thread is useless.

;;; (adb/add-agent! (some #(when (= :response-analysis-agent (:base-type %)) %) adb/agent-infos))
(defn add-agent!
  "Create an agent (assistant + thread) and store it in one or more databases.
   There are three arrangements by which agents are stored:
     1) The agent and a single thread is shared by all projects
           -- It is entirely stored in the system db.
     2) It is the exclusive property of a project, such as the surrogate expert:
           -- It is entirely stored in the project. These have a pid, but no shared-assistant?
     3) It is a shared-assistant? with a thread private to a project, such as an interview agent:
           -- It has everything but the tid in the system db. It has a copy of everything in the project-db.

   Returns the object in DB attributes, combined, if necessary."
  [{:keys [base-type llm-provider pid model-class shared-assistant? surrogate? expertise]
    :or {llm-provider @default-llm-provider
         model-class :gpt}}]
  (let [user (-> (System/getenv) (get "USER"))
        {:keys [instruction-path response-format-path vector-store-paths tools]} (some #(when (= base-type (:base-type %)) %) agent-infos)
        a-name (-> base-type name (str "-" (name llm-provider)) keyword)
        assist (llm/make-assistant (cond-> {}
                                     true                  (assoc :name a-name)
                                     true                  (assoc :instructions (-> instruction-path io/resource slurp))
                                     true                  (assoc :metadata {:usage :stbd-agent :user user :base-type base-type})
                                     tools                 (assoc :tools tools)
                                     response-format-path  (assoc :response-format (-> response-format-path io/resource slurp edn/read-string))
                                     vector-store-paths    (doall (for [v vector-store-paths] (io/resource v)))))
        aid    (:id assist)
        thread (when-not shared-assistant?
                 (llm/make-thread {:assistant-id aid
                                   :llm-provider llm-provider
                                   :metadata {:usage :stbd-agent :user user :base-type base-type}}))
        tid     (:id thread)
        eid-sys (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @(connect-atm :system))
        eid-pid (when pid (d/q '[:find ?eid . :where [?eid :project/agents]]       @(connect-atm pid)))
        a-map (cond-> {:agent/id a-name
                       :agent/base-type base-type
                       :agent/llm-provider llm-provider
                       :agent/model-class model-class
                       :agent/timestamp (util/now)
                       :agent/assistant-id aid}
                tid         (assoc :agent/thread-id tid)
                surrogate?  (assoc :agent/surrogate? true)
                expertise   (assoc :agent/subject-of-expertise expertise))]
    (reset! diag a-map)
    (cond shared-assistant? (do (d/transact (connect-atm :system)
                                            {:tx-data [{:db/id eid-sys :system/agents (dissoc a-map :agent/thread)}]})
                                (d/transact (connect-atm pid)
                                            {:tx-data [{:db/id eid-pid :project/agents a-map}]}))
          pid                (d/transact (connect-atm pid)
                                         {:tx-data [{:db/id eid-pid :project/agents a-map}]})
          :else              (d/transact (connect-atm :system)
                                            {:tx-data [{:db/id eid-sys :system/agents a-map}]}))
    a-map))

(defn ensure-agent!
  "Return agent info in db attributes if the agent exists, otherwise create the agent, store it and and  return the info.
   This is used because currently openai assistants and threads are deleted after 30 days."
  [& {:keys [base-type llm-provider pid force-new?] :as args}]
  (let [conn-atm (if pid (connect-atm pid) (connect-atm :system))]
    (if-let [eid (agent-eid base-type llm-provider conn-atm)]
      (let [{:agent/keys [assistant-id thread-id] :as a-map} (dp/pull @conn-atm '[*] eid)
            exists? (llm/agent-exists? assistant-id thread-id args)]
        (cond force-new?                               (add-agent! args)
              exists?                                  a-map
              :else                                    (add-agent! args))))))

(defn newest-file-modification-date
  "Return the modification date (Instant object) of the most recently modified file used to define the agent."
  [base-type]
  (let [{:keys [instruction-path response-format-path vector-store-paths]} (some #(when (= base-type (:base-type %)) %) agent-infos)
        files (cond-> [instruction-path]
                response-format-path (conj response-format-path)
                vector-store-paths (into vector-store-paths))
        newest (apply max (mapv #(->> % io/resource io/file .lastModified) files))]
    (java.time.Instant/ofEpochSecond (/ newest 1000))))

(defn agent-timestamp
  "Return the timestamp of the agent of the given base-type and llm-provider."
  [base-type llm-provider pid]
  (let [conn-atm (if pid (connect-atm pid) (connect-atm :system))]
    (d/q '[:find ?time .
           :in $ ?btype ?llm
           :where
           [?ent :agent/base-type ?btype]
           [?ent :agent/llm-provider ?llm]
           [?ent :agent/timestamp ?time]]
         @conn-atm base-type llm-provider)))

;;; OpenAI may delete them after 30 days. https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
(defn agent-status
  "Compare the most-recently modified file used to create the agent to the agent timestamp.
   Return a map with
     {:missing-here?         -- a boolean indicating that no agent matches the parameters in the relevant db.
      :missing-at-provider?  -- a boolean indicating that the llm-provider no longer maintains an asssistant with the :agent/assistant-id in the DB.
      :outdated?  -- a boolean indicating that the :agent/timestamp is older than the most recently modified files used to create it,
      :files-date -- an Instant, the modification timepoint (Instant) of the most recently modified file used to create the agent, and
      :agent-date -- if not missing?,  an Instant, the timepoint (Instant) when the agent was created.}

   :agent-date is not provided if the agent is not present in the any database."
  ([base-type llm-provider] (agent-status base-type llm-provider nil))
  ([base-type llm-provider pid]
   (let [files-modify-date (newest-file-modification-date base-type)
         conn-atm (if pid (connect-atm pid) (connect-atm :system))
         eid (agent-eid base-type llm-provider conn-atm)
         {:agent/keys [timestamp assistant-id]} (when eid (dp/pull @conn-atm '[*] eid))
         provider-has? (llm/get-assistant assistant-id)]
     (cond-> {:files-date files-modify-date}
       timestamp                                          (assoc :agent-date timestamp)
       (not eid)                                          (assoc :missing-here? true)
       (and eid (not provider-has?))                      (assoc :missing-at-provider true)
       #_#_(and timestamp
            (== 1 (compare files-modify-date timestamp))) (assoc :outdated? true)))))



(defn update-assistants!
  "List what assistants the LLM-provider's currently maintains that have metadata indicating that
   they are part of our project (where they are called agents). Store this in the sytems DB."
  []
  (let [conn-atm (connect-atm :system)
        eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn-atm)
        old-ones (d/q '[:find [?s ...] :where [_ :system/openai-assistants ?s]] @conn-atm)
        new-ones (llm/list-assistants)]
    (d/transact conn-atm {:tx-data (vec (for [o old-ones] [:db/retract eid :system/openai-assistants o]))})
    (d/transact conn-atm {:tx-data (vec (for [n new-ones] [:db/add     eid :system/openai-assistants n]))})))

;;; You could look at the file date on instructions and use that to :force ensure-agent.


;;; Put the log message in agents-log.txt too!
(.lastModified (io/file "/home/msid/pdenno/Documents/git/schedulingTBD/data/agents/interviewers/process.txt"))
;;; Maybe these belong in resources!

(-> "agents/interviewers/process.txt" io/resource io/file .lastModified)

(def update-outdated? (atom true))

(defn init-agents!
  "Warn or update system and project agents, depending on the value of the update-outdated? atom."
  []
  ;;(doseq [
  )

(defstate agents
  :start (init-agents!))
