(ns scheduling-tbd.surrogate
  "Functions and operators implementing surrogate users"
  (:require
   [clojure.string           :as str]
   [datahike.api             :as d]
   [datahike.pull-api        :as dp]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.agent-db  :as adb]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere        :refer [log!]]))

(def ^:diag diag (atom nil))

;;; ToDo: Fix this: "You manage a company that makes %s." The problem that some are services and we want to start well.
;;; I think we could use an independent LLM call to guess whether "paving" or "general contracting" are services.
;;; With that function we can choose between "makes" and "provides the services of"
(defn system-instruction
  "This is the instruction that configures the role of the OpenAI assistant."
  [role]
  (format "You manage a company that makes %s.
   You are an expert in production and manage your company's supply chains.
   You help me by answering questions that will allow us to collaborate in building a scheduling systems for your company.
   Your answers typically are short, just a few sentences each.
   If you don’t have information to answer my questions, you provide a plausible answer nonetheless." role))

;;; (sur/start-surrogate! {:product "optical fiber" :client-id (ws/recent-client!)})
(defn start-surrogate!
   "Create a surrogate and ask client to :load-proj.
   :load-proj will cause the client to get-conversation (http and chat). The chat part will :resume-conversation (call back to server).
    product - a string describing what product type the surrogate is going to talk about (e.g. 'plate glass').
               Any of the :segment/name from the 'How it's Made' DB would work here.
    force? - This is about naming of the DB. Any old DB having this pid is deleted."
  [{:keys [product client-id]} & {:keys [force?] :or {force? true}}]
  (log! :info (str "======= Starting a surrogate: product = " product " ======================="))
  (let [pid (as-> product ?s (str/trim ?s) (str/lower-case ?s) (str/replace ?s #"\s+" "-") (str "sur-" ?s) (keyword ?s))
        pname (as->  product ?s (str/trim ?s) (str/split ?s #"\s+") (map str/capitalize ?s) (interpose " " ?s) (conj ?s "SUR ") (apply str ?s))
        [_ _ expertise] (re-matches #"(SUR )?(.*)" pname)
        expertise (str/lower-case expertise)
        pid (db/create-proj-db! {:project/id pid :project/name pname} {} {:force-this-name? force?})
        instructions (system-instruction expertise)]
    (try
      (let [agent-info {:base-type pid :agent-type :project :instruction-string instructions :surrogate? true :expertise expertise}]
        (adb/put-agent-info! pid agent-info)
        (adb/ensure-agent! (-> agent-info (assoc :pid pid) (assoc :force-new? true))) ; force-new? means new assistant and thread.
        (db/add-claim! pid {:string (str `(~'surrogate ~pid)) :cid :process})
        (ws/send-to-chat {:dispatch-key :load-proj :client-id client-id  :promise? false
                          :new-proj-map {:project/name pname :project/id pid}}))
      (catch Exception e
        (log! :error (str "Error starting surrogate:\n" e))))))

(defn surrogate-follow-up
  "Handler for 'SUR?:' manual follow-up questions to a surrogate."
  [{:keys [client-id pid question] :as obj}]
  (log! :info (str "SUR follow-up:" obj))
  (let [chat-args {:client-id client-id :dispatch-key :sur-says}
        cid (db/get-current-cid pid)]
    (try (when-let [answer (adb/query-agent pid question)]
           (log! :info (str "SUR's answer:" answer))
           (when (string? answer)
             (ws/send-to-chat (assoc chat-args :text answer))
             (db/add-msg {:pid pid :cid cid :from :system :text question})
             (db/add-msg {:pid pid :cid cid :from :surrogate :text answer})))
         (catch Exception e
           (log! :error (str "Failure in surrogate-follow-up:" (-> e Throwable->map :via first :message)))
           (ws/send-to-chat (assoc chat-args :text "We had a problem answering this questions."))))))

;;; ----------------------- Starting and stopping -----------------------------------------
(defn add-surrogate-agent-infos
  "Add an agent-info object to adb/agent-infos for every project's surrogate.
   Does not check that it is viable (does not check that the assistant and thread are still
   maintained by llm provider). Thus, it is lazy, with updating done by adb/ensure-agent! as needed."
  []
  (log! :info "Adding surrogates from all project so adb/agent-infos.")
  (doseq [pid (db/list-projects)]
    (let [conn @(connect-atm pid)]
      (when-let [eid (d/q '[:find ?eid . :where [?eid :agent/surrogate? true]] conn)]
        (let [{:agent/keys [system-instruction expertise]} (dp/pull conn '[*] eid)
              info {:base-type pid
                    :agent-type :project
                    :model-class :gpt
                    :instruction-string (or system-instruction "++ elsewhere ++") ; ToDo: Investigate.
                    :surrogate? true
                    :expertise expertise}]
          (adb/put-agent-info! pid info))))))

(defn init-surrogates! []
  (mount/start (find-var 'scheduling-tbd.db/sys&proj-database-cfgs)) ; ToDo: Investigate. I expected this to have happened already.
  (add-surrogate-agent-infos)
  (ws/register-ws-dispatch :start-surrogate start-surrogate!)         ; User types 'SUR:'
  (ws/register-ws-dispatch :surrogate-follow-up surrogate-follow-up)  ; User types 'SUR?:'
  :surrogate-ws-fns-registered)

(defn stop-surrogates! []
  :done)

(defstate surrogates
  :start (init-surrogates!)
  :stop (stop-surrogates!))
