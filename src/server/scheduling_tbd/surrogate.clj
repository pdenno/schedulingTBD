(ns scheduling-tbd.surrogate
  "Functions and operators implementing surrogate users"
  (:require
   [clojure.string           :as str]
   [datahike.api             :as d]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere        :refer [log!]]))

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

#_(defn similar-surrogate?
  "Return a :project/id of a project named similar to the argument if one exists.
   For example :craft-beer-surrogate --> :craft-beer-surrogate-42."
  [pid]
  (let [pid-str (name pid)
        projects (->> (db/list-projects) (mapv name))
        pattern (re-pattern (format "%s(-\\d+)?" pid-str))]
    (some #(when (re-matches pattern %) (keyword %)) projects)))

(defn ensure-project-and-surrogate
  "If a surrogate with given expertise exists and force-new?=false, return the DB map of it.
   Otherwise create and store a project with the given expertise and return the DB map of its surrogate."
  [pid force-new?]
  (if force-new?
    (let [conn-atm (connect-atm pid)
          eid (db/project-exists? pid)
          user (-> (System/getenv) (get "USER"))
          proj-info (resolve-db-id {:db/id eid} conn-atm :keep-set #{:project/name})
          [_ _ expertise] (re-matches #"(SUR )?(.*)" (:project/name proj-info)) ; ToDo: Ugh!
          expertise (str/lower-case expertise)
          instructions (system-instruction expertise)
          assist (llm/make-assistant :name (str expertise " surrogate")
                                     :instructions instructions
                                     :metadata {:usage :stbd-surrogate :user user})
          aid    (:id assist)
          thread (llm/make-thread {:assistant-id aid :metadata {:usage :stbd-surrogate :user user}})]
      (d/transact conn-atm {:tx-data [{:db/id (db/project-exists? pid)        ;; ToDo: Right now we have projects that are viewed as surrogates.
                                       :project/surrogate {:surrogate/id pid  ;;       What we need is a conversation with a surrogate. See also analyze-response for the :warm-up question-type.
                                                           :surrogate/subject-of-expertise expertise
                                                           :surrogate/system-instruction instructions
                                                           :surrogate/assistant-id aid
                                                           :surrogate/thread-id (:id thread)}}]})
      (db/add-claim! pid {:string (str `(~'surrogate ~pid)) :cid :process})
      (db/get-surrogate-agent-info pid))
    (db/get-surrogate-agent-info pid)))

;;; (sur/start-surrogate {:product "fountain pens" :client-id (ws/recent-client!)})
(defn start-surrogate!
  "Create or recover a surrogate and ask client to :load-proj.
   :load-proj will cause the client to get-conversation (http and chat). The chat part will :resume-conversation (call back to server to restart planner).
    product - a string describing what product type the surrogate is going to talk about (e.g. 'plate glass').
               Any of the :segment/name from the 'How it's Made' DB would work here.
    force? - This is about naming of the DB. Unrelated to this is the fact that any old DB having the pid calculated here is deleted.
  "
  [{:keys [product client-id]} & {:keys [force?] :or {force? true}}] ; ToDo: handle force?=false, See similar-surrogate?
  (log! :info (str "======= Start a surrogate: product = " product "======================="))
  (let [pid (as-> product ?s (str/trim ?s) (str/lower-case ?s) (str/replace ?s #"\s+" "-") (str "sur-" ?s) (keyword ?s))
        pname (as->  product ?s (str/trim ?s) (str/split ?s #"\s+") (map str/capitalize ?s) (interpose " " ?s) (conj ?s "SUR ") (apply str ?s))
        pid (db/create-proj-db! {:project/id pid :project/name pname} {} {:force-this-name? force?})]
      (try
        (ensure-project-and-surrogate pid force?)
        (ws/send-to-chat {:dispatch-key :load-proj :client-id client-id  :promise? false
                          :new-proj-map {:project/name pname :project/id pid}})
        (catch Exception e
          (log! :warn "Failed to start surrogate.")
          (log! :error (str "Error starting surrogate:" e))))))

(defn surrogate-follow-up
  "Handler for 'SUR?:' manual follow-up questions to a surrogate."
  [{:keys [client-id pid question] :as obj}]
  (log! :info (str "SUR follow-up:" obj))
  (let [chat-args {:client-id client-id :dispatch-key :sur-says}
        {:surrogate/keys [assistant-id thread-id]} (db/get-surrogate-agent-info pid)
        cid (db/get-current-cid pid)]
    (when (and assistant-id thread-id)
      (try (when-let [answer (llm/query-on-thread :aid assistant-id :tid thread-id :query-text question)]
             (log! :info (str "SUR's answer:" answer))
             (when (string? answer)
               (ws/send-to-chat (assoc chat-args :text answer))
               (db/add-msg {:pid pid :cid cid :from :system :text question})
               (db/add-msg {:pid pid :cid cid :from :surrogate :text answer})))
           (catch Exception e
             (log! :error (str "Failure in surrogate-follow-up:" (-> e Throwable->map :via first :message)))
             (ws/send-to-chat (assoc chat-args :text "We had a problem answering this questions.")))))))

(defn ^:diag get-surrogate-messages-provider
  [pid]
  (when-let [tid (d/q '[:find ?tid .
                        :where
                        [_ :project/surrogate ?sur]
                        [?sur  :surrogate/thread-id ?tid]]
                      @(sutil/connect-atm pid))]
    (llm/list-thread-messages tid 100)))

(defn ^:diag list-surrogates
  "Return a set of the surrogate (its ID string) of each project that has one."
  []
  (->> (db/list-projects)
       (map #(d/q '[:find ?aid .
                    :where
                    [_ :project/surrogate ?e]
                    [?e :surrogate/assistant-id ?aid]]
                  @(connect-atm %)))
       (filterv identity)
       set))

(defn ^:diag delete-surrogates!
  "Delete surrogates that aren't used in projects."
  []
  (let [in-a-project? (list-surrogates)]
    (llm/delete-surrogates!
     {:selection-fn #(and (= "surrogate" (-> % :metadata (get :usage)))
                          (not (in-a-project? (:id %))))})))

;;; ----------------------- Starting and stopping -----------------------------------------
(defn init-surrogates! []
  (ws/register-ws-dispatch :start-surrogate start-surrogate!)         ; User types SUR:
  (ws/register-ws-dispatch :surrogate-follow-up surrogate-follow-up) ; User types SUR?:
  :surrogate-ws-fns-registered)

(defn stop-surrogates! []
  :done)

(defstate surrogates
  :start (init-surrogates!)
  :stop (stop-surrogates!))
