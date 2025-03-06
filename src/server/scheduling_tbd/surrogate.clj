(ns scheduling-tbd.surrogate
  "Functions and operators implementing surrogate users"
  (:require
   [clojure.core.unify            :as uni]
   [clojure.string           :as str]
   [datahike.api             :as d]
   [datahike.pull-api        :as dp]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.agent-db  :as adb :refer [agent-log]]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.interviewing.domain.process-analysis :as pan]
   [scheduling-tbd.interviewing.interviewers :as inv]
   [scheduling-tbd.interviewing.response-utils :as ru]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere        :refer [log!]]))

(def ^:diag diag (atom nil))

;;; ToDo: Fix this: "You manage a company that makes %s." The problem that some are services and we want to start well.
;;; I think we could use an independent LLM call to guess whether "paving" or "general contracting" are services.
;;; With that function we can choose between "makes" and "provides the services of"

;;; ToDo: Is this table stuff really what I want? I could use a strict response format, but
(def how-to-handle-tables
  (str "Typically you answer in sentences. However, the interviewers may ask you to complete an HTML table.\n"
       "In that case, they will provide the HTML table partially filled in (based on your earlier answers) and will ask you to complete it, best you can.\n"
       "For example, they might say:\n\n"
       "'Here is a list of the processes you mentioned. Could you please review and adjust the table below, filling in the approximate duration of each step in the 'Duration' column:'\n"
       "<table>\n"
       "  <tr><th>Process Step</th>                <th>Duration</th></tr>\n"
       "  <tr><td>--Some step--</td>               <td></td></tr>\n"
       "  <tr><td>--Some other step--</td>         <td></td></tr>\n"
       "</table>\n\n"
       "Your response in this case can be text plus the provided table modified to answer the question.\n"
       "However, it is important that you wrap the table part of your answer in #+begin_src HTML ... #+end_src\n"
       "So, if for example --Some step-- takes 2 hours, and --Some other step-- takes 3 days, and you think there is some other step they should have asked about, you could respond with:\n\n"
       "Here are my estimates of the step durations. Note that I added --Yet another step-- which I think should be included in this context:\n\n"
       "#+begin_src HTML\n"
       "<table>\n"
       "  <tr><th>Process Step</th>                <th>Duration</th></tr>\n"
       "  <tr><td>--Some step--</td>               <td>2 hours</td></tr>\n"
       "  <tr><td>--Some other step--</td>         <td>3 days</td></tr>\n"
       "  <tr><td>--Yet anoter step--</td>         <td>1 days</td></tr>\n"
       "</table>\n"
       "#+end_src"))

(def how-to-provide-tables
 (str "Additionally, if you are asked to upload a table, please do you best to provide a reasonable example for what that table could look like, in HTML."
      "Here is an example of something that you could be asked, and how you would be expected to answer.
       They: "
      "Could you upload the orders spreadsheet (or a redacted version of it) for discussion? Please note the risks of uploading sensitive data, which you can read about here. Let us know if you'd like guidance on redacting sensitive information before uploading."
      "Your response: "
      "Of course! Here is our orders spreadsheet:
       <table>
        <thead>
            <tr>
                <th>Order ID</th>
                <th>Customer Name</th>
                <th>Order Date</th>
                <th>Product</th>
                <th>Quantity</th>
                <th>Delivery Date</th>
                <th>Status</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>101</td>
                <td>ABC Industries</td>
                <td>2023-10-20</td>
                <td>Carpet Type A</td>
                <td>200</td>
                <td>2023-11-01</td>
                <td>In Production</td>
            </tr>
            <tr>
                <td>102</td>
                <td>XYZ Corporation</td>
                <td>2023-10-22</td>
                <td>Carpet Type B</td>
                <td>150</td>
                <td>2023-11-03</td>
                <td>Pending</td>
            </tr>
            <tr>
                <td>103</td>
                <td>Global Solutions</td>
                <td>2023-10-23</td>
                <td>Carpet Type C</td>
                <td>300</td>
                <td>2023-11-05</td>
                <td>Shipped</td>
            </tr>
            <tr>
                <td>104</td>
                <td>Acme Co.</td>
                <td>2023-10-24</td>
                <td>Carpet Type A</td>
                <td>250</td>
                <td>2023-11-07</td>
                <td>In Production</td>
            </tr>
            <tr>
                <td>105</td>
                <td>Widget Works</td>
                <td>2023-10-25</td>
                <td>Carpet Type B</td>
                <td>100</td>
                <td>2023-11-09</td>
                <td>Pending</td>
            </tr>
            <tr>
                <td>106</td>
                <td>Pinnacle Group</td>
                <td>2023-10-26</td>
                <td>Carpet Type A</td>
                <td>180</td>
                <td>2023-11-11</td>
                <td>In Production</td>
            </tr>
            <tr>
                <td>107</td>
                <td>Echo Dynamics</td>
                <td>2023-10-27</td>
                <td>Carpet Type C</td>
                <td>220</td>
                <td>2023-11-13</td>
                <td>Shipped</td>
            </tr>
            <tr>
                <td>108</td>
                <td>NextGen Solutions</td>
                <td>2023-10-28</td>
                <td>Carpet Type B</td>
                <td>300</td>
                <td>2023-11-15</td>
                <td>Pending</td>
            </tr>
            <tr>
                <td>109</td>
                <td>Future Enterprises</td>
                <td>2023-10-29</td>
                <td>Carpet Type A</td>
                <td>400</td>
                <td>2023-11-17</td>
                <td>In Production</td>
            </tr>
            <tr>
                <td>110</td>
                <td>Prime Ventures</td>
                <td>2023-10-30</td>
                <td>Carpet Type C</td>
                <td>350</td>
                <td>2023-11-19</td>
                <td>Shipped</td>
            </tr>
        </tbody>
    </table>"))

(defn system-instruction
  "This is the instruction that configures the role of the OpenAI assistant for a surrogate domain expert."
  [role]
  (str
   (format "You manage a company that makes %s.\n" role)
   "You are an expert in production and manage your company's supply chains.\n"
   "You help me by answering an interviewer's questions that will allow us to collaborate in building a scheduling system for your company.\n"
   "Your answers typically are short, just a few sentences each.\n\n"
   how-to-handle-tables how-to-provide-tables))

;;; (sur/start-surrogate! {:product "optical fiber" :client-id (ws/recent-client!)})
(defn start-surrogate!
   "Create a surrogate and ask client to :load-proj.
    :load-proj will cause the client to get-conversation (http and chat). The chat part will :resume-conversation (call back to server).
    product - a string describing what product type the surrogate is going to talk about (e.g. 'plate glass').
              Any of the :segment/name from the 'How it's Made' DB would work here.
    force? - This is about naming of the DB. Any old DB having this pid is deleted."
  [{:keys [product client-id]} & {:keys [force?] :or {force? true}}]
  ;(log! :info (str "======= Starting a surrogate: product = " product " ======================="))
  (let [pid (as-> product ?s (str/trim ?s) (str/lower-case ?s) (str/replace ?s #"\s+" "-") (str "sur-" ?s) (keyword ?s))
        pname (as->  product ?s (str/trim ?s) (str/split ?s #"\s+") (map str/capitalize ?s) (interpose " " ?s) (conj ?s "SUR ") (apply str ?s))
        [_ _ expertise] (re-matches #"(SUR )?(.*)" pname)
        expertise (str/lower-case expertise)
        pid (db/create-proj-db! {:project/id pid :project/name pname} {} {:force-this-name? force?})
        instructions (system-instruction expertise)]
    (agent-log "============= Start surrogate " pid " :process  =========================")
    (adb/put-agent-info! pid {:base-type pid :agent-type :project :instruction-string instructions :surrogate? true :expertise expertise})
    (db/add-claim! pid {:string (str `(~'surrogate ~pid)) :cid :process})
    (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
    (try ;; Now do the warm-up question.
      (let [ctx (inv/ctx-surrogate {:pid pid
                                    :cid :process
                                    :question pan/the-warm-up-type-question
                                    :client-id client-id
                                    :force-new? true}) ; This is about the agent; make a new project agent.
            conversation (inv/get-an-answer ctx)
            response (-> conversation last :text)
            warm-up-claims (ru/analyze-warm-up :process response)
            needed-claims (remove #('#{temp-project-id temp-project-name} (first %)) warm-up-claims)
            bindings {'?pid pid}]
        (doseq [{:keys [from text tags]} conversation]
          (db/add-msg {:pid pid :cid :process :from from :tags tags :text text}))
        (doseq [claim needed-claims]
          (db/add-claim! pid {:string (-> claim (uni/subst bindings) str)
                              :q-type :process/warm-up
                              :cid :process}))
        ;; This will cause a resume-conversation, which will start with a conversation-history, so the interviewer should see the warm-up question.
        (ws/send-to-chat {:dispatch-key :load-proj :client-id client-id  :promise? false
                          :new-proj-map {:project/name pname :project/id pid}}))
      (catch Exception e
        (log! :error (str "Error starting surrogate:\n" e)))
      (finally
        (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id})))))

(defn surrogate-follow-up
  "Handler for 'SUR?:' manual follow-up questions to a surrogate."
  [{:keys [client-id pid question] :as obj}]
  (log! :info (str "SUR follow-up:" obj))
  (let [chat-args {:client-id client-id :dispatch-key :sur-says}
        cid (db/get-current-cid pid)]
    (try (when-let [answer (adb/query-agent pid question {:asking-role :surrogate-follow-up})]
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
