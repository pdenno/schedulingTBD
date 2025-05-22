(ns scheduling-tbd.surrogate
  "Functions and operators implementing surrogate users"
  (:require
   [clojure.core.unify       :as uni]
   [clojure.edn              :as edn]
   [clojure.string           :as str]
   [datahike.api             :as d]
   [datahike.pull-api        :as dp]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.agent-db  :as adb :refer [agent-log]]
   [scheduling-tbd.db        :as db]
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
  (str "Typically you answer in sentences. However, the interviewers may ask you provide a table or add information to a table that the interviewer provides.\n"
       "In these cases, respond with an HTML table wrapped in #+begin_src HTML ... #+end_src\n"
       "All the tables you generate / complete should have the header as the first table row.\n"
       "For example if you were asked to generate / complete a table about process durations for process steps you might respond with:\n"
       "\"Here are my estimates of the step durations. Note that I added --Yet another step-- which I think should be included in this context:\"\n\n"
       "#+begin_src HTML\n"
       "<table>\n"
       "  <tr><th>Process Step</th>                <th>Duration</th></tr>\n"
       "  <tr><td>--Some step--</td>               <td>2 hours</td></tr>\n"
       "  <tr><td>--Some other step--</td>         <td>3 days</td></tr>\n"
       "  <tr><td>--Yet anoter step--</td>         <td>1 days</td></tr>\n"
       "</table>\n"
       "#+end_src"))

(defn system-instruction
  "This is the instruction that configures the role of the OpenAI assistant for a surrogate domain expert."
  [role]
  (str
   (format "You manage a company that makes %s.\n" role)
   "You are an expert in production and manage your company's supply chains.\n"
   "You help me by answering an interviewer's questions that will allow us to collaborate in building a scheduling system for your company.\n"
   "Your answers typically are short, just a few sentences each.\n\n"
   how-to-handle-tables))

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
    (ws/send-to-client {:dispatch-key :interviewer-busy? :value true :client-id client-id})
    (try ;; Now do the warm-up question.
      (let [ctx (inv/ctx-surrogate {:pid pid
                                    :cid :process
                                    :question (ru/get-warm-up-q :process)
                                    :client-id client-id
                                    :force-new? true}) ; This is about the agent; make a new project agent.
            conversation (inv/get-an-answer ctx) ; get-an-answer also used by q-and-a, but here we have the q.
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
        (adb/ensure-agent! (-> (get @adb/agent-infos :orchestrator-agent) (assoc :pid pid)))
        ;; This will cause a resume-conversation, which will start with a conversation-history, so the interviewer should see the warm-up question.
        (ws/send-to-client {:dispatch-key :load-proj :client-id client-id  :promise? false
                            :new-proj-map {:project/name pname :project/id pid}}))
      (catch Exception e
        (log! :error (str "Error starting surrogate:\n" e)))
      (finally ; ToDo: Not sure this is needed.
        (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id})))))


#_(sur/start-surrogate+
   {:client-id (ws/recent-client!)
    :map-str (str {:pid :sur-music-school
                   :pname "Music School"
                   :sur-instructions (str "You run a music school. Until today you've been scheduling student lessons by fiddling with a spreadsheet.\n"
                                          "But that is error-prone and time consuming. The problem is that you need to bring each student together with their instructor\n"
                                          "in a room having the right equipment. (Only some practice rooms have pianos or drums.) Of course, instructors don't want\n"
                                          "to come in to teach just one student. Most instructors would like to do a block of a few hours of lessons on days they come in.\n"
                                          "Lessons are either 30 min, 45, or an hour.")
                   :warm-up-response "Thanks! Yes, we run a music school and we'd like a system that helps us schedule our practice rooms with students and instructors."
                   :expertise "running a music school"})})

(defn start-surrogate+
  "This is like start-surrogate! but the call to it is provided with the project name, challenge descriptiion,
   and a map of all the warm-up stuff. This is especially useful for starting projects for test where the
   the default system-instruction (currently about production) doesn't fit the needs of the interviewees.
   For example, you run a music school and you want to schedule music lessons with instructors, rooms, and students."
  [{:keys [map-str client-id]}]
  (let [{:keys [pid pname sur-instructions warm-up-response expertise]} (edn/read-string map-str)]
    (db/create-proj-db! {:project/id pid :project/name pname} {} {:force-this-name? true})
    (db/add-claim! pid {:string (str `(~'surrogate ~pid)) :cid :process})
    (let [q-id (db/add-msg {:pid pid :cid :process :from :system :tags [:query :warm-up]
                         :text (str "We understand that you have a special request to us:\n"
                                    "You'd like to create a scheduling system, but you aren't a manufacturer.\n"
                                    "OK, I think we can still help. What's the scheduling problem you are trying to solve.")})
          a-id (db/add-msg {:pid pid :cid :process :from :surrogate :tags [:response :warm-up] :text warm-up-response})]
      (db/update-msg pid :process a-id  {:message/answers-question q-id})
      (agent-log "============= Start surrogate " pid " :process  =========================")
      (adb/put-agent-info! pid {:base-type pid :agent-type :project :instruction-string sur-instructions :surrogate? true :expertise expertise})
      (adb/ensure-agent! (-> (get @adb/agent-infos :orchestrator-agent) (assoc :pid pid)))
      (ws/send-to-client {:dispatch-key :load-proj :client-id client-id  :promise? false
                          :new-proj-map {:project/name pname :project/id pid}}))))

(defn surrogate-follow-up
  "Handler for 'SUR?:' manual follow-up questions to a surrogate."
  [{:keys [client-id pid question] :as obj}]
  (log! :info (str "SUR follow-up:" obj))
  (let [chat-args {:client-id client-id :dispatch-key :sur-says}
        cid (db/get-current-cid pid)]
    (try (when-let [answer (adb/query-agent pid question {:asking-role :surrogate-follow-up})]
           (log! :info (str "SUR's answer:" answer))
           (when (string? answer)
             (ws/send-to-client (assoc chat-args :text answer))
             (db/add-msg {:pid pid :cid cid :from :system :text question})
             (db/add-msg {:pid pid :cid cid :from :surrogate :text answer})))
         (catch Exception e
           (log! :error (str "Failure in surrogate-follow-up:" (-> e Throwable->map :via first :message)))
           (ws/send-to-client (assoc chat-args :text "We had a problem answering this questions."))))))

;;; ----------------------- Starting and stopping -----------------------------------------

;;; ToDo: This probably belongs in agent_db.clj, as part of adb/init-agent-infos.
;;;       Also, maybe the atom should be replaced with a db.
(defn add-surrogate-agent-infos
  "Add an agent-info object to adb/agent-infos for every project's surrogate.
   Does not check that it is viable (does not check that the assistant and thread are still
   maintained by llm provider). Thus, it is lazy, with updating done by adb/ensure-agent! as needed."
  []
  (log! :info "Adding surrogates from all project to adb/agent-infos.")
  (doseq [pid (db/list-projects)]
    (let [conn @(connect-atm pid)]
      (when-let [eid (d/q '[:find ?eid . :where [?eid :agent/surrogate? true]] conn)]
        (let [{:agent/keys [expertise]} (dp/pull conn '[*] eid)
              info {:base-type pid
                    :agent-type :project
                    :model-class :gpt
                    :surrogate? true
                    :expertise expertise}]
          (adb/put-agent-info! pid info))))))

(defn init-surrogates! []
  (mount/start (find-var 'scheduling-tbd.db/sys&proj-database-cfgs)) ; ToDo: Investigate. I expected this to have happened already.
  (add-surrogate-agent-infos)
  (ws/register-ws-dispatch :start-surrogate start-surrogate!)         ; User types 'SUR:'
  (ws/register-ws-dispatch :start-surrogate+ start-surrogate+)        ; User types 'SUR+:'
  (ws/register-ws-dispatch :surrogate-follow-up surrogate-follow-up)  ; User types 'SUR?:'
  :surrogate-ws-fns-registered)

(defn stop-surrogates! []
  :done)

(defstate surrogates
  :start (init-surrogates!)
  :stop (stop-surrogates!))
