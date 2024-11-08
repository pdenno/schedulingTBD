(ns scheduling-tbd.interviewers
    "This provides functions to prune a planning domain and run an interview."
  (:require
   [clojure.datafy                :refer [datafy]]
   [clojure.spec.alpha            :as s]
   [datahike.api                  :as d]
   [jsonista.core                 :as json]
   [mount.core                    :as mount :refer [defstate]]
   [promesa.core                  :as p]
   [promesa.exec                  :as px]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.llm            :as llm]
   [scheduling-tbd.response-utils :as ru]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm output-struct2clj]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.timbre               :as log]))

(def diag (atom nil))

;;; (chat-pair-aux {:pid :sur-fountain-pens :surrogate? true :agent-query "Describe your most significant scheduling problem in a few sentences."} {})
(defn chat-pair-aux
  "Run one query/response pair of chat elements with a human or a surrogate.
   Returns promise which will resolve to the original obj argument except:
     1) :agent-query is adapted from the input argument value for the agent type (human or surrogate)
     2) :response is added. Typically its value is a string."
  [query {:keys [sur-aid sur-tid responder-role preprocess-fn tries] :as ctx
          :or {tries 1 preprocess-fn identity}}]
  ;(log/info "chat-pair-aux: query =" query)
  ;(log/info "chat-pair-aux: ctx =" ctx)
  (let [prom (if (= :human responder-role)
               (ws/send-to-chat (-> ctx                        ; This cannot timeout.
                                    (assoc :msg query)
                                    (assoc :promise? true)
                                    (assoc :dispatch-key :tbd-says)))
               (px/submit! (fn []
                             (try
                                 (llm/query-on-thread :aid sur-aid   ; This can timeout.
                                                      :tid sur-tid
                                                      :query-text query
                                                      :tries tries
                                                      :preprocess-fn preprocess-fn)
                               (catch Exception e {:error e})))))]
    (-> prom
       (p/catch (fn [e]
                  (reset! diag (datafy e))
                  (log/error "Failed in chat-pair-aux:" (datafy e))))
       p/await)))

(s/def ::chat-pair-context (s/and (s/keys :req-un [::pid ::responder-role ::client-id] :opt-un [::tags ::topic])
                                  #(or (and (= :surrogate (:responder-role %)) (contains? % :sur-aid) (contains? % :sur-tid))
                                       (= :human (:responder-role %)))))
(s/def ::pid keyword?)
(s/def ::responder-role keyword?)
(s/def ::client-id string?)
(s/def ::tags (s/coll-of keyword?))
(s/def ::topic #(#{:process :resource :data} %))

(defn chat-pair
  "Call to run the chat and put the query and response into the project's database.
   Updates the UI and returns response text."
  [query {:keys [pid responder-role client-id tags topic question-type]
          :or {tags [] topic :process} :as ctx}] ; ToDo: How do I implement tags? I suppose I need an agent to classify the question. Also no default for topic?
  (s/assert ::chat-pair-context ctx)
  (let [response-text (chat-pair-aux query ctx)
        q-type (keyword question-type)]
    (if (string? response-text)
      (if (llm/immoderate? response-text)
        (ws/send-to-chat (assoc ctx :msg "I won't respond to that."))
        (do (db/add-msg {:pid pid
                         :from :system
                         :text query
                         :question-type q-type
                         :tags (conj tags :query)})
            (db/add-msg {:pid pid
                         :from responder-role
                         :text response-text
                         :question-type q-type
                         :tags (conj tags :response)})
            (ws/refresh-client client-id pid topic)
            response-text))
      (throw (ex-info "Unknown response from operator" {:response response-text})))))

;;; Note that in OpenAI every thread has its own separate context window, which stores the conversation history specific to
;;; that thread, preventing cross-contamination with other threads.
(defn interview-agent
  "Return a map with project agent info (aid, tid). The agent is shared by all projects, but the threads are project-specific.
   If a thread has not yet been created for this project, this creates it and stores that in the DB before returning the info."
  [conv-id pid]
  (assert (#{:process :resource :data} conv-id))
  (let [agent-type (-> conv-id name (str "-interview-agent") keyword)
        interview-agent-atm (atom (or (db/get-agent :base-type agent-type :pid pid :db-attrs? true)
                                      (db/get-agent :base-type agent-type :db-attrs? true)))]
    (when-not (:agent/thread-id @interview-agent-atm)
      (let [user (-> (System/getenv) (get "USER"))
            aid (:agent/assistant-id @interview-agent-atm)
            tid (:id (llm/make-thread {:assistant-id aid :llm-provider :openai :metadata {:usage :stbd-project-agent :user user}}))]
        (swap! interview-agent-atm #(assoc % :agent/thread-id tid))
        (let [eid (d/q '[:find ?eid . :where [?eid :project/id _]] @(connect-atm pid))]
          (d/transact (connect-atm pid) {:tx-data [{:db/id eid :project/agents (dissoc @interview-agent-atm :db/id)}]}))))
    ;; We don't want to pass around DB attributes, agent/thread-id, assistant-id are ambiguous, as are :aid and :tid!
    (reduce-kv (fn [m k v]
                 (cond (= k :agent/assistant-id) (assoc m :iview-aid v)
                       (= k :agent/thread-id)    (assoc m :iview-tid v)
                       :else m))
               {}
               @interview-agent-atm)))

(s/def ::interviewer-cmd (s/and (s/keys :req-un [::command]
                                        :opt-un [::response ::told_what ::conclusions ::advice])
                                #(case (:command %)
                                   "SUPPLY-QUESTION"    true
                                   "HUMAN-RESPONDS"     (contains? % :response)
                                   "ANALYSIS-CONCLUDES" (contains? % :conclusions)
                                   "HUMAN-TOLD"         (contains? % :told_what)
                                   "COURSE-CORRECTION"  (contains? % :advice)
                                   nil)))
(s/def ::command string?)
(s/def ::response string?)
(s/def ::conclusions string?)
(s/def ::told_what string?)
(s/def ::advice string?)

(defn tell-interviewer
  "Send a command to an interviewer agent and wait for response; translate it.
   :aid and :tid in the ctx should be for the interviewer agent."
  [cmd {:keys [iview-aid iview-tid] :as _ctx}]
  (s/assert ::interviewer-cmd cmd)
  (log/info "Interviewer told:" cmd)
  (let [cmd-string (json/write-value-as-string cmd)
        res (-> {:aid iview-aid :tid iview-tid :role "user" :query-text cmd-string}
                llm/query-on-thread
                output-struct2clj)
        res (if (contains? res :question-type) (update res :question-type keyword) res)]
    (log/info "Interviewer returns:" res)
    res))

;;; ToDo: Would be different for different interviews. For example, the resource interview would want to see the process interview.
(defn initial-advice
  "Return in an :ANALYSIS-CONCLUDES command map a list of conclusions used to set the context of the conversation just before
   the interviewer agent starts interviewing. This might, for example, tell the interviewer what the interviewee manufactures
   and whether the interviewee is an actual human or a surrogate."
  [expert]
  {:command "ANALYSIS-CONCLUDES",
   :conclusions (str "1) They make "
                     (:surrogate/subject-of-expertise expert) ". "
                     "2) You are talking to surrogate humans (machine agents).")})

(defn answers-question?
  "Return true if the answer to the Q/A pair appears to answer the question.
   The answer text argument might be nil, for example when answer was immoderate.
   In that case, return nil."
  [q-txt a-txt]
  (when (string? a-txt)
    (let [{:keys [aid tid]} (db/get-agent :base-type :answers-the-question?)
          res (-> (llm/query-on-thread
                   {:aid aid :tid tid
                    :query-text (format "QUESTION: %s \nANSWER: %s" q-txt a-txt)})
                  json/read-value
                  (get "answer"))]
      (or res
          (throw (ex-info "(Temporary) Surrogate Q/A is misaligned." {:q-txt q-txt :a-txt a-txt})))))) ; ToDo: Temporary

(defn answers-yes-or-more
  "Return true if the answer (a-txt) seems to answer yes to whatever I asked (q-txt)."
  [_q-txt _a-txt]
  true) ; ToDo: Write an agent.

(defn ^:diag response-category
  "Return a keyword indicating the class of the interviewee response to the interviewer's prior statement."
  [_viewer-txt _viewee-txt]
  :other) ; ToDo: Write an agent.

(defn ^:diag respond-to-non-interview
  "Respond to questions that aren't part of the interview."
  [_question _ctx]
  "I don't know.") ; ToDo: NYI.

(defn loop-for-answer
  "Get a response to the argument question, looping through non-responsive side conversation, if necessary.
   Response returned as {:command 'HUMAN-RESPONDS' :response <some-text> :question-type <a keyword>}.

   If the interviewee is responsive to the question, return it as a map containing :question and :answer.
   Otherwise use a 'concierge' agent to handle the discussion until the (human) agent
   indicates that they are ready to return to the interview.
   All discussion uses chat-pair and is recorded.
   Note that in chat-pair only questions to surrogates time out. "
  [question {:keys [question-type] :as ctx}]
  (let [save-ctx ctx
        ask-continue "Shall we continue with the interview?"
        iviewee-response-txt (chat-pair question ctx)]
    (if (answers-question? question iviewee-response-txt)
      {:command "HUMAN-RESPONDS" :response iviewee-response-txt :question-type question-type}
      (let [ctx (merge ctx (db/get-agent :base-type :concierge))]
        (loop [resp iviewee-response-txt]
          (chat-pair resp ctx)
          (Thread/sleep 5000)
          (let [continue? (chat-pair ask-continue ctx)
                {:keys [yes? more]} (answers-yes-or-more ask-continue continue?)]
            (cond more (recur more)
                  yes? (loop-for-answer question save-ctx)
                  :else (throw (ex-info "I'm getting lost in the conversation!" {})))))))))

;;; ToDo: It isn't obvious that the interview agent needs to see this. Only useful when the thread was destroyed?
(defn prior-responses
  "Construct a prior-response command structure for the argument project.
   This tells the interview agent what has already been asked. If the thread hasn't been deleted
   (i.e. by OpenAI, because it is more than 30 days old) then the agent knows what has already been
   asked. Thus, this covers that possibility. We also start fresh interviews with this information,
   just to make sure."
  [pid conv-id]
  (let [msgs (->> (db/get-conversation pid conv-id)
                  (filter #(= :surrogate (:message/from %)))
                  (mapv #(reduce-kv (fn [m k v] (cond (= k :message/content)         (assoc m :answer v)
                                                      (= k :message/question-name)   (assoc m :question-name (name v))
                                                      :else m))
                                    {} %)))]
    {:command "PRIOR-RESPONSES"
     :already-answered (->>  msgs (map :question-name) set vec)
     :responses msgs}))

(defn off-course?
  "Check whether the response from SUPPLY-QUESTION is appropriate and if not, send a COURSE-CORRECTION
   and ask for another. This is simply based on commonly experienced problems."
  [{:keys [question-type pid conv-id] :as ctx}]
  (let [prior (prior-responses pid conv-id)]
    (cond (and (-> prior :already-answered empty?)
               (not= :warm-up-question question-type))
          (do (log/warn "off-course: Didn't ask the warm-up-question!")
              (tell-interviewer {:command "COURSE-CORRECTION"
                                 :advice (str "There are no questions in already-answered of the PRIOR-RESPONSES command we are sent. "
                                              "You should have responded with the warm-up-question. "
                                              "Next time we send you a SUPPLY-QUESTION command, provide the warm-up-question.")}
                                ctx)
              (let [resp (tell-interviewer {:command "SUPPLY-QUESTION"} ctx)]
                (if (= "warm-up-question" (:question-type resp))
                  resp
                  (do (log/warn "off-course: STILL didn't ask the warm-up-question!")
                    {:question-type :warm-up-question
                     :question "What do you make and what is your biggest scheduling challenge?"}))))

          :else ctx)))

;;; Hint for diagnosing problems: Once you have the ctx (stuff it in an atom) you can call next-question
;;; over and over and the interview will advance each time until you get back DONE.
(defn q-and-a
  "Call loop-for-answer until the answer is responsive to a question that we obtain from the interviewer here.
   Returns a map of {:command 'HUMAN-RESPONDS' :response <some-text> :question-type <a keyword>}."
  [ctx]
  (let [result (tell-interviewer {:command "SUPPLY-QUESTION"} ctx)
        {:keys [question question-type status]} (off-course? (merge ctx result))]
    (if (= status "DONE")
      result
      (loop-for-answer question (-> ctx
                                    (assoc :question-type question-type)
                                    (assoc :question question))))))

;;; From the REPL you can change this to false anytime you want things to stop.
(def ^:diag active? "Debugging tool to stop the interview when false." (atom true))
(def diag2 (atom nil))

;;; This is typical of what is in ctx when a conversation is running.
#_{:iview-aid "asst_EK84QXpYS5TIN9PqssvClSAk",
   :iview-tid "thread_JR1W0DHOrvpU7NQQYCPK23ku",
   :client-id "ed3143ba-f0ec-42be-91fa-aa7114d57ef7",
   :sur-aid "asst_lR9mnX0DG1sFhUIIAqJSWSat",
   :sur-tid "thread_7Gj4jvks8SKqxhxYPkaIY2eL",
   :pid :sur-ice-cream,
   :conv-id :process,
   :dispatch-key :resume-conversation-plan,               ; probably not useful.
   :responder-role :surrogate}                            ; "responder" is a kind of interviewee :surrogate or :human

(defn resume-conversation
  "Start the interview loop. :resume-conversation-plan is a dispatch key from client.
   This is called even when PID is :START-A-NEW-PROJECT."
  [{:keys [client-id pid conv-id] :as ctx}]
  (assert (string? client-id))
  (try
    (let [conv-id (or conv-id ; One of #{:process :resource :data} currently.
                      (d/q '[:find ?conv-id . :where [_ :project/current-conversation ?conv-id]] @(connect-atm pid)))
          sur (db/get-surrogate-agent-info pid)
          ctx (-> ctx
                  (merge (interview-agent conv-id pid))
                  (assoc :responder-role :surrogate)
                  (assoc :sur-aid (:surrogate/assistant-id sur))
                  (assoc :sur-tid (:surrogate/thread-id sur)))]
      ;; The conversation loop.
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
      (when @active?
        (when (== 0 (-> pid (db/get-conversation-eids conv-id) count))
          (-> sur initial-advice (tell-interviewer ctx)))
        (-> (prior-responses pid conv-id) (tell-interviewer ctx))
        (loop [cnt 0
               response (q-and-a ctx)]
          (if (or (= "DONE" (:status response)) (> cnt 10))
            :interview-completed
              (when @active? ; Keep this despite (when @active? ...) above. Can set to false while running.
                (ru/analyze-response (merge ctx response))
                (recur (inc cnt)
                       (q-and-a ctx)))))))
      (finally
        (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;;------------------------------------ Starting and stopping --------------------------------
(defn init-iviewers!
  []
  (ws/register-ws-dispatch :resume-conversation-plan resume-conversation))

(defstate iviewers
  :start (init-iviewers!))
