(ns scheduling-tbd.interviewers
    "Runs an interview uing an interview agent."
  (:require
   [clojure.pprint                :refer [pprint]]
   [clojure.spec.alpha            :as s]
   [datahike.api                  :as d]
   [jsonista.core                 :as json]
   [mount.core                    :as mount :refer [defstate]]
   [promesa.core                  :as p]
   [promesa.exec                  :as px]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.domain.process-analysis :refer [the-warm-up-type-question]]
   [scheduling-tbd.llm            :as llm]
   [scheduling-tbd.response-utils :as ru :refer [find-claim]]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm elide output-struct2clj starting-new-project?]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere             :as tel :refer [log!]]
   [taoensso.truss                 :as truss :refer [have]]))

;;; This is typical of what is in contex (ctx) when a conversation is running:

#_{:command "INTERVIEWEES-RESPOND",
   :question-type :warm-up
   :answers :warm-up,
   :response "We primarily produce various types of plate glass,..."
   :iview-aid "asst_ToYvIv293mV7wQck8tUjidUY",
   :iview-tid "thread_Vws6JuTf64GwpgI0qU7m8itJ",
   :sur-aid "asst_qfiFxk5VrS0oO3B3mOHd9cOO",
   :sur-tid "thread_jsMawPCXoz3ivkJU0yxnBgIt",
   :client-id "455c85e2-0e01-4580-9d4a-219fcabd6828",
   :pid :sur-plate-glass,
   :cid :process,
   :dispatch-key :resume-conversation-plan,
   :responder-role :surrogate}

(def diag (atom nil))

;;; From the REPL you can change the active? atom to false anytime you want things to stop
;;; (like when it is burning OpenAI asking the same question over and over ;^)).
;;; If it doesn't stop, it is probably the case that you need to test for it in more places.
(def ^:diag active? "Debugging tool to stop the interview when false." (atom true))

;;; (chat-pair-aux {:pid :sur-fountain-pens :surrogate? true :agent-query "Describe your most significant scheduling problem in a few sentences."} {})
(defn chat-pair-aux
  "Run one query/response pair of chat elements with a human or a surrogate.
   Returns promise which will resolve to the original obj argument except:
     1) :agent-query is adapted from the input argument value for the agent type (human or surrogate)
     2) :response is added. Typically its value is a string."
  [{:keys [question sur-aid sur-tid responder-role preprocess-fn tries] :as ctx
    :or {tries 1 preprocess-fn identity}}]
  (let [prom (if (= :human responder-role)
               (ws/send-to-chat (-> ctx                        ; This cannot timeout.
                                    (assoc :text question)
                                    (assoc :promise? true)
                                    (assoc :dispatch-key :tbd-says)))
               (px/submit! (fn []
                             (try
                                 (llm/query-on-thread :aid sur-aid   ; This can timeout.
                                                      :tid sur-tid
                                                      :query-text question
                                                      :tries tries
                                                      :preprocess-fn preprocess-fn)
                               (catch Exception e {:error e})))))]
    (-> prom
        #_(p/catch (fn [e]
                  (datafy e)
                  (log! :error "Failed in chat-pair-aux:" (datafy e))))
       p/await)))

;;; For use of chat-pair and other things defined below.
(s/def ::chat-pair-ctx (s/or :surrogate (s/and #(s/valid? ::surrogate-ctx %) #(s/valid? ::common-ctx %))
                             :human     (s/and #(s/valid? ::human-ctx %)     #(s/valid? ::common-ctx %))))

(s/def ::common-ctx (s/keys :req-un [::client-id ::pid ::cid]))
(s/def ::surrogate-ctx (s/and ::common-ctx (s/keys :req-un [:sur/responder-role ::sur-aid ::sur-tid ])))
(s/def ::human-ctx     (s/and ::common-ctx (s/keys :req-un [:hum/responder-role])))

(s/def ::pid keyword?)
(s/def ::cid keyword?)
(s/def :sur/responder-role #(= % :surrogate)) ; Keep. Bug in kondo?
(s/def :hum/responder-role #(= % :human))     ; Keep. Bug in kondo?
(s/def ::client-id string?)
(s/def ::sur-aid string?)
(s/def ::sur-tid string?)
(s/def ::question-type keyword?)

;;; Optional
(s/def ::tags (s/coll-of keyword?))
(s/def ::topic #(#{:process :resource :data} %))

(defn chat-pair
  "Call to run the chat and put the query and response into the project's database.
   Updates the UI and returns response text."
  [{:keys [pid responder-role client-id tags cid question question-type]
    :or {tags []} :as ctx}] ; ToDo: How do I implement tags? I suppose I need an agent to classify the question. Also no default for topic?
  (s/assert ::chat-pair-ctx ctx)
  (let [response-text (chat-pair-aux ctx)
        q-type (keyword question-type)]
    (if (string? response-text)
      (if (llm/immoderate? response-text)
        (ws/send-to-chat (assoc ctx :text "I won't respond to that."))
        (do (db/add-msg {:pid pid
                         :from :system
                         :text question
                         :question-type q-type
                         :tags (conj tags :query)})
            (db/add-msg {:pid pid
                         :from responder-role
                         :text response-text
                         :question-type q-type
                         :tags (conj tags :response)})
            (when cid (ws/refresh-client client-id pid cid))
            response-text))
      (throw (ex-info "Unknown response from operator" {:response response-text})))))

;;; Note that in OpenAI every thread has its own separate context window, which stores the conversation history specific to
;;; that thread, preventing cross-contamination with other threads.
(defn interview-agent
  "Return a map with project agent info (aid, tid). The agent is shared by all projects, but the threads are project-specific.
   If a thread has not yet been created for this project, this creates it and stores that in the DB before returning the info."
  [cid pid]
  (assert (#{:process :resource :data} cid))
  (let [agent-type (-> cid name (str "-interview-agent") keyword)
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

;;; To check the structure of commands to the interviewer:
(s/def ::interviewer-cmd (s/and (s/keys :req-un [::command])
                                #(let [{:keys [advice already-answered answers claims command conclusions response responses told-what]} %]
                                   (case command
                                     "SUPPLY-QUESTION"         (and (s/valid? ::already-answered already-answered) (s/valid? ::claims claims))
                                     "INTERVIEWEES-RESPOND"    (and (s/valid? ::response response) (s/valid? ::answers answers))
                                     "ANALYSIS-CONCLUDES"      (s/valid? ::conclusions conclusions)
                                     "INTERVIEWEES-TOLD"       (s/valid? ::told-what told-what)
                                     "COURSE-CORRECTION"       (s/valid? ::advice advice)
                                     "CONVERSATION-HISTORY"    (and (s/valid? ::already-answered already-answered)
                                                                    (s/valid? ::claims claims)
                                                                    (s/valid? ::responses responses))
                                   nil))))

(s/def ::advice string?)
(s/def ::already-answered (s/coll-of keyword?))
(s/def ::answer string?)
(s/def ::answers keyword?)
(s/def ::claims string?)
(s/def ::conclusions string?)
(s/def ::command string?)
(s/def ::question-type keyword?)
(s/def ::response string?)
(s/def ::responses (s/coll-of (s/keys :req-un [::question-type ::answer])))
(s/def ::told-what string?)

(defn tell-interviewer
  "Send a command to an interviewer agent and wait for response; translate it.
   :aid and :tid in the ctx should be for the interviewer agent."
  [cmd {:keys [iview-aid iview-tid] :as ctx}]
  (when-not (s/valid? ::interviewer-cmd cmd) ; We don't s/assert here because old project might not be up-to-date.
    (log! :warn (str "Invalid interviewer-cmd: " (with-out-str (pprint cmd)))))
  (log! :info (-> (str "Interviewer told: " cmd) (elide 150)))
  (let [cmd-string (json/write-value-as-string cmd)
        res (-> {:aid iview-aid :tid iview-tid :role "user" :query-text cmd-string}
                llm/query-on-thread
                output-struct2clj)
        res (if (contains? res :question-type) (update res :question-type keyword) res)]
    (log! :info (-> (str "Interviewer returns: " res) (elide 150)))
    res))

;;; ToDo: Would be different for different interviews. For example, the resource interview would want to see the process interview.
;;;       This one is for surrogates.
(defn initial-advice
  "Return in an :ANALYSIS-CONCLUDES command map a list of conclusions used to set the context of the conversation just before
   the interviewer agent starts interviewing. This might, for example, tell the interviewer what the interviewee manufactures
   and whether the interviewee is an actual human or a surrogate."
  [pid]
  (let [expertise (-> pid db/get-project :project/surrogate :surrogate/subject-of-expertise)]
    {:command "ANALYSIS-CONCLUDES",
     :conclusions (str "1) They make " expertise ". 2) You are talking to surrogate humans (machine agents).")}))

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
   Returns {:command 'INTERVIEWEES-RESPOND' :response <some-text> :question-type <a keyword>}."
  [{:keys [question question-type] :as ctx}]
  (s/assert ::chat-pair-ctx ctx)
  (let [save-ctx ctx
        ask-continue "Shall we continue with the interview?"
        iviewee-response-txt (chat-pair ctx)]
    (if (answers-question? question iviewee-response-txt)
      {:command "INTERVIEWEES-RESPOND" :response iviewee-response-txt :answers question-type :question-type question-type}
      ;; Does not answer the question. ToDo: This hasn't been tested; needs a human.
      (loop [_resp iviewee-response-txt]
        (chat-pair ctx)
        (Thread/sleep 5000)
        (let [continue? (chat-pair (merge ctx {:question ask-continue
                                               :question-type :prompt-focus}))
              {:keys [yes? more]} (answers-yes-or-more ask-continue continue?)]
          (cond more (recur more)
                yes? (loop-for-answer save-ctx)
                :else (throw (ex-info "I'm getting lost in the conversation!" {}))))))))

(defn already-answered?
  "Return a set of what has already been answered."
  [pid cid]
  (-> (d/q '[:find [?q-type ...]
             :where [_ :message/question-type ?q-type]]
           @(connect-atm pid))
      set))

;;; ToDo: It isn't obvious that the interview agent needs to see this. Only useful when the thread was destroyed?
(defn conversation-history
  "Construct a conversation-history command structure for the argument project.
   This tells the interview agent what has already been asked. If the thread hasn't been deleted
   (i.e. by OpenAI, because it is more than 30 days old) then the agent knows what has already been
   asked. Thus, this covers that possibility. We also start fresh interviews with this information,
   just to make sure."
  [pid cid]
  (let [msgs (->> (db/get-conversation pid cid)
                  (filter #(= :surrogate (:message/from %)))
                  (mapv #(reduce-kv (fn [m k v] (cond (= k :message/content)         (assoc m :answer v)
                                                      (= k :message/question-type)   (assoc m :question-type v)
                                                      :else m))
                                    {} %)))]
    {:command "CONVERSATION-HISTORY"
     :claims (-> (db/get-claims pid) vec str)
     :already-answered (vec (already-answered? pid cid))
     :responses msgs}))

(defn supply-question
  "Create a supply question command for the conversation."
  [pid cid]
  {:command "SUPPLY-QUESTION"
   :already-answered (already-answered? pid cid)
   :claims (->> (db/get-claims pid) (interpose " ") vec (apply str))})

(declare fix-off-course)
;;; ToDo: These should be methods with tags in response-utils.
;;; ToDo: Remove this one? Or maybe put it in process_analysis
(defn off-course--do-warm-up
  "This was a problem for a while. The interviewer did not start with the warm-up question.
   The returns the warm-up question."
  [{:keys [pid cid] :as ctx}]
  (log! :warn "off-course: Didn't ask a warm-up type question!")
  (tell-interviewer {:command "COURSE-CORRECTION"
                     :advice (str "There are no questions in already-answered of the CONVERSATION-HISTORY command we are sent. "
                                  "You should have responded with the question-type = \"warm-up\" (and a warm-up question, etc.). "
                                  "Next time we send you a SUPPLY-QUESTION command, provide a warm-up type question.")}
                    ctx)
  (-> (supply-question pid cid) (tell-interviewer ctx) (fix-off-course ctx)))

(def process-flow-shop-questions
  "All questions that should be asked for a flow-shop."
  [:warm-up
   :work-type
   :production-location
   :production-motivation
   :production-system-type
   :process-steps
   :process-durations
   :process-ordering])

(defn off-course--not-done
  "This is called when interviewer sends {:status 'DONE'} but it wasn't
   We give it a course correction and hope checks that it returns the right thing."
  [{:keys [pid cid] :as ctx}]
  (log! :warn "off-course: Returning 'DONE' when there is more to be done.")
  (let [answered? (already-answered? pid cid)
        ask-next (have keyword? (some #(when (not (answered? %)) %) process-flow-shop-questions))]
    (tell-interviewer {:command "COURSE-CORRECTION"
                       :advice (str "You are not done."
                                    "Next time we send you a SUPPLY-QUESTION command "
                                    (name ask-next) ".")}
                      ctx)
    (-> (supply-question pid cid) (tell-interviewer ctx) (fix-off-course ctx))))

;;; ToDo: This is currently very much dependent on knowledge of the domain being interviewed, and therefore also
;;;       apt to be modified often. It would be nice were we to capture this knowledge in structures some how.
;;;       It might be possible to ask the agent for these, for example. (That could be a deftest too.)
(defn done?
  "A conversation-specific boolean function that determines whether everything that needs to be asked has been asked."
  [pid cid]
  (let [answered? (already-answered? pid cid)]
    (case cid
      :process  (cond (find-claim '(flow-shop ?x) (db/get-claims pid))    (every? answered? process-flow-shop-questions)
                      :else true)
      :data     true
      :resource true)))

(defn off-course--invalid-response
  [candidate-q {:keys [pid cid] :as ctx}]
  (log! :warn (str "Not a valid response to SUPPLY-QUESTION: " candidate-q))
  (tell-interviewer {:command "COURSE-CORRECTION"
                     :advice (str "Responses to SUPPLY-QUESTION should be either "
                                  "1) an object have attributes 'question' and 'question-type', or "
                                  "2) an object with just the attribute 'status' with value 'DONE'.\n"
                                  "We will ask for a question again.")}
                     ctx)
  (-> (supply-question pid cid) (tell-interviewer ctx) (fix-off-course ctx)))

;;; For responses to the interviewer protocol command SUPPLY-QUESTION:
(s/def ::supply-q-response (s/or :done #(= "DONE" (:status %))
                                 :not-done (s/keys :req-un [:q-resp/question :q-resp/question-type])))

(s/def :q-resp/question string?)
(s/def :q-resp/question-type keyword?)

(defn fix-off-course
  "Check whether the response from SUPPLY-QUESTION is appropriate and if not, send a COURSE-CORRECTION
   and ask for another. This is simply based on commonly experienced problems.
   Return the question (candidate-q) that should be asked. That might be the argument question, of course."
  [{:keys [question-type status] :as candidate-q}
   {:keys [pid cid] :as ctx}]
  (when @active?
    (let [prior (conversation-history pid cid)]
      (cond  (not (s/valid? ::supply-q-response candidate-q))            (off-course--invalid-response candidate-q ctx),
             (and (= status "DONE") (not (done? pid cid)))               (off-course--not-done ctx),
             :else
             (case cid
               :process (cond (and (-> prior :already-answered empty?)
                                   (not= :warm-up question-type))        (off-course--do-warm-up ctx),
                              :else                                      candidate-q)
               :data      candidate-q ; NYI
               :resource  candidate-q))))) ; NYI

;;; Hint for diagnosing problems: Once you have the ctx (stuff it in an atom) you can call q-and-a
;;; over and over and the interview will advance each time until you get back DONE.
(defn q-and-a
  "Call loop-for-answer until the answer is responsive to a question that we obtain from the interviewer here.
   Returns a map of {:command 'INTERVIEWEES-RESPONDS' :response <some-text> :question-type <a keyword>}."
  [{:keys [pid cid] :as ctx}]
  (reset! diag ctx)
  (let [{:keys [status] :as resp}
        (-> (supply-question pid cid)
            (tell-interviewer ctx)
            (fix-off-course ctx))]
    (if (= status "DONE")
      (-> (merge ctx resp) (assoc :question-type :DONE))
      (loop-for-answer (merge ctx resp)))))

(defn ctx-surrogate
  "Return context updated with surrogate info."
  [{:keys [pid cid] :as ctx}]
  (let [sur (db/get-surrogate-agent-info pid)]
    (-> ctx
        (merge (interview-agent cid pid))
        (assoc :responder-role :surrogate)
        (assoc :sur-aid (:surrogate/assistant-id sur))
        (assoc :sur-tid (:surrogate/thread-id sur)))))

(defn ctx-human
  "Return context updated with human info."
  [{:keys [pid cid] :as ctx}]
  (let [ctx (-> ctx
                (merge (interview-agent cid pid))
                (assoc :responder-role :human))]
    (if (s/valid? ::human-ctx ctx)
      ctx
      (do
        (reset! diag ctx)
        (throw (ex-info "invalid human context:" {:ctx ctx}))))))

(defn start-human-project!
  "Ask the warm-up question, get a pid and return context updated for human by creating a project for the human.
   Return an updated context (s/valid? ::human-ctx) with :question and :answer, which then can be communicated to the app."
  [{:keys [pid client-id] :as ctx} & {:keys [use-this-answer] :as _opts}]
  (assert (or use-this-answer (starting-new-project? pid))) ; use-this-answer is for debugging.
  (when-not use-this-answer
    (ws/send-to-chat {:dispatch-key :tbd-says :client-id client-id :text the-warm-up-type-question}))
  (let [response (or use-this-answer
                     (loop-for-answer (merge ctx {:responder-role :human
                                                  :question-type :warm-up
                                                  :question the-warm-up-type-question})))
        ;; This, the :warm-up method of defanalyze creates the project and updates the chat about the project name.
        pid (ru/analyze-response-meth (merge ctx {:question-type :warm-up :response response}))]
    (-> ctx
        (assoc :pid pid)
        (assoc :cid :process)
        (assoc :question-type :warm-up)
        ctx-human)))

(defn start-surrogate-project
  "Return context updated for surrogate usage. Project was already made because resume-conversation was called by
  sur/start-surrogate which is a web-socket handler for :start-surrogate. That sends a :load-proj to the app,
  which provides :proj/id on which we can then call resume-conversation."
  [ctx]
  (let [ctx (ctx-surrogate ctx)]
    (s/valid? ::surrogate-ctx ctx)
    ctx))

(defn ready-for-discussion?
  "Return true if the state of conversation is such that we can now have
   discussion on the given topic."
  [pid cid]
  (if (starting-new-project? pid)
    (= cid :process)
    (case cid
      (:data :resource) (find-claim '(flow-shop ?x) (db/get-claims pid)) ; ToDo: Someday we will be able to do better than this!
      :process true)))

(defn redirect-user-to-discussion
  "Put a message in the current chat to go to the recommended chat."
  [client-id current-chat recommended-chat]
  (let [nice-names {:resource "resources" :data "data" :optimality "optimality" :process "process"} ; Note "resources".
        current (nice-names current-chat)
        recommended (nice-names recommended-chat)
        text (str
              (format "We aren't quite ready to discuss %s until we've talked more about %s. "
                      current recommended)
              (format "Could you pop over to the %s discussion (menu on left, 'Process', 'Data' etc.) and chat more with that agent?"
                      recommended))]
    ;; Note that you don't send cid; it lands in whatever conversation the users is looking at.
    (ws/send-to-chat {:dispatch-key :tbd-says :client-id client-id :text text})))

(defn resume-conversation
  "Start the interview loop. :resume-conversation-plan is a dispatch key from client.
   This is called even when PID is :START-A-NEW-PROJECT."
  [{:keys [client-id pid cid] :as ctx}]
  (assert (string? client-id))
  (try
    ;; ToDo: If :process isn't pretty far along.  Put them on process if they are not there.
    (let [cid (or (#{:process :resource :data :optimality} cid)
                  (d/q '[:find ?cid . :where [_ :project/current-conversation ?cid]] @(connect-atm pid)))]
      (if (and (not= :process cid) (not (ready-for-discussion? pid cid)))
        (redirect-user-to-discussion client-id cid :process)
        (let [{:keys [pid] :as ctx} (if (starting-new-project? pid) (start-human-project! ctx) (start-surrogate-project ctx))]
          ;; The conversation loop.
          (when-not (db/conversation-done? pid cid)
            (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
            (when @active?
              (when (== 0 (-> (db/get-conversation-eids pid cid) count))
                (-> ctx :pid initial-advice (tell-interviewer ctx)))
              (-> (conversation-history pid cid) (tell-interviewer ctx))
              (loop [cnt 0
                     response (q-and-a ctx)]
                (cond
                  (> cnt 15)                      :exceeded-questions-safety-stop
                  (= "DONE" (:status response))   (db/assert-conversation-done! pid cid)
                  :else
                  (when @active? ; Keep this despite (when @active? ...) above. Can set to false while running.
                    (tell-interviewer response ctx) ; This is a INTERVIEWEES-RESPONDS
                    (ru/analyze-response-meth (merge ctx response))
                    (recur (inc cnt)
                           (q-and-a ctx))))))))))
      (finally
        (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;;------------------------------------ Starting and stopping --------------------------------
(defn init-iviewers!
  []
  (ws/register-ws-dispatch :resume-conversation-plan resume-conversation))

(defstate iviewers
  :start (init-iviewers!))
