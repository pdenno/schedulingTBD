(ns scheduling-tbd.interviewers
    "Runs an interview uing an interview agent."
    (:require
     [clojure.core.unify          :as uni]
     [clojure.pprint                :refer [pprint]]
     [clojure.spec.alpha            :as s]
     [clojure.string                :as str]
     [datahike.api                  :as d]
     [jsonista.core                 :as json]
     [mount.core                    :as mount :refer [defstate]]
     [promesa.core                  :as p]
     [promesa.exec                  :as px]
     [scheduling-tbd.db             :as db]
     [scheduling-tbd.domain.data-analysis]
     [scheduling-tbd.domain.process-analysis  :as pan :refer [the-warm-up-type-question]]
     [scheduling-tbd.domain.optimality-analysis]
     [scheduling-tbd.domain.resources-analysis]
     [scheduling-tbd.llm            :as llm]
     [scheduling-tbd.response-utils :as ru :refer [find-claim]]
     [scheduling-tbd.sutil          :as sutil :refer [connect-atm elide output-struct2clj]]
     [scheduling-tbd.web.websockets :as ws]
     [taoensso.telemere             :as tel :refer [log!]]
     [taoensso.truss                 :as truss :refer [have]]))

;;; This is typical of what is in contex (ctx) when a conversation is running:
#_{:command "INTERVIEWEES-RESPOND",
   :question-type :process/warm-up
   :answers :warm-up,
   :response "We primarily produce various types of plate glass,..."
   :iview-aid "asst_ToYvIv293mV7wQck8tUjidUY",
   :iview-tid "thread_Vws6JuTf64GwpgI0qU7m8itJ",
   :sur-aid "asst_qfiFxk5VrS0oO3B3mOHd9cOO",
   :sur-tid "thread_jsMawPCXoz3ivkJU0yxnBgIt",
   :client-id "455c85e2-0e01-4580-9d4a-219fcabd6828",
   :pid :sur-plate-glass,
   :cid :process,
   :dispatch-key :resume-conversation,
   :responder-role :surrogate}

(def diag (atom nil))
(s/def ::pid keyword?)
(s/def ::cid keyword?)

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
  [{:keys [question sur-aid sur-tid responder-role preprocess-fn tries client-id] :as ctx
    :or {tries 1 preprocess-fn identity}}]
  (log! :debug (str "ctx in chat-pair-aux: " (with-out-str (pprint ctx))))
  (let [prom (if (= :human responder-role)
               (do
                 (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id})
                 (ws/send-to-chat (-> ctx                        ; This cannot timeout.
                                      (assoc :text question)
                                      (assoc :promise? true)
                                      (assoc :dispatch-key :tbd-says))))
               (px/submit! (fn [] ; surrogate responder...
                             (try
                                 (llm/query-on-thread :aid sur-aid   ; This can timeout.
                                                      :tid sur-tid
                                                      :query-text question
                                                      :tries tries
                                                      :preprocess-fn preprocess-fn)
                               (catch Exception e {:error e})))))]
    (-> prom p/await)))

;;; For use of chat-pair and other things defined below.
(s/def ::chat-pair-ctx (s/and ::common-ctx
                              (s/or :surrogate (s/and #(s/valid? ::surrogate-ctx %) #(s/valid? ::common-ctx %))
                                    :human     (s/and #(s/valid? ::human-ctx %)     #(s/valid? ::common-ctx %)))))

(s/def ::common-ctx (s/keys :req-un [::client-id ::question])) ; ::question-type not needed here.
(s/def ::surrogate-ctx (s/keys :req-un [:sur/responder-role ::sur-aid ::sur-tid ]))
(s/def ::human-ctx     (s/keys :req-un [:hum/responder-role]))

(s/def :sur/responder-role #(= % :surrogate))
(s/def :hum/responder-role #(= % :human))
(s/def ::client-id string?)
(s/def ::sur-aid string?)
(s/def ::sur-tid string?)
(s/def ::question-type (s/and keyword? namespace))
(s/def ::question string?)

;;; Optional
(s/def ::tags (s/coll-of keyword?))
(s/def ::topic #(#{:process :data :resources :optimality} %))

;;; ToDo: Maybe return a string ===immoderate=== and end the conversation if needed.
(defn chat-pair
  "Call to run ask and wait for an answer, returns the response, a string.
   If response is immoderate return '===IMMODERATE==='."
  [{:keys [responder-role client-id] :as ctx}]
  (let [response-text (chat-pair-aux ctx)]
    (if (string? response-text)
      (if (and (= :human responder-role) (llm/immoderate? response-text))
        (do (ws/send-to-chat {:dispatch-key :tbd-says :client-id client-id :text "This is not helpful."})
            "===IMMODERATE===") ; ToDo: Continue with this to stop interactions.
        response-text)
    (throw (ex-info "Unknown response from operator" {:response response-text})))))

(s/def ::interview-agent (s/keys :req-un [::iview-aid ::iview-tid]))
(s/def ::iview-aid string?)
(s/def ::iview-tid string?)

;;; Note that in OpenAI every thread has its own separate context window, which stores the conversation history specific to
;;; that thread, preventing cross-contamination with other threads.
(defn ensure-interview-agent!
  "Return a map with project agent info (iview-aid, iview-tid). The agent is shared by all projects, but the threads are project-specific.
   If a thread has not yet been created for this project, this creates it and stores that in the DB before returning the info."
  [pid cid]
  (assert (#{:process :data :resources :optimality} cid))
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
    (->> (reduce-kv (fn [m k v]
                      (cond (= k :agent/assistant-id) (assoc m :iview-aid v)
                            (= k :agent/thread-id)    (assoc m :iview-tid v)
                            :else m))
                    {}
                    @interview-agent-atm)
         (s/assert ::interview-agent))))

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
(s/def ::response string?)
(s/def ::responses (s/coll-of (s/keys :req-un [::question-type ::answer])))
(s/def ::told-what string?)

(defn tell-interviewer
  "Send a command to an interviewer agent and wait for response; translate it.
   :aid and :tid in the ctx should be for the interviewer agent."
  [cmd {:keys [iview-aid iview-tid cid] :as _ctx}]
  (when-not (s/valid? ::interviewer-cmd cmd) ; We don't s/assert here because old project might not be up-to-date.
    (log! :warn (str "Invalid interviewer-cmd: " (with-out-str (pprint cmd)))))
  (log! :info (-> (str "Interviewer told: " cmd) (elide 150)))
  (let [cmd-string (json/write-value-as-string cmd)
        res (-> {:aid iview-aid :tid iview-tid :role "user" :query-text cmd-string}
                llm/query-on-thread
                output-struct2clj)
        res (if (contains? res :question-type)
              (update res :question-type #(keyword (name cid) %))
              res)]
    (log! :info (-> (str "Interviewer returns: " res) (elide 150)))
    res))

;;; ToDo: Would be different for different interviews. For example, the resource interview would want to see the process interview.
(defn initial-advice
  "Return in an :ANALYSIS-CONCLUDES command map a list of conclusions used to set the context of the conversation just before
   the interviewer agent starts interviewing. This might, for example, tell the interviewer what the interviewee manufactures
   and whether the interviewee is an actual human or a surrogate."
  [pid]
  (let [claims (db/get-claims pid)
        surrogate?       (ru/find-claim '(surrogate ?pid) claims)
        [_ _ expertise]  (ru/find-claim '(principal-expertise ?pid ?makes) claims)
        conclusions (cond-> ""
                      expertise  (str " Their expertise: " expertise ". ")
                      surrogate? (str " You are talking to to a surrogate (machine agent). "))]

    {:command "ANALYSIS-CONCLUDES",
     :conclusions conclusions}))

(defn response-analysis
  "Return a map of booleans including
    - :answers-the-question? : which is true (some text) if the answer to the Q/A pair appears to answer the question.
    - :raises-a-question? : which is true (some text) if the response raises a question.
    - :wants-a-break?  : which is true (some text) if the user explicitly asks for a break?"
  [q-txt a-txt]
  (assert (string? q-txt))
  (assert (string? a-txt))
  (let [{:keys [aid tid]} (db/get-agent :base-type :response-analysis-agent)]
    (-> (llm/query-on-thread
         {:aid aid :tid tid
          :query-text (format "QUESTION: %s \nRESPONSE: %s" q-txt a-txt)})
        json/read-value
        (update-keys str/lower-case)
        (update-keys keyword)
        (update-vals #(if (empty? %) false %)))))

;;; ToDo: Implement these (next three). They will probably involve some looping.
(defn handle-wants-a-break
  [_question response _ctx]
  (log! :warn (str "handle-wants-a-break not yet implemented: response = " response))
  [])

(defn handle-raises-a-question
  "The responder-role is :human. Handle a digression returning when the original-question is answered.
   Return a vector of the entire digression."
  [_question response _ctx]
  (log! :warn (str "handle-raises-a-question not yet implemented: response = " response))
  [])

(defn handle-other-non-responsive
  [_question response _ctx]
  (log! :warn (str "handle-other-non-responsive not yet implemented: response = " response))
  [])

;;; ToDo: The design question I still have here is how to get back on track when the user's response doesn't answer the question.
;;;       This is mostly a matter for handle-raises-a-question ...(Thread/sleep 20000).
(defn get-an-answer
  "Get a response to the argument question, looping through non-responsive side conversation, if necessary.
   Return a vector of the conversation (objects suitable for db/add-msg) terminating in an answer to the question.
   Might talk to the client to keep things moving if necessary. Importantly, this requires neither PID nor CID."
  [{:keys [question question-type responder-role] :as ctx}]
  (s/assert ::chat-pair-ctx ctx)
  (let [conversation [{:text question :from :system :question-type question-type :tags [:query]}]
        response  (chat-pair ctx) ; response here is just text.
        answered? (= :surrogate responder-role) ; Surrogate doesn't beat around the bush, I think!
        {:keys [answers-the-question? raises-a-question? wants-a-break?]} (when-not answered? (response-analysis question response))
        answered? (or answered? answers-the-question?)]
    (if (not (or answered? wants-a-break? raises-a-question?))
      (let [same-question (str "Okay, but we are still interested in this: " question)]
        (into conversation (get-an-answer (-> ctx (assoc :question same-question)))))
      (cond-> conversation
        answered?                  (conj {:text response :from responder-role :question-type question-type :tags [:response]})
        wants-a-break?             (into (handle-wants-a-break question response ctx))
        raises-a-question?         (into (handle-raises-a-question question response ctx))
        (not
         (or answered?
             wants-a-break?
             raises-a-question?))  (into (handle-other-non-responsive question response ctx))))))

(defn already-answered?
  "Return a set of what has already been answered."
  [pid cid]
  (-> (d/q '[:find [?q-type ...]
             :in $ ?cid
             :where
             [?ceid :conversation/id ?cid]
             [?ceid :conversation/messages ?meid]
             [?meid :message/question-type ?q-type]]
           @(connect-atm pid) cid)
      set))

;;; ToDo: It isn't obvious that the interview agent needs to see all this. Only useful when the thread was destroyed?
;;;       It might be sufficient to explain the claims.
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
     :claims (->> (db/get-claims pid :objects? true)
                  (filter #(= cid (:conversation-id %)))
                  (map :claim)
                  (map str)
                  (interpose " ")
                  (apply str))
     :already-answered (vec (already-answered? pid cid))
     :responses msgs}))

(defn supply-question
  "Create a supply question command for the conversation."
  [{:keys [pid cid] :as _ctx}]
    {:command "SUPPLY-QUESTION"
     :already-answered (already-answered? pid cid)
     :claims (->> (db/get-claims pid) (interpose " ") vec (apply str))})

(declare fix-off-course)
;;; ToDo: These should be methods with tags in response-utils.
;;; ToDo: Remove this one? Or maybe put it in process_analysis
(defn off-course--do-warm-up
  "This was a problem for a while. The interviewer did not start with the warm-up question.
   The returns the warm-up question."
  [ctx]
  (log! :warn "off-course: Didn't ask a warm-up type question!")
  (tell-interviewer {:command "COURSE-CORRECTION"
                     :advice (str "There are no questions in already-answered of the CONVERSATION-HISTORY command we are sent. "
                                  "You should have responded with the question-type = \"warm-up\" (and a warm-up question, etc.). "
                                  "Next time we send you a SUPPLY-QUESTION command, provide a warm-up type question. "
                                  "The type of a warm-up question is \"warm-up\", not \"warm-up-question\" or anything else!")}
                    ctx)
  (-> (supply-question ctx) (tell-interviewer ctx) (fix-off-course ctx)))

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
    (-> (supply-question ctx) (tell-interviewer ctx) (fix-off-course ctx))))

;;; ToDo: This is currently very much dependent on knowledge of the domain being interviewed, and therefore also
;;;       apt to be modified often. It would be nice were we to capture this knowledge in structures some how.
;;;       It might be possible to ask the agent for these, for example. (That could be a deftest too.)
(defn done?
  "A conversation-specific boolean function that determines whether everything that needs to be asked has been asked."
  [pid cid]
  (let [answered? (already-answered? pid cid)]
    (case cid
      :process   (cond (find-claim '(flow-shop ?x) (db/get-claims pid))    (every? answered? process-flow-shop-questions)
                       :else true)
      :data       true
      :resources  true
      :optimality true)))

(defn off-course--invalid-response
  [candidate-q ctx]
  (log! :warn (str "Not a valid response to SUPPLY-QUESTION: " candidate-q))
  (tell-interviewer {:command "COURSE-CORRECTION"
                     :advice (str "Responses to SUPPLY-QUESTION should be either "
                                  "1) an object have attributes 'question' and 'question-type', or "
                                  "2) an object with just the attribute 'status' with value 'DONE'.\n"
                                  "We will ask for a question again.")}
                     ctx)
  (-> (supply-question ctx) (tell-interviewer ctx) (fix-off-course ctx)))

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
      (cond  (not (s/valid? ::supply-q-response candidate-q))               (off-course--invalid-response candidate-q ctx),
             (and (= status "DONE") (not (done? pid cid)))                  (off-course--not-done ctx),
             :else
             (case cid
               :process  (cond (and (-> prior :already-answered empty?)
                                    (not= :process/warm-up question-type))  (off-course--do-warm-up ctx), ; This for surrogate.
                               :else                                        candidate-q)
               :data       candidate-q ; NYI
               :resources  candidate-q ; NYI
               :optimality candidate-q))))) ; NYI

;;; Hint for diagnosing problems: Once you have the ctx (stuff it in an atom) you can call q-and-a
;;; over and over and the interview will advance each time until you get back DONE.
;;; ToDo: This goes somewhere when you are done: {:command 'INTERVIEWEES-RESPOND' :response <some-text> :question-type <a keyword>}." <===========================================
(defn q-and-a
  "Call interviewer to supply a question; call  get-an-answer for an answer prefaced by zero or more messages non-responsive to the question.
   Returns a vector of message objects suitable for db/add-msg."
  [ctx]
  (let [supplied-question
        (-> (supply-question ctx)   ; This just makes the question map.
            (tell-interviewer ctx)  ; This translates it to JSON and tells it to the agent.
            (fix-off-course ctx))]  ; This will return the argument if it is okay, otherwise has a conversation with the interviewer to fix things.
    (log! :info (str "supplied-question = " supplied-question))
    (if (= (:status supplied-question) "DONE")
      (-> (assoc ctx :status "DONE") vector) ; This in lieu of a vector of messages.
      (get-an-answer (merge ctx (dissoc supplied-question :status))))))

(defn ready-for-discussion?
  "Return true if the state of conversation is such that we can now have
   discussion on the given topic."
  [pid cid]
  (case cid
    ;; ToDo: Someday we will be able to do better than this!
    (:data :resources :optimality) (find-claim '(flow-shop ?x) (db/get-claims pid))
    :process true))

(defn redirect-user-to-discussion
  "Put a message in the current chat to go to the recommended chat."
  [client-id current-chat recommended-chat]
  (let [text (str
              (format "We aren't quite ready to discuss %s until we've talked more about %s. "
                      (name current-chat) (name recommended-chat))
              (format "Could you pop over to the %s discussion (menu on left, 'Process', 'Data' etc.) and chat more with that agent?"
                      (name recommended-chat)))]
    ;; Note that you don't send cid; it lands in whatever conversation the users is looking at.
    (ws/send-to-chat {:dispatch-key :tbd-says :client-id client-id :text text})))

(defn surrogate? [pid] (ru/find-claim '(surrogate ?x) (db/get-claims pid)))

(defn ctx-surrogate
  "Return context updated with surrogate info."
  [{:keys [pid cid] :as ctx}]
  (let [sur (db/get-surrogate-agent-info pid)
        ctx (-> ctx
                (merge (ensure-interview-agent! pid cid))
                (assoc :responder-role :surrogate)
                (assoc :sur-aid (:surrogate/assistant-id sur))
                (assoc :sur-tid (:surrogate/thread-id sur)))]
    (if (s/valid? ::surrogate-ctx ctx)
      ctx
      (do
        (reset! diag ctx)
        (throw (ex-info "invalid surrogate context:" {:ctx ctx}))))))

(defn ctx-human
  "Return  part of context specific to humans."
  [{:keys [pid cid]}]
  (merge {:responder-role :human} (ensure-interview-agent! pid cid)))

(defn start-human-project!
  "This does a few things to 'catch up' with the surrogate-mode of operation, which alreadys knows what the project is about.
      1) Analyze the response from the :process/warm-up question.
          a) make a project (now you know the real pid).
          b) add scheduling challenge claims.
      2) Add the process-interview-agent.
   Return pid of new project."
  [response]
  (let [warm-up-claims (pan/analyze-warm-up-response response)
        [_ pid] (ru/find-claim '(temp-project-id ?pid) warm-up-claims)
        [_ _ pname] (ru/find-claim '(temp-project-name ?pid ?pname) warm-up-claims)
        pid (db/create-proj-db! {:project/id pid :project/name pname})
        bindings {'?pid pid}
        needed-claims (remove #('#{temp-project-id temp-project-name} (first %)) warm-up-claims)]
      (doseq [claim needed-claims]
        (db/add-claim! pid {:string (-> claim (uni/subst bindings) str)
                            :q-type :process/warm-up
                            :cid :process}))
      pid))

(defn resume-conversation
  "Start the interview loop. :resume-conversation is a dispatch key from client.
   This is called even when PID is :START-A-NEW-PROJECT."
  [{:keys [client-id pid cid] :as ctx}]
  (assert (string? client-id))
  (assert (#{:process :data :resources :optimality} cid))
  (log! :info (str "--------- Resume conversation: ctx:\n" (with-out-str (pprint ctx))))
  (try
    (if (and (not= :process cid) (not (ready-for-discussion? pid cid)))
      (redirect-user-to-discussion client-id cid :process)
      ;; The conversation loop.
      (let [ctx (if (surrogate? pid) (merge ctx (ctx-surrogate ctx)) (merge ctx (ctx-human ctx)))]
        (when-not (db/conversation-done? pid cid)
          (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
          (when @active?
            (when (== 0 (-> (db/get-conversation-eids pid cid) count))
              (-> ctx :pid initial-advice (tell-interviewer ctx)))
            (-> (conversation-history pid cid) (tell-interviewer ctx))
            (loop [cnt 0
                   conversation (q-and-a ctx)] ; Returns a vec of msg objects suitable for db/add-msg.
              (let [response (-> conversation last)]
                (doseq [msg conversation]
                  (when-not (= (:status msg) "DONE")
                    (db/add-msg (merge {:pid pid :cid cid} msg))))
                (cond
                  (> cnt 15)                                  :exceeded-questions-safety-stop
                  (= "DONE" (-> conversation last :status))   (db/assert-conversation-done! pid cid)
                  :else
                  (when @active? ; Keep this despite (when @active? ...) above. Can set to false while running.
                    (ru/analyze-response-meth (merge ctx
                                                     {:response (:text response)
                                                      :question-type (:question-type response)}))
                    (tell-interviewer {:command "INTERVIEWEES-RESPOND"
                                       :response (:text response)
                                       :question-type (:question-type response)}
                                      ctx)
                    (recur (inc cnt)
                           (q-and-a ctx))))))))))
      (finally
        (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

(defn start-conversation
  "Ask first question, create a project and call resume-conversation.
   :start-conversation is a dispatch-key form the client."
  [{:keys [client-id] :as ctx}]
  (ws/send-to-chat {:dispatch-key :tbd-says :client-id client-id :promise? true
                    :text (str "You want to start a new project? This is the right place! "
                               (db/conversation-intros :process))})
  (try
    (let [ctx (merge ctx {:responder-role :human :question-type :process/warm-up :question the-warm-up-type-question})
          conversation (get-an-answer ctx)
          response (-> conversation last :text) ; ToDo: Could be more to it...wants-a-break?.
          _effect! (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
          pid (start-human-project! response)
          [_ _ pname] (ru/find-claim '(project-name ?pid ?pname) (db/get-claims pid))]
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
      (doseq [m conversation]
        (let [{:keys [from text tags question-type status]} m]
          (when-not (= status "DONE") ; ToDo: Set :status "DONE" if they back-out of starting a project.
            (db/add-msg {:pid pid :cid :process :from from :tags tags :text text :question-type question-type}))))
      (db/add-msg {:pid pid
                   :cid :process
                   :from :system
                   :text (str "Great, we'll call your project " pname ".")
                   :tags [:process/warm-up :name-project :informative]})
      ;; This will cause a resume-conversation:
      (ws/send-to-chat {:dispatch-key :load-proj :client-id client-id  :promise? false
                        :new-proj-map {:project/name pname :project/id pid}}))
    (finally
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;;------------------------------------ Starting and stopping --------------------------------
(defn init-iviewers!
  []
  (ws/register-ws-dispatch :start-conversation start-conversation)
  (ws/register-ws-dispatch :resume-conversation resume-conversation))

(defstate iviewers
  :start (init-iviewers!))
