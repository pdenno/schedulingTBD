(ns scheduling-tbd.interviewers
    "This provides functions to prune a planning domain and run an interview."
  (:require
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
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm output-struct2clj elide]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere             :as tel :refer [log!]]
   [taoensso.truss                 :as truss :refer [have]]))

(def diag (atom nil))

;;; (chat-pair-aux {:pid :sur-fountain-pens :surrogate? true :agent-query "Describe your most significant scheduling problem in a few sentences."} {})
(defn chat-pair-aux
  "Run one query/response pair of chat elements with a human or a surrogate.
   Returns promise which will resolve to the original obj argument except:
     1) :agent-query is adapted from the input argument value for the agent type (human or surrogate)
     2) :response is added. Typically its value is a string."
  [{:keys [question sur-aid sur-tid responder-role preprocess-fn tries] :as ctx
    :or {tries 1 preprocess-fn identity}}]
  (reset! diag ctx)
  (s/assert ::chat-pair-context ctx)
  (let [prom (if (= :human responder-role)
               (ws/send-to-chat (-> ctx                        ; This cannot timeout.
                                    (assoc :msg question)
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

(s/def ::chat-pair-context (s/and (s/keys :req-un [::pid ::responder-role ::client-id ::sur-aid ::sur-tid ::question ::question-type]
                                          :opt-un [::tags ::topic])
                                  #(or (and (= :surrogate (:responder-role %)) (contains? % :sur-aid) (contains? % :sur-tid))
                                       (= :human (:responder-role %)))))
(s/def ::pid keyword?)
(s/def ::responder-role keyword?)
(s/def ::client-id string?)
(s/def ::sur-aid string?)
(s/def ::sur-tid string?)
(s/def ::question string?)
(s/def ::question-type keyword?)

;;; Optional
(s/def ::tags (s/coll-of keyword?))
(s/def ::topic #(#{:process :resource :data} %))

(defn chat-pair
  "Call to run the chat and put the query and response into the project's database.
   Updates the UI and returns response text."
  [{:keys [pid responder-role client-id tags topic question question-type]
    :or {tags [] topic :process} :as ctx}] ; ToDo: How do I implement tags? I suppose I need an agent to classify the question. Also no default for topic?
  (s/assert ::chat-pair-context ctx)
  (let [response-text (chat-pair-aux ctx)
        q-type (keyword question-type)]
    (if (string? response-text)
      (if (llm/immoderate? response-text)
        (ws/send-to-chat (assoc ctx :msg "I won't respond to that."))
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
            (ws/refresh-client client-id pid topic)
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
  [cmd {:keys [iview-aid iview-tid] :as ctx}]
  (s/assert ::interviewer-cmd cmd)
  (log! :info (-> (str "Interviewer told: " cmd) (elide 150)))
  (let [cmd-string (json/write-value-as-string cmd)
        res (-> {:aid iview-aid :tid iview-tid :role "user" :query-text cmd-string}
                llm/query-on-thread
                output-struct2clj)
        res (if (contains? res :question-type) (update res :question-type keyword) res)]
    (log! :info (-> (str "Interviewer returns: " res) (elide 150)))
    (merge ctx res)))

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
   Resturns {:command 'HUMAN-RESPONDS' :response <some-text> :question-type <a keyword>}."

  [question {:keys [question-type] :as ctx}]
  (s/assert ::chat-pair-context ctx)
  (let [save-ctx ctx
        ask-continue "Shall we continue with the interview?"
        iviewee-response-txt (chat-pair ctx)]
    (if (answers-question? question iviewee-response-txt)
      {:command "HUMAN-RESPONDS" :response iviewee-response-txt :question-type question-type}
      ;; Does not answer the question. ToDo: This hasn't been tested; needs a human.
      (loop [resp iviewee-response-txt]   ; <=================================================== HUH?
        (chat-pair ctx)
        (Thread/sleep 5000)
        (let [continue? (chat-pair (merge ctx {:question ask-continue
                                               :question-type :prompt-focus}))
              {:keys [yes? more]} (answers-yes-or-more ask-continue continue?)]
          (cond more (recur more)
                yes? (loop-for-answer question save-ctx)
                :else (throw (ex-info "I'm getting lost in the conversation!" {}))))))))

(defn already-answered?
  "Return a set of what has already been answered."
  [pid cid]
  (-> (d/q '[:find [?q-type ...]
             :where [_ :message/question-name ?q-type]]
           @(connect-atm pid))
      set))

;;; ToDo: It isn't obvious that the interview agent needs to see this. Only useful when the thread was destroyed?
(defn prior-responses
  "Construct a prior-response command structure for the argument project.
   This tells the interview agent what has already been asked. If the thread hasn't been deleted
   (i.e. by OpenAI, because it is more than 30 days old) then the agent knows what has already been
   asked. Thus, this covers that possibility. We also start fresh interviews with this information,
   just to make sure."
  [pid cid]
  (let [msgs (->> (db/get-conversation pid cid)
                  (filter #(= :surrogate (:message/from %)))
                  (mapv #(reduce-kv (fn [m k v] (cond (= k :message/content)         (assoc m :answer v)
                                                      (= k :message/question-name)   (assoc m :question-name (name v))
                                                      :else m))
                                    {} %)))]
    {:command "PRIOR-RESPONSES"
     :already-answered (vec (already-answered? pid cid))
     :responses msgs}))

(defn off-course-do-warm-up
  [ctx]
  (log! :warn "off-course: Didn't ask a warm-up type question!")
  (tell-interviewer {:command "COURSE-CORRECTION"
                     :advice (str "There are no questions in already-answered of the PRIOR-RESPONSES command we are sent. "
                                  "You should have responded with the question-type = \"warm-up\" (and a warm-up question, etc.). "
                                  "Next time we send you a SUPPLY-QUESTION command, provide a warm-up type question.")}
                    ctx)
  (let [{:keys [question-type] :as resp} (tell-interviewer {:command "SUPPLY-QUESTION"} ctx)]
    (if (= :warm-up question-type)
      resp
      (do (log! :warn (str "off-course: STILL didn't ask a warm-up type question! question-type =" question-type))
          {:question-type :warm-up
           :question "What do you make and what is your biggest scheduling challenge?"}))))

(def process-flow-shop-questions
  "All questions that should be asked for a flow-shop. These are ordered, thus a vector."
  [:warm-up
   :work-type
   :production-location
   :production-motivation
   :production-system-type
   :process-steps
   :process-durations
   :process-ordering])

(defn off-course-not-done
  "This is called when interviewer sends {:status 'DONE'} but it wasn't
   We give it a course correction and hope checks that it returns the right thing."
  [{:keys [pid cid] :as ctx}]
  (log! :warn "off-course: Returning 'DONE' when there is more to be done.")
  (let [answered? (already-answered? pid cid)
        ask-next (have keyword? (some #(when (not (answered? %)) %) process-flow-shop-questions))]
    (have [:ks= #{:status}]
          (tell-interviewer {:command "COURSE-CORRECTION"
                             :advice (str "You are not done."
                                          "Next time we send you a SUPPLY-QUESTION command "
                                          (name ask-next) ".")}
                            ctx))
    (have [:ks= #{:question :question-type :status}]
          (tell-interviewer {:command "SUPPLY-QUESTION"} ctx))))

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

(defn off-course?
  "Check whether the response from SUPPLY-QUESTION is appropriate and if not, send a COURSE-CORRECTION
   and ask for another. This is simply based on commonly experienced problems."
  [{:keys [question-type status pid cid] :as ctx}]
  (let [prior (prior-responses pid cid)]
    (cond  (and (= status "DONE") (not (done? pid cid)))     (off-course-not-done ctx)
           :else
           (case cid
             :process (cond (and (-> prior :already-answered empty?)
                                 (not= :warm-up question-type))        (off-course-do-warm-up ctx)
                            :else false)
             :data      false ; NYI
             :resource  false)))) ; NYI

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
   :cid :process,
   :dispatch-key :resume-conversation-plan,               ; probably not useful.
   :responder-role :surrogate}                            ; "responder" is a kind of interviewee :surrogate or :human

(defn ctx-surrogate
  "Return context updated with surrogate info"
  [{:keys [pid cid] :as ctx}]
  (let [sur (db/get-surrogate-agent-info pid)]
    (-> ctx
        (merge (interview-agent cid pid))
        (assoc :responder-role :surrogate)
        (assoc :sur-aid (:surrogate/assistant-id sur))
        (assoc :sur-tid (:surrogate/thread-id sur)))))

;;; ToDo: Check that they are on :process. Put them on process if they are not there.
(defn ctx-human
  "Return context updated with surrogate info"
  [{:keys [pid cid] :as ctx}]
  (-> ctx
      (merge (interview-agent cid pid))
      (assoc :responder-role :surrogate)))

(defn start-human-project
  "Return context updated for human by creating a project for the human." ; ToDo: The idea of human-project isn't quite right!
  [ctx {:keys [use-this-answer]}]
    ;; Add a (s/valid?  here.  ; <=========================
  (let [ctx (ctx-human ctx)
        response (or use-this-answer (loop-for-answer the-warm-up-type-question ctx))]
    (let [res (ru/analyze-response-meth {:question-type :warm-up :response response})]
      #_(tell-interviewer {:command "HUMAN-RESPONDS" :response response} ctx) ; <================= when done.
      ;; More updating of ctx from creation of the project.
    ctx)))

(defn start-surrogate-project
  "Return context updated for surrogate usage. Project was already made because resume-conversation was called by
  sur/start-surrogate which is a web-socket handler for :start-surrogate. That sends a :load-proj to the app,
  which provides :proj/id on which we can then call resume-conversation."
  [ctx]
  (let [ctx (ctx-surrogate ctx)]
    ;; Add a (s/valid?  here.  ; <=========================
    ctx))

(defn resume-conversation
  "Start the interview loop. :resume-conversation-plan is a dispatch key from client.
   This is called even when PID is :START-A-NEW-PROJECT."
  [{:keys [client-id pid cid] :as ctx}]
  (assert (string? client-id))
  (try
    (let [cid (or cid ; One of #{:process :resource :data} currently.
                      (d/q '[:find ?cid . :where [_ :project/current-conversation ?cid]] @(connect-atm pid)))
          {:keys [pid] :as ctx} (if (= :START-A-NEW-PROJECT pid) (start-human-project ctx) (start-surrogate-project ctx))]
      ;; The conversation loop.
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
      (when @active?
        (when (== 0 (-> (db/get-conversation-eids pid cid) count))
          (-> ctx initial-advice (tell-interviewer ctx))) ; <============================================== Start here.
        (-> (prior-responses pid cid) (tell-interviewer ctx))
        (loop [cnt 0
               response (q-and-a ctx)]
          (if (or (= "DONE" (:status response)) (> cnt 10))
            :interview-completed
            (when @active? ; Keep this despite (when @active? ...) above. Can set to false while running.
              (tell-interviewer response ctx) ; This is a HUMAN-RESPONDS
              (ru/analyze-response-meth (merge ctx response))
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
