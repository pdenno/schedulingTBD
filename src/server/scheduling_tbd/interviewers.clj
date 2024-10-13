(ns scheduling-tbd.interviewers
    "This provides functions to prune a planning domain and run an interview."
  (:require
   [clojure.datafy            :refer [datafy]]
   [clojure.spec.alpha        :as s]
   [datahike.api              :as d]
   [jsonista.core             :as json]
   [mount.core                :as mount :refer [defstate]]
   [promesa.core              :as p]
   [promesa.exec              :as px]
   [scheduling-tbd.db         :as db]
   [scheduling-tbd.llm        :as llm]
   [scheduling-tbd.surrogate  :as sur]
   [scheduling-tbd.sutil      :as sutil :refer [connect-atm output-struct2clj]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.timbre           :as log]))

(def diag (atom nil))

(defn interview-agent
  "Return a map with project agent info (aid, tid). The agent is shared by all projects, but the threads are project-specific.
   If a thread has not yet been created for this project, this creates it and stores that in the DB before returning the info."
  [conv-id pid]
  (assert (#{:process :resource :data} conv-id))
  (let [agent-type (-> conv-id name (str "-interview-agent") keyword)
        interview-agent-atm (atom (or (db/get-agent :base-type agent-type :pid pid :db-attrs? true)
                                      (db/get-agent :base-type agent-type :db-attrs? true)))]
    (when-not (:agent/thread-id @interview-agent-atm)
      (let [aid (:agent/assistant-id @interview-agent-atm)
            tid (:id (llm/make-thread {:assistant-id aid :llm-provider :openai :metadata {:usage :project-agent}}))]
        (swap! interview-agent-atm #(assoc % :agent/thread-id tid))
        (let [eid (d/q '[:find ?eid . :where [?eid :project/id _]] @(connect-atm pid))]
          (d/transact (connect-atm pid) {:tx-data [{:db/id eid :project/agents (dissoc @interview-agent-atm :db/id)}]}))))
    ;; We don't want to pass around DB attributes, agent/thread-id, assistant-id are ambiguous.
    (reduce-kv (fn [m k v]
                 (cond (= k :agent/thread-id) (assoc m :iview-tid v)
                       (= k :agent/assistant-id) (assoc m :iview-aid v)
                       :else m))
               {}
               @interview-agent-atm)))

;;; (chat-pair-aux {:pid :sur-fountain-pens :surrogate? true :agent-query "Describe your most significant scheduling problem in a few sentences."} {})
(defn chat-pair-aux
  "Run one query/response pair of chat elements with a human or a surrogate.
   Returns promise which will resolve to the original obj argument except:
     1) :agent-query is adapted from the input argument value for the agent type (human or surrogate)
     2) :response is added. Typically its value is a string."
  [query {:keys [pid surrogate? preprocess-fn tries] :as ctx
          :or {tries 1 preprocess-fn identity}}]
  (let [prom (if surrogate?
               (let [{:surrogate/keys [assistant-id thread-id]} (sur/get-surrogate pid)]
                 (px/submit! (fn []
                               (try
                                 (llm/query-on-thread :tid thread-id            ; This can timeout.
                                                      :aid assistant-id
                                                      :query-text query
                                                      :tries tries
                                                      :preprocess-fn preprocess-fn)
                                 (catch Exception e {:error e})))))
               (ws/send-to-chat (-> ctx
                                    (assoc :msg query)
                                    (assoc :promise? true)
                                    (assoc :dispatch-key :tbd-says))))] ; This cannot timeout.
    (-> prom
       (p/catch (fn [e]
                  (reset! diag (datafy e))
                  (log/error "Failed in chat-pair-aux:" (datafy e))))
       p/await)))

(defn chat-pair
  "Call to run the chat and put the query and response into the project's database.
   Returns the argument object with :response set to some text.
   Note that this also updates the UI (ws/refresh-client)"
  [query {:keys [pid surrogate? client-id tags topic]
          :or {tags [] topic :process} :as ctx}] ; ToDo: How do I implement tags? I suppose I need an agent to classify the question.
  (let [response-text (chat-pair-aux query ctx)]
    (if (string? response-text)
      (let [user-role (if surrogate? :surrogate :human)]
        (db/add-msg pid :system query (conj tags :query))
        (db/add-msg pid user-role response-text (conj tags :response))
        (ws/refresh-client client-id pid topic)
        response-text)
      (throw (ex-info "Unknown response from operator" {:response response-text})))))

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
  "Send a command to an interviewer agent and wait for response; translate it."
  [cmd {:keys [iview-aid iview-tid] :as _ctx}]
  (s/assert ::interviewer-cmd cmd)
  (log/info "Interviewer told:" cmd)
  (let [cmd-string (json/write-value-as-string cmd)
        res (-> {:aid iview-aid :tid iview-tid :role "user" :query-text cmd-string}
                llm/query-on-thread
                output-struct2clj)]
    (log/info "Interviewer returns:" res)
    res))

;;; ToDo: Would be different for different interviews. For example, the resource interview would want to see the process interview.
(defn initial-advice
  "Return in an :ANALYSIS-CONCLUDES command map a list of conclusions used to set the context of the conversation just before
   the interviewer agent starts interviewing. This might, for example, tell the interviewer what the interviewee manufactures
   and whether the interviewee is an actual human or a surrogate."
  [expert]
  {:command :ANALYSIS-CONCLUDES
   :conclusions (str "1) They make "
                     (:surrogate/subject-of-expertise expert) ". "
                     "2) You are talking to surrogate humans (machine agents).")})

(defn answers-question?
  "Return true if the answer to the Q/A pair appears to answer the question."
  [_q-txt _a-txt]
  true) ; ToDo: Write an agent.

(defn answers-yes?
  "Return true if the answer (a-txt) seems to answer yes to whatever I asked (q-txt)."
  [_q-txt _a-txt]
  true) ; ToDo: Write an agent.

(defn response-category
  "Return a keyword indicating the class of the interviewee response to the interviewer's prior statement."
  [_viewer-txt _viewee-txt]
  :other) ; ToDo: Write an agent.

;;; This could get weird. Maybe I don't want to keep asking the same question.
;;; What would be ideal? Waiting a minute an ask "Shall we continue the interview?"
(defn loop-for-answer
  "Run a discussion with the experts that does not answer the argument question.
   Return the answer once it is responsive to the question."
  [question ctx]
  (loop [viewee-response-txt (chat-pair question ctx)]
    (if (answers-question? question viewee-response-txt)
      {:question question :answer viewee-response-txt}
      (recur :NYI))))

(defn next-question
  "Call loop-for-answer until the answer is responsive to a question that we obtain from the interviewer here.
   Return the Q/A pair or {:status 'DONE'}."
  [ctx]
  (let [{:keys [question] :as res} (tell-interviewer {:command "SUPPLY-QUESTION"} ctx)]
    (if question ; Interviewer poses a question. Loop through chat-pair until it looks like an answer.
      (loop-for-answer question ctx)
      res)))

(def ^:diag active? (atom false))

(defn resume-conversation
  "Start the interview loop. :resume-conversation-plan is a dispatch key from client.
   This is called even for where PID is :START-A-NEW-PROJECT."
  [{:keys [client-id pid conv-id] :as ctx}]
  (assert (string? client-id))
  (try
    (let [conv-id (or conv-id ; One of #{:process :resource :data} currently.
                      (d/q '[:find ?conv-id . :where [_ :project/current-conversation ?conv-id]] @(connect-atm pid)))
          iview-agent (interview-agent conv-id pid)]
      ;; The conversation loop.
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
      (when (== 0 (-> pid (db/get-conversation-eids conv-id) count))
        (-> pid sur/get-surrogate initial-advice (tell-interviewer ctx)))
      (let [ctx (merge iview-agent {:surrogate? true} ctx)] ; ToDo: Not always surrogate?=true.
        (loop [cnt 0
               response nil]
          (if (or (= "DONE" (:status response)) (> cnt 16))
            :done
            (when @active?
              (recur (inc cnt)
                     (next-question ctx)))))))
      (finally
        (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;;------------------------------------ Starting and stopping --------------------------------
(defn init-iviewers!
  []
  (ws/register-ws-dispatch :resume-conversation-plan resume-conversation))

(defstate iviewers
  :start (init-iviewers!))
