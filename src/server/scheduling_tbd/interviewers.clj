(ns scheduling-tbd.interviewers
    "This provides functions to prune a planning domain and run an interview."
  (:require
   [clojure.datafy            :refer [datafy]]
   [datahike.api              :as d]
   [jsonista.core             :as json]
   [mount.core                :as mount :refer [defstate]]
   [promesa.core              :as p]
   [promesa.exec              :as px]
   [scheduling-tbd.db         :as db]
   [scheduling-tbd.llm        :as llm]
   [scheduling-tbd.surrogate  :as sur]
   [scheduling-tbd.sutil      :as sutil :refer [connect-atm chat-status domain-conversation output-struct2clj]]
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
  [query {:keys [pid surrogate? agent-query client-id domain-id tags]
          :or {tags []} :as ctx}] ; ToDo: How do I implement tags? I suppose I need an agent to classify the question.
  (let [response-text (chat-pair-aux query ctx)]
    (if (string? response-text)
      (let [user-role (if surrogate? :surrogate :human)]
        (db/add-msg pid :system query (conj tags :query))
        (db/add-msg pid user-role response-text (conj tags :response))
        (ws/refresh-client client-id pid :process #_(domain-conversation domain-id)) ; ToDo: fix this.
        response-text)
      (throw (ex-info "Unknown response from operator" {:response response-text})))))

(defn tell-interviewer
  "Send a message to an interviewer agent and wait for response; translate it."
  [msg {:keys [iview-aid iview-tid] :as ctx}]
  (assert (map? msg))
  (let [msg-string (json/write-value-as-string msg)]
    (log/info "Interviewer told:" msg-string)
    (let [res (-> (llm/query-on-thread {:aid iview-aid :tid iview-tid :role "user" :query-text msg-string}) output-struct2clj)]
      (log/info "Interviewer returns:" res)
      res)))

(defn next-cmd
  "Oversee the interview and determine whether the interviewer should just continue doing its thing, {:command :NEXT-QUESTION},
   or whether intervention or stopping is more appropriate.
   The argument interviewer-ack is a 'structured output' response from an interviewer agent."
  [iview-ack {:keys [pid action] :as ctx}] ; interviewer ack
  (let [expert (sur/get-surrogate pid)]
    (case action
      :init? {:command :ANALYSIS-CONCLUDES
              :conclusions (str "1) They make " (:surrogate/subject-of-expertise expert) ". 2) You are talking to surrogate humans (machine agents).")}
      (let [{:keys [status question]} iview-ack]
        (if question ; Interviewer poses a question. Get response from surrogate, inform interviewer
          (let [expert-response (chat-pair question ctx)]
            (log/info "Expert responds:" expert-response)
            (tell-interviewer {:command :HUMAN-RESPONDS :response expert-response} ctx)
            {:command :NEXT-QUESTION}) ; <==== ToDo: Analyze human/surrogate response before doing this.
          {:command :NEXT-QUESTION})))))

(def active? (atom false))

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
      (let [ctx (merge iview-agent {:surrogate? true} ctx)]
        (loop [cnt 0
               cmd (next-cmd nil ctx #_(assoc ctx :action :init?))] ; Might be restarting, not a new surrogate.
          (if (or (> cnt 6) (= cmd :stop))
            :done
            (when @active?
              (let [iview-ack (tell-interviewer cmd ctx)]
                (recur (inc cnt)
                       (next-cmd iview-ack ctx))))))))
      (finally
        (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;;------------------------------------ Starting and stopping --------------------------------
(defn init-iviewers!
  []
  (ws/register-ws-dispatch :resume-conversation-plan resume-conversation))

(defstate iviewers
  :start (init-iviewers!))
