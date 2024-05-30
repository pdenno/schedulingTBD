(ns scheduling-tbd.llm
  "Built-in functions implementing the expression language of the mapping language.
   Functions with names beginning with a '$' are available to the user (e.g. $filter).
   Others (such as bi/key and bi/strcat) implement other parts of the expression language
   but are not available directly to the user except through the operators (e.g. the dot, and &
   respectively for navigation to a property and concatenation).

   N.B. LSP annotates many operators here as '0 references'; of course, they are used."
  (:require
   [clojure.edn                  :as edn]
   [clojure.spec.alpha           :as s]
   [datahike.api                 :as d]
   [scheduling-tbd.sutil         :refer [api-credentials llm-provider markdown2html connect-atm]]
   [scheduling-tbd.util          :refer [now]]
   [scheduling-tbd.web.websockets :as ws]
   [camel-snake-kebab.core       :as csk]
   [clojure.pprint               :refer [cl-format]]
   [clojure.string               :as str]
   [mount.core                   :as mount :refer [defstate]]
   [taoensso.timbre              :as log]
   [wkok.openai-clojure.api      :as openai]))

(def ^:diag diag (atom nil))

(def llms-used
  "A map keyed by 'simple' keywords associating a model name recognized by openai. Example {:gpt4 'gpt-4-turbo-preview'...}"
  (atom {}))

(def preferred-llms
  "These names (keywords) are the models we use, and the models we've been using lately."
  {:openai {:gpt-3.5     "gpt-3.5-turbo-0125"
            :gpt-4       "gpt-4-turbo-2024-04-09" ; "gpt-4o" "gpt-4o-2024-05-13" "gpt-4-0125-preview"
            :davinci     "davinci-002"}
   :azure  {:gpt-3.5     "gpt-3.5-turbo-0125"}})

(defn pick-llm
  "Return a string recognizable by OpenAI naming a model of the class provide.
   For example (pick-llm :gpt-4) --> 'gpt-4-0125-preview'."
  ([k] (pick-llm k llm-provider))
  ([k provider]
   (assert (contains? (get @llms-used provider) k))
   (-> @llms-used provider k)))

(defn query-llm
  "Given the vector of messages that is the argument, return a string (default)
   or Clojure map (:raw-string? = false) that is read from the string created by the LLM."
  [messages & {:keys [model-class raw-text?] :or {model-class :gpt-4 raw-text? true}}]
  (let [res (-> (openai/create-chat-completion {:model (pick-llm model-class)
                                                :messages messages}
                                               (api-credentials llm-provider))
                :choices
                first
                :message
                :content
                (cond-> (not raw-text?) edn/read-string))]
    (if (or (map? res) (string? res))
      res
      (throw (ex-info "Did not produce a map nor string." {:result res})))))

;;; When you update this code, call this again:
;;; (ws/register-ws-dispatch  :ask-llm llm/llm-directly)
(defn llm-directly
  "User can ask anything outside of session by starting the text with 'LLM:.
   This is a blocking call since the caller is a websocket thread and it responds with ws/send-to-chat."
  [{:keys [client-id question]}]
  (assert (string? client-id))
  (assert (string? question))
  (log/info "llm-directly:" question)
  (let [chat-args {:client-id client-id :dispatch-key :tbd-says :promise? false}]
    (try
      (let [res (-> (query-llm [{:role "system"    :content "You are a helpful assistant."}
                                {:role "user"      :content question}])
                    markdown2html)]
        (if (nil? res)
          (throw (ex-info "Timeout or other exception." {}))
          (ws/send-to-chat (-> chat-args
                               (assoc :msg res)
                               (assoc :time (now))))))
      (catch Exception e
        (log/error "Failure in llm-directly:" e)
        (ws/send-to-chat (assoc chat-args :msg "There was a problem answering that."))))))

;;; ------------------------------- naming variables --------------------------------------
(def good-var-partial
    [{:role "system"    :content "You are a helpful assistant."}
     {:role "user"      :content "Produce a Clojure map containing one key, :name, the value of which is a camelCase string used to name a programming variable and matching the requirements described in the text in square brackets."}

     {:role "user"      :content "[less than 15 chars for how long a task will take]"}
     {:role "assistant" :content "{:name \"taskDuration\"}"}

     {:role "user"      :content "[less than 10 chars for the number of users]"}
     {:role "assistant" :content "{:name \"numUsers\"}"}

     {:role "user"      :content "[less than 10 chars for an array of tasks indexed by the resource doing the task and day in which it is doing it]"}
     {:role "assistant" :content "{:name \"busyWith\"}"}

     {:role "user"      :content "[less than 10 chars for an array of tasks indexed by the resource doing the task and day in which it is doing it]"}
     {:role "assistant" :content "{:name \"resBusyWithOnDay\"}"}
     {:role "user"      :content "WRONG: \"resBusyWithOnDay\" contains more contains 10 chars."}])

(defn name-var
  "Return a Clojure map {:name <some-string>} where <some-string> is a good name for a variable matching the requirements defined
   by the clojure map provided.
   The argument map has the following keys:
     :purpose - describes how the variable will be used; this usually beings 'less than n chars for ....'.
     :string-type - one of :camelCase, :kebab-case :snake_case,
     :capitalize? - either true or false."
  [{:keys [purpose string-type capitalize?] :or {string-type :camelCase}}]
  (when purpose
    (let [prompt (conj good-var-partial
                       {:role "user"
                        :content (cl-format nil "[~A]" purpose)})
          res (query-llm prompt {:model-class :gpt-4 :raw-text? false})]
      (if-let [var-name (:name res)]
        (cond-> var-name
          (= :kebab-case string-type) (csk/->kebab-case-string)
          (= :snake-case string-type) (csk/->snake_case_string)
          capitalize?                 (str/capitalize))
        (throw (ex-info "No :name provided, or :name is not a string." {:res res}))))))

(defn list-openai-models [] (->> (openai/list-models (api-credentials :openai)) :data (sort-by :created) reverse))

(defn select-llm-models-openai
  []
  (let [models (list-openai-models)]
    (swap! llms-used
           (fn [atm] (assoc atm :openai
                            (reduce (fn [res k]
                                      (let [preferred (-> preferred-llms :openai k)
                                            chosen (or (some #(when (= preferred (:id %)) (:id %)) models)
                                                       (let [pattern (re-pattern (str "^" (name k) ".*"))]
                                                         (some #(when (re-matches pattern (:id %)) (:id %)) models)))]
                                        (assoc res k chosen)))
                                    {}
                                    (-> preferred-llms :openai keys)))))
    (when-not (every? #(-> @llms-used :openai %) (-> preferred-llms :openai keys))
      (throw (ex-info "No openai-model found for a required model type." {:models (:openai @llms-used)})))
    (doseq [[k mod] (-> @llms-used :openai)]
      (when (not= mod (-> preferred-llms :openai k))
        (log/warn "Preferred model for" k "was not available. Chose" mod)))))

(defn select-llm-models-azure []
  (swap! llms-used #(assoc % :azure {:gpt-3.5 "mygpt-35"})))

(defn select-llm-models!
  "Set the open-ai-models atom to models in each class"
  []
  (select-llm-models-openai)
  (select-llm-models-azure))

;;;------------------------------------- assistants and threads  -------------------------------------------
(s/def ::name string?)
(s/def ::instructions string?)
(s/def ::assistant-args (s/keys :req-un [::name ::instructions]))
(defn make-assistant
  "Create an assistant with the given parameters. Provide a map like with following keys:
     :name - a string, no default.
     :instructions - a string, the systems instructions; defaults to 'You are a helpful assistant.',
     :model - a string; defaults to whatever is the chosen 'gpt-4',
     :tools - a vector containing maps; defaults to [{:type 'code_interpreter'}]."
  [& {:keys [name model-class instructions tools metadata]
      :or {model-class :gpt-4
           metadata {}
           tools [{:type "code_interpreter"}]} :as obj}]
  (s/valid? ::assistant-args obj)
  (openai/create-assistant {:name         name
                            :model        (pick-llm model-class)
                            :metadata     metadata
                            :instructions instructions
                            :tools        tools} ; Will be good for csv and xslx, at least.
                           (api-credentials llm-provider)))

(defn make-thread
  [& {:keys [assistant-id metadata] :or {metadata {}}}]
  (openai/create-thread {:assistant_id assistant-id
                         :metadata metadata}
                        (api-credentials llm-provider)))

(defn openai-messages-matching
  "Argument is a vector of openai messages from an assistant.
   Return a vector of messages matching the argument conditions.
     :role  - #{'assistant', 'user'}
     :text  - A complete match on the (-> % :content first :text value).
     :after - Messages with :created_at >= than this."
  [msg-list {:keys [role text after]}]
  (cond->> msg-list
    role     (filterv #(= role (:role %)))
    text     (filterv #(= text (-> % :content first :text :value)))
    after    (filterv #(<= after (:created_at %)))))

(defn response-msg
  "Return the text that is response to the argument question."
  [question msg-list]
  (when-let [question-time (-> (openai-messages-matching msg-list {:role "user" :text question}) first :created_at)]
    (when-let [responses (openai-messages-matching msg-list {:role "assistant" :after question-time})]
      (->> responses (sort-by :created_at) first :content first :text :value))))

;;; You can also use this with role 'assistant', but the use cases might be a bit esoteric. (I can't think of any.)
;;; https://platform.openai.com/docs/api-reference/messages/createMessage#messages-createmessage-role
(defn query-on-thread
  "Create a message for ROLE on the project's (PID) thread and run it, returning the result text.
    aid      - assistant ID (the OpenAI notion)
    tid      - thread ID (ttheOpenAI notion)
    role     - #{'user' 'assistant'},
    query-text - a string.
   Returns text but uses promesa internally to deal with errors."
  [& {:keys [tid aid role query-text timeout-secs] :or {timeout-secs 60 role "user"} :as _obj}] ; "user" when "assistant" is surrogate.
  ;(log/info "query-on-thread: query-text =" query-text)
  (assert (#{"user" "assistant"} role))
  (assert (string? query-text))
  (assert (not-empty query-text))
  (let [creds (api-credentials llm-provider)
        ;; Apparently the thread_id links the run to msg.
        _msg (openai/create-message {:thread_id tid :role role :content query-text} creds)
        ;; https://platform.openai.com/docs/assistants/overview?context=without-streaming
        ;; Once all the user Messages have been added to the Thread, you can Run the Thread with any Assistant.
        run (openai/create-run {:thread_id tid :assistant_id aid} creds)
        timestamp (inst-ms (java.time.Instant/now))
        timeout   (+ timestamp (* timeout-secs 1000))]
    (loop [now timeout]
      (Thread/sleep 1000)
      (let [r (openai/retrieve-run {:thread_id tid :run-id (:id run)} creds)
            msg-list (-> (openai/list-messages {:thread_id tid :limit 20} creds) :data) ; ToDo: 20 is a guess.
            response (response-msg query-text msg-list)]
        (cond (> now timeout)                        (throw (ex-info "query-on-thread: Timeout:" {:query-text query-text})),

              (and (= "completed" (:status r))
                   (not-empty response))              (markdown2html response),

              (and (= "completed" (:status r))
                   (empty? response))                 (throw (ex-info "query-on-thread empty response:" {:status (:status r)})),


              (#{"expired" "failed"} (:status r))     (throw (ex-info "query-on-thread failed:" {:status (:status r)})),

              :else                                   (recur (inst-ms (java.time.Instant/now))))))))

(defn ^:diag list-thread-messages
  "Return a vector of maps describing the discussion that has occurred on the thread in the order it occurred"
  ([tid] (list-thread-messages tid 20))
  ([tid limit]
   (->> (openai/list-messages {:thread-id tid :limit limit} (api-credentials llm-provider))
        :data
        (map #(dissoc % :file_ids :run_id :assistant_id :thread_id :id :metadata :object :annotations))
        (map #(assoc % :text (-> % :content first :text :value)))
        (map #(dissoc % :content))
        (sort-by :created_at)
        (map  (fn [m] (update m :created_at #(str (java.util.Date. (* 1000 %))))))
        (mapv (fn [m] (update m :role keyword))))))

(defn ^:diag get-assistant-openai
  "Retrieve the assistant object from OpenAI using its ID, a string you can
   find on the list-assistants, or while logged into the OpenAI website."
  [id]
  (openai/retrieve-assistant {:assistant_id id} (api-credentials llm-provider)))

(defn list-assistants-openai [] (openai/list-assistants {:limit 30} (api-credentials llm-provider)))

(defn delete-assistant-openai
  "Delete the assistant having the given ID, a string."
  [id]
  (openai/delete-assistant {:assistant_id id} (api-credentials llm-provider)))

(defn ^:diag delete-assistants-openai!
  "Delete from the openai account all assistants that match the argument filter function.
   The default function check for metadata :usage='surrogate'."
  ([] (delete-assistants-openai! #(#{"surrogate" "agent"} (-> % :metadata (get :usage)))))
  ([selection-fn]
   (doseq [a (->> (list-assistants-openai) :data (filterv selection-fn))]
     (log/info "Deleting assistant" (:name a) (:id a))
     (delete-assistant-openai (:id a)))))

;;; (ws/register-ws-dispatch  :run-long llm/run-long)
(defn ^:diag run-long
  "Diagnostic for exploring threading/blocking with websocket."
  [& _]
  (loop [cnt 0]
    (log/info "Run-long: cnt =" cnt)
    (Thread/sleep 5000) ; 5000 * 30, about 4 minutes.
    (when (< cnt 20) (recur (inc cnt)))))

(defn ^:diag throw-it
  [& _]
  (throw (ex-info "Just because I felt like it." {})))

#_(def theory-bridging-agent
  {:id :theory-bridging-agent
   :instruction
   (str "You are a helpful assistant that interprets our requests and output results as a vector of Clojure maps. "
        "Your response must be directly readable by the Clojure function 'read-string' to a vector of Clojure maps. "
        "The maps you will produce contain two keys :SENTENCE and :TYPE \n"
        "For each sentence in the input, :SENTENCE is the sentence. \n"
        "For each sentence in the input, :TYPE is a Clojure vector of two keywords, both ...")})

#_(defn ^:diag recreate-system-agents!
  []
  (doseq [{:keys [id instruction training]} [process-agent text-function-agent]]
    (recreate-agent! id instruction training)))

(defn query-agent
  "Make a query to the Clojure agent."
  [agent-id query]
  (assert (#{:process-agent :text-function-agent} agent-id))
  (let [{:keys [aid tid]} (-> (d/q '[:find ?aid ?tid
                                     :keys aid tid
                                     :in $ ?agent-id
                                     :where
                                     [?e :agent/id ?agent-id]
                                     [?e :agent/assistant-id ?aid]
                                     [?e :agent/thread-id ?tid]]
                                   @(connect-atm :system)
                                   agent-id)
                              first)
        result (-> (query-on-thread {:aid aid :tid tid :query-text query})
                   (str/replace #"\s+" " "))
        #_#_[success? useful] (re-matches #"```clojure(.*)```" result)
        #_#_result (if success? useful result)]
    (try (->> (edn/read-string result)
              (mapv (fn [m] (update-keys m #(-> % name str/lower-case keyword)))))
         (catch Exception _e
           (log/error "query-agent failed:" result)
           nil))))

;;;------------------- Starting and stopping ---------------
(defn llm-start []
  (select-llm-models!)
  (ws/register-ws-dispatch :ask-llm llm-directly)
  (ws/register-ws-dispatch :run-long run-long)
  (ws/register-ws-dispatch :throw-it throw-it)
  [:llm-fns-registered-for-ws-dispatch])

(defn llm-stop []
  (reset! llms-used {})
  :llm-stopped)

(defstate llm-tools
  "Initialize llm-tools, so far just registering for llm-directly."
  :start (llm-start)
  :stop  (llm-stop))
