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
   [wkok.openai-clojure.api      :as openai]
   [scheduling-tbd.db            :as db]
   [scheduling-tbd.sutil         :refer [get-api-key resolve-db-id connect-atm]]
   [scheduling-tbd.web.routes.websockets :as ws]
   [camel-snake-kebab.core       :as csk]
   [clojure.pprint               :refer [cl-format]]
   [clojure.string               :as str]
   [mount.core                   :as mount :refer [defstate]]
   [promesa.core                 :as p]
   [taoensso.timbre              :as log]))

(def ^:diag diag (atom nil))

(def openai-models
  "A map keyed by 'simple' keywords associating a model name recognized by openai. Example {:gpt4 'gpt-4-turbo-preview'...}"
  (atom {}))

(def openai-models-preferred
  "These names (keywords) are the models we use, and the models we've been using lately."
  {:gpt-3.5     "gpt-3.5-turbo-0125"
   :gpt-4       "gpt-4-0125-preview"
   :davinci     "davinci-002"})

(defn pick-llm
  "Return a string recognizable by OpenAI naming a model of the class provide.
   For example (pick-llm :gpt-4) --> 'gpt-4-0125-preview'."
  [k]
  (assert (contains? @openai-models k))
  (get @openai-models k))

(defn query-llm
  "Return a Clojure map that is read from the string created by the LLM given the vector of messages that is the argument."
  [messages & {:keys [model-class raw-text?] :or {model :gpt-4} :as other}]
  (reset! diag {:msg messages :other other})
  (if-let [key (get-api-key :llm)]
    (try (let [res (-> (openai/create-chat-completion {:model (pick-llm model-class)
                                                       :messages messages}
                                                      {:api-key key})
                       :choices
                       first
                       :message
                       :content)
               res (cond-> res (not raw-text?) edn/read-string)]
           (if (or (map? res) (string? res))
             res
             (throw (ex-info "Did not produce a map." {:result res}))))
         (catch Throwable e
           (log/error "Call to query-llm failed.")
           (ex-info "OpenAI API call failed." {:message (.getMessage e)})))
    (throw (ex-info "No key for use of LLM API found." {}))))

;;; (llm/llm-start)
(defn llm-directly
  "User can ask anything outside of session by starting the text with 'LLM:."
  [{:keys [client-id question]}]
  (-> (p/future (query-llm [{:role "system"    :content "You are a helpful assistant."}
                            {:role "user"      :content question}]
                           {:raw-text? true}))
      (p/await 20000)
      (p/then #(ws/send-to-chat {:client-id client-id :promise? false :msg-vec [{:msg-text/string %}]}))
      (p/catch (fn [e]
                 (log/error "Failure in llm-directly:")
                 (ws/send-to-chat {:client-id client-id
                                   :promise? false
                                   :msg-vec [{:msg-text/string "There was a problem answering that."}]})))))


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
                        :content (cl-format nil "[~A]" purpose)})]
      (try (let [res (query-llm prompt {:model-class :gpt-4})]
             (if-let [var-name (:name res)]
               (cond-> var-name
                 (= :kebab-case string-type) (csk/->kebab-case-string)
                 (= :snake-case string-type) (csk/->snake_case_string)
                 capitalize?                 (str/capitalize))
               (throw (ex-info "No :name provided, or :name is not a string." {:res res}))))
           (catch Throwable e
             (throw (ex-info "OpenAI API call failed."
                             {:message (.getMessage e)
                              #_#_:details (-> e .getData :body json/read-str)})))))))

(defn select-openai-models!
  "Set the open-ai-models atom to models in each class"
  []
  (if-let [api-key (get-api-key :llm)]
    (let [models (->> (openai/list-models {:api-key api-key}) :data  (sort-by :created) reverse)]
      (reset! openai-models
              (reduce (fn [res k]
                        (let [preferred (get openai-models-preferred k)
                              chosen (or (some #(when (= preferred (:id %)) (:id %)) models)
                                         (let [pattern (re-pattern (str "^" (name k) ".*"))]
                                           (some #(when (re-matches pattern (:id %)) (:id %)) models)))]
                          (assoc res k chosen)))
                      {}
                      (keys openai-models-preferred))))
    (throw (ex-info "Could not find LLM api key." {})))
  (when-not (every? #(get @openai-models %) (keys openai-models-preferred))
    (throw (ex-info "No openai-model found for a required model type." {:models @openai-models})))
  (doseq [[k mod] @openai-models]
    (when (not= mod (get openai-models-preferred k))
      (log/warn "Preferred model for" k "was not available. Chose" mod))))

;;;------------------------------------- assistants ----------------------------------------------------
(s/def ::name string?)
(s/def ::instructions string?)
(s/def ::assistant-args (s/keys :req-un [::name ::instructions]))
(defn make-assistant
  "Create an assistant with the given parameters. Provide a map like with following keys:
     :name - a string, no default.
     :instructions - a string, the systems instructions; defaults to 'You are a helpful assistant.',
     :model - a string; defaults to 'gpt-4-1106-preview',
     :tools - a vector containing maps; defaults to [{:type 'code_interpreter'}]."
  [& {:keys [name model-class instructions tools metadata]
      :or {model-class :gpt-4
           metadata {}
           tools [{:type "code_interpreter"}]} :as obj}]
  (s/valid? ::assistant-args obj)
  (let [key (get-api-key :llm)]
    (openai/create-assistant {:name         name
                              :model        (pick-llm model-class)
                              :metadata     metadata
                              :instructions instructions
                              :tools        tools} ; Will be good for csv and xslx, at least.
                             {:api-key key})))

(defn make-thread
  [& {:keys [assistant-id metadata] :or {metadata {}}}]
  (let [key (get-api-key :llm)]
    (openai/create-thread {:assistant_id assistant-id
                           :metadata metadata}
                          {:api-key key})))

(defn get-thread
  "Get the thread object of the argument PID."
  [pid]
  (let [eid (db/project-exists? pid)]
    (-> (resolve-db-id {:db/id eid}
                       (connect-atm pid)
                       :keep-set #{:project/surrogate :surrogate/thread-str})
        :project/surrogate
        :surrogate/thread-str
        edn/read-string)))

(defn get-assistant
  "Get the thread object of the argument PID."
  [pid]
  (let [eid (db/project-exists? pid)]
    (-> (resolve-db-id {:db/id eid}
                       (connect-atm pid)
                       :keep-set #{:project/surrogate :surrogate/assistant-obj-str})
        :project/surrogate
        :surrogate/assistant-obj-str
        edn/read-string)))

;;; This is entirely because the OpenAI objects have stuff I find distracting!
(def keep-prop? #{:role :content :created_at})
(defn message-salient
  "Return interesting parts of messages from the structure returned from openai/list-messages."
  [msgs]
  (let [_has-more? (:has_more msgs)] ; ToDo: later.
    (->> msgs
         :data
         (mapv (fn [msg] (reduce-kv (fn [m k v] (if (keep-prop? k) (assoc m k v) m)) {} msg)))
         (sort-by :created)
         vec)))

(defn query-on-thread
  "Create a message for ROLE on the project's (PID) thread and run it, returning the result text.
    pid      - The project's ID (keyword),
    role     - #{'user' 'assistant'},
    msg-text - a string."
  [pid role msg-text & {:keys [timeout-secs] :or {timeout-secs 40}}]
  (assert (keyword? pid))
  (assert (#{"user" "assistant"} role))
  (assert (string? msg-text))
  (let [key (get-api-key :llm)
        aid (-> pid get-assistant :id)
        tid (-> pid get-thread :id)
        _msg (openai/create-message ; Apparently the thread_id links the run to msg.
              {:thread_id tid
               :role role
               :content msg-text}
              {:api-key key})
        ;; https://platform.openai.com/docs/assistants/overview?context=without-streaming
        ;; Once all the user Messages have been added to the Thread, you can Run the Thread with any Assistant.
        run (openai/create-run
             {:thread_id tid
              :assistant_id aid}
             {:api-key key})]
    (loop [secs 0]
      (let [r (openai/retrieve-run {:thread_id tid :run-id (:id run)} {:api-key key})]
        (Thread/sleep 1000)
        (cond (> secs timeout-secs)                  (throw (ex-info "query-on-thread: Timeout:" {:msg-text msg-text})),

              (= "completed" (:status r))            (let [[m1 m2] (-> (openai/list-messages {:thread_id tid :limit 2} {:api-key key})
                                                                       message-salient)]
                                                       (if (= msg-text (-> m2 :content first :text :value))
                                                         (-> m1 :content first :text :value)
                                                         (throw (ex-info "query-on-thread: Response not synced to query:" {:m1 m1 :m2 m2})))),

              (#{"expired" "failed"} (:status r))     (throw (ex-info "query-on-thread failed:" {:status (:status r)}))

              :else                                   (recur (inc secs)))))))

(defn ^:diag get-assistant-openai
  "Retrieve the assistant object from OpenAI using its ID, a string you can
   find on the list-assistants, or while logged into the OpenAI website."
  [id]
  (if-let [key (get-api-key :llm)]
    (openai/retrieve-assistant {:assistant_id id}
                               {:api-key key})
    (log/warn "Couldn't get OpenAI API key.")))

(defn list-assistants-openai
  []
  (if-let [key (get-api-key :llm)]
    (openai/list-assistants {:limit 30} {:api-key key})
    (log/warn "Couldn't get OpenAI API key.")))

(defn delete-assistant-openai
  "Delete the assistant having the given ID, a string."
  [id]
  (if-let [key (get-api-key :llm)]
    (openai/delete-assistant {:assistant_id id} {:api-key key})
    (log/warn "Couldn't get OpenAI API key.")))

;;;------------------- Starting and stopping ---------------
(defn llm-start []
  (ws/register-ws-dispatch  :ask-llm llm-directly)
  (select-openai-models!)
  [:ask-directly-registered :opena-models-selected])

(defn llm-stop []
  (reset! openai-models {})
  :llm-stopped)

(defstate llm-tools
  "Initialize llm-tools, so far just registering for llm-directly."
  :start (llm-start)
  :stop  (llm-stop))
