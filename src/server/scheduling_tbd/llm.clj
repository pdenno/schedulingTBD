(ns scheduling-tbd.llm
  "Built-in functions implementing the expression language of the mapping language.
   Functions with names beginning with a '$' are available to the user (e.g. $filter).
   Others (such as bi/key and bi/strcat) implement other parts of the expression language
   but are not available directly to the user except through the operators (e.g. the dot, and &
   respectively for navigation to a property and concatenation)."
  (:require
   [clojure.datafy               :refer [datafy]]
   [clojure.edn                  :as edn]
   [clojure.java.io              :as io]
   [clojure.spec.alpha           :as s]
   [datahike.api                 :as d]
   [scheduling-tbd.sutil         :refer [api-credentials default-llm-provider markdown2html connect-atm]]
   [scheduling-tbd.util          :refer [now]]
   [scheduling-tbd.web.websockets :as ws]
   [clojure.string               :as str]
   [mount.core                   :as mount :refer [defstate]]
   [taoensso.telemere            :as tel :refer [log!]]
   [wkok.openai-clojure.api      :as openai]))

(def ^:diag diag (atom nil))

(def llms-used
  "A map keyed by 'simple' keywords associating a model name recognized by openai. Example {:gpt4 'gpt-4-turbo-preview'...}"
  (atom {}))

(def preferred-llms
  "These names (keywords) are the models we use, and the models we've been using lately."
  {:openai {:gpt         "gpt-4o-2024-08-06"
            :analysis    "o1-preview"
            :mini        "gpt-4o-mini"}
   :azure  {:gpt         "mygpt-4"}}) ; "mygpt-4o" "mygpt4-32k"

(defn pick-llm
  "Return a string recognizable by OpenAI naming a model of the class provide.
   For example (pick-llm :gpt) --> 'gpt-4-0125-preview'."
  ([k] (pick-llm k @default-llm-provider))
  ([k provider]
   (assert (contains? (get @llms-used provider) k))
   (-> @llms-used provider k)))

(s/def ::role string?)
(s/def ::content string?)
(s/def ::query-llm-msg (s/keys :req-un [::role ::content]))

(defn query-llm
  "Given the vector of messages that is the argument, return a string (default)
   or Clojure map (:raw-string? = false) that is read from the string created by the LLM."
  [messages & {:keys [model-class raw-text? llm-provider response-format] :or {model-class :gpt raw-text? true llm-provider @default-llm-provider}}]
  (assert (every? #(s/valid? ::query-llm-msg %) messages))
  (log! :debug (str "llm-provider = " llm-provider))
  (let [res (-> (openai/create-chat-completion {:model (pick-llm model-class llm-provider)
                                                :response_format response-format
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
  (log! :info (str "llm-directly: " question))
  (let [chat-args {:client-id client-id :dispatch-key :tbd-says :promise? false}]
    (try
      (let [res (-> (query-llm [{:role "system"    :content "You are a helpful assistant."}
                                {:role "user"      :content question}])
                    markdown2html)]
        (if (nil? res)
          (throw (ex-info "Timeout or other exception." {}))
          (ws/send-to-chat (-> chat-args
                               (assoc :text res)
                               (assoc :time (now))))))
      (catch Exception e
        (log! :error (str "Failure in llm-directly: " e))
        (ws/send-to-chat (assoc chat-args :text "There was a problem answering that."))))))

(defn list-openai-models
  "List id and create date of all available models.
   BTW, if there is no internet connection, on startup, this will be the first complaint."
  []
  (->> (openai/list-models (api-credentials :openai))
       :data
       (sort-by :created)
       reverse
       (mapv (fn [x] (update x :created #(-> % (* 1000) java.time.Instant/ofEpochMilli str))))))

(defn list-azure-models
  "List id and create date of all available models.
   BTW, if there is no internet connection, on startup, this will be the first complaint."
  []
  (->> (openai/list-models (api-credentials :azure)) :data (sort-by :created) reverse))

(defn select-llm-models-openai
  "Referencing sutil/preferred-llms, pick out one model for each of what we are calling 'model-class'."
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
        (log! :warn (str "Preferred model for" k "was not available. Chose " mod))))))

(defn select-llm-models-azure
  "Since in Azure you have to create the model, this is just hard-coded."
  []
  (swap! llms-used #(assoc % :azure {:gpt-3.5 "mygpt-35" :gpt "mygpt-4"})))

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
     :tools - a vector containing maps, for example [{:type 'code_interpreter'}]."
  [& {:keys [name model-class instructions tools tool-resources metadata llm-provider response-format]
      :or {model-class :gpt
           llm-provider @default-llm-provider
           metadata {:usage :stbd-agent}} :as obj}]
  (s/valid? ::assistant-args obj)
  (openai/create-assistant {:name            name
                            :model           (pick-llm model-class llm-provider)
                            :response_format response-format
                            :metadata        metadata
                            :instructions    instructions
                            :tools           tools
                            :tool_resources  tool-resources}
                           (api-credentials llm-provider)))

;;; Not clear that this does anything! See notes 2024-10-31.
#_(defn update-assistant
  "Update an assistant with a file for file-search."
  [& {:keys [aid fids llm-provider] :or {llm-provider @default-llm-provider}}]
  (assert (string? aid))
  (assert (every? #(string? %) fids))
  (reset! diag (openai/modify-assistant {:assistant_id aid :file_ids fids}
                                        (api-credentials llm-provider))))

(defn make-thread
  [& {:keys [assistant-id metadata llm-provider] :or
      {metadata {:usage :stbd-agent}
       llm-provider @default-llm-provider}}]
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
(defn query-on-thread-aux
  "Create a message for ROLE on the project's (PID) thread and run it, returning the result text.
    aid      - assistant ID (the OpenAI notion)
    tid      - thread ID (ttheOpenAI notion)
    role     - #{'user' 'assistant'},
    query-text - a string.
   Returns text but uses promesa internally to deal with errors."
  [aid tid role query-text timeout-secs llm-provider]
  (assert (string? aid))
  (assert (string? tid))
  (assert (#{"user" "assistant"} role))
  (assert (number? timeout-secs))
  (assert (keyword? llm-provider))
  (assert (and (string? query-text) (not-empty query-text)))
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
        (cond (> now timeout)                        (do (log! :warn "Timeout")
                                                         (throw (ex-info "query-on-thread: Timeout:" {:query-text query-text}))),

              (and (= "completed" (:status r))
                   (not-empty response))              (markdown2html response),

              (and (= "completed" (:status r))
                   (empty? response))                 (do (log! :warn "empty response")
                                                          (throw (ex-info "query-on-thread empty response:" {:status (:status r)}))),


              (#{"expired" "failed"} (:status r))     (do (log! :warn (str "failed/expired last_error = " (:last_error r)))
                                                          (throw (ex-info "query-on-thread failed:" {:status (:status r)}))),

              :else                                   (recur (inst-ms (java.time.Instant/now))))))))

(defn query-on-thread
  "Wrap query-on-thread-aux to allow multiple tries at the same query.
    :test-fn a function that should return true on a valid result from the response. It defaults to a function that returns true.
    :preprocesss-fn is a function that is called before test-fn; it defaults to identity."
  [& {:keys [aid tid role query-text timeout-secs llm-provider test-fn preprocess-fn]
      :or {test-fn (fn [_] true), preprocess-fn identity
           llm-provider @default-llm-provider
           timeout-secs 60
           role "user"} :as obj}]
  (let [obj (cond-> obj ; All recursive calls will contains? :tries.
              (or (not (contains? obj :tries))
                  (and (contains? obj :tries) (-> obj :tries nil?))) (assoc :tries 1))]
    (assert (< (:tries obj) 10))
    (if (> (:tries obj) 0)
      (try (let [raw (reset! diag (query-on-thread-aux aid tid role query-text timeout-secs llm-provider))
                 res (preprocess-fn raw)]
             (if (test-fn res) res (throw (ex-info "Try again" {:res res}))))
           (catch Exception e
             (let [d-e (datafy e)]
               (log! :warn (str "query-on-thread failed (tries = " (:tries obj) "): "
                                (or (:cause d-e) (-> d-e :via first :message)))))
             (query-on-thread (update obj :tries dec))))
      (log! :warn "Query on thread exhausted all tries.")))) ; ToDo: Or throw?

(defn ^:diag list-thread-messages
  "Return a vector of maps describing the discussion that has occurred on the thread in the order it occurred"
  ([tid] (list-thread-messages tid 20 {:llm-provider @default-llm-provider}))
  ([tid limit] (list-thread-messages tid limit {:llm-provider @default-llm-provider}))
  ([tid limit {:keys [llm-provider] :or {llm-provider @default-llm-provider}}]
   (->> (openai/list-messages {:thread-id tid :limit limit} (api-credentials llm-provider))
        :data
        (map #(dissoc % :file_ids :run_id :assistant_id :thread_id :id :metadata :object :annotations))
        (map #(assoc % :text (-> % :content first :text :value)))
        (map #(dissoc % :content))
        (sort-by :created_at)
        (map  (fn [m] (update m :created_at #(str (java.util.Date. (* 1000 %))))))
        (mapv (fn [m] (update m :role keyword))))))

(defn ours?
  "Returns true if assistant metadata :usage key indiciates it is ours."
  [usage]
  (#{"stbd-surrogate" "surrogate" "agent" "stbd-agent"} usage))

;;; When you call list-assistants with :after or with :limit=1, you get an object with keys (:object :data :first_id :last_id :has_more)
;;; :after takes the id of an assistant (a string).
(defn list-assistants
  "Return a vector of all assistants (their id) on the account that are related to this project (stbd).
   The elements are sorted by creating timestamp, most recent first."
  [& {:keys [llm-provider] :or {llm-provider @default-llm-provider}}]
  (let [creds (api-credentials llm-provider)]
    (loop [ours []
           response (openai/list-assistants {:limit 1} creds)]
      (if (:has_more response)
        (recur (into ours (->> response :data (filter #(-> % :metadata :usage ours?)) (map :id)))
               (openai/list-assistants {:limit 99 :after (:last_id response)} creds)) ; 99 is max in openai.
        (into ours (->> response :data (filter #(-> % :metadata :usage ours?)) (map :id)))))))

(defn delete-assistant!
  "Delete the assistant having the given ID, a string."
  [id & {:keys [llm-provider] :or {llm-provider @default-llm-provider}}]
  (openai/delete-assistant {:assistant_id id} (api-credentials llm-provider)))

;;; There is a function in db.clj that calls this with a selection-fn that avoids deleting surrogates used in projects.
(defn ^:diag delete-surrogates!
  "Delete from the openai account all assistants that match the argument filter function.
   The default selection-function checks for metadata :usage='stbd-surrogate'.
   If, for example, you want to not delete surrogates that are used in projects, you will have to override this."
  [& {:keys [selection-fn llm-provider] :or {selection-fn #(= "stbd-surrogate" (-> % :metadata (get :usage)))
                                             llm-provider @default-llm-provider}}]
  (doseq [a (->> (list-assistants) (filterv selection-fn))]
    (log! :info (str "Deleting assistant " (:name a) (:id a)))
    (delete-assistant! (:id a) {:llm-provider llm-provider})))

(defn ^:diag delete-agents!
  "Delete from the openai account all assistants that match the argument filter function.
   The default function check for metadata :usage='stbd-surrogate'."
  [{:keys [selection-fn llm-provider] :or {selection-fn #(= "stbd-agent" (-> % :metadata (get :usage))) llm-provider @default-llm-provider}}]
  (doseq [a (->> (list-assistants)  (filterv selection-fn))]
    (log! :info (str "Deleting assistant " (:name a) (:id a)))
    (delete-assistant! (:id a) {:llm-provider llm-provider})))

;;; (ws/register-ws-dispatch  :run-long llm/run-long)
(defn ^:diag run-long
  "Diagnostic for exploring threading/blocking with websocket."
  [& _]
  (loop [cnt 0]
    (log! :info (str "Run-long: cnt = " cnt))
    (Thread/sleep 5000) ; 5000 * 30, about 4 minutes.
    (when (< cnt 20) (recur (inc cnt)))))

(defn ^:diag throw-it
  [& _]
  (throw (ex-info "Just because I felt like it." {})))


(defn query-agent
  "Make a query to a named agent."
  [base-type query & {:keys [llm-provider] :or {llm-provider @default-llm-provider}}]
  (let [{:keys [aid tid]} (-> (d/q '[:find ?aid ?tid
                                     :keys aid tid
                                     :in $ ?base-type ?provider
                                     :where
                                     [?e :agent/base-type ?base-type]
                                     [?e :agent/llm-provider ?provider]
                                     [?e :agent/assistant-id ?aid]
                                     [?e :agent/thread-id ?tid]]
                                   @(connect-atm :system)
                                   base-type llm-provider)
                              first)]
    (if-not (and aid tid)
      (throw (ex-info "Could not find the agent requested. (Not created for this LLM provider?:" {:llm-provider llm-provider}))
      (let [result (-> (query-on-thread {:aid aid :tid tid :query-text query})
                       (str/replace #"\s+" " "))]
        (try (->> (edn/read-string result)
                  (mapv (fn [m] (update-keys m #(-> % name str/lower-case keyword)))))
             (catch Exception _e
               (log! :error (str "query-agent failed: " result))
               nil))))))

(def moderation-checking?
  "Set to true if you want to filter immoderate user text."
  (atom false))

(defn immoderate?
  "Return true if LLM flags text as immoderate."
  [txt]
  (when @moderation-checking?
    (-> (openai/create-moderation {:input txt}) :results first :flagged)))

;;; --------------------------------- Files -------------------------------
(defn upload-file
  "Upload a file and get back a file id response."
  [& {:keys [fname purpose llm-provider]
      :or {purpose "assistants"
           llm-provider @default-llm-provider}}]
  (openai/create-file {:purpose purpose
                       :file (io/file fname)}
                      (api-credentials llm-provider)))

(defn ^:diag delete-file
  [& {:keys [file-id llm-provider]
      :or {llm-provider @default-llm-provider}}]
  (assert (string? file-id))
  (openai/delete-file {:file-id file-id} (api-credentials llm-provider)))

;;; --------------------------------- Vector stores -------------------------------
(defn make-vector-store
  [& {:keys [name file-ids metadata llm-provider]
      :or {llm-provider @default-llm-provider
           name "STBD agent vector store"
           metadata {:usage :stbd-agent}}}]
  (openai/create-vector-store
   {:name name :file_ids file-ids :metadata metadata}
   (api-credentials llm-provider)))

(defn list-vector-stores
  [& {:keys [llm-provider] :or {llm-provider @default-llm-provider}}]
  (let [creds (api-credentials llm-provider)]
    (loop [ours []
           response (openai/list-vector-stores {:limit 1} creds)]
      (if (:has_more response)
        (recur (into ours (->> response :data (filter #(-> % :metadata :usage ours?)) (map :id)))
               (openai/list-vector-stores {:limit 99 :after (:last_id response)} creds)) ; 99 is max in openai.
        (into ours (->> response :data (filter #(-> % :metadata :usage ours?)) (map :id)))))))

(defn modify-vector-store
  [& {:keys [vector-store-id name llm-provider] :or {llm-provider @default-llm-provider}}]
  (openai/modify-vector-store {:vector_store_id vector-store-id :name name}
                              (api-credentials llm-provider)))

;;; Calls to openai/retieve-vector-store raises an error "Could not find route :retrieve-vector-store"openai/retrieve-vector-store
;;; Instead of calling openai/retieve-vector-store, I think you can call modify-vector-store with params specifying just :vector_store_id.
(defn retrieve-vector-store
  [& {:keys [vector-store-id llm-provider] :or {llm-provider @default-llm-provider}}]
  (modify-vector-store {:vector-store-id vector-store-id :llm-provider llm-provider}))


(defn delete-vector-store
  [& {:keys [vector-store-id llm-provider] :or {llm-provider @default-llm-provider}}]
  (openai/delete-vector-store {:vector_store_id vector-store-id}
                              (api-credentials llm-provider)))

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
