(ns scheduling-tbd.llm
  "Built-in functions implementing the expression language of the mapping language.
   Functions with names beginning with a '$' are available to the user (e.g. $filter).
   Others (such as bi/key and bi/strcat) implement other parts of the expression language
   but are not available directly to the user except through the operators (e.g. the dot, and &
   respectively for navigation to a property and concatenation).

   N.B. LSP annotates many operators here as '0 references'; of course, they are used."
  (:require
   [clojure.core :as c]
   [wkok.openai-clojure.api      :as openai]
   [scheduling-tbd.sutil         :refer [get-api-key]]
   [camel-snake-kebab.core       :as csk]
   [clojure.pprint               :refer [cl-format]]
   [clojure.string               :as str]
   [taoensso.timbre              :as log]))

(def diag (atom nil))

(def factors-enumeration nil)

;;; GPT-3.5 Turbo Instruct is a large language model that builds upon the capabilities of GPT-3.5 Turbo.
;;; It is specifically optimized for understanding and executing instructions in a wide range of tasks and scenarios.
;;; Unlike chat-focused models, GPT-3.5-turbo-instruct excels in providing direct and precise responses to instructions.
#_[{:id "gpt-3.5-turbo",               :created 1677610602, :owned_by "openai"}
   {:id "gpt-3.5-turbo-0301",          :created 1677649963, :owned_by "openai"}
   {:id "gpt-3.5-turbo-16k",           :created 1683758102, :owned_by "openai-internal"}
   {:id "gpt-3.5-turbo-16k-0613",      :created 1685474247, :owned_by "openai"}
   {:id "gpt-3.5-turbo-0613",          :created 1686587434, :owned_by "openai"}
   {:id "gpt-4-0613",                  :created 1686588896, :owned_by "openai"}
   {:id "gpt-4-0314",                  :created 1687882410, :owned_by "openai"}
   {:id "gpt-4",                       :created 1687882411, :owned_by "openai"}
   {:id "gpt-3.5-turbo-instruct",      :created 1692901427, :owned_by "system"} ; "This is not a chat model and thus not supported in the v1/chat/completions endpoint. Did you mean to use v1/completions?
   {:id "gpt-3.5-turbo-instruct-0914", :created 1694122472, :owned_by "system"}]

(def chat-model? #{"gpt-3.5-turbo", "gpt-3.5-turbo-0301", "gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613", "gpt-3.5-turbo-0613", "gpt-4-0613", "gpt-4-0314", "gpt-4"})

(def diag (atom nil))

(defn query-llm
  "Return a Clojure map that is read from the string created by the LLM given the vector of messages that is the argument."
  [messages & {:keys [model raw-text?] :or {model "gpt-3.5-turbo"} :as other}]
  (reset! diag {:msg messages :other other})
  (assert (chat-model? model)) ; Otherwise you get an http 404; you might not connect the dots!
  (if-let [key (get-api-key :llm)]
    (try (let [res (-> (openai/create-chat-completion {:model model :messages messages} {:api-key key})
                       :choices first :message :content)
               res (cond-> res (not raw-text?) read-string)]
           (if (or (map? res) (string? res))
             res
             (throw (ex-info "Did not produce a map." {:result res}))))
         (catch Throwable e
           (log/error "Call to query-llm failed.")
           (ex-info "OpenAI API call failed." {:message (.getMessage e)})))
    (throw (ex-info "No key for use of LLM API found." {}))))

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
      (try (let [res (query-llm prompt {:model "gpt-4"})]
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
