(ns scheduling-tbd.llm
  "Built-in functions implementing the expression language of the mapping language.
   Functions with names beginning with a '$' are available to the user (e.g. $filter).
   Others (such as bi/key and bi/strcat) implement other parts of the expression language
   but are not available directly to the user except through the operators (e.g. the dot, and &
   respectively for navigation to a property and concatenation).

   N.B. LSP annotates many operators here as '0 references'; of course, they are used."
  (:require
   [clojure.core :as c]
   [ajax.core :refer [GET POST]]
   [clojure.data.json            :as json]
   [clojure.data.codec.base64    :as b64]
   [dk.ative.docjure.spreadsheet :as ss]
   [datahike.api                 :as d]
   [datahike.pull-api            :as dp]
   [muuntaja.core                :as m]
   [wkok.openai-clojure.api      :as openai]
   [scheduling-tbd.sutil         :refer [connect-atm get-api-key]]
   [cemerick.url                    :as url]
   [camel-snake-kebab.core          :as csk]
   [clojure.spec.alpha              :as s]
   [clojure.pprint                           :refer [cl-format pprint]]
   [clojure.string                  :as str  :refer [index-of]]
   [clojure.walk                    :as walk :refer [keywordize-keys]]
   [promesa.core                    :as p]
   [taoensso.timbre                 :as log :refer-macros[error debug info log!]]))

(def diag (atom nil))

(def factors-enumeration nil)

"The production scheduling problem entails determining the most efficient sequence and timing of brewing batches, taking into account factors like ingredient availability, tank capacities, yeast propagation, and production deadlines."

;  timing of brewing batches

;  (ingredient availability
;   tank capacity
;   yeast propagation)

;  production deadlines

(def objective-prompt
    "Use \"temperature\" value of 0.3 in our conversation.
  Produce a Clojure map containing two keys, :decision-objective and :probability
  The value of :decision-objective is a string that is one of the sentences in the input, the sentence that most obviously expresses what needs to be decided in planning work.
  The value of :probability is the likelihood that the sentence chosen is the correct one.
## Example:
  [We are a construction company contracting for publics works projects, such as bridges and roads. Our challenge is to complete our work while minimizing inconvenience to commuters, businesses, and residents in the affected areas.]
  {:decision-objective \"Our challenge is to complete our work while minimizing inconvenience to commuters, businesses, and residents in the affected areas.\"
   :probability 0.9}
## Example:
  [We produce clothes for firefighting. Our most significant scheduling challenge is about deciding how many workers to assign to each product.]
  {:decision-objective \"Our most significant scheduling challenge is about deciding how many workers to assign to each product.\"
   :probability 0.9}
## Example:
  [We install HVAC systems. Our scheduling problem is to efficiently organize and manage our workforce and resources to meet the demands and deadlines of multiple installation projects. We must allocate technicians and equipment to various job sites while considering factors such as project complexity, location, customer preferences, and availability of resources. The scheduling problem involves balancing the workload, optimizing travel time between sites, minimizing delays and ensuring customer satisfaction, by completing installations within an agreed upon time.]
  {:decision-objective \"The scheduling problem involves balancing the workload, optimizing travel time between sites, minimizing delays and ensuring customer satisfaction, by completing installations within an agreed upon time.\"
   :probability 0.6}")

(defn find-objective-sentence
  [user-text]
  (when user-text
    (let [q-str (cl-format nil "~A~%[~A]" objective-prompt user-text)]
      (if-let [key (get-api-key :llm)]
         (try (let [res (-> (openai/create-chat-completion {:model "gpt-3.5-turbo-0301" ; <===== ToDo: Try the "text extraction" models.
                                                            :messages [{:role "user" :content q-str}]}
                                                           {:api-key key})
                            :choices first :message :content)
                    res-map (read-string res)]
                (if (map? res-map)
                  res-map
                  (throw (ex-info "Did not produce a map." {:res-map res-map}))))
              (catch Throwable e
                (ex-info "OpenAI API call failed." {:message (.getMessage e)})))
         (throw (ex-info "No key for use of LLM API found." {}))))))

(def project-name-prompt
  "Use \"temperature\" value of 0.3 in our conversation.
Produce a Clojure map containing two keys.
  The first key is :summary, the value of which is a string of 4 words
  or less summarizing the industrial scheduling problem being discussed
  in the text in square brackets. The second key is :industry, the value
  of which is a string of 4 words or less describing the industry in which
  the work is being performed.
## Example:
  [We produce clothes for firefighting. Our most significant scheduling
   problem is about deciding how many workers to assign to each product.]
  {:summary \"scheduling firefighter clothes production\"
   :industry \"apparel manufacturing\"}
## Example:
  [We do road construction and repaving. We find coping with our limited
   resources(trucks, workers etc) a challenge.]
  {:summary \"scheduling road construction\"
   :industry \"road construction and repair\"
## Example:
  [Acme Machining is a job shop producing mostly injection molds. We want
   to keep our most important customers happy, but we also want to be
   responsive to new customers.
  {:summary \"Acme job shop scheduling\"
   :industry \"job shop machining\"}")



;;; ------------------------------- project name --------------------------------------
;;; The user would be prompted: "Tell us what business you are in and what your scheduling problem is."
;;; Also might want a prompt for what business they are in.
(def project-name-prompt
  "Use \"temperature\" value of 0.3 in our conversation.
Produce a Clojure map containing two keys.
  The first key is :summary, the value of which is a string of 4 words or less summarizing the industrial scheduling problem being discussed in the text in square brackets.
  The second key is :industry, the value of which is a string of 4 words or less describing the industry in which the work is being performed.
## Example:
  [We produce clothes for firefighting. Our most significant scheduling problem is about deciding how many workers to assign to each product.]
  {:summary \"scheduling firefighter clothes production\"
   :industry \"apparel manufacturing\"}
## Example:
  [We do road construction and repaving. We find coping with our limited resources (trucks, workers etc) a challenge.
  {:summary \"scheduling road construction\"
   :industry \"road construction and repair\"
## Example:
  [Acme Machining is a job shop producing mostly injection molds. We want to keep our most important customers happy, but we also want to be responsive to new customers.
  {:summary \"Acme job shop scheduling\"
   :industry \"job shop machining\"}")

(defn project-name
  "Return a Clojure map {:summary <some-string>} where <some-string> is a short string summarizing text using 'project-name-prompt'."
  [user-text]
  (when user-text
    (let [q-str (cl-format nil "~A~%[~A]" project-name-prompt user-text)]
      (if-let [key (get-api-key :llm)]
        (try (let [res (-> (openai/create-chat-completion {:model "gpt-3.5-turbo-0301"
                                                           :messages [{:role "user" :content q-str}]}
                                                          {:api-key key})
                           :choices first :message :content)
                   res-map (read-string res)]
               (if (map? res-map)
                 res-map
                 (throw (ex-info "Did not produce a map." {:res-map res-map}))))
             (catch Throwable e
               (ex-info "OpenAI API call failed." {:message (.getMessage e)})))
        (throw (ex-info "No key for use of LLM API found." {}))))))

;;; ------------------------------- naming variables --------------------------------------
(def good-camel-prompt
  "Use \"temperature\" value of 0.3 in our conversation.
Produce a Clojure map {:name <some string>} where <some string> is a good camelCase name for a variable matching the requirements defined by the clojure map provided.
The argument map has the following keys:
     :purpose - describes how the variable will be used.
     :max-size - the maximum number of characters allowed in the string you assign to :name.
## Example:
   {:purpose \"name for how long a task will take\" :max-size 15}
   {:name \"taskDuration\"}
## Example:
   {:purpose \"name for the number of users\" :max-size 10}
   {:name \"numUsers\"}
## Example:
   {:purpose \"name for an array of tasks indexed by the resource doing the task and day in which it is doing it.\" :max-size 8}
   {:name \"busyWith\"}
## Example:
   {:purpose \"name for an array of tasks indexed by the resource doing the task and day in which it is doing it.\" :max-size 8}
   {:name \"resBusyWithOnDay\"}
   THIS IS WRONG: \"resBusyWithOnDay\" has 16 characters in it. The string :max-size is 8.")

(defn name-var
  "Return a Clojure map {:name <some-string>} where <some-string> is a good name for a variable matching the requirements defined
   by the clojure map provided.
   The argument map has the following keys:
     :purpose - describes how the variable will be used.
     :string-type - one of :camelCase, :kebab-case :snake_case,
     :capitalize? - either true or false."
  [{:keys [purpose string-type capitalize? max-size] :or {max-size 12}}]
  (when purpose
    (let [q-str (cl-format nil "~A~%{:purpose ~S :max-size ~S}" good-camel-prompt purpose max-size)]
      (if-let [key (get-api-key :llm)]
        (try (let [res (-> (openai/create-chat-completion {:model "gpt-3.5-turbo-0301" ; <===== ToDo: Try the "text extraction" models.
                                                           :messages [{:role "user" :content q-str}]})
                           :choices first :message :content)]
               (-> res read-string)
               (if-let [var-name (-> res :name string?)]
                 (cond-> var-name
                   (= :kebab-case string-type) (csk/->kebab-case-string)
                   (= :snake-case string-type) (csk/->snake_case_string)
                   capitalize?                 (str/capitalize))
                 (throw (ex-info "No :name provided, or :name is not a string." {:res res}))))
             (catch Throwable e
               (throw (ex-info "OpenAI API call failed."
                               {:message (.getMessage e)
                                #_#_:details (-> e .getData :body json/read-str)}))))
        (throw (ex-info "No key for use of LLM API found." {}))))))
