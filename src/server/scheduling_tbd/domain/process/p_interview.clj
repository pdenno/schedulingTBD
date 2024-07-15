(ns scheduling-tbd.domain.process.p-interview
  "Supporting functions for running the process interview. These are mostly called from process_op.clj
     - analyze: Function names that begin with 'analyze' study user/surrogate response and produce planning propositions.
     - mzn: Function names that begin with 'mzn' use planning state to modify MiniZinc code."
  (:require
   [clojure.core.unify            :as uni]
   [clojure.edn                   :as edn]
   [clojure.pprint                :refer [cl-format]]
   [clojure.spec.alpha            :as s]
   [clojure.string                :as str]
   [datahike.api                  :as d]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.llm            :as llm :refer [query-llm]]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm register-planning-domain yes-no-unknown string2sym]]
   [taoensso.timbre               :as log]))

(def ^:diag diag (atom nil))

;;; Place this in any file where you are developing the process interview so updates will be found.
(register-planning-domain :process  (-> "data/planning-domains/process-interview.edn" slurp edn/read-string))

;;; ------------------------------- project name --------------------------------------
;;; The user would be prompted: "Tell us what business you are in and what your scheduling problem is."
;;; Also might want a prompt for what business they are in.
(def project-name-partial
  "This is used to name the project.  Wrap the user's paragraph in square brackets."
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "Produce a Clojure map containing 1 key, :project-name, the value of which is a string of 3 words or less describing the scheduling project being discussed in the text in square brackets.
                                If the text says something like 'We make <x>', <x> ought to be part of your answer."}

   {:role "user"      :content "[We produce clothes for firefighting (PPE). Our most significant scheduling problem is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "{:project-name \"PPE production scheduling\"}"}

   {:role "user"      :content "[We do road construction and repaving. We find coping with our limited resources (trucks, workers etc) a challenge.]"}
   {:role "assistant" :content "{:project-name \"road construction and repaving scheduling\"}"}
   {:role "user"      :content "WRONG. That is more than 3 words."} ; This doesn't help, except with GPT-4!

   {:role "user"      :content "[We do road construction and repaving. We find coping with our limited resources (trucks, workers etc) a challenge.]"}
   {:role "assistant" :content "{:project-name \"supply chain scheduling\"}"}
   {:role "user"      :content "WRONG. Be more specific!"} ; This doesn't help, except with GPT-4!

   {:role "user"      :content "[Acme Machining is a job shop producing mostly injection molds. We want to keep our most important customers happy, but we also want to be responsive to new customers.]"}
   {:role "assistant" :content "{:project-name \"job shop scheduling\"}"}])

(def project-objective-partial
  "This is used to scan text for an objective. Wrap the user's paragraph in square brackets."
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "You will be given a paragraph enclosed in square brackets.
From it, produce a Clojure map containing 2 keys, :decision-objective and :probability.
  The value of :objective is a string that is one of the sentences in the input, the sentence that most obviously expresses what needs to be decided in planning work.
  The value of :probability is the likelihood that the sentence really is the one that best expresses an objective."}

   {:role "user"      :content "[We are a construction company contracting for publics works projects, such as bridges and roads.
Our challenge is to complete our work while minimizing inconvenience to commuters, businesses, and residents in the affected areas.]"}
   {:role "assistant" :content "{:objective \"Our challenge is to complete our work while minimizing inconvenience to commuters, businesses, and residents in the affected areas.\" :probability 0.9}"}


   {:role "user"      :content "[We install HVAC systems. Our scheduling problem is to efficiently organize and manage our workforce and resources to meet the demands and deadlines of multiple installation projects.
 We must allocate technicians and equipment to various job sites while considering factors such as project complexity, location, customer preferences, and availability of resources.
 The scheduling problem involves balancing the workload, optimizing travel time between sites, minimizing delays and ensuring customer satisfaction, by completing installations within an agreed upon time.]"}
   {:role "assistant" :content "{:objective \"The scheduling problem involves balancing the workload, optimizing travel time between sites, minimizing delays and ensuring customer satisfaction,
 by completing installations within an agreed upon time.\"
   :probability 0.6}"}
   {:role "user"      :content "Correct. In this one, the probability is lower because the sentence beginning with \"Our scheduling problem is to efficiently organize...\" also seems like an objective."}

   {:role "user"      :content "[We produce clothes for firefighting. It is fun to do. Our most significant scheduling challenge is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "{:objective \"Our most significant scheduling challenge is about deciding how many workers to assign to each product.\" :probability 0.9}"}])

;;; ToDo: Not sure that "disjoint and covering...therefore" is logically correct here!
(def service-vs-artifact-partial
  "This provides probabilities for whether the scheduling concerns a service vs artifact. We assume those two classes are disjoint and covering."
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "The text in square brackets describes a scheduling problem.
 Classify it using probabilities expressing how well it describes each of two classes;
 the classes are assumed to be disjoint and covering so the sum of the two probabilties should be 1.0.
 The classes are:
   :service - the text describes the scheduling of a service to be rendered.
   :artifact - the text describes the scheduling of the production of physical artifacts.
 Express your result as a Clojure map where the keys are the keywords :service and :artifact."}
   {:role "user"      :content "[We provide music lessons. Our scheduling challenges is finding each week a practice room and time at which students and instructors can meet.]"}
   {:role "assistant" :content "{:service 1.0, :artifact 0.0}"}
   {:role "user"      :content "[We produce and repair clothes for firefighting. Our most significant scheduling challenge is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "{:service 0.2, :artifact 0.8}"}
   {:role "user"      :content "[We repair traffic signal and street lights, replacing bulbs, etc. Our scheduling challenge is in reaching and repairing the most important jobs quickly.]"}
   {:role "assistant" :content "{:service 0.9, :artifact 0.2}"}
   {:role "user"      :content "WRONG: The sum of 0.9 and 0.2 is not 1.0."}])

(defn project-name-llm-query
  "Wrap the user-text in square brackets and send it to an LLM to suggest a project name.
   Returns a string consisting of just a few words."
  [user-text]
  (as-> (conj project-name-partial
              {:role "user" :content (str "[ " user-text " ]")}) ?r
    (query-llm ?r {:model-class :gpt-4 :raw-text? false}) ; 2024-03-23 I was getting bad results with :gpt-3.5. This is too important!
    (:project-name ?r)
    (if (string? ?r) ?r (throw (ex-info "Could not obtain a project name suggestion from and LLM." {:user-text user-text})))))

(def raw-material-challenge-partial
  [{:role "system"    :content "You are a helpful assistant."}
   {:role "user"      :content "Respond with either 'yes' or 'no' to whether the text in square brackets alludes to a raw-materials shortage."}

   {:role "user"      :content "[We produce clothes for firefighting. Our most significant scheduling problem is about deciding how many workers to assign to each product.]"}
   {:role "assistant" :content "no"}

   {:role "user"      :content "[We do road construction and repaving. We find coping with our limited resources (trucks, workers, aggregate, etc.) a challenge.]"}
   {:role "assistant" :content "yes"}

   {:role "user"      :content "[Our principal scheduling problem revolves around coordinating raw material procurement with production schedules and customer delivery deadlines.
 We often face the challenge of ensuring a continuous supply of high-quality raw aluminium, which is subject to market availability and price fluctuations.]"}
   {:role "assistant" :content "yes"}

   {:role "user"      :content "[We run several products simultaneously and simply would like to be able to have the beer bottled and ready to ship as near as possible to
 the dates defined in our sales plan.]"}
   {:role "assistant" :content "no"}])

(defn text-cites-raw-material-challenge?
  "Return :yes, :no, or :unknown depending on whether the text cites an inventory challenge."
  [text]
  (-> (conj raw-material-challenge-partial
            {:role "user" :content (format "[%s]" text)})
      (query-llm {:model-class :gpt-4})
      yes-no-unknown))

(defn mzn-process-steps
  "Use state information to write a string enumerating process steps."
  [state]
  (let [procs (->> state (filter #(uni/unify % '(process-step ?pid ?num ?proc))) (sort-by #(nth % 2)) (mapv #(nth % 3)))]
    (cl-format nil "enum Task = {~{~a~^, ~}};" procs)))

;;; -------------------------------- response analysis  -----------------------------------------------
;;; ToDo: This was written before I decided that domain.clj ought to require db and use it. Refactor this and its caller, !initial-question ?
(defn analyze-intro-response
  "Analyze the response to the initial question, adding to the init-state vector."
  [response init-state]
  (log/info "prelim-analysis: init-state =" init-state "response =" response)
  (let [[_ pid] (sutil/find-fact '(proj-id ?x) init-state)
        proj-state (if (= pid :START-A-NEW-PROJECT)
                     (let [proj-name (as-> (project-name-llm-query response) ?s
                                       (str/trim ?s)
                                       (str/split ?s #"\s+")
                                       (map str/capitalize ?s)
                                       (interpose " " ?s)
                                       (apply str ?s))
                           proj-id (as-> proj-name ?s (str/lower-case ?s) (str/replace ?s #"\s+" "-") (symbol ?s))]
                       (into (filterv #(not= % '(proj-id START-A-NEW-PROJECT)) init-state)
                             `[(~'proj-id ~proj-id)
                               (~'proj-name ~proj-name)]))
                     ;; Otherwise, it is a surrogate; proj-id and proj-name are already known.
                     init-state)]
    (if (= :yes (text-cites-raw-material-challenge? response))
        (conj proj-state (list 'cites-raw-material-challenge (-> pid name symbol)))
        proj-state)))

;;; ToDo: If this ever fails, replace it with an agent!
(defn analyze-process-steps-response
  "Return state addition for analyzing a Y/N user response about process steps.
   The state objects look like (have-process <pid> <num> <proc-name) OR (fails-process-step <line>)."
  [{:keys [response pid] :as _obj}]
  (let [pid-sym (-> pid name symbol)]
    (->> (for [line (str/split-lines response)]
           (let [[success step-num proc-name] (re-matches #"^\s*(\d+)\.?\s+([\w,\s]+).*" line)]
             (if success
               (list 'process-step pid-sym (edn/read-string step-num) (str/trim proc-name))
               (if (re-matches #"^\s*$" line)
                 nil
                 (list 'fails-process-step line)))))
         (filterv identity))))

(defn extreme-dur-span?
  "Return the vector of units used in the process if :qty/units span from from :minutes to :days or more or :hours to :weeks or more."
  [pid process-id]
  (let [units (atom #{})]
    (letfn [(get-units [obj]
              (cond (map? obj)    (doseq [[k v] (seq obj)]
                                     (when (= k :quantity/units) (swap! units conj v))
                                     (get-units v))
                    (vector? obj) (doseq [v obj] (get-units v))))]
      (-> (db/get-process pid process-id) get-units)
      (let [units @units]
        (cond (and (units :minutes) (or (units :days)  (units :weeks) (units :months)))   (vec units)
              (and (units :hours)   (or (units :weeks) (units (units :months))))          (vec units))))))

;;; ----------------- analyze-process-durs-response (done with :process-dur-agent) ----------------------------
;;; These are named by the output of the given revision.
(s/def :rev-1/PROCESS string?)
(s/def :rev-1/PROCESS-STEP number?)
(s/def :rev-1/DURATION string?)
(s/def ::rev-1-map (s/keys :req-un [:rev-1/PROCESS-STEP :rev-1/PROCESS :rev-1/DURATION]))

(s/def :rev-2/PROCESS string?)
(s/def :rev-2/PROCESS-STEP number?)
(s/def :rev-2/DURATION string?)
(s/def :rev-2/COMMENT string?)
(s/def ::rev-2-map (s/keys :req-un [:rev-2/PROCESS-STEP :rev-2/PROCESS :rev-2/DURATION] :opt-un [:rev-2/COMMENT]))


(s/def :rev-3/AMOUNT-STRING string?)
(s/def :rev-3/UNITS keyword?)
(s/def :rev-3/simple-duration (s/keys :req-un [:rev-3/AMOUNT-STRING :rev-3/UNITS]))
(s/def :rev-3/range-duration  (s/keys :req-un [:rev-3/QUANTITY-LOW :rev-3/QUANTITY-HIGH]))
(s/def :rev-3/QUANTITY-LOW  #(s/valid? :rev-3/simple-duration %))
(s/def :rev-3/QUANTITY-HIGH #(s/valid? :rev-3/simple-duration %))
(s/def :rev-3/PROCESS string?)
(s/def :rev-3/PROCESS-STEP number?)
(s/def :rev-3/DURATION (s/or
                        :simple :rev-3/simple-duration
                        :range :rev-3/range-duration
                        :string string?))
(s/def :rev-3/COMMENT string?)
(s/def ::rev-3-map (s/keys :req-un [:rev-3/PROCESS-STEP :rev-3/PROCESS :rev-3/DURATION] :opt-un [:rev-3/COMMENT]))

(s/def ::rev-4-map #(and (s/valid? ::rev-3-map %)
                         (if (contains? % :SUPPLY-CHAIN?) (= (:SUPPLY-CHAIN? %) true) true)))

(s/def ::rev-5-map #(and (s/valid? ::rev-4-map %)
                         (string? (:VAR %))))

;;; clj-kondo/LSP won't know about most of these because the call to s/valid? is programmatic.
(s/def ::rev-1 (fn [v] (every? #(s/valid? ::rev-1-map %) v)))
(s/def ::rev-2 (fn [v] (every? #(s/valid? ::rev-2-map %) v)))
(s/def ::rev-3 (fn [v] (every? #(s/valid? ::rev-3-map %) v)))
(s/def ::rev-4 (fn [v] (every? #(s/valid? ::rev-4-map %) v)))
(s/def ::rev-5 (fn [v] (every? #(s/valid? ::rev-5-map %) v)))

(defn remove-preamble
  "The LLM might put text and markup around the answer, return the answer without this crap."
  [response]
  (let [response (str/replace response #"\s" " ")]
    (if (re-matches #".*```clojure.*" response)
      (let [pos (str/index-of response "```clojure")
            response (subs response (+ pos 10))
            pos (str/index-of response "```")]
        (subs response 0 pos))
      response)))

(defn switch-keys
  "Switch from LLM keys to keys used in DB."
  [obj]
  (let [db-key {:PROCESS-STEP  :PROCESS-STEP
                :PROCESS       :PROCESS
                :DURATION      :process/duration
                :QUANTITY-LOW  :quantity-range/low
                :QUANTITY-HIGH :quantity-range/high
                :AMOUNT-STRING :quantity/value-string
                :UNITS         :quantity/units
                :VAR           :process/var-name
                :SUPPLY-CHAIN? :process/supply-chain?
                :COMMENT       :process/duration-comment}]
    (letfn [(sk [obj]
              (cond (map? obj)  (-> (reduce-kv (fn [m k v]
                                                 (let [v (sk v)]
                                                   (if (and (= k :DURATION) (string? v))
                                                     (assoc m (db-key k) {:box/string-val v})
                                                     (assoc m (db-key k) v))))
                                               {} obj)
                                    (dissoc :PROCESS-STEP))
              (vector? obj)     (mapv sk obj)
              :else             obj))]
      (sk obj))))

(defn post-process
  "Make the process-agent's LLM output suitable for the project's DB (process object etc.)"
  [process-maps super-process]
  (let [db-style (->> process-maps
                      (map switch-keys)
                      (map #(assoc % :process/id (->> % :process/var-name (str super-process "--") keyword)))
                      (mapv #(dissoc % :PROCESS)))]
    (reduce (fn [res ix]
              (let [process-map (nth db-style ix)]
                (if (== ix 0)
                  (conj res process-map) ; ToDo: process/pre-processes is sort of a guess! Maybe do this later?
                  (conj res (assoc process-map :process/pre-processes [(-> (nth db-style (dec ix)) :process/id)])))))
            []
            (-> db-style count range))))

;;; (inv/run-process-agent-steps data2)
(defn run-process-dur-agent-steps
  "Run the steps of the process agent, checking work after each step."
  [response pid]
  (let [{:keys [aid tid]} (db/get-agent :process-dur-agent)
        past-rev (atom response)]
    (doseq [rev [::rev-1 ::rev-2 ::rev-3 ::rev-4 ::rev-5]]
      (when @past-rev
        (let [result (llm/query-on-thread
                      {:aid aid :tid tid :role "user"
                       :tries 3 :test-fn (fn [resp] (s/valid? rev resp))
                       :preprocess-fn (fn [resp] (-> resp remove-preamble edn/read-string))
                       :query-text (str "Perform " (-> rev name str/upper-case) " on the following:\n\n" @past-rev)})]
          (reset! past-rev result)
          #_(log/info "***************" rev "="  @past-rev))))
    (post-process @past-rev (name pid))))

(defn put-process-sequence!
  "Write project/process-sequence to the project's database.
   The 'infos' argument is a vector of maps such as produced by analyze-process-durs-response."
  [pid full-obj]
  (reset! diag full-obj)
  (let [conn (connect-atm pid)
        eid (db/project-exists? pid)]
    (d/transact conn {:tx-data [{:db/id eid :project/processes full-obj}]})))

(defn analyze-process-durs-response
  "Used predominantly with surrogates, study the response to a query about process durations,
   writing findings to the project database and returning state propositions."
  [{:keys [response pid] :as _obj}]
  (let [process-objects (run-process-dur-agent-steps response pid)
        full-obj {:process/id pid
                  :process/desc response
                  :process/sub-processes process-objects}]
    (put-process-sequence! pid full-obj)
    (let [extreme-span? (extreme-dur-span? pid pid) ; This look at the DB just updated.
          proj-sym (-> pid name symbol)]
      ;;(log/info "extreme-span? =" extreme-span?)
      (cond-> [(list 'have-process-durs proj-sym)]
        extreme-span?    (conj `(~'extreme-duration-span ~proj-sym ~@(map #(-> % name symbol) extreme-span?)))))))

(defn ^:diag check-instructions
  "It might be the case that the system instructions were too long. This asks what it knows about."
  []
  (let [{:keys [aid tid]} (db/get-agent :process-dur-agent)]
    (llm/query-on-thread
     {:aid aid :tid tid :role "user"
      :query-text (str "I provided instructions to perform a number of transformation we call 'REVs', "
                       "REV-1, REV-2, etc. What are the REVs that you know about, and what do they do?")})))

;;; --------------------------------------- unimplemented (from the plan) -----------------------------
(defn ^:diag analyze-process-ordering-response
  "Return state addition for analyzing a Y/N user response about process ordering."
  [_response]
  [])
