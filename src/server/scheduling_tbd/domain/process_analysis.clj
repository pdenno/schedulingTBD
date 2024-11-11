(ns scheduling-tbd.domain.process-analysis
  "Analysis of the process interview"
  (:require
   [clojure.core.unify                        :as uni]
   [clojure.edn                               :as edn]
   [clojure.pprint                            :refer [cl-format]]
   [clojure.spec.alpha                        :as s]
   [clojure.string                            :as str]
   [scheduling-tbd.db                         :as db]
   [scheduling-tbd.llm                        :as llm :refer [query-llm]]
   [scheduling-tbd.minizinc                   :as mzn]
   [scheduling-tbd.response-utils             :as ru :refer [defanalyze find-claim make-human-project surrogate?]]
   [scheduling-tbd.sutil                      :as sutil :refer [elide]]
   [scheduling-tbd.web.websockets             :as ws]
   [taoensso.telemere                         :as tel :refer [log!]]))

;;; Place this in any file where you are developing the process interview so updates will be found.
#_(register-planning-domain :process  (-> "data/planning-domains/process-interview.edn" slurp edn/read-string))

(def ^:diag diag (atom nil))
(declare analyze-intro-response analyze-process-steps-response analyze-process-durs-response analyze-batch-size-response analyze-process-ordering-response)

(def the-warm-up-type-question
  (str "What are products you make or services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences."))

;;; ToDo: warm-up question might suggest several avenues of investigation. For example, consider this, from sur-plate-glass:
;;; "Our most significant scheduling problem revolves around coordinating the manufacturing process with the fluctuating availability of raw materials,
;;; particularly high-quality silica sand and soda ash, and accommodating the variable demand from our customers.
;;; We struggle with optimizing machine utilization time and reducing downtime, while also managing delivery schedules that are dependent
;;; on these unpredictable elements. Additionally, managing the workforce to align with these changes,
;;; ensuring we have enough skilled workers available when needed, adds another layer of complexity to our operations.",
;;; This might get into skills classification, delivery schedules, downtime, machine utilization.

;;; Note that pid here can be :START-A-NEW-PROJECT. In that case, we don't have a DB for the project yet. We make it here.
(defanalyze :warm-up [{:keys [response client-id pid] :as _ctx}]
  (log! :info (str "*******analysis :warm-up, response = " response))
  (let [init-state (db/get-claims pid)
        new-claims (analyze-intro-response response init-state)
        all-claims (into init-state new-claims)
        surrogate? (find-claim '(surrogate ?x) init-state)] ; return state props proj-id and proj-name if human, otherwise argument state.
    (when-not surrogate? (make-human-project (into init-state new-claims)))
    ;;--------  Now human/surrogate can be treated nearly identically ---------
    (let [[_ pid]   (find-claim '(proj-id ?x) all-claims)
          [_ pname] (find-claim '(proj-name ?x) all-claims)]
      (doseq [claim new-claims] (db/add-claim pid claim))
      (db/add-msg {:pid pid
                   :from :system
                   :text (format "Great, we'll call your project %s." pname)
                   :tags [:!describe-challenge :informative]})
      (ws/refresh-client client-id pid :process))))

(defanalyze :work-type [{:keys [pid response] :as _ctx}]
  (let [new-fact (cond (re-matches #".*(?i)PRODUCT.*" response) (list 'provides-product pid)
                       (re-matches #".*(?i)SERVICE.*" response) (list 'provides-service pid)
                       :else                                    (list 'fails-query 'product-or-service? pid))]
    (db/add-claim pid new-fact)))

(defanalyze :production-motivation [{:keys [pid response] :as _ctx}]
  (let [new-fact (cond (re-matches #".*(?i)MAKE-TO-STOCK.*" response)        (list 'production-mode pid 'make-to-stock)
                       (re-matches #".*(?i)MAKE-TO-ORDER.*" response)        (list 'production-mode pid 'make-to-order)
                       (re-matches #".*(?i)ENGINEER-TO-ORDER.*" response)    (list 'production-mode pid 'engineer-to-order)
                       :else                                                 (list 'fails-query 'production-mode pid))]
    (db/add-claim pid new-fact)))

;;; ToDo: This one isn't being used.
(defanalyze :work-location [{:keys [pid response] :as _ctx}]
  (let [new-fact (cond (re-matches #".*(?i)OUR-FACILITY.*" response)  (list 'has-production-facility pid)
                       (re-matches #".*(?i)CUSTOMER-SITE.*" response) (list 'performed-at-customer-site pid)
                       :else                                          (list 'fails-query 'facility-vs-site pid))]
    (db/add-claim pid new-fact)))

(defanalyze :production-system-type [{:keys [pid response] :as _ctx}]
  (let [new-fact (cond (re-matches #".*(?i)FLOW-SHOP.*" response)  (list 'flow-shop pid)
                       (re-matches #".*(?i)JOB-SHOP.*"  response)  (list 'job-shop pid)
                       :else                                       (list 'fails-query 'flow-vs-job pid))]
    (db/add-claim pid new-fact)))

(defanalyze :process-steps [{:keys [pid response client-id] :as obj}]
  ;; Nothing to do here but update state from a-list.
  (let [new-claims (analyze-process-steps-response obj)]
    (log! :info (str "---------------------- !query-process-steps: new-claims = " new-claims))
    (doseq [claim new-claims] (db/add-claim pid claim))))

(defanalyze :process-durations [{:keys [client-id response pid] :as obj}]
  (log! :info (str "process-durations (action): response =\n" response))
  (let [new-claims (analyze-process-durs-response obj)]
    (if (-> (db/get-process pid :initial-unordered) :process/sub-processes not-empty)
      (let [new-code (mzn/minimal-mzn-for-process pid)
            see-code-msg
            (str "Okay, we now know enough to get started on a MiniZinc solution. "
                 "In the code pane (upper right of the app) we created a simplistic scheduling system."
                 "It only illustrates the idea of running one job through each of the tasks you mentioned"
                 "(excepting any tasks that weren't part of making the product, those we'll deal with later.")]
        (ws/send-to-chat {:client-id client-id :dispatch-key :update-code :text new-code})
        (db/put-code pid new-code)
        ;; ToDo: This should really be just for humans. Maybe use the DB's message tags to decide that. (or to decide what to send).
        (db/add-msg {:pid pid
                     :from :system
                     :text see-code-msg
                     :tags [:info-to-user :minizinc]})
        (ws/refresh-client client-id pid :process)
        (doseq [claim new-claims] (db/add-claim pid claim)))
      (ws/send-to-chat
       {:client-id client-id
        :dispatch-key :tbd-says
        :text "There was a problem defining a process corresponding to what you said."}))))

(defanalyze :batch-size [{:keys [response] :as ctx}]
  (log! :info (str "process-ordering: response = " response))
  (analyze-batch-size-response response))

(defanalyze :process-ordering [{:keys [response] :as ctx}]
  (log! :info (str "process-ordering: response = " response))
  (analyze-process-ordering-response response))

;;; ================================ Supporting functions ===========================================
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

(defn project-name-llm-query
  "Wrap the user-text in square brackets and send it to an LLM to suggest a project name.
   Returns a string consisting of just a few words."
  [user-text]
  (as-> (conj project-name-partial
              {:role "user" :content (str "[ " user-text " ]")}) ?r
    (query-llm ?r {:model-class :gpt :raw-text? false}) ; 2024-03-23 I was getting bad results with :gpt-3.5. This is too important!
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
      (query-llm {:model-class :gpt})
      sutil/yes-no-unknown))

(defn mzn-process-steps
  "Use state information to write a string enumerating process steps."
  [state]
  (let [procs (->> state (filter #(uni/unify % '(process-step ?pid ?num ?proc))) (sort-by #(nth % 2)) (mapv #(nth % 3)))]
    (cl-format nil "enum Task = {~{~a~^, ~}};" procs)))

;;; -------------------------------- response analysis  -----------------------------------------------
;;; ToDo: Could be several more "cites" in the intro paragraph. For example, consider this, from sur-plate-glass:
;;; "Our most significant scheduling problem revolves around coordinating the manufacturing process with the fluctuating availability of raw materials,
;;; particularly high-quality silica sand and soda ash, and accommodating the variable demand from our customers.
;;; We struggle with optimizing machine utilization time and reducing downtime, while also managing delivery schedules that are dependent
;;; on these unpredictable elements. Additionally, managing the workforce to align with these changes,
;;; ensuring we have enough skilled workers available when needed, adds another layer of complexity to our operations.",
;;;
;;; This might get into skills classification, delivery schedules, downtime, machine utilization.

;;; ToDo: This was written before I decided that domain.clj ought to require db and use it. Refactor this and its caller, !initial-question ?
(defn analyze-intro-response
  "Analyze the response to the initial question.
   If init-state argument is just #{(proj-id :START-A-NEW-PROJECT)}, we are talking to humans and there is not yet a project.
   Returns new claims."
  [response init-state]
  (log! :info (-> (str "analyze-intro-response: init-state = " init-state " response = " response) (elide 150)))
  (let [[_ pid] (find-claim '(proj-id ?x) init-state)
        new-claims (atom #{})]
    (when (= pid :START-A-NEW-PROJECT)
      (let [proj-name (as-> (project-name-llm-query response) ?s
                        (str/trim ?s)
                        (str/split ?s #"\s+")
                        (map str/capitalize ?s)
                        (interpose " " ?s)
                        (apply str ?s))
            pid (as-> proj-name ?s (str/lower-case ?s) (str/replace ?s #"\s+" "-") (keyword ?s))]
        (reset! new-claims (into (filterv #(not= % '(proj-id :START-A-NEW-PROJECT)) init-state)
                                 `[(~'proj-id ~pid)
                                   (~'proj-name ~proj-name)]))))
    ;; ToDo: Maybe rework this to additionally handle skills classification, delivery schedules, downtime, machine utilization.
    (when (= :yes (text-cites-raw-material-challenge? response))
      (let [[_ pid] (find-claim '(proj-id ?x) @new-claims)]
        (swap! new-claims #(conj % `(~'cites-raw-material-challenge ~pid)))))
    @new-claims))

;;; ToDo: If this ever fails, replace it with an agent!
(defn analyze-process-steps-response
  "Return state addition for analyzing a Y/N user response about process steps.
   The state objects look like (have-process <pid> <num> <proc-name) OR (fails-process-step <line>)."
  [{:keys [response pid] :as _ctx}]
  (->> (for [line (str/split-lines response)]
         (let [[success step-num proc-name] (re-matches #"^\s*(\d+)\.?\s+([\w,\s]+).*" line)]
           (if success
             (list 'process-step pid (edn/read-string step-num) (str/trim proc-name))
             (if (re-matches #"^\s*$" line)
               nil
               (list 'fails-process-step line)))))
       (filterv identity)))

;;; ----------------- analyze-process-durs-response (done with :process-dur-agent) ----------------------------
(defn extreme-dur-span?
  "Return the vector of units used in the process if :qty/units span from from :minutes to :days or more or :hours to :weeks or more.
   This always looks at :process/inverview-class = :initial-unordered."
  [pid]
  (let [units (atom #{})]
    (letfn [(get-units [obj]
              (cond (map? obj)    (doseq [[k v] (seq obj)]
                                     (when (= k :quantity/units) (swap! units conj v))
                                     (get-units v))
                    (vector? obj) (doseq [v obj] (get-units v))))]
      (-> (db/get-process pid :initial-unordered) get-units)
      (let [units @units]
        (cond (and (units :minutes) (or (units :days)  (units :weeks) (units :months)))   (vec units)
              (and (units :hours)   (or (units :weeks) (units (units :months))))          (vec units))))))

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

(s/def ::rev-1 (fn [v] (every? #(s/valid? ::rev-1-map %) v)))
(s/def ::rev-2 (fn [v] (every? #(s/valid? ::rev-2-map %) v)))
(s/def ::rev-3 (fn [v] (every? #(s/valid? ::rev-3-map %) v)))
(s/def ::rev-4 (fn [v] (every? #(s/valid? ::rev-4-map %) v)))
(s/def ::rev-5 (fn [v] (every? #(s/valid? ::rev-5-map %) v)))

(defn switch-keys
  "Switch from LLM keys to keys used in DB."
  [obj]
  (let [db-key {:PROCESS-STEP  :PROCESS-STEP
                :PROCESS       :process/name
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

(def cvt-unit {:second :seconds, :minute :minutes, :hour :hours, :day :days, :week :weeks, :year :years})
(def bad-unit? (-> cvt-unit keys set))

(defn fix-duration-units
  "Sometimes :quantity/units values generated are the singular of what was sought.
   For example :hour was used instead of hours."
  [process-map]
  (letfn [(fdu [obj]
            (cond (map? obj)      (reduce-kv (fn [m k v]
                                               (if (= k :quantity/units)
                                                 (if (bad-unit? v)
                                                   (assoc m k (cvt-unit v))
                                                   (assoc m k (fdu v)))
                                                 (assoc m k (fdu v))))
                                             {}
                                             obj)
                  (vector? obj)   (mapv fdu obj)
                  :else           obj))]
    (fdu process-map)))

;;; (inv/run-process-agent-steps data2)
(defn run-process-dur-agent-steps
  "Run the steps of the process agent, checking work after each step."
  [response]
  (let [{:keys [aid tid]} (db/get-agent :base-type :process-dur-agent)
        past-rev (atom response)]
    (doseq [rev [::rev-1 ::rev-2 ::rev-3 ::rev-4 ::rev-5]] ; These are also spec defs, thus ns-qualified.
      (log! :info (str "Starting rev " (name rev)))
      (when @past-rev
        (let [result (llm/query-on-thread
                      {:aid aid :tid tid :role "user"
                       :tries 3 :test-fn (fn [resp] (s/valid? rev resp))
                       :preprocess-fn (fn [resp] (-> resp sutil/remove-preamble edn/read-string))
                       :query-text (str "Perform " (-> rev name str/upper-case) " on the following:\n\n" @past-rev)})]
          (reset! past-rev result)
          (log! :info (str "***************" rev " = "  @past-rev)))))
    @past-rev))

(defn post-process-durs
  "Make the process-agent's LLM output suitable for a project's DB process object."
  [process-maps pid]
  (let [cnt (atom 0)
        sub-processes (->> process-maps
                           (map switch-keys)
                           (map fix-duration-units)
                           (map #(assoc % :process/id (->> % :process/var-name (str (name pid) "--initial-unordered--") keyword)))
                           (reduce (fn [r m] (conj r (assoc m :process/step-number (swap! cnt inc)))) []))]
    {:process/id pid
     :process/interview-class :initial-unordered
     :process/sub-processes sub-processes}))


;;;------------------------------------- process ordering is similar to process durs, it has CoT "revs" -----------------------
(s/def :step-1/PROCESS-STEP number?)
(s/def :step-1/PROCESS-NAME string?)
(s/def :step-1/INPUT string?)


(s/def :step-2/PROCESS-STEP number?)
(s/def :step-2/PROCESS-NAME string?)
(s/def :step-2/RAW string?)
(s/def :step-2/SOURCE-PROCESS string?)
(s/def :step-2/INTERMEDIATE string?)
(s/def :step-2/INPUT (s/coll-of (s/or :raw           (s/keys :req-un [:step-2/RAW])
                                      :intermediage  (s/keys :req-un [:step-2/SOURCE-PROCESS :step-2/INTERMEDIATE]))
                                :kind vector?))


(s/def ::step-1-map (s/keys :req-un [:step-1/PROCESS-STEP :step-1/PROCESS-NAME :step-1/INPUT]))
(s/def ::step-2-map (s/keys :req-un [:step-2/PROCESS-STEP :step-2/PROCESS-NAME :step-2/INPUT]))

(s/def ::step-1 (fn [v] (every? #(s/valid? ::step-1-map %) v)))
(s/def ::step-2 (fn [v] (every? #(s/valid? ::step-2-map %) v)))

(defn analyze-process-ordering-response
  "Return state addition for analyzing a Y/N user response about process ordering."
  [{:keys [response] :as _ctx}]
    (let [{:keys [aid tid]} (db/get-agent :base-type :process-ordering-agent)
        past-step (atom response)]
    (doseq [step [::step-1 ::step-2]]
      (log! :info (str "Starting step " (name step)))
      (when @past-step
        (let [result (llm/query-on-thread
                      {:aid aid :tid tid :role "user"
                       :tries 3 :test-fn (fn [resp] true #_(s/valid? step resp))
                       :preprocess-fn (fn [resp] (-> resp sutil/remove-preamble edn/read-string))
                       :query-text (str "Perform " (-> step name str/upper-case) " on the following:\n\n" @past-step)})]
          (reset! past-step result)
          (log! :info (str "***************" step " = "  @past-step)))))
    @past-step))

;;; -----------------------------------------------------------------------------------------
;;; This will always be asked after process-durations.
(defn analyze-batch-size-response
  [{:keys [response pid] :as _ctx}])

(defn analyze-process-durs-response
  "Used predominantly with surrogates, study the response to a query about process durations,
   writing findings to the project database and returning state propositions."
  [{:keys [response pid] :as _ctx}]
  (let [process-objects (run-process-dur-agent-steps response)
        full-obj (post-process-durs process-objects pid)]
    (db/put-process-sequence! pid full-obj)
    (let [extreme-span? (extreme-dur-span? pid)] ; This look at the DB just updated.
      (log! :debug (str "extreme-span? = " extreme-span?))
      (cond-> [(list 'have-process-durs pid)]
        extreme-span?  (conj `(~'extreme-duration-span ~pid ~@(map #(-> % name symbol) extreme-span?)))))))
