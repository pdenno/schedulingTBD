(ns scheduling-tbd.domain.process-analysis
  "Analysis of the process interview"
  (:require
   [clojure.core.unify                 :as uni]
   [clojure.edn                        :as edn]
   [clojure.pprint                     :refer [pprint]]
   [clojure.spec.alpha                 :as s]
   [clojure.string                     :as str]
   [jsonista.core                      :as json]
   [scheduling-tbd.agent-db            :as adb]
   [scheduling-tbd.db                  :as db]
   [scheduling-tbd.minizinc            :as mzn]
   [scheduling-tbd.response-utils      :as ru :refer [defanalyze]]
   [scheduling-tbd.sutil               :as sutil]
   [scheduling-tbd.web.websockets      :as ws]
   [taoensso.telemere                  :as tel :refer [log!]]))

(def ^:diag diag (atom nil))
(declare analyze-warm-up-response analyze-process-steps-response analyze-process-durs-response
         analyze-batch-size-response analyze-process-ordering-response)

(def the-warm-up-type-question
  (str "What are the products you make or the services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences."))

;;; ToDo: warm-up question might suggest several avenues of investigation. For example, consider this, from sur-plate-glass:
;;; "Our most significant scheduling problem revolves around coordinating the manufacturing process with the fluctuating availability of raw materials,
;;; particularly high-quality silica sand and soda ash, and accommodating the variable demand from our customers.
;;; We struggle with optimizing machine utilization time and reducing downtime, while also managing delivery schedules that are dependent
;;; on these unpredictable elements. Additionally, managing the workforce to align with these changes,
;;; ensuring we have enough skilled workers available when needed, adds another layer of complexity to our operations.",
;;; This might get into skills classification, delivery schedules, downtime, machine utilization.

;;; start-conversation doesn't call this. It calls analyze-warm-up-response directly. This is only used by surrogates, so far.
(defanalyze :process/warm-up [{:keys [response pid] :as _ctx}]
  (assert (db/project-exists? pid)) ; This is only called where the project already exists.
  (let [warm-up-claims (analyze-warm-up-response response)
        needed-claims (remove #('#{temp-project-id temp-project-name} (first %)) warm-up-claims)
        bindings {'?pid pid}]
      (doseq [claim needed-claims]
        (db/add-claim! pid {:string (-> claim (uni/subst bindings) str)
                            :q-type :process/warm-up
                            :cid :process}))))

(defanalyze :process/work-type [{:keys [pid response] :as _ctx}]
  (let [new-fact (cond (re-matches #".*(?i)PRODUCT.*" response) (list 'provides-product pid)
                       (re-matches #".*(?i)SERVICE.*" response) (list 'provides-service pid)
                       :else                                    (list 'fails-query 'work-type pid))]
    (db/add-claim! pid {:string (str new-fact) :cid :process :q-type :process/work-type})))

(defanalyze :process/production-location [{:keys [pid response] :as _ctx}]
  (let [new-fact (cond (re-matches #".*(?i)OUR-FACILITY.*" response)  (list 'production-location pid 'factory)
                       (re-matches #".*(?i)CUSTOMER-SITE.*" response) (list 'production-location pid 'customer-site)
                       :else                                    (list 'fails-query 'production-location pid))]
    (db/add-claim! pid {:string (str new-fact) :cid :process :q-type :process/production-location})))


(defanalyze :process/production-motivation [{:keys [pid response] :as _ctx}]
  (let [new-fact (cond (re-matches #".*(?i)MAKE-TO-STOCK.*" response)        (list 'production-mode pid 'make-to-stock)
                       (re-matches #".*(?i)MAKE-TO-ORDER.*" response)        (list 'production-mode pid 'make-to-order)
                       (re-matches #".*(?i)ENGINEER-TO-ORDER.*" response)    (list 'production-mode pid 'engineer-to-order)
                       :else                                                 (list 'fails-query 'production-mode pid))]
    (db/add-claim! pid {:string (str new-fact) :cid :process :q-type :process/production-motivation})))

(defanalyze :process/production-system-type [{:keys [pid response] :as _ctx}]
  (let [new-fact (cond (re-matches #".*(?i)FLOW-SHOP.*" response)  (list 'flow-shop pid)
                       (re-matches #".*(?i)JOB-SHOP.*"  response)  (list 'job-shop pid)
                       :else                                       (list 'fails-query 'flow-vs-job pid))]
    (db/add-claim! pid {:string (str new-fact) :cid :process :q-type :process/production-system-type})))

(defanalyze :process/process-steps [{:keys [pid] :as obj}]
  (let [new-claims (analyze-process-steps-response obj)]
    (log! :debug (str "---------------------- !query-process-steps: new-claims = " new-claims))
    (doseq [claim new-claims]
      (db/add-claim! pid {:string (str claim) :cid :process :q-type :process/process-steps}))))

(defanalyze :process/process-durations [{:keys [client-id response pid] :as obj}]
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
                     :cid :process
                     :from :system
                     :text see-code-msg
                     :tags [:info-to-user :minizinc]})
        (doseq [claim new-claims]
          (db/add-claim! pid {:string (str claim) :cid :process :q-type :process/process-durations})))
      (ws/send-to-chat
       {:client-id client-id
        :dispatch-key :tbd-says
        :text "There was a problem defining a process corresponding to what you said."}))))

;;; ToDo: Write to db.
(defanalyze :process/batch-size [{:keys [response] :as ctx}]
  (log! :info (str "process-ordering: response = " response))
  (analyze-batch-size-response response))

;;; ToDo: Write to db.
(defanalyze :process/process-ordering [{:keys [response] :as ctx}]
  (log! :info (str "process-ordering: response = " response))
  (analyze-process-ordering-response response))

(s/def ::scheduling-challenges-response (s/keys :req-un [::product-or-service-name, ::challenges ::one-more-thing]))
(s/def ::product-or-service-name string?)
(s/def ::challenges (s/coll-of keyword? :kind vector?))
(s/def ::one-more-thing string?)

(defn run-scheduling-challenges-agent
  "Analyze the response for project name,a fixed set of 'scheduling challenges' and 'one-more-thing', an observation.
   Returns a map {:project-or-service-name, :challenge <keywords naming challenges> :one-more-thing <a string>}."
  [response]
  (let [{:keys [one-more-thing] :as res}  (-> (adb/query-agent :scheduling-challenges-agent response {:tries 2})
                                              json/read-value
                                              (update-keys str/lower-case)
                                              (update-keys keyword)
                                              (update :challenges #(mapv keyword %)))]
    (when (not-empty one-more-thing)
      (log! :info (str "one-more-thing: " one-more-thing))) ; This just to see whether another claim should be added to the agent.
    (when-not (s/valid? ::scheduling-challenges-response res)
      (log! :error (str "Invalid scheduling-challenges-response: " (with-out-str (pprint res)))))
    res))

;;; (pan/analyze-warm-up-response pant/ice-cream-answer-warm-up)
(defn analyze-warm-up-response
  "Analyze the response to the initial question.
   Returns a collection of new NON-GROUND claims, including scheduling-challenges and project-id and project-name predicates.
   Not everything here will be asserted as claims:
     1) Actual project-name and project-id are calculated by db/create-proj-db, which already happened for surrogate.

   If this is called by a human project the project-id and project-name can be retracted later.
   If this is called by a surrogate project, the project-id and project-name are already known."
  [response]
  (let [{:keys [product-or-service-name challenges]}  (run-scheduling-challenges-agent response)
        project-name (as-> product-or-service-name ?s
                       (str/trim ?s)
                       (str/split ?s #"\s+")
                       (map str/capitalize ?s)
                       (interpose " " ?s)
                       (apply str ?s))
        pid (as-> product-or-service-name ?s
              (str/trim ?s)
              (str/lower-case ?s)
              (str/split ?s #"\s+")
              (interpose "-" ?s)
              (apply str ?s)
              (keyword ?s))]
    ;; We will unify on DB's ?pid later.
    (-> #{}
        (conj (list 'temp-project-id pid))                 ; Used to create project database if human.
        (conj (list 'temp-project-name '?pid project-name))   ; Used to create project database if human.
        (conj (list 'principal-expertise '?pid project-name))
        (into (for [claim challenges]
                (list 'scheduling-challenge '?pid claim))))))

;;; ToDo: I can't decide whether or not I want these process-steps in claims. I suppose it can't hurt.
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
;;; ToDo: This isn't quite sufficient. If, for example, you have hours & days, which is commmon, for example, with craft beer,
;;;       then it won't be recognized as extreme. It would be better to gather the smallest amount and largest amount. It would also be
;;;       useful to note where there are groups of short processes that can be combined.
;;; enum Task = {millGrains, mashing, lautering, boiling, fermentation, conditioning, filtering, carbonation, bottlingCanning, packaging};
;;; array [Product, Task] of float: taskDuration = [|1.000, 1.500, 1.000, 1.000, 252.000, 60.000, 3.000, 24.000, 3.000, 3.000|];
;;; ... So you could use the code that does this MZn translation. Makes use of averages too.
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
  (let [past-rev (atom response)]
    (doseq [rev [::rev-1 ::rev-2 ::rev-3 ::rev-4 ::rev-5]] ; These are also spec defs, thus ns-qualified.
      (log! :info (str "process-dur-agent, rev " (name rev)))
      (when @past-rev
        (let [action-text (str "Perform " (-> rev name str/upper-case) " on the following:\n\n" @past-rev)
              result (adb/query-agent :process-dur-agent
                                      action-text
                                      {:tries 3
                                       :test-fn (fn [resp] (s/valid? rev resp))
                                       :preprocess-fn (fn [resp] (-> resp sutil/remove-preamble edn/read-string))})]
          (reset! past-rev result)
          (log! :debug (str "process-dur-agent " (name rev) " =\n" (with-out-str (pprint @past-rev)))))))
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
  "Return structured object for process ordering question."
  [{:keys [response] :as _ctx}]
    (let [past-step (atom response)]
    (doseq [step [::step-1 ::step-2]]
      (log! :info (str "Starting step " (name step)))
      (when @past-step
        (let [action-text (str "Perform " (-> step name str/upper-case) " on the following:\n\n" @past-step)
              result (adb/query-agent :process-ordering-agent
                                      action-text
                                      {:tries 3
                                       :test-fn (fn [_resp] true #_(s/valid? step resp)) ; ToDo: check s/valid?
                                       :preprocess-fn (fn [resp] (-> resp sutil/remove-preamble edn/read-string))})]
          (reset! past-step result)
          (log! :debug (str "process-ordering-agent " (name step) " :\n" (with-out-str (pprint @past-step)))))))
    @past-step))

;;; -----------------------------------------------------------------------------------------
;;; This will always be asked after process-durations.
(defn analyze-batch-size-response
  [{:keys [_response _pid] :as _ctx}])

(defn analyze-process-durs-response
  "Used predominantly with surrogates, study the response to a query about process durations,
   writing findings to the project database and returning state propositions."
  [{:keys [response pid] :as _ctx}]
  (let [process-objects (run-process-dur-agent-steps response)
        full-obj (post-process-durs process-objects pid)]
    (db/put-process-sequence! pid full-obj)
    (let [extreme-span? (extreme-dur-span? pid)] ; This look at the DB just updated.
      (log! :debug (str "extreme-span? = " extreme-span?))
      (cond-> []
        extreme-span?  (conj `(~'extreme-duration-span ~pid ~@(map #(-> % name symbol) extreme-span?)))))))
