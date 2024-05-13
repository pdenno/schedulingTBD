(ns scheduling-tbd.domain
  "Scheduling domain prompts and analysis
     - analyze: Function names that begin with 'analyze' study user/surrogate response and produce planning propositions.
     - mzn: Function names that begin with 'mzn' use planning state to modify MiniZinc code."
  (:require
   [clojure.core.unify            :as uni]
   [clojure.edn                   :as edn]
   [clojure.pprint                :refer [cl-format]]
   [clojure.string                :as str]
   [datahike.api                  :as d]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.llm            :as llm :refer [query-llm]]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm find-fact yes-no-unknown string2sym]]
   [taoensso.timbre               :as log]))

(def ^:diag diag (atom nil))

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

;;; ToDo: Seeing how we are getting so much text about supply chain/inventory management, maybe not mention supply chain.
;;;       I'd like to say "manufacturing processes" rather than "business's processes" (where appropriate) so maybe run this after mfg/service query.
;;;       In the mfg case, the user would ask about manufacturing scheduling problems.
;;;       Thus have two versions of this, one for manufacturing and one for services.
(defn pretend-you-manage-prompt
  [manage-what]
  [{:role "system"    :content (format "Pretend you manage %s. You have good knowledge of the business's processes and supply chain." manage-what)}
   {:role "user"      :content "In no more than 5 sentences, describe your principal scheduling problem."}])

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
      (query-llm {:model-class :gpt-3.5})
      yes-no-unknown))

;;; ----------------------- parallel expert preliminary analysis  ---------------------------------------------------------
(defn ask-about-process-on-thread!
  "Ask the surrogate to describe production processes. We don't care what is returned, we just
   are developing context to do some downstream preliminary analysis.
   We put these into the conversation under surrogate."
  [aid tid]
  (let [msg "Please briefly describe your production processes."]
    {:query msg
     :answer (llm/query-on-thread :aid aid :tid tid :query-text msg)}))

(defn product-vs-service
  "Return a vector of ground predicates (so far just either [(provides-product ?x)] or [(provides-service ?x)],
   depending on whether the project describes respectively work to provide a product or work to provide a service."
  [aid tid]
  (let [query (str "Would you characterize your company's work as primarily providing a product or a service? " ; ToDo: "firm's work" in all of these not good?
                   "Respond respectively with either the single word PRODUCT or SERVICE.")
        answer (llm/query-on-thread {:aid aid :tid tid :query-text query})
        preds (cond (re-matches #".*(?i)PRODUCT.*" answer) '[(provides-product ?x)]
                    (re-matches #".*(?i)SERVICE.*" answer) '[(provides-service ?x)]
                    :else                                  '[(fails-query product-vs-service ?x)])]
    {:query query :answer answer :preds preds}))

;;; ToDo: Maybe what I really want is to split scheduling vs some notion of constraint satisfaction (which would include project management and cyclical scheduling)
;;;       But for the time meaning, it was this that came to mind.
(defn production-mode
  "Return a vector of one predicates of the form (production-mode ?x <mode>) where <mode> is on of make-to-stock, make-to-order, or engineer-to-order.
   depending on what the agent deems more relevant to their operations."
  [aid tid]
  (let [query (str "Three commonly recognized ways of production are termed MAKE-TO-STOCK, MAKE-TO-ORDER, and ENGINEER-TO-ORDER. "
                   "In MAKE-TO-STOCK you make product to replenish inventory based on forecasted demand. "
                   "In MAKE-TO-ORDER you make product because a customer has specifically asked you to, and the customer has described characteristics of the product in your own terminology, perhaps using your catalog of offerings. "
                   "ENGINEER-TO-ORDER is something like MAKE-TO-ORDER but here the customer also expects you to do some creative problem solving to meet their need. "
                   "For example, a commercial aircraft might be ENGINEER-TO-ORDER because though the customer may have specified the engine type and seating capacity it wants, "
                   "it is relying on you to determine how to best accommodate the engine and arrange the seats. "
                   "Other examples of ENGINEER-TO-ORDER include general contracting for building construction, film production, event planning, and 3rd party logisistics. "
                   "Respond with just one of the terms MAKE-TO-STOCK, MAKE-TO-ORDER or ENGINEER-TO-ORDER according to which most accurately describes your mode of production. ")
        answer (llm/query-on-thread {:aid aid :tid tid :query-text query})
        preds (cond (re-matches #".*(?i)MAKE-TO-STOCK.*" answer)        '[(production-mode ?x make-to-stock)]
                    (re-matches #".*(?i)MAKE-TO-ORDER.*" answer)        '[(production-mode ?x make-to-order)]
                    (re-matches #".*(?i)ENGINEER-TO-ORDER.*" answer)    '[(production-mode ?x engineer-to-order)]
                    :else                                               '[(fails-query production-mode ?x)])]
    {:query query :answer answer :preds preds}))

(defn facility-vs-site
  "Return a vector of ground predicates (so far just either [(is-product ?x)] or [(is-service ?x)],
   depending on whether the project describes respectively work to provide a product or work to provide a service."
  [aid tid]
  (let [query (str "Some work, for example factory work, must be performed in a specially designed facility. "
                   "Other work, like cutting down a tree, can only be performed at a location designated by the customer. "
                   "Are the processes you describe things that must be performed at your facility, or are they things that must be done at the customer's site? "
                   "Respond respectively with either the single term OUR-FACILITY or CUSTOMER-SITE.")
        answer (llm/query-on-thread {:aid aid :tid tid :query-text query})
        preds (cond (re-matches #".*(?i)OUR-FACILITY.*" answer)  '[(has-production-facility ?x)]
                    (re-matches #".*(?i)CUSTOMER-SITE.*" answer) '[(performed-at-customer-site ?x)]
                    :else                                        '[(fails-query facility-vs-site ?x)])]
    {:query query :answer answer :preds preds}))

(defn flow-vs-job
  "Return a vector of ground predicates (so far just either [(is-flow-shop ?x)] or [(is-job-shop ?x)] or [])
   depending on whether the project describes respectively work to provide a product or work to provide a service.
   Note that this question is only applied where (provides-product ?x) (scheduling-problem ?x) (has-production-facility ?x)."
  [aid tid]
  (let [query (str "A FLOW-SHOP is a production system designed so that all jobs follows the same sequence of steps through production resources. "
                   "A JOB-SHOP is a production system where each job might follow its own route, depending on its unique requirements. "
                   "Is the process you described more like a flow-shop or a job-shop? "
                   "Respond respectively with either the single term FLOW-SHOP or JOB-SHOP.")
        answer (llm/query-on-thread {:aid aid :tid tid :query-text query})
        preds (cond (re-matches #".*(?i)FLOW-SHOP.*" answer) '[(flow-shop ?x)]
                    (re-matches #".*(?i)JOB-SHOP.*" answer)  '[(job-shop ?x)]
                    :else                                    '[(fails-query flow-vs-job ?x)])]
    {:query query :answer answer :preds preds}))

;;; ToDo: Define attributes in DB and test starting a parallel expert.
(defn parallel-expert-prelim-analysis
  "Query surogate for preliminary characterization of the scheduling problem including:
      - product vs. service
      - production facility vs. customer site,
      - scheduling vs. project management, and
      - flow-shop vs. job-shop   (of course, this one only for product/production facility/scheduling).
   If the project is a surrogate expert, this work is done in the surrogate thread,
   otherwise a new surrogate (called a 'parallel expert') is started and its
   Assistant ID and Thread ID are stored. (This part not done yet.)
   Return a vector of new propositions."
  ([pid] (parallel-expert-prelim-analysis pid true))
  ([pid write?]
   (when-not (find-fact '(surrogate ?proj) (db/get-planning-state pid)) ; ToDo: Complete for human expert.
     (throw (ex-info "Parallel expert not yet implemented." {})))
   (let [proj-sym (-> pid name symbol)
         proj-bind {'?x proj-sym}
         aid (db/get-assistant-id pid)
         tid (db/get-thread-id pid)
         new-props (atom [])
         {:keys [query answer]} (ask-about-process-on-thread! aid tid)] ; This one asks a question that sets up context.
     (if (string? answer)
       (do (when write?
             (db/add-msg pid :system query)
             (db/add-msg pid :surrogate answer [:process-description]))
           (doseq [f [product-vs-service production-mode facility-vs-site]]
             (let [{:keys [query answer preds]} (f aid tid)]
               (swap! new-props into (map #(uni/subst % proj-bind) preds))
               (Thread/sleep 1000) ; ToDo: I'm guessing here. There might be problems in polling OpenAI to quickly???
               (when write?
                 (db/add-msg pid :system query [:query])
                 (db/add-msg pid :surrogate answer [:response]))))
           ;; The job-shop/flow-shop question is relevant only in situations shown.
           (when (and (find-fact '(provides-product ?x) @new-props)
                      (find-fact '(has-production-facility ?x) @new-props))
             (let [{:keys [query answer preds]} (flow-vs-job aid tid)
                   more-props (map #(uni/subst % proj-bind) preds)]
             (swap! new-props into more-props)
             (when write?
               (db/add-msg pid :system query)
               (db/add-msg pid :surrogate answer))))
           @new-props)
       [(list 'fails-query 'process-description proj-sym)]))))

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

;;; ToDo: The process-step propositions are used to create a MZn enum. Otherwise not needed; they are going to be in the DB.
(defn analyze-process-steps-response
  "Return state addition for analyzing a Y/N user response about process steps."
  [{:keys [response pid] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        steps (->> (for [line (str/split-lines response)]
                     (let [[success step-num proc-name] (re-matches #"^\s*(\d+)\.?\s+(\w+).*" line)]
                       (if success
                         (list 'process-step pid-sym (edn/read-string step-num) (string2sym proc-name))
                         (if (re-matches #"^\s*$" line)
                           nil
                           (list 'fails-process-step line)))))
                   (filterv identity))]
    (conj steps (list 'have-process-steps pid-sym))))

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

;;; ToDo: If this prove unreliable, consider using an LLM. I anticipate using this function only with surrogates.
(defn parse-duration
  "Do whatever it takes to understand a string that supposed to have a duration in it.
   Return a map with properties describing a quantity of time.
   The properties may include :dur :dur-low :dur-high :conditions and :error"
  [s]
  (let [minutes? (when (re-matches #".*minute.*" s) :minutes)
        hours?   (when (re-matches #".*hour.*" s) :hours)
        days?    (when (re-matches #".*day.*" s) :days)
        weeks?   (when (re-matches #".*week.*" s) :weeks)
        months?  (when (re-matches #".*month.*" s) :months)
        units (filter identity [minutes? hours? days? weeks? months?])
        complex? (> (count units) 1)
        units (first units)
        [_ range-low range-high] (re-matches #".*(\d+)\-(\d+).*" s)
        [_ qty] (re-matches #".*(\d+).*" s)
        qty (or qty (cond (re-matches #".*\s*several\s+.*" s) :several
                          (re-matches #".*\s*many\s+.*" s) :many
                          (re-matches #".*\s*a few\s+.*" s) :a-few))]
    ;; ToDo: "between...and ... ??? Or just forget it???
    (if complex?
      (when-let [[low high] (str/split s #"\s+to\s+")]
        {:quantity-range/low  (parse-duration low)
         :quantity-range/high (parse-duration high)})
      (cond-> (cond (not qty) {:error s}
                    range-low {}
                    :else     {:quantity/value-string (str qty) :quantity/units units})
        range-low  (assoc :quantity-range/low {:quantity/value-string range-low  :quantity/units units})
        range-high (assoc :quantity-range/high {:quantity/value-string range-high :quantity/units units})))))

(defn put-process-sequence!
  "Write project/process-sequence to the project's database.
   The 'infos' argument is a vector of maps such as produced by analyze-process-durs-response."
  [infos pid response]
  (let [p-names (mapv #(-> (sutil/string2sym (str (name pid) "--" (:process %))) keyword) infos)
        objs (reduce (fn [res ix]
                       (let [{:keys [duration]} (nth infos ix)]
                         (conj res (cond-> {:process/id (nth p-names ix)}
                                     duration (assoc :process/duration duration)
                                     (> ix 0) (assoc :process/pre-processes [(nth p-names (dec ix))])))))
                     []
                     (-> p-names count range))
        full-obj {:process/id pid
                  :process/desc response
                  :process/sub-processes objs}
        conn (connect-atm pid)
        eid (db/project-exists? pid)]
    (d/transact conn {:tx-data [{:db/id eid :project/processes full-obj}]})))


(defn analyze-process-durs-response
  "Used predominantly with surrogates, study the response to a query about process durations,
   writing findings to the project database and returning state propositions."
  [{:keys [response pid] :as _obj}]
  (let [failures (atom [])
        proj-sym (-> pid name symbol)
        lines (str/split-lines response)
        processes (mapv #(let [[line process-order _ process] (re-matches #"^(\d+)(\.)?([^\(]+).*" %)]
                           (if line
                             {:line line
                              :order (edn/read-string process-order)
                              :process (str/trim process)}
                             {:failure %}))
                        lines)
        infos (->> processes
                   (map (fn [{:keys [line failure] :as obj}]
                          (if line
                            (if-let[[_ dur-str] (re-matches #"^\s*\d+[^\(]+(.+)" line)] ; I'm keeping the parentheses for now.
                              (assoc obj :dur-str dur-str)
                              (do (swap! failures #(into % (list 'fails-duration-parse line))) nil))
                            (do (swap! failures #(into % (list 'fails-duration-parse failure))) nil))))
                   (filter identity)
                   (mapv #(assoc % :duration (parse-duration (:dur-str %)))))]
    (put-process-sequence! infos pid response)
    (let [extreme-span? (extreme-dur-span? pid pid)]
      ;;(log/info "extreme-span? =" extreme-span?)
      (cond-> [(list 'have-process-durs proj-sym)]
        true              (into @failures)
        extreme-span?    (conj `(~'extreme-duration-span ~proj-sym ~@(map #(-> % name symbol) extreme-span?)))))))

;;; --------------------------------------- unimplemented (from the plan) -----------------------------

(defn analyze-process-ordering-response
  "Return state addition for analyzing a Y/N user response about process ordering."
  [_response]
  [])
