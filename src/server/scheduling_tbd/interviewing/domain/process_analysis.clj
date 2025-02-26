(ns scheduling-tbd.interviewing.domain.process-analysis
  "Analysis of the process interview"
  (:require
   [clojure.core.unify                 :as uni]
   [clojure.pprint                     :refer [pprint]]
   [clojure.spec.alpha                 :as s]
   [clojure.string                     :as str]
   [jsonista.core                      :as json]
   [scheduling-tbd.agent-db            :as adb]
   [scheduling-tbd.db                  :as db]
   [scheduling-tbd.minizinc            :as mzn]
   [scheduling-tbd.response-utils      :as ru :refer [defanalyze]]
   [taoensso.telemere                  :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

(def the-warm-up-type-question
  (str "What are the products you make or the services you provide, and what is the scheduling challenge involving them? Please describe in a few sentences."))

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
        (conj (list 'temp-project-id pid))                    ; Used to create project database if human.
        (conj (list 'temp-project-name '?pid project-name))   ; Used to create project database if human.
        (conj (list 'principal-expertise '?pid project-name))
        (into (for [claim challenges]
                (list 'scheduling-challenge '?pid claim))))))

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
(defn extreme-dur-span?
  "Return the vector of units used in the process if :qty/units span from :minutes to :days or more or :hours to :weeks or more.
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
