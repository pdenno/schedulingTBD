(ns scheduling-tbd.interviewers-test
  (:require
   [clojure.edn                   :as edn]
   [clojure.pprint                :refer [pprint]]
   [clojure.test                  :refer [deftest is testing]]
   [datahike.api                  :as d]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.interviewers   :as inv]
   [scheduling-tbd.llm            :as llm]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere             :as tel :refer [log!]]
   [taoensso.truss                :as truss :refer [have have?]]))

(def ^:diag diag (atom nil))

(def alias? (atom (-> (ns-aliases *ns*) keys set)))

(defn safe-alias
  [al ns-sym]
  (when (and (not (@alias? al))
             (find-ns ns-sym))
    (alias al ns-sym)))

(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (reset! alias? (-> (ns-aliases *ns*) keys set))
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 's      'clojure.spec.alpha)
  (safe-alias 'uni    'clojure.core.unify)
  (safe-alias 'edn    'clojure.edn)
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 'str    'clojure.string)
  (safe-alias 'd      'datahike.api)
  (safe-alias 'dp     'datahike.pull-api)
  (safe-alias 'json   'jsonista.core)
  (safe-alias 'mount  'mount.core)
  (safe-alias 'p      'promesa.core)
  (safe-alias 'px     'promesa.exec)
  (safe-alias 'core   'scheduling-tbd.core)
  (safe-alias 'pan    'scheduling-tbd.domain.process-analysis)
  (safe-alias 'db     'scheduling-tbd.db)
  (safe-alias 'how    'scheduling-tbd.how-made)
  (safe-alias 'llm    'scheduling-tbd.llm)
  (safe-alias 'llmt   'scheduling-tbd.llm-test)
  (safe-alias 'mzn    'scheduling-tbd.minizinc)
  (safe-alias 'mznt   'scheduling-tbd.minizinc-test)
  (safe-alias 'ou     'scheduling-tbd.op-utils)
  (safe-alias 'opt    'scheduling-tbd.operators-test)
  (safe-alias 'ru     'scheduling-tbd.response-utils)
  (safe-alias 'spec   'scheduling-tbd.specs)
  (safe-alias 'sutil  'scheduling-tbd.sutil)
  (safe-alias 'sur    'scheduling-tbd.surrogate)
  (safe-alias 'surt   'scheduling-tbd.surrogate-test)
  (safe-alias 'util   'scheduling-tbd.util)
  (safe-alias 'resp   'scheduling-tbd.web.controllers.respond)
  (safe-alias 'ws     'scheduling-tbd.web.websockets)
  (safe-alias 'tel    'taoensso.telemere)
  (safe-alias 'openai 'wkok.openai-clojure.api))

;; THIS is 2 (the other namespace I am hanging out in recently).
;;; Remember to db/backup-system-db once you get things straight.

(defn ^:diag recreate-interview-agent!
  "This is used to experiment with different LLMs and value of flow-charts.
   Use, for example process-read-the-flowchart.txt rather than the full instructions."
  []
  (db/recreate-system-agents!
   [{:id :process-interview-agent
     :project-thread?  true
     :model-class :gpt ; :analysis is o1-preview, and it cannot be used with the Assistants API.
     :tools [{:type "file_search"}]
     :vector-store-files ["data/instructions/interviewers/process-interview-flowchart.pdf"]
     :instruction-path "data/instructions/interviewers/process.txt" #_"data/instructions/interviewers/process-read-the-flowchart.txt"
     ;; I chose not to uses response format: "Invalid tools: all tools must be of type `function` when `response_format` is of type `json_schema`
     #_#_:response-format (-> "data/instructions/interviewers/response-format.edn" slurp edn/read-string)}]))

;;; Used to query the flowchart using the process-interview-agent.
(defonce process-interview-system-agent (atom {}))

(defn ^:diag setup-flowchart-with-agent! []
  (let [agent (db/get-agent :base-type :process-interview-agent)
        thread (llm/make-thread :assistant-id (:aid agent))]
    (reset! process-interview-system-agent {:aid (:aid agent) :tid (:id thread)})))

;;; ToDo: Consider asking these two questions first on every thread!
;;; (ask-about-flowchart "Summarize what you see in the uploaded file 'Process Interview Flowchart'."  @process-interview-system-agent)
;;; (ask-about-flowchart "Suppose the production-system-type was FLOW-SHOP. In the order in which they are to be asked, what are all the question-types you would ask?" @process-interview-system-agent)
;;; Typical response to the query directly above this line:

;;; If the production-system-type is FLOW-SHOP, the questions would be asked in the following order:
;;;
;;; 1. warm-up
;;; 2. work-type
;;; 3. production-location
;;; 4. production-motivation
;;; 5. production-system-type
;;; 6. process-steps
;;; 7. process-durations
;;; 8. batch-size
;;; 9. process-ordering
;;;
;;; These question-types guide the interview from understanding what the entity does to detailed aspects of their production process,
;;; ultimately helping in designing an effective scheduling system for their FLOW-SHOP production system.
(defn ^diag ask-about-flowchart
  [query {:keys [aid tid]}]
  (println (llm/query-on-thread :aid aid :tid tid :query-text query)))

(defn ^:diag ask-about-flowchart:inverviewer [])

(defn tell-one
  "Diagnostic for one interaction with interviewer."
  [cmd {:keys [pid conv-id] :as ctx}]
  (inv/tell-interviewer cmd
                        (merge ctx (inv/interview-agent conv-id pid))))

(deftest finished-process-test
  (testing "Testing that :sur-ice-cream has finished all process questions."
    (is (= {:status "DONE"}
           (do (tell-one (inv/prior-responses :sur-ice-cream :process) {:pid :sur-ice-cream :conv-id :process})
               (tell-one {:command "SUPPLY-QUESTION"} {:pid :sur-ice-cream :conv-id :process}))))))

(deftest test-vector-stores
  (let [{:keys [id object] :as obj}
        (llm/make-vector-store
         {:name "Process Interview flowchart"
          :file_ids [(llm/upload-file {:fname "data/instructions/interviewers/process-interview-flowchart.pdf"})]})]
    (reset! diag obj)
    (is (and (string? id) (= object "vector_store")))))

(defn ^:diag check-instructions
  "It might be the case that the system instructions were too long. This asks what it knows about."
  []
  (let [{:keys [aid tid]} (db/get-agent :base-type :process-dur-agent)]
    (llm/query-on-thread
     {:aid aid :tid tid :role "user"
      :query-text (str "I provided instructions to perform a number of transformation we call 'REVs', "
                       "REV-1, REV-2, etc. What are the REVs that you know about, and what do they do?")})))

;;; ToDo: BTW, "consulting" was thrown in there to test "DONE" in interviewing. Consulting is a kind of service; maybe it is an okay scheduling domain.
(def q-work-type
  "This is typical of what you get back from the interviewer when you do SUPPLY-QUESTION."
  {:question-type :work-type,
   :status "OK",
   :question "Would you characterize your company's work as primarily providing a product, a service, or consulting? Respond respectively with the single word PRODUCT, SERVICE, or CONSULTING."})

(defn ask-one-question
  "Use inv/chat-pair to get back an answer"
  [pid {:keys [question question-type]}]
  (let [{:surrogate/keys [assistant-id thread-id]} (db/get-surrogate-agent-info pid)]
    (inv/loop-for-answer
     question
     {:pid pid
      :sur-aid assistant-id
      :sur-tid thread-id
      :responder-role :surrogate
      :client-id (ws/recent-client!)
      :question-type question-type})))

(deftest question-asking
  (testing "Testing question asking capability through loop-for-answer."
    (when (or (nil? (db/get-project :sur-fountain-pens :error? false))
              (nil? (ws/recent-client!)))
      (log/warn "No project or client to run interview_test/question-asking."))
    (is (or (nil? (db/get-project :sur-fountain-pens :error? false))
            (nil? (ws/recent-client!))
            (let [pid :sur-fountain-pens]
              (= {:command "HUMAN-RESPONDS", :response "PRODUCT", :question-type :work-type}
                 (ask-one-question pid q-work-type)))))))
;;; ================================================= Throw-away-able updating of project.edn =====================================
(def ^:diag tag2qname
  {:!describe-challenge :initial-question
   :query-product-or-service :work-type
   :!query-product-or-service :work-type
   :query-production-mode :production-motivation
   :!query-production-mode :production-motivation
   :query-activity-location-type :production-location
   :!query-activity-location-type :production-location
   :query-shop-type :production-system-type
   :!query-shop-type :production-system-type
   :query-process-steps :process-steps
   :!query-process-steps :process-steps
   :query-process-durs :process-durations
   :!remark-raw-material-challenge :remark-raw-material-challenge
   :!query-process-durs :process-durations})

(def qname? (-> tag2qname vals set))

(defn ^:diag rename-tag-to-qname
  "If a tag matches any tag2qname, replace it with that qname."
  [project-map]
  (letfn [(rename-tag [obj]
            (cond (and (map? obj) (contains? obj :message/id))  (reduce-kv (fn [m k v]
                                                                             (if (= k :message/tags)
                                                                               (assoc m k (mapv #(if-let [new-tag (get tag2qname %)] new-tag %) v))
                                                                               (assoc m k v)))
                                                                           {}
                                                                           obj)
                  (map? obj)                                     (reduce-kv (fn [m k v] (assoc m k (rename-tag v))) {} obj)
                  (vector? obj)                                  (mapv rename-tag obj)
                  :else                                          obj))]
    (rename-tag project-map)))

(defn ^:diag add-qname
  [project-map]
  (letfn [(add-qn [obj]
            (cond (and (map? obj) (contains? obj :message/id))  (if-let [qname (some #(when (qname? %) %) (:message/tags obj))]
                                                                  (assoc obj :message/question-name qname)
                                                                  obj)
                  (map? obj)                                     (reduce-kv (fn [m k v] (assoc m k (add-qn v))) {} obj)
                  (vector? obj)                                  (mapv add-qn obj)
                  :else                                          obj))]
    (add-qn project-map)))

(defn update-project [pid]
  (as-> pid ?o
    (db/get-project ?o)
    ;;(rename-tag-to-qname ?o)
    ;;(add-qname ?o)
    (vector ?o)
    (with-out-str (pprint ?o))
    (spit (str "data/new-projects/" (name pid) ".edn") ?o)))

(defn add-state-vector [pid]
  (let [conn (connect-atm pid)
        eid (d/q '[:find ?eid . :where [?eid :conversation/id :process]] @conn)
        new-ones [:initial-question
                  :work-type
                  :production-motivation
                  :production-system-type
                  :process-steps
                  :process-durations
                  :batch-size
                  :process-ordering]]
    (if eid
      (d/transact conn {:tx-data (vec (for [n new-ones] [:db/add eid :conversation/state-vector n]))})
      (log/error "No eid"))))
