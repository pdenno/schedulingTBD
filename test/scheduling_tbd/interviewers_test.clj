(ns scheduling-tbd.interviewer-test
  (:require
   [clojure.edn                   :as edn]
   [clojure.java.io               :as io]
   [clojure.pprint                :refer [pprint]]
   [clojure.test                  :refer [deftest is testing]]
   [datahike.api                  :as d]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.domain.process-analysis :as pan]
   [scheduling-tbd.interviewers   :as inv]
   [scheduling-tbd.llm            :as llm]
   [scheduling-tbd.response-utils :as ru]
   [scheduling-tbd.sutil          :as sutil :refer [connect-atm default-llm-provider]]
   [taoensso.timbre :as log :refer [warn debug]]))

;;; THIS is the namespace I am hanging out in recently.

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
  (safe-alias 'openai 'wkok.openai-clojure.api))

(defn tryme []
  (let [aid (:agent/assistant-id (db/get-agent :base-type :process-interview-agent))
        tid (:id (llm/make-thread {:assistant-id aid
                                   :llm-provider :openai
                                   :metadata {:usage :project-agent}}))]
    (letfn [(tell-agent [text] (log/info (llm/query-on-thread {:aid aid :tid tid :role "user" :query-text text})))]
      (tell-agent "{'command' : 'ANALYSIS-CONCLUDES', 'conclusions' : '1) They make plate glass. 2) You are talking to surrogate humans (machine agents).'}")
      (tell-agent "{'command' : 'NEXT-QUESTION'}")
      (tell-agent "{'command' : 'HUMAN-RESPONDS',
                    'response' :
'One of our most significant scheduling problems involves coordinating the production schedule with the supply of raw materials.
 Fluctuations in raw material deliveries can disrupt planned production runs, leading to delays and inefficiencies.
 Additionally, we need to balance these inconsistencies with varying demand from customers, ensuring that we meet order deadlines without overproducing and holding excess inventory.'}")
      (tell-agent "{'command' : 'NEXT-QUESTION'}")
      ;; Here I'm guessing, of course.
      (tell-agent "{'command' : 'HUMAN-RESPONDS',
'Our production process for plate glass involves several key stages.
 It starts with raw material preparation, where materials like silica sand, soda ash, and limestone are accurately measured and mixed.
 This mixture is then fed into a furnace and melted at very high temperatures to form molten glass.
 The molten glass is then formed into sheets using the float glass process, where it is floated on a bed of molten tin.
 After forming, the glass is slowly cooled to prevent stress in a process called annealing.
 Finally, the glass is cut to size, inspected for quality, and prepared for shipment.'")
      (tell-agent "{'command' : 'NEXT-QUESTION'}"))))

;;; Remember to db/backup-system-db once you get things straight.
(defn new-interviewer
  "Use this whenever you need to adjust the instructions."
  []
  (let [{:keys [id instruction-path response-path]}
        {:id :process-interview-agent
         :instruction-path     "data/instructions/interviewer-process.txt"
         :response-format-path "data/instructions/interviewer-response-format.edn"}]
    (db/add-agent!
     {:id (-> id name (str "-" "openai") keyword)
      :base-type id
      :project-thread? true
      :llm-provider :openai
      :response-format (-> response-path slurp edn/read-string)
      :instructions (slurp instruction-path)})))

;;; ================================================= Throw-away-able updating of project.edn =====================================
(def qnames [:process-steps
             :work-type
             :batch-size
             :initial-question
             :production-motivation
             :production-location
             :process-ordering
             :production-system-type
             :process-durations])


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


(defn update-projects! []
  "Write new project DBs"
  (doseq [p (db/list-projects)]
    #_(update-project p)
    (add-state-vector p)))

(defn tryme []
  (let [conn (connect-atm :system)
        eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn)
        found (d/q '[:find [?s ...]
                     :in $ ?eid
                     :where [?eid :system/openai-assistants ?s]]
                   @conn eid)]
    (println "eid = " eid)
    (println "found = " found)
    (doseq [f found]
      (println "f = " f)
      (d/transact conn {:tx-data [[:db/delete eid :system/openai-assistants f]]}))))

(defn tryme2 []
  (let [conn (connect-atm :system)
        eid (d/q '[:find ?eid . :where [?eid :system/name "SYSTEM"]] @conn)
        found (d/q '[:find [?s ...] :where [_ :system/openai-assistants ?s]]  @conn)]
    (doseq [f found]
      (d/transact conn {:tx-data [[:db/retract eid :system/openai-assistants f]]}))))

#_(defn tell-one
  "Diagnostic for one interaction with interviewer."
  [cmd {:keys [pid conv-id] :as ctx}]
  (let [sur (db/get-surrogate-info pid)
        ctx (-> ctx
                (merge (inv/interview-agent conv-id pid))
                (assoc :responder-role :surrogate)
                (assoc :sur-aid (:surrogate/assistant-id sur))
                (assoc :sur-tid (:surrogate/thread-id sur)))]
    (inv/tell-interviewer cmd ctx)))

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


(defn ^:diag huh?
  ([] (huh? "What are your instructions?"))
  ([q-txt]
   (let [{:keys [iview-aid iview-tid]} (inv/interview-agent :process :sur-ice-cream)]
     (llm/query-on-thread {:aid iview-aid :tid iview-tid :query-text q-txt}))))

(def ^:diag diag (atom nil))

;;; "vs_5bIXuiVz9aCYQt9F6HBIrzY5"
(defn ^:diag test-query-on-vstore []
  (let [flowchart (llm/upload-file {:fname "data/instructions/interviewers/process-interview-flowchart.pdf"})
        vec-store (llm/make-vector-store :name "Process Interview Vector Store" :file-ids [(:id flowchart)])
        agent (llm/make-assistant :name "Tryme agent"
                                  :instructions (slurp "data/instructions/interviewers/process.txt")
                                  :tools [{:type "file_search"}]
                                  :tool-resources {"file_search" {"vector_store_ids" [(:id vec-store)]}})
        thread (llm/make-thread :assistant-id (:id agent))]
    (reset! diag {:aid (:id agent) :tid (:id thread)}) ; diag so that you can ask it questions off-line.
    (llm/query-on-thread :aid (:aid @diag)
                         :tid (:tid @diag)
                         :query-text "Summarize what you see in the uploaded file 'Process Interview Flowchart'")))

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


(def response {:client-id (ws/recent-client!)
               :response "1. Mix Ingredients (30 minutes)
2. Pasteurize Mixture (45 minutes)
3. Homogenize Mixture (20 minutes)
4. Age Mixture (4 hours)
5. Flavor Addition (15 minutes)
6. Freeze Mixture (30 minutes)
7. Add Inclusions (10 minutes)
8. Fill Containers (20 minutes)
9. Harden Ice Cream (4 hours)
10. Package (40 minutes)
11. Store in Cold Storage (ongoing)"
               :pid :sur-ice-cream})

(defn  ^:diag tryme []
  (pan/analyze-process-durs-response response))

(defn ^:diag tryme2 []
  (ru/analyze-response (assoc response :question-type :process-durations)))
