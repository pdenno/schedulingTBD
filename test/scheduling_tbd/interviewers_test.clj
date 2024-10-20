(ns scheduling-tbd.interviewer-test
  (:require
   [clojure.edn                 :as edn]
   [clojure.pprint              :refer [pprint cl-format]]
   [datahike.api                :as d]
   [scheduling-tbd.db           :as db]
   [scheduling-tbd.interviewers :as inv]
   [scheduling-tbd.llm          :as llm]

   [scheduling-tbd.sutil        :as sutil :refer [connect-atm default-llm-provider]]
   [taoensso.timbre :as log :refer [debug]]))

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
  (safe-alias 'pinv   'scheduling-tbd.domain.process.p-interview)
  (safe-alias 'db     'scheduling-tbd.db)
  (safe-alias 'how    'scheduling-tbd.how-made)
  (safe-alias 'llm    'scheduling-tbd.llm)
  (safe-alias 'llmt   'scheduling-tbd.llm-test)
  (safe-alias 'mzn    'scheduling-tbd.minizinc)
  (safe-alias 'mznt   'scheduling-tbd.minizinc-test)
  (safe-alias 'ou     'scheduling-tbd.op-utils)
  (safe-alias 'opt    'scheduling-tbd.operators-test)
  ;(safe-alias 'plan   'scheduling-tbd.planner)
  (safe-alias 'resp   'scheduling-tbd.web.controllers.respond)
  (safe-alias 'spec   'scheduling-tbd.specs)
  (safe-alias 'sutil  'scheduling-tbd.sutil)
  (safe-alias 'sur    'scheduling-tbd.surrogate)
  (safe-alias 'surt   'scheduling-tbd.surrogate-test)
  (safe-alias 'util   'scheduling-tbd.util)
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
    (rename-tag-to-qname ?o)
    (add-qname ?o)
    (vector ?o)
    (with-out-str (pprint ?o))
    (spit (str "data/new-projects/" (name pid) ".edn") ?o)))

(defn update-projects! []
  "Write new project DBs"
  (doseq [p (db/list-projects)]
    (update-project p)))

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
