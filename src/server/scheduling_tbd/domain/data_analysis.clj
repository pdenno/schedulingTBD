(ns scheduling-tbd.domain.data-analysis
    "Planning operators for the data interview"
  (:require
   [clojure.edn                               :as edn]
   [clojure.spec.alpha                        :as s]
   [clojure.string                            :as str]
   [jsonista.core                             :as json]
   [scheduling-tbd.db                         :as db]
   [scheduling-tbd.llm                        :as llm]
   [scheduling-tbd.minizinc                   :as mzn]
   [scheduling-tbd.response-utils             :as ru :refer [defanalyze]]
   [scheduling-tbd.sutil                      :as sutil]
   [scheduling-tbd.web.websockets             :as ws]
   [taoensso.telemere                         :as tel :refer [log!]]
   [scheduling-tbd.agent-db                   :as adb]
   [wkok.openai-clojure.api                   :as openai]))

(def ^:diag diag (atom nil))

(def the-warm-up-type-question
  (str "What kind of spreadsheets do you use to help with your production scheduling?"))

(s/def ::table-list vector?)
(s/def ::data-step-2-conclusion 
       (s/keys :opt-un [::production-bottlenecks ::maintenance-scheduling ::unscheduled-downtime
                 ::worker-availability ::skilled-worker-availability ::demand-variation
                 ::product-variation])) ;make some of these required

(s/def ::production-bottlenecks (s/coll-of ::production-bottleneck))
(s/def ::production-bottleneck string?) ;make specific bottleneck list?
(s/def ::maintenance-scheduling number?)
(s/def ::unscheduled-downtime number?)
(s/def ::worker-availability number?)
(s/def ::skilled-worker-availability number?)

(s/def ::demand-variation keyword?)
(s/def ::product-variation keyword?)

;Can't get this to work
;(defn possible-demand-variation [x] (#{:seasonal :constant} x))
;(s/def ::demand-variation possible-demand-variation)

;(defn possible-product-variation [x] {#{:none :internal :external :both} x})
;(s/def ::product-variation possible-product-variation)

(defanalyze :data/warm-up [{:keys [response _client-id _pid] :as _ctx}]
  (log! :debug (str "*******analysis :data/warm-up, response = " response)))

(defanalyze :data/start-table-analysis [{:keys [response _client-id _pid :as ctx]}]
  (let [agent (adb/ensure-agent! {:base-type :table-analysis-agent})])
)


(defn create-table-analysis-agent
 []
  (llm/make-assistant {:name "table reader assistant"
                       :instructions (slurp "resources/agents/table-analysis-agent.txt")
                       :tools [{:type "file_search"}]})
)

(defn list-assistants
  []
  (:data (openai/list-assistants {:limit 30}))
)

(defn check-if-assistant-exists
  [assistant-name]
  (let [assistants (list-assistants)]
    (some #(= assistant-name (-> % :name)) assistants))
)

(defn get-assistant-id
  [assistant-name]
  (if (check-if-assistant-exists assistant-name) 
    (:id (first (filter #(= assistant-name (-> % :name)) (list-assistants))))
    (:id (create-table-analysis-agent))
  ))

(defn list-vector-stores
  []
  (:data (openai/list-vector-stores {:limit 30})))

(defn check-if-vector-store-exists
  [vector-store-name]
  (let [vector-stores (list-vector-stores)]
    (some #(= vector-store-name (-> % :name)) vector-stores))
  )

(defn upload-file
  [filepath]
  (llm/upload-file {:fname filepath
                    :purpose "assistants"})
)

(defn create-vector-store
  [vector-store-name file-id]
  (llm/make-vector-store {:name vector-store-name
                          :file-ids [file-id]
                          :metadata {:info "test tables"}})
)

(defn create-vector-store-from-filepath
  [vector-store-name filepath]
  (let [file-id (:id (upload-file filepath))]
    (create-vector-store vector-store-name file-id)))


(defn get-vector-store-id
  [vector-store-name filepath]
  (if (check-if-vector-store-exists vector-store-name)
    (:id (first (filter #(= vector-store-name (-> % :name)) (list-vector-stores))))
    (:id (create-vector-store-from-filepath vector-store-name filepath)))
)

(defn add-vector-store-to-assistant [assistant-name vector-store-name filepath]
  (let [aid (get-assistant-id assistant-name)
        vector-store-id (get-vector-store-id vector-store-name filepath)]
    (log! aid)
    (log! vector-store-id)
    (openai/modify-assistant {:assistant-id aid
                              :tool-resources {:file_search {:vector-store-ids [vector-store-id]}}}))

)