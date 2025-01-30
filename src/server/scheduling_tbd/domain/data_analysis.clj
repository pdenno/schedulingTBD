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

(def warm-up-question
  (str "What kind of spreadsheets do you use to help with your production scheduling?"))

(defanalyze :data/warm-up [{:keys [response _client-id _pid] :as _ctx}]
  (log! :debug (str "*******analysis :data/warm-up, response = " response)))


(defanalyze :data/start-table-analysis [{:keys [response _client-id _pid :as ctx]}]
  (let [agent (adb/ensure-agent! {:base-type :table-analysis-agent})])
)

(defn add-vector-store-to-agent
  [aid vector-store-id]
(log! "Unimplemented")
  )

(defn create-table-analysis-agent
 []
  (llm/make-assistant {:name "table reader assistant"
                       :instructions (slurp "resources/agents/table-analysis-agent.txt")
                       :tools [{:type "file_search"}]})
)

(defn list-assistants
  []
  (:data (openai/list-assistants {:limit 30}) )
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
