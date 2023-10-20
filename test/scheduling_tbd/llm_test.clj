(ns scheduling-tbd.llm-test
  (:require
   [clojure.string        :as str]
   [clojure.test          :refer [deftest is testing]]
   [scheduling-tbd.domain :as domain]
   [scheduling-tbd.llm    :as llm :refer [query-llm]]))

(def proj-summary-msgs
  (conj domain/project-name-partial ; This asks for 3 or less words; that requirement will often be ignored by gpt-3.5-turbo.
                       {:role "user"
                        :content
                        "[We are a medium-sized craft beer brewery. We produce about 100,000 barrels/year.
  We run several products simultaneously and simply would like to be able to have the beer bottled and ready
  to ship as near as possible to the dates defined in our sales plan.]"}))

(deftest query-llm-tests
  (testing "Testing a simple query with different LLMs."
    (is (let [res (query-llm proj-summary-msgs {:model "gpt-4"})]
          (and (map? res)
               (= (keys res) '(:summary))
               (-> res :summary (str/split #"\s+") count (<= 3)))))
    (is (let [res (query-llm proj-summary-msgs {:model "gpt-3.5-turbo"})]
            (and (map? res)
                 (= (keys res) '(:summary))
                 (-> res :summary (str/split #"\s+") count (<= 4)))))))
