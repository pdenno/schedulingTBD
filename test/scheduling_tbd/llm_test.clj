(ns scheduling-tbd.llm-test
  (:require
   [clojure.string        :as str]
   [clojure.test          :refer [deftest is testing]]
   [promesa.core          :as p]
   [scheduling-tbd.domain :as domain]
   [scheduling-tbd.llm    :as llm :refer [query-llm]]
   [taoensso.timbre       :as log]))

(def proj-summary-msgs
  (conj domain/project-name-partial ; This asks for 3 or less words; that requirement will often be ignored by gpt-3.5-turbo.
                       {:role "user"
                        :content
                        "[We are a medium-sized craft beer brewery. We produce about 100,000 barrels/year.
  We run several products simultaneously and simply would like to be able to have the beer bottled and ready
  to ship as near as possible to the dates defined in our sales plan.]"}))

(def ^:diag diag (atom nil))

;;; ToDo: This works when I run it by hand, but not when I run it through Kaocha.
(deftest query-llm-tests
  (testing "Testing a simple query with different LLMs."
    (let [res (-> (query-llm proj-summary-msgs {:model-class :gpt-4 :raw-text? false}) (p/await! 20000))]
      (reset! diag res)
      (log/info "result =" res)
      (is (and (map? res)
               (= (keys res) '(:project-name))
               (-> res :project-name (str/split #"\s+") count (<= 3)))))

    (let [res (-> (query-llm proj-summary-msgs {:model-class :gpt-3.5 :raw-text? false}) (p/await! 20000))]
      (log/info "result =" res)
      (is (and (map? res)
               (= (keys res) '(:project-name))
               (-> res :project-name (str/split #"\s+") count (<= 4)))))))
