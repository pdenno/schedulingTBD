(ns scheduling-tbd.llm-test
  (:require
   [clojure.string        :as str]
   [clojure.test          :refer [deftest is testing]]
   [jsonista.core         :as json]
   [scheduling-tbd.llm    :as llm]))

(def ^:diag diag (atom nil))

(deftest testing-query-agent
  (testing "the function llm/query-agent"
    (let [res (-> (llm/query-agent :response-analysis-agent
                                   (str "QUESTION: Would you characterize some process as being a relatively persistent bottleneck? "
                                        "RESPONSE: Yes, sewing is typically the bottleneck."))
                  json/read-value
                  (update-keys str/lower-case)
                  (update-keys keyword)
                  (update-vals #(if (empty? %) false %)))]
      (reset! diag res)
      (is (= (-> res keys set) #{:answers-the-question? :raises-a-question? :wants-a-break?}))
      (is (-> res :answers-the-question? string?))
      (is (not (:raises-a-question? res)))
      (is (not (:wants-a-break? res))))))
