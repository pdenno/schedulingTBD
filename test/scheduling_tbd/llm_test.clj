(ns scheduling-tbd.llm-test
  (:require
   [clojure.string        :as str]
   [clojure.test          :refer [deftest is testing]]
   [scheduling-tbd.domain.process.p-interview :as inv]
   [scheduling-tbd.llm    :as llm :refer [query-llm query-agent]]
   [taoensso.timbre       :as log]))

(def ^:diag diag (atom nil))

(deftest simple-query
  (let [the-query [{:role "system" :content "You are a helpful assistant."}
                   {:role "user" :content "What is the capital of Iowa?"}]]
    (testing "Testing that simple queries (llm/query-llm) work with all llm-providers."
      (testing "Testing OpenAi"
        (is (re-matches #".*Des Moines.*" (llm/query-llm the-query {:llm-provider :openai}))))
      (testing "Testing Azure"
        (is (re-matches #".*Des Moines.*" (llm/query-llm the-query {:llm-provider :azure})))))))

;;; ToDo: This works when I run it by hand, but not when I run it through Kaocha.
(deftest query-llm-few-shot
  (let [proj-summary-msgs
        (conj inv/project-name-partial ; This asks for 3 or less words; that requirement will often be ignored by gpt-3.5-turbo.
              {:role "user"
               :content
               "[We are a medium-sized craft beer brewery. We produce about 100,000 barrels/year.
  We run several products simultaneously and simply would like to be able to have the beer bottled and ready
  to ship as near as possible to the dates defined in our sales plan.]"})]
    (testing "Testing a simple query with different LLMs."
      (testing "Testing with OpenAI"
        (let [res (query-llm proj-summary-msgs {:model-class :gpt-4 :raw-text? false :llm-provider :openai})]
          (reset! diag res)
          (log/info "result =" res)
          (is (and (map? res)
                 (= (keys res) '(:project-name))
                 (-> res :project-name (str/split #"\s+") count (<= 3)))))
        (let [res (query-llm proj-summary-msgs {:model-class :gpt-3.5 :raw-text? false :llm-provider :openai})]
          (log/info "result =" res)
          (is (and (map? res)
                   (= (keys res) '(:project-name))
                   (-> res :project-name (str/split #"\s+") count (<= 4))))))

      (testing "Testing with Azure"
        (let [res (query-llm proj-summary-msgs {:model-class :gpt-4 :raw-text? false :llm-provider :azure})]
          (reset! diag res)
          (log/info "result =" res)
          (is (and (map? res)
                   (= (keys res) '(:project-name))
                   (-> res :project-name (str/split #"\s+") count (<= 3)))))
        (let [res (query-llm proj-summary-msgs {:model-class :gpt-3.5 :raw-text? false :llm-provider :azure})]
          (log/info "result =" res)
          (is (and (map? res)
                   (= (keys res) '(:project-name))
                   (-> res :project-name (str/split #"\s+") count (<= 4)))))))))

(deftest test-agents
  (testing "Testing the text-function-agent."
    (testing "Testing on OpenAI"
      (is (= [{:sentence "You are so silly!" :type :exclamatory}
              {:sentence "I like you." :type :declarative}
              {:sentence "Do you like me?" :type :interrogative}
              {:sentence "Don't step in that puddle." :type :imperative}]
             (llm/query-agent :text-function-agent-openai
                              "You are so silly! I like you. Do you like me? Don't step in that puddle."
                              {:llm-provider :openai}))))
    (testing "Testing on OpenAI"
      (is (= [{:sentence "You are so silly!" :type :exclamatory}
              {:sentence "I like you." :type :declarative}
              {:sentence "Do you like me?" :type :interrogative}
              {:sentence "Don't step in that puddle." :type :imperative}]
             (llm/query-agent :text-function-agent-azure
                              "You are so silly! I like you. Do you like me? Don't step in that puddle."
                              {:llm-provider :azure}))))))
