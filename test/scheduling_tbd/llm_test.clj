(ns scheduling-tbd.llm-test
  (:require
   [clojure.string           :as str]
   [clojure.test             :refer [deftest is testing]]
   [jsonista.core            :as json]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.agent-db  :as adb]))

(def ^:diag diag (atom nil))

(deftest testing-query-agent
  (testing "the function llm/query-agent"
    (let [res (-> (adb/query-agent :response-analysis-agent
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

;;; (llmt/rchat-basic)
(defn rchat-basic []
  (testing "Whether I can use llm/query-llm for simple interactions with NIST rchat."
    (llm/query-llm
     [{:role "system" :content "You are a helpful assistant."}
      {:role "user"   :content "How many 'r's are there in 'raspberry'?"}]
     :llm-provider :meta)))

(defn rchat-basic-2 []
  (testing "Whether I can use llm/query-llm for simple interactions with NIST rchat."
    (llm/query-llm
     [{:role "system" :content "You are a helpful assistant."}
      {:role "user"   :content (str "Describe a plan for counting the numer of 'r's in the word 'raspberry',\n"
                                    "then execute that plan.")}]
     :llm-provider :meta)))

(defn real-chat-mgmt []
  (testing "Whether I can present the ideas intelligibly."
    (llm/query-llm [{:role "Peter"    :content "Slide x???"}
                    {:role "Simon"    :content "You aren't getting it."}
                    {:role "Peter"    :content "Slide y???"}
                    {:role "Simon"    :content "You still aren't getting it."}
                    {:role "Peter"    :content "..."}]
                   :llm-provider :real-world)))
