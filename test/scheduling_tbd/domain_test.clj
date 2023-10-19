(ns scheduling-tbd.domain-test
  (:require
   [clojure.string        :as str]
   [clojure.test          :refer [deftest is testing]]
   [scheduling-tbd.domain :as domain]
   [scheduling-tbd.llm    :as llm :refer [chat2clj]]))

(def proj-objective-prompt
  (conj domain/project-objective-partial
        {:role "user"
         :content "[We bake cookies and sell them through grocery chains. Our challenge is to supply fresh cookies and never stock-out.
 We want a schedule that will ensure we meet demand and have the ingredients we need.]"}))

(deftest project-objective-test
  (testing "Testing that project objective prompt is okay. Really this only cares that it returns a clojure map with the correct keys.")
  (let [res (chat2clj proj-objective-prompt {:model "gpt-4"})]
    (is (= #{:objective :probability} (-> res keys set))))

  ;; This one is interest, in some sense better than GPT-4. It sometimes returns two sentences.
  (let [res (chat2clj proj-objective-prompt {:model "gpt-3.5-turbo"})]
    (is (= #{:objective :probability} (-> res keys set)))))
