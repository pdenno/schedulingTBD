(ns scheduling-tbd.domain-test
  (:require
   [clojure.string        :as str]
   [clojure.test          :refer [deftest is testing]]
   [promesa.core          :as p]
   [scheduling-tbd.domain :as domain]
   [scheduling-tbd.llm    :as llm :refer [query-llm]]
   [taoensso.timbre          :as log]))

(def proj-objective-prompt
  (conj domain/project-objective-partial
        {:role "user"
         :content "[We bake cookies and sell them through grocery chains. Our challenge is to supply fresh cookies and never stock-out.
 We want a schedule that will ensure we meet demand and have the ingredients we need.]"}))

(deftest project-objective-test
  (testing "Testing that project objective prompt is okay. Really this only cares that it returns a clojure map with the correct keys.")
  (let [res (-> (query-llm proj-objective-prompt {:model-class :gpt-4 :raw-text? false}) (p/await))]
    (is (= #{:objective :probability} (-> res keys set))))

  ;; This one is interesting; in some sense better than GPT-4. It sometimes returns two sentences.
  (let [res (-> (query-llm proj-objective-prompt {:model-class :gpt-3.5 :raw-text? false}) (p/await))]
     (is (= #{:objective :probability} (-> res keys set)))))
