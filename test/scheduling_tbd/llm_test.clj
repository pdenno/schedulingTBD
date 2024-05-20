(ns scheduling-tbd.llm-test
  (:require
   [clojure.string        :as str]
   [clojure.test          :refer [deftest is testing]]
   [promesa.core          :as p]
   [scheduling-tbd.domain.process.interview :as inv]
   [scheduling-tbd.llm    :as llm :refer [query-llm query-agent]]
   [taoensso.timbre       :as log]))

(def proj-summary-msgs
  (conj inv/project-name-partial ; This asks for 3 or less words; that requirement will often be ignored by gpt-3.5-turbo.
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

(deftest agents
  (testing "Testing agents"
    (testing "Testing the :clj-agent"
      (is (= [{:num 1, :process "Milling", :duration "1-2 hours"}
              {:num 2, :process "Mashing", :duration "1-2 hours"}
              {:num 3, :process "Lautering", :duration "1-2 hours"}
              {:num 4, :process "Boiling", :duration "1 hour"}
              {:num 5, :process "Cooling", :duration "1-2 hours"}
              {:num 6, :process "Fermentation", :duration "1-3 weeks"}
              {:num 7, :process "Conditioning", :duration "1-4 weeks"}
              {:num 8, :process "Packaging", :duration "2-4 hours"}]
             (query-agent :clj-agent
                          (str "Extract NUM, PROCESS, and DURATION:\n"
                               "1. Milling (1-2 hours)\n"
                               "2. Mashing (1-2 hours)\n"
                               "3. Lautering (1-2 hours)\n"
                               "4. Boiling (1 hour)\n"
                               "5. Cooling (1-2 hours)\n"
                               "6. Fermentation (1-3 weeks)\n"
                               "7. Conditioning (1-4 weeks)\n"
                               "8. Packaging (2-4 hours)"))))
      (is (= [{:id 1, :process "Material Preparation", :duration "2 days"}
              {:id 2, :process "Component Machining", :duration "5 days"}
              {:id 3, :process "Assembly", :duration "3 days"}
              {:id 4, :process "Quality Control (QC)", :duration "1 day"}
              {:id 5, :process "Packaging", :duration "1 day"}
              {:id 6, :process "Shipping", :duration "Varies"}]
             (llm/query-agent :clj-agent
                              (str "Extract ID PROCESS and DURATION:\n"
                                   "1. Material Preparation - 2 days\n"
                                   "2. Component Machining - 5 days\n"
                                   "3. Assembly - 3 days\n"
                                   "4. Quality Control (QC) - 1 day\n"
                                   "5. Packaging - 1 day\n"
                                   "6. Shipping - Varies")))))
    (testing "The text-function-agent."
      (is (= [{:sentence "You are so silly!" :type :exclamatory}
              {:sentence "I like you." :type :declarative}
              {:sentence "Do you like me?" :type :interrogative}
              {:sentence "Don't step in that puddle." :type :imperative}]
             (llm/query-agent :text-function-agent
                              "You are so silly! I like you. Do you like me? Don't step in that puddle."))))))
