(ns scheduling-tbd.mock-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [scheduling-tbd.mock :as mock]))

(deftest test-project-data-loading
  (testing "Loading project data from EDN file"
    (let [project-data (mock/load-project-data "data/projects/sur-music-school.edn")]
      (is (some? project-data) "Project data should be loaded")
      (is (vector? project-data) "Project data should be a vector")
      (is (contains? (first project-data) :project/conversations) "Should contain conversations"))))

(deftest test-conversation-extraction
  (testing "Extracting conversations from project data"
    (let [project-data (mock/load-project-data "data/projects/sur-music-school.edn")
          conversations (mock/get-conversations project-data)]
      (is (vector? conversations) "Conversations should be a vector")
      (is (some #(= (:conversation/id %) :process) conversations) "Should have process conversation"))))

(deftest test-mock-response-generation
  (testing "Generating mock responses"
    (mock/with-mock-project "sur-music-school.edn"
      (fn []
        (let [response (mock/get-mock-response :process 2)]
          (is (string? response) "Response should be a string")
          (is (not-empty response) "Response should not be empty"))))))

(deftest test-mock-enabled-configuration
  (testing "Mock enabled configuration"
    (let [original-state (mock/mock-enabled?)]
      (mock/set-mock-enabled! true)
      (is (mock/mock-enabled?) "Mock should be enabled")
      (mock/set-mock-enabled! false)
      (is (not (mock/mock-enabled?)) "Mock should be disabled")
      (mock/set-mock-enabled! original-state))))
