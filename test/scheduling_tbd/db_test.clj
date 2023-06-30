(ns scheduling-tbd.db-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [datahike.api                 :as d]
   ;[datahike.pull-api            :as dp]
   [scheduling-tbd.db   :as db]
   [scheduling-tbd.llm  :as llm]
   [scheduling-tbd.util :as util]))

(def craft-brewing-desc
  "In medium-scale craft beer brewing, a significant challenge arises in the form of production scheduling. Craft breweries often produce a diverse range of beer styles with varying ingredients, fermentation times, and packaging requirements. Coordinating the brewing process to meet customer demands while optimizing resources can be complex. The production scheduling problem entails determining the most efficient sequence and timing of brewing batches, taking into account factors like ingredient availability, tank capacities, yeast propagation, and production deadlines. Balancing these variables is crucial to ensure optimal utilization of equipment, minimize idle time, reduce inventory holding costs, and meet customer expectations. Effective production scheduling plays a vital role in maintaining consistent beer quality, managing production costs, and maximizing overall brewery efficiency.")

(db/create-proj-db!
 (let [{:keys [summary industry]} (llm/project-name craft-brewing-desc)
       {:keys [decision-objective probability]} (llm/find-objective-sentence craft-brewing-desc)
       id (-> summary str/lower-case (str/replace #"\s+" "-") keyword)]
   {:project/id id
    :project/name summary
    :project/desc craft-brewing-desc
    :project/industry industry}))

(def proj-obj
  (util/resolve-db-id
   {:db/id (d/q '[:find ?e . :where [?e :project/id _]] @(db/connect-proj))}
   (db/connect-proj)
   #{:db/id}))

(def sys-obj
  (util/resolve-db-id
   {:db/id (d/q '[:find ?e . :in $ ?name :where [?e :project/id ?name]]
                @(db/connect-sys)
                (:project/id proj-obj))}
   (db/connect-sys)
   #{:db/id}))

(deftest start-a-project
  (testing "Testing the creation of a project."
    (is (and (keyword? (:project/id proj-obj))
             (= (:project/id proj-obj) (:project/id sys-obj))
             (every? #(contains? proj-obj %) [:project/desc :project/id :project/name])
             (every? #(contains? sys-obj %) [:project/id :project/dir :project/name])))))
