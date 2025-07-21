(ns scheduling-tbd.db-test
  (:require
   [clojure.set    :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [datahike.api                 :as d]
   ;[datahike.pull-api            :as dp]
   [scheduling-tbd.db     :as db]))

(defn used-db-attrs
  "Create a set of all used DB attrs by looking at all projects."
  []
  (let [used (atom #{})]
    (letfn [(uda [x]
              (cond (map? x)     (doseq [[k v] (seq x)] (swap! used conj k) (uda v))
                    (vector? x)  (doall (map uda x))))]
      (doseq [p (db/list-projects)]
        (uda (db/get-project p))))
    @used))

(defn unused-db-attrs
  "Return the set of unused DB attributes, as the difference of those found in project
   and those in the schema definition."
  []
  (sort (set/difference db/project-schema-key? (used-db-attrs))))


(deftest unused-db-attrs
  (testing "that there isn't any junk in the DB schema."
    (let [used-attrs (used-db-attrs)]
      (is (empty? (unused-db-attrs))))))

(defn fix-project-agent-ids
  [pid]
  (letfn [(fix-agent [a]
            (if (contains? a :agent/agent-id)
              a
              (assoc a :agent/agent-id (-> a :agent/base-type name (str "-openai") keyword))))]
    (let [proj-data (db/get-project pid)
          agents (:project/agents proj-data)]
      (assoc proj-data :project/agents (mapv fix-agent agents)))))

(defn ^:debug fix-all-project-agent-ids!
  "I deleted :agent/id but I should have replaced it with :agent/agent-id."
  []
  (doseq [pid (db/list-projects)]
    (let [updated (fix-project-agent-ids pid)]
      (db/recreate-project-db! pid updated))))

(defn add-agent-pid
  [pid]
  (letfn [(fix-agent [a] (assoc a :agent/pid pid))]
    (let [proj-data (db/get-project pid)
          agents (:project/agents proj-data)]
      (assoc proj-data :project/agents (mapv fix-agent agents)))))

(defn ^:debug add-all-project-agent-pid!
  "I deleted :agent/id but I should have replaced it with :agent/agent-id."
  []
  (doseq [pid (db/list-projects)]
    (let [updated (add-agent-pid pid)]
      (db/recreate-project-db! pid updated))))



(def craft-brewing-desc
  "In medium-scale craft beer brewing, a significant challenge arises in the form of production scheduling. Craft breweries often produce a diverse range of beer styles with varying ingredients, fermentation times, and packaging requirements. Coordinating the brewing process to meet customer demands while optimizing resources can be complex. The production scheduling problem entails determining the most efficient sequence and timing of brewing batches, taking into account factors like ingredient availability, tank capacities, yeast propagation, and production deadlines. Balancing these variables is crucial to ensure optimal utilization of equipment, minimize idle time, reduce inventory holding costs, and meet customer expectations. Effective production scheduling plays a vital role in maintaining consistent beer quality, managing production costs, and maximizing overall brewery efficiency.")

;;; ToDo: Most of the following needs to be either removed or corrected.
;;;   - llm/find-objective-sentence does not exist. See how pretend-you-manage-interview does it. That's probably the basis for a good test.
;;;   - db/connect-proj does not exist.

#_(db/create-proj-db!
 (let [{:keys [summary industry]} (domain/project-name craft-brewing-desc)
       {:keys [decision-objective probability]} (llm/find-objective-sentence craft-brewing-desc)
       id (-> summary str/lower-case (str/replace #"\s+" "-") keyword)]
   {:project/id id
    :project/name summary
    :project/desc craft-brewing-desc
    :project/industry industry}))

#_(def proj-obj
  (util/resolve-db-id
   {:db/id (d/q '[:find ?e . :where [?e :project/id _]] @(db/connect-proj))}
   (db/connect-proj)
   #{:db/id}))

#_(def sys-obj
  (util/resolve-db-id
   {:db/id (d/q '[:find ?e . :in $ ?name :where [?e :project/id ?name]]
                @(db/connect-sys)
                (:project/id proj-obj))}
   (db/connect-sys)
   #{:db/id}))

#_(deftest start-a-project
  (testing "Testing the creation of a project."
    (is (and (keyword? (:project/id proj-obj))
             (= (:project/id proj-obj) (:project/id sys-obj))
             (every? #(contains? proj-obj %) [:project/desc :project/id :project/name])
             (every? #(contains? sys-obj %) [:project/id :project/dir :project/name])))))
