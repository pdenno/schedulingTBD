(ns scheduling-tbd.ds2mermaid-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [scheduling-tbd.iviewr.eads-util :as eu]
   [scheduling-tbd.ds2mermaid       :as ds2m]))

(def bad-struct
  "Something that definitely should not be provided to ds2m/ds2mermaid."
  {:inquiry-areas
   [{:inquiry-area-id "equipment-availability",
     :fact-types
     {:val
      [{:fact-type-id "EQUIPMENT-has-TYPE",
        :arity 2,
        :objects ["equipment" "equipment-type"],
        :reference-modes ["equipment-id" "type"],
        :examples
        {:column-headings ["Equipment ID" "Equipment Type"],
         :rows [["EQ-001" "Fermentation Tank"] ["EQ-002" "Mash Tun"] ["EQ-003" "Bottling Line"] ["EQ-004" "Conditioning Tank"]]}}
       {:fact-type-id "EQUIPMENT-has-STATUS",
        :arity 2,
        :objects ["equipment" "availability-status"],
        :reference-modes ["equipment-id" "status"],
        :examples
        {:column-headings ["Equipment ID" "Availability Status"],
         :rows [["EQ-001" "In Use"] ["EQ-002" "Available"] ["EQ-003" "Cleaning"] ["EQ-004" "In Use"]]}}
       {:fact-type-id "EQUIPMENT-has-CURRENT-TASK",
        :arity 2,
        :objects ["equipment" "task"],
        :reference-modes ["equipment-id" "task-name"],
        :examples
        {:column-headings ["Equipment ID" "Current/Upcoming Task"],
         :rows [["EQ-001" "Fermenting IPA"] ["EQ-002" "Scheduled for Pale Ale mashing"] ["EQ-003" "N/A"] ["EQ-004" "Conditioning Stout"]]}}
       {:fact-type-id "TASK-has-DATES",
        :arity 2,
        :objects ["task" "task-date"],
        :reference-modes ["task-name" "timeperiod"],
        :examples
        {:column-headings ["Current/Upcoming Task" "Start Date/Time" "End Date/Time"],
         :rows
         [["Fermenting IPA" "2023-12-15 08:00" "2023-12-20 08:00"]
          ["Scheduled for Pale Ale mashing" "2023-12-18 08:00" "2023-12-18 10:00"]
          ["N/A" "" ""]
          ["Conditioning Stout" "2023-12-14 10:00" "2023-12-28 10:00"]]}}
       {:fact-type-id "EQUIPMENT-has-MAINTENANCE-SCHEDULE",
        :arity 2,
        :objects ["equipment" "maintenance-schedule"],
        :reference-modes ["equipment-id" "schedule-info"],
        :examples
        {:column-headings ["Equipment ID" "Maintenance Schedule"],
         :rows [["EQ-001" "2024-01-10"] ["EQ-002" "2024-01-05"] ["EQ-003" "2024-01-12"] ["EQ-004" "2024-01-15"]]}}]}}],
   :EADS-ref :process/flow-shop,
   :EADS-id :process/flow-shop})

(def good-struct :NYI)

(defn good-mermaid?
  [m]
  (throw (ex-info "NYI" {})))

(deftest false-positive-semantics
  (testing "Testing for false positives; things that aren't flow charts."
    (is (not (eu/graph-semantics-ok? bad-struct)))))

(deftest making-ffbd
  (testing "Testing that a mermaid graph can be made from good :process/flow-shop summary data structures."
    (is (not (good-mermaid? (ds2m/ds2mermaid bad-struct))))
    (is (good-mermaid? (ds2m/ds2mermaid good-struct)))))
