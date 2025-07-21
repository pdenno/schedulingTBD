(ns scheduling-tbd.ds2mermaid-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [scheduling-tbd.iviewr.eads-util :as eu]
   [scheduling-tbd.ds2mermaid       :as ds2m]))

(def bad-input
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

(def good-input
  {:EADS-ref :process/flow-shop,
   :process-id "pencil-manufacturing",
   :inputs ["graphite" "clay" "water" "cedar wood" "metal" "eraser material" "paint"],
   :outputs [{:item-id "finished pencils", :quantity {:units "finished pencils", :value-string "100000"}}],
   :resources ["extruder" "kiln" "milling machine" "glue applicator" "shaping machine"],
   :duration {:units "hours", :value-string "4"},
   :subprocesses
   [{:process-id "graphite-core-production",
     :inputs ["graphite" "clay" "water"],
     :outputs [{:item-id "finished graphite rods", :quantity {:units "graphite cores", :value-string "100000"}}],
     :resources ["mixer" "extruder" "kiln"],
     :subprocesses
     [{:process-id "mix-graphite-and-clay",
       :inputs ["graphite" "clay" "water"],
       :outputs [{:item-id "graphite clay paste", :quantity {:units "liters", :value-string "100"}}],
       :resources ["mixer"],
       :duration {:units "hours", :value-string "1"},
       :subprocesses []}
      {:process-id "extrude-core",
       :inputs ["graphite clay paste"],
       :outputs [{:item-id "extruded graphite rods", :quantity {:units "extruded graphite core", :value-string "100000"}}],
       :resources ["extruder"],
       :duration {:units "minutes", :value-string "20"},
       :subprocesses []}
      {:process-id "dry-and-bake-core",
       :inputs ["extruded graphite rods"],
       :outputs [{:item-id "finished graphite rods", :quantity {:units "extruded graphite core", :value-string "100000"}}],
       :resources ["kiln"],
       :duration {:units "hours", :value-string "2"},
       :subprocesses []}]}
    {:process-id "wood-casing-production",
     :inputs ["cedar wood"],
     :outputs ["wood slats with grooves"],
     :resources ["milling machine"],
     :subprocess-flow "individuals-from-batch",
     :duration {:units "hours", :value-string "2"},
     :subprocesses
     [{:process-id "mill-wood-slats",
       :inputs ["cedar wood"],
       :outputs ["milled wood slats"],
       :resources ["milling machine"],
       :duration {:units "hours", :value-string "2"},
       :subprocess-flow :individuals-from-batch,
       :subprocesses []}
      {:process-id "cut-grooves-in-slats",
       :inputs ["milled wood slats"],
       :outputs ["wood slats with grooves"],
       :resources ["groove cutter"],
       :duration {:units "hours", :value-string "2"},
       :subprocesses []}]}
    {:process-id "assemble",
     :inputs
     [{:item-id "finished graphite rods", :from "graphite-core-production"}
      {:item-id "wood slats with grooves", :from "wood-casing-production"}
      "metal"
      "erasers"
      "paint"],
     :outputs ["finished pencil"],
     :resources ["glue applicator" "shaping machine"],
     :subprocesses
     [{:process-id "insert-core-into-slats",
       :inputs ["graphite core" "wood slats with grooves"],
       :outputs ["pencil blanks"],
       :resources ["glue applicator"],
       :subprocesses []}
      {:process-id "shape-and-paint-pencil",
       :inputs ["pencil blanks" "paint"],
       :outputs ["shaped and painted pencils"],
       :resources ["shaping machine" "painting station"],
       :subprocesses []}
      {:process-id "attach-eraser",
       :optional? true,
       :inputs ["shaped and painted pencils" "metal" "erasers"],
       :outputs ["finished pencils"],
       :resources ["crimping tool"],
       :subprocesses []}]}]})

(def good-output "flowchart TD\nsubgraph pencil-manufacturing\nsubgraph graphite-core-production\nsubgraph mix-graphite-and-clay\nend\nsubgraph extrude-core\nend\nsubgraph dry-and-bake-core\nend\nend\nsubgraph wood-casing-production\nsubgraph mill-wood-slats\nend\nsubgraph cut-grooves-in-slats\nend\nend\nsubgraph assemble\nsubgraph insert-core-into-slats\nend\nsubgraph shape-and-paint-pencil\nend\nsubgraph attach-eraser\nend\nend\nend\nstart@{shape: sm-circ} -- erasers,metal --> attach-eraser\nstart@{shape: sm-circ} -- cedar wood --> mill-wood-slats\nstart@{shape: sm-circ} -- water,clay,graphite --> mix-graphite-and-clay\nextrude-core -- extruded graphite rods --> dry-and-bake-core\nmill-wood-slats -- milled wood slats --> cut-grooves-in-slats\nshape-and-paint-pencil -- shaped and painted pencils --> attach-eraser\ninsert-core-into-slats -- pencil blanks --> shape-and-paint-pencil\nstart@{shape: sm-circ} -- graphite core --> insert-core-into-slats\nstart@{shape: sm-circ} -- paint --> shape-and-paint-pencil\nmix-graphite-and-clay -- graphite clay paste --> extrude-core\ndry-and-bake-core -- finished graphite rods --> stop@{shape: framed-circle}\nattach-eraser -- finished pencils --> stop@{shape: framed-circle}\ncut-grooves-in-slats -- wood slats with grooves --> insert-core-into-slats\n")

(deftest generates-mermaid-ffbd-ok
  (testing "That a moderately complex graph gets the correct output."
    (is (= good-output (ds2m/ds2mermaid good-input)))))

(deftest graph-semantics-tests
  (testing "Testing for false positives; things that aren't flow charts."
    (is (not (eu/graph-semantics-ok? bad-input)))
    (is (eu/graph-semantics-ok? good-input))))
