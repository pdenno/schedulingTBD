(ns scheduling-tbd.datastructure2mermaid
  (:require [clojure.core :as c]
            [scheduling-tbd.interviewing.eads-util :as eads-util]
            [scheduling-tbd.db :as sdb]
            [clojure.edn  :as edn]
            [clojure.data.json :as json]
            [clojure.walk :as w]
            [taoensso.telemere       :refer [log!]]))

(defn decompose-eads [eads]
  "This function takes in an EADS and simply splits processes and subprocess for better visual representation of what happens.
   This does NOT take inputs/outputs into account
   For example, in the pencil EADS, it would generate this code: 
    subgraph pencil-manufacturing
       subgraph graphite-core-production
           subgraph mix-graphite-and-clay
           end
           subgraph extrude-core
           end
           subgraph dry-and-bake-core
           end
       end
       subgraph wood-casing-production
          subgraph mill-wood-slats
          end
          subgraph cut-grooves-in-slats
          end
      end
      subgraph assembly
          subgraph insert-core-into-slats
          end
          subgraph shape-and-paint-pencil
          end
          subgraph attach-eraser
          end
      end
    end"
  (let [title (if (empty? (get-in eads [:process-id :comment])) (get eads :process-id) (get-in eads [:process-id :val]))
        subprocesses (get eads :subprocesses)]
    (str "subgraph " title "\n"
         (apply str (for [subprocess subprocesses](decompose-eads subprocess)))
         "end\n")
    ))

(defn gather-connections [eads]
  (let [get-safe (fn [data key]
                   (some #(get-in data %) [[key :val] [key]]))
        extract-data (fn [data key]
                       (let [process-ids (get-safe data key)]
                         (mapv (fn [process-id]
                                 (if (map? process-id)
                                   (:item-id process-id)
                                   process-id))
                               process-ids)))
        extract-inputs (fn [data]
                         (get-safe data :inputs))
        extract-outputs (fn [data]
                          (get-safe data :outputs))
        title (get-safe eads :process-id)
        inputs (extract-inputs eads)
        outputs (extract-outputs eads)
        subprocesses (get eads :subprocesses)]

    (let [from-connections (keep (fn [input]
                                   (when (and (map? input) (:from input))
                                     {:title (:from input) :output (:item-id input) :input ""}))
                                 inputs)
          leaf-connections (when (and (empty? subprocesses) (not= inputs outputs))
                             [{:title title :inputs (extract-data eads :inputs) :outputs (extract-data eads :outputs)}])]
      
      (concat from-connections
              (reduce (fn [acc subprocess]
                        (concat acc (gather-connections subprocess)))
                      []
                      subprocesses)
              leaf-connections))))

(defn find-connections [data]
  (let [input-map (reduce (fn [acc {:keys [title inputs]}]
                            (reduce (fn [m input]
                                      (update m input conj title))
                                    acc inputs))
                          {}
                          data)
        output-map (reduce (fn [acc {:keys [title outputs]}]
                             (reduce (fn [m output]
                                       (update m output conj title))
                                     acc outputs))
                           {}
                           data)
        materials (set (concat (keys input-map) (keys output-map)))
        connections (mapcat (fn [material]
                              (let [sources (get output-map material [])
                                    targets (get input-map material [])]
                                (concat
                                 ;; Connect sources to targets for the material
                                 (for [source sources
                                       target targets]
                                   {:source source 
                                    :material material 
                                    :target target})
                                 ;; "start" connection if the material has no sources
                                 (when (and (empty? sources) (not (empty? targets)))
                                   [{:source "start@{shape: sm-circ}" 
                                     :material material 
                                     :target (first targets)}])
                                 ;; "stop" connection if the material has no targets
                                 (when (and (empty? targets) (not (empty? sources)))
                                   [{:source (first sources) 
                                     :material material 
                                     :target "stop@{shape: framed-circle}"}]))))
                            materials)]
    connections))


(defn combine-connections [connections]
  (reduce (fn [acc {:keys [source material target]}]
            (let [key [source target]
                  existing (get acc key)]
              (assoc acc key
                     (if existing
                       (assoc existing :material (str (:material existing) "," material))
                       {:source source, :material material, :target target}))))
          {}
          connections))

(defn format-combined-connections [combined-connections]
  (vals combined-connections))

(defn format-connections [connections]
    (map (fn [{:keys [source material target]}]
                                  (str source " -- " material " --> " target "\n"))
                                connections))

(defn parse-eads [eads-str]
  (json/read-str eads-str :key-fn keyword))

(defn datastructure2mermaid [eads]
    (when (eads-util/graph-semantics-ok? eads)
      (str "flowchart TD\n"
                  (decompose-eads eads)
      (apply str (-> eads
                            gather-connections
                            find-connections
                            combine-connections
                            format-combined-connections
                            format-connections)))))

(defn latest-datastructure2mermaid [pid cid]
  (let [latest-EADS-message (sdb/get-EADS-ds pid cid)]
    (-> latest-EADS-message
        edn/read-string
        :data-structure
        w/keywordize-keys
        datastructure2mermaid)
    ))


;(def test {:message-type "DATA-STRUCTURE-REFINEMENT", :commit-notes "Addressed scaling concerns related to increased task volume, complexity, variability, resource constraints, supply chain integration, real-time monitoring, workforce management, and cost control. Included strategies to future-proof scheduling systems.", :data-structure {"process-id" "job-shop-scheduling", "scaling-concerns" [{"concern" "Increased Volume of Tasks and Jobs", "indicators" ["Difficulty distributing work evenly across machines or workers." "Frequent delays in creating or adjusting schedules." "Decreased visibility into job status with higher volumes."], "solutions" ["Transition to automated scheduling software." "Invest in real-time monitoring tools." "Optimize sequencing with batching algorithms."]} {"concern" "Managing Increased Complexity", "indicators" ["Struggles prioritizing tasks with competing deadlines or specialized needs." "Chaotic planning for multi-stage processes with dependencies." "Failure to model complex workflows accurately."], "solutions" ["Upgrade to dependency-mapping scheduling systems." "Use simulation and scenario planning tools." "Synchronize scheduling with supply chain systems."]} {"concern" "Coping with Variability in Demand", "indicators" ["Difficulty balancing long-term goals with rush orders." "Time-consuming rework of schedules due to shifting demands." "Inability to adapt to seasonal fluctuations."], "solutions" ["Leverage predictive analytics for demand forecasting." "Implement adaptive scheduling tools for real-time re-prioritization." "Create flexible buffer zones to absorb variability."]} {"concern" "Resource Constraints and Utilization", "indicators" ["Overuse of key machines while others remain idle." "Labor or tooling bottlenecks due to increased demand." "Conflicts in overlapping resource requirements."], "solutions" ["Implement capacity-based scheduling tools." "Track resource availability dynamically." "Use load-balancing algorithms to distribute resources."]} {"concern" "Integration with Growing Supply Chain", "indicators" ["Disruptions from late materials or low inventory." "Disconnects between scheduling and procurement." "Difficulty coordinating JIT manufacturing."], "solutions" ["Deploy integrated ERP systems for synchronized workflows." "Monitor materials with real-time tracking alerts." "Account for dynamic lead times in scheduling adjustments."]} {"concern" "Real-Time Monitoring and Scalability", "indicators" ["Difficulties tracking job statuses in expanded operations." "Missed opportunities for dynamic adjustments." "Unnoticed bottlenecks or delays."], "solutions" ["Introduce IoT-enabled monitoring systems." "Adopt advanced visualization tools like Gantt charts." "Enable AI-driven alert mechanisms for proactive issue resolution."]} {"concern" "Workforce Management Challenges", "indicators" ["Frequent overbooking or underutilization of operators." "Trouble scheduling tasks requiring specialized skills." "Rising overtime costs as complexity grows."], "solutions" ["Integrate dynamic workforce scheduling features." "Cross-train employees for flexible task assignment." "Balance shifts to prevent fatigue while meeting demand."]} {"concern" "Cost Control in Scaling Operations", "indicators" ["Increased energy, overtime, and material costs." "Quality issues from rushed or mismanaged jobs." "Escalating tool wear and machine inefficiency."], "solutions" ["Deploy cost-aware scheduling tools." "Simulate financial impacts of scenarios before implementation."]}], "scaling-strategies" ["Implement cloud-based or modular software tools for scalability." "Invest in AI and ML technologies for adaptive and predictive scheduling." "Automate scheduling processes to reduce manual intervention." "Integrate real-time data collection and analysis for actionable insights."]}})