(ns excel-reader.eads-to-mermaid
  (:require [clojure.data.json :as json]
            [clojure.core :as c]))

(defn decompose-eads [eads]
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

(defn eads-to-mermaid [eads-str]
  (let [eads (parse-eads eads-str)]
    (print (str "flowchart TD\n"
        (decompose-eads eads)))
    (print (apply str (-> eads
               gather-connections
               find-connections
               combine-connections
               format-combined-connections
               format-connections)))))
