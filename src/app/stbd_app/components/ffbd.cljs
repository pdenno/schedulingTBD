(ns stbd-app.components.ffbd
  "A React component to display Functional Flow Block Diagrams using Cytoscape.js."
  (:require
   [applied-science.js-interop :as j]
   [clojure.edn :as edn]
   [helix.core :as helix :refer [defnc $]]
   [helix.hooks :as hooks]
   ["cytoscape" :as cytoscape-lib]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/Dialog$default" :as Dialog]
   [stbd-app.util :as util :refer [register-fn]]
   [taoensso.telemere :refer [log!]]))

(def ^:diag diag (atom nil))

;;; Utility functions for EADS to Cytoscape transformation

(defn flatten-processes
  "Recursively flatten EADS structure into a list of process nodes."
  [eads-data]
  (let [{:keys [process-id inputs outputs resources duration subprocesses]} eads-data
        current-node {:id process-id
                      :label process-id
                      :inputs (or inputs [])
                      :outputs (or outputs [])
                      :resources (or resources [])
                      :duration (or duration {})}
        subprocess-nodes (mapcat flatten-processes (or subprocesses []))]
    (cons current-node subprocess-nodes)))

(defn extract-connections
  "Extract material flow connections between processes based on inputs/outputs."
  [process-nodes]
  (let [output-map (reduce
                    (fn [acc {:keys [id outputs]}]
                      (reduce
                       (fn [m output]
                         (let [output-id (if (map? output) (:item-id output) output)]
                           (assoc m output-id id)))
                       acc
                       outputs))
                    {}
                    process-nodes)

        input-map (reduce
                   (fn [acc {:keys [id inputs]}]
                     (reduce
                      (fn [m input]
                        (let [input-info (if (map? input)
                                           {:id (:item-id input) :from (:from input)}
                                           {:id input :from nil})]
                          (update m (:id input-info) #(conj (or % []) {:target id :from (:from input-info)}))))
                      acc
                      inputs))
                   {}
                   process-nodes)]

    ;; Create connections based on material flow
    (mapcat
     (fn [[material targets]]
       (let [source (or (get output-map material)
                       ;; Handle explicit 'from' references
                        (when-let [from-ref (:from (first targets))]
                          from-ref))]
         (when source
           (map (fn [target]
                  {:source source
                   :target (:target target)
                   :label material})
                targets))))
     input-map)))

(defn eads->cytoscape-elements
  "Transform EADS data structure to Cytoscape elements with hierarchical compound nodes."
  [eads-data]
  (when eads-data
    (let [elements (atom [])

          ;; Add a compound node for a process and its subprocesses
          add-compound-node! (fn add-compound-node! [process-data parent-id]
                               (let [{:keys [process-id subprocesses]} process-data
                                     node-id process-id
                                     has-subprocesses? (seq subprocesses)]

                                ;; Add the process node (compound if has subprocesses)
                                 (swap! elements conj
                                        {:data (merge {:id node-id
                                                       :label process-id}
                                                      (when parent-id {:parent parent-id}))
                                         :classes (if has-subprocesses?
                                                    "compound-node"
                                                    "process-node")
                                         :grabbable true})

                                ;; Recursively add subprocesses
                                 (doseq [subprocess subprocesses]
                                   (add-compound-node! subprocess node-id))))

          ;; Add all nodes with hierarchy
          _ (add-compound-node! eads-data nil)

          ;; Extract connections from flattened structure (for edges)
          all-processes (flatten-processes eads-data)
          connections (extract-connections all-processes)

          ;; Add edges
          edges (map (fn [{:keys [source target label]}]
                       {:data {:source source :target target :label (or label "")}
                        :classes "process-edge"})
                     connections)]

      (concat @elements edges))))

(defn ffbd-stylesheet
  "Cytoscape stylesheet for FFBD diagrams with string diagram principles and compound nodes."
  []
  [{:selector "node.process-node"
    :style {:background-color "#4a90e2"
            :label "data(label)"
            :width 140
            :height 80
            :shape "round-rectangle"
            :text-valign "center"
            :text-halign "center"
            :color "#ffffff"
            :font-size "14px"
            :font-weight "bold"
            :border-width 2
            :border-color "#2c5aa0"
            :text-wrap "wrap"
            :text-max-width "120px"}}
   {:selector "node.compound-node"
    :style {:background-color "#f8f9fa"
            :background-opacity 0.1
            :label "data(label)"
            :text-valign "top"
            :text-halign "center"
            :color "#2c3e50"
            :font-size "16px"
            :font-weight "bold"
            :border-width 3
            :border-color "#6c757d"
            :border-style "dashed"
            :text-margin-y 10
            :padding 20}}
   {:selector "edge.process-edge"
    :style {:width 4
            :line-color "#e74c3c"
            :target-arrow-color "#e74c3c"
            :target-arrow-shape "triangle"
            :target-arrow-size 12
            :curve-style "bezier"
            :label "data(label)"
            :font-size "12px"
            :font-weight "bold"
            :text-rotation "autorotate"
            :color "#2c3e50"
            :text-background-color "#ffffff"
            :text-background-opacity 0.8
            :text-background-padding "3px"
            :text-border-color "#bdc3c7"
            :text-border-width 1
            :text-border-opacity 0.8}}
   {:selector "node:selected"
    :style {:border-width 4
            :border-color "#ff6b6b"}}
   {:selector "edge:selected"
    :style {:line-color "#ff6b6b"
            :target-arrow-color "#ff6b6b"}}])

(defnc FFBDModal
  "Modal dialog containing the FFBD diagram using Cytoscape.js."
  [{:keys [graph]}]
  (let [cy-ref (hooks/use-ref nil)
        modal (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]

    (hooks/use-effect
     [open graph]
     (log! :info (str "FFBD useEffect: open=" open " graph=" (type graph) " ref=" (j/get cy-ref :current)))
     (when open
       (js/setTimeout
        (fn []
          (log! :info (str "FFBD setTimeout: ref=" (j/get cy-ref :current) " graph keys=" (when graph (keys graph))))
          (when (and (j/get cy-ref :current) graph)
            (try
              (let [parsed-graph (if (string? graph)
                                   (edn/read-string graph)
                                   graph)]
                (log! :info (str "Creating FFBD diagram for: " (:process-id parsed-graph)))
                (log! :info (str "Parsed graph keys: " (keys parsed-graph)))
                (let [elements (eads->cytoscape-elements parsed-graph)]
                  (log! :info (str "FFBD elements count: " (count elements)))
                  (log! :info (str "FFBD elements sample: " (take 2 elements)))
                  (let [cy-instance (cytoscape-lib
                                     (clj->js
                                      {:container (j/get cy-ref :current)
                                       :elements elements
                                       :style (ffbd-stylesheet)
                                       :layout {:name "breadthfirst" :directed true :circle false :padding 30 :spacingFactor 1.5 :avoidOverlap true}
                                       :userZoomingEnabled true
                                       :userPanningEnabled true}))]
                    (log! :info (str "FFBD Cytoscape instance created with " (count elements) " elements"))
                  ;; Force fit and center the view
                    (js/setTimeout
                     (fn []
                       (.fit cy-instance)
                       (.center cy-instance)
                       (log! :info "FFBD diagram fitted to viewport"))
                     100)
                    (reset! diag {:cy-instance cy-instance :elements elements}))))
              (catch js/Error e
                (log! :info (str "Error creating FFBD diagram: " e))))))
        500)))

    (letfn [(handle-open [] (set-open true))
            (handle-close [] (set-open false))]
      ($ Box {:ref modal}
         ($ Button {:onClick handle-open :color "primary"} "FFBD Graph")
         ($ Dialog {:open open :onClose handle-close :fullScreen true :maxWidth false}
            ($ "div" {:style {:width "800px" :height "600px" :margin "20px auto" :position "relative"}}
               ($ "div" {:ref cy-ref
                         :style {:width "800px" :height "600px" :background-color "#f8f9fa"}})))))))
