(ns stbd-app.components.ffbd
  "A React component to display Functional Flow Block Diagrams using GoJS."
  (:require
   [applied-science.js-interop :as j]
   [clojure.edn :as edn]
   [helix.core :as helix :refer [defnc $]]
   [helix.dom :as dom]
   [helix.hooks :as hooks]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/Dialog$default" :as Dialog]
   ["gojs" :as go]
   [stbd-app.util :as util :refer [register-fn]]
   [taoensso.telemere :refer [log!]]))

;;; Utility functions for EADS to GoJS transformation

(defn flatten-processes
  "Recursively flatten EADS structure into a list of process nodes."
  [eads-data]
  (let [{:keys [process-id inputs outputs resources duration subprocesses]} eads-data
        current-node {:key process-id
                      :text process-id
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
                    (fn [acc {:keys [key outputs]}]
                      (reduce
                       (fn [m output]
                         (let [output-id (if (map? output) (:item-id output) output)]
                           (assoc m output-id key)))
                       acc
                       outputs))
                    {}
                    process-nodes)

        input-map (reduce
                   (fn [acc {:keys [key inputs]}]
                     (reduce
                      (fn [m input]
                        (let [input-info (if (map? input)
                                           {:id (:item-id input) :from (:from input)}
                                           {:id input :from nil})]
                          (update m (:id input-info) #(conj (or % []) {:target key :from (:from input-info)}))))
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
                  {:from source
                   :to (:target target)
                   :text material})
                targets))))
     input-map)))

(defn eads->gojs-model
  "Transform EADS data structure to GoJS model format."
  [eads-data]
  (when eads-data
    (let [processes (flatten-processes eads-data)
          connections (extract-connections processes)
          ;; Map processes to GoJS node format
          node-data (map (fn [{:keys [key]}]
                           #js {:key key :text key})
                         processes)
          ;; Map connections to GoJS link format
          link-data (map (fn [{:keys [from to text]}]
                           #js {:from from :to to :text (or text "")})
                         connections)]
      #js {:nodeDataArray (into-array node-data)
           :linkDataArray (into-array link-data)})))

;;; React Components

(defnc FFBDPane
  "GoJS diagram component for displaying FFBD."
  [{:keys [init-graph]}]
  (let [diagram-ref (hooks/use-ref nil)
        [diagram set-diagram] (hooks/use-state nil)]

    ;; Initialize diagram when component mounts or graph data changes
    (hooks/use-effect
     [init-graph]
     (when-let [div-element (j/get diagram-ref :current)]
       (try
         (log! :info "Checking GoJS availability...")

         ;; First check if GoJS is available
         (if (and js/window.go (.-Diagram js/window.go))
           (do
             (log! :info "GoJS found, creating diagram...")
             ;; Create diagram using global go object
             (let [new-diagram (js/window.go.Diagram. div-element)]

               ;; Try to set a very basic node template
               (set! (.-nodeTemplate new-diagram)
                     (js/window.go.GraphObject.make "Node"
                                                    (js/window.go.GraphObject.make "TextBlock"
                                                                                   (js-obj "text" "Hello"))))

               ;; Set simple model
               (set! (.-model new-diagram)
                     (js/window.go.Model. #js [#js {:text "Test Node"}]))

               (set-diagram new-diagram)
               (log! :info "Basic GoJS diagram created")))

           ;; GoJS not found, try alternative access methods
           (do
             (log! :info "GoJS not found at window.go, trying alternative...")
             (if (exists? go)
               (do
                 (log! :info "Found 'go' namespace, creating diagram...")
                 (let [new-diagram (go/Diagram. div-element)]
                   (set-diagram new-diagram)
                   (log! :info "Created diagram using 'go' namespace")))
               (do
                 (log! :error "GoJS not available - neither window.go nor 'go' namespace found")
                 (set! (.-innerHTML div-element) "<div style='padding: 20px; color: red;'>GoJS library not found</div>")))))

         (catch :default e
           (log! :error (str "Error in GoJS initialization: " e))
           (when-let [div (j/get diagram-ref :current)]
             (set! (.-innerHTML div) (str "<div style='padding: 20px; color: red;'>GoJS Error: " e "</div>")))))))

    ;; Render the diagram container
    (dom/div {:ref diagram-ref
              :style {:width "100%"
                      :height "400px"
                      :border "1px solid #ccc"
                      :background-color "#f0f0f0"}})))

(defnc FFBDModal
  "Modal dialog containing the FFBD diagram."
  [{:keys [graph]}]
  (let [modal (hooks/use-ref nil)
        [open set-open] (hooks/use-state false)]
    (letfn [(handle-open [] (when (j/get modal :current) (set-open true)))
            (handle-close [] (set-open false))]
      (dom/div {:ref modal}
               ($ Button {:color "primary" :onClick handle-open} "FFBD Graph")
               ($ Dialog {:open open
                          :onClose handle-close
                          :fullScreen true
                          :maxWidth "lg"}
                  ($ Box {:p 2}
                     ($ FFBDPane {:init-graph graph})))))))
