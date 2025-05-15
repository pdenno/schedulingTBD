(ns scheduling-tbd.interviewing.eads-util
  (:require
   [clojure.spec.alpha    :as s]
   [taoensso.telemere       :refer [log!]]))

;;; ---------- These are used to check process EADS for valid graphs.
(defn list-process-ids
  "Return a vector of all process-ids defined in the graph."
  [graph]
  (let [coll (atom #{})]
    (letfn [(lp [obj]
              (cond (map? obj)    (do (when (contains? obj :process-id) (swap! coll conj (:process-id obj)))
                                      (doseq [[_ v] obj] (lp v)))
                    (vector? obj) (doseq [x obj] (lp x))))]
      (lp graph)
      (-> coll deref vec))))

(defn get-process
  "Return the process with the given process id."
  [graph process-id]
  (let [found? (atom nil)]
    (letfn [(fp [obj]
              (when-not @found?
                (cond (map? obj) (if (= process-id (:process-id obj))
                                   (reset! found? obj)
                                   (doseq [[_ v] obj] (fp v)))
                      (vector? obj) (doseq [x obj] (fp x)))))]
      (fp graph)
      @found?)))

(defn process-produces-output?
  "EADS process graphs generated through conversation can use an object with :item-id and :from to indicate
   a relationship between processes. For example, {:item-id 'graphite core', :from 'graphite-core-production'}.
   This function returns true if :from process does, in fact, list the :item-id as an :output."
  [graph {:keys [item-id from]}]
  (if-let [process (get-process graph from)]
    (if (some #(= item-id %) (->> process :outputs (map #(if (map? %) (:item-id %) %))))
      true
      (log! :error (str "Process " (:process-id process) " exists but does not have " item-id " as an output.")))
    (log! :error (str "No process " from " found."))))

(defn graph-inputs-from-outputs
  "Return a vector of all the graph maps containing both :item-id and :from."
  [graph]
  (let [coll (atom #{})]
    (letfn [(fp [obj]
              (cond (map? obj) (if (and (contains? obj :item-id) (contains? obj :from))
                                 (swap! coll conj obj)
                                 (doseq [[_ v] obj] (fp v)))
                    (vector? obj) (doseq [x obj] (fp x))))]
      (fp graph)
      (-> coll deref vec))))

(defn outputs-exist-where-inputs-claim?
  "Return true if for every process that claims to get an input from a certain process, that certain process in fact produces the item as output.
   Error messages are generated wherever this is not true."
  [graph]
  (every? #(process-produces-output? graph %) (graph-inputs-from-outputs graph)))

(defn inputs-match-in-hierarchy?
  "Return true if inputs of a superprocess collects the inputs of subprocesses.
   Error messages are generated wherever this is not true."
  [graph]
  (let [ok? (atom true)]
    (doseq [process-id (list-process-ids graph)]
      (let [inputs (->> process-id (get-process graph) :inputs (filter map?))]
        (doseq [{:keys [item-id from] :as input} inputs]
          (if-let [feeding-process (get-process graph from)]
            (when-not (some #(= item-id %) (->> feeding-process :outputs (map #(if (string? %) % (:item-id %)))))
              (log! :error (str "No matching output for input " input))
              (reset! ok? false))
            (do (log! :error (str "Could not find process " from " using " input))
                (reset! ok? false))))))
    @ok?))

(defn graph-semantics-ok?
  "This is used in spec testing of :flow-shop/graph. (See flow_shop.clj)."
  [graph]
  (and (outputs-exist-where-inputs-claim? graph)
       (inputs-match-in-hierarchy? graph)))
