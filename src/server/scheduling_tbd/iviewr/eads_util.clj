(ns scheduling-tbd.iviewr.eads-util
  (:require
   [scheduling-tbd.db       :as db]
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
  (and
   (= #{:EADS-ref :process-id :inputs :outputs :resources :duration :subprocesses}
      (-> graph keys set))
   (outputs-exist-where-inputs-claim? graph)
   (inputs-match-in-hierarchy? graph)))

;;; --------------------------- The following are more generally applicable -----------------------------
(defn dispatch-ds-complete?
  [tag _pid]
  (assert ((db/system-EADS?) tag))
  tag)

(defmulti ds-complete? #'dispatch-ds-complete?)

(defn dispatch-combine-ds!
  [tag _pid]
  (assert ((db/system-EADS?) tag))
  tag)

(defmulti combine-ds! #'dispatch-combine-ds!)

(defn strip-annotations
  "Transfom the EADS argument in the following ways:
     1) Replace {:val v :comment c} maps with v.
     2) Remove property :comment wherever it occurs.
     3) Remove property :invented.
   This is typically used to make s/valid?-dation easier."
  [obj]
  (cond (and (map? obj)
             (contains? obj :val))              (strip-annotations (:val obj)) ; Sometimes they won't contain :comment.
        (map? obj)                              (reduce-kv (fn [m k v]
                                                             ;; Sometimes interviewers think we allow comment like this; we don't!
                                                             (if (#{:comment :invented} k)
                                                               m
                                                               (assoc m k (strip-annotations v))))
                                                           {} obj)
        (vector? obj)                           (mapv strip-annotations obj)
        :else                                    obj))

(defn collect-keys-vals
  "Collect the values of the argument property, which is a key to some nested object in the argument obj."
  [obj prop]
  (let [result (atom #{})]
    (letfn [(ck [obj]
              (cond (map? obj)           (doseq [[k v] obj] (if (= k prop) (swap! result conj v) (ck v)))
                    (vector? obj)        (doseq [v obj] (ck v))))]
      (ck obj))
    @result))

(defn insert-by-id
  "The argument obj has a key SOMEWHERE, prop, the value of which is a vector.
   Conj the third argument object onto that vector.
   Return the modified object."
  [obj vec-prop add-this]
  (letfn [(i-by-i [obj]
            (cond (map? obj)                  (reduce-kv (fn [m k v]
                                                           (if (= k vec-prop)
                                                             (assoc m k (conj v add-this))
                                                             (assoc m k (i-by-i v))))
                                                         {} obj)
                  (vector? obj)               (mapv i-by-i obj)
                  :else                       obj))]
    (i-by-i obj)))

(defn remove-by-id
  "The argument obj has a key SOMEWHERE, vec-prop, the value of which is a vector.
   There should be in that vector an element with that has and elem-prop of value elem-id.
   Remove the object that has that elem id and return the modified top-level object, obj"
  [obj vec-prop elem-prop elem-id]
  (letfn [(r-by-i [obj]
            (cond (map? obj)                  (reduce-kv (fn [m k v]
                                                           (if (= k vec-prop)
                                                             (assoc m k (reduce (fn [rr vv]
                                                                                  (if (= elem-id (get vv elem-prop))
                                                                                    rr
                                                                                    (conj rr vv)))
                                                                                [] v))
                                                             (assoc m k (r-by-i v))))
                                                         {} obj)
                  (vector? obj)               (mapv r-by-i obj)
                  :else                       obj))]
    (r-by-i obj)))

(defn replace-by-id
  [obj vec-prop elem-prop add-this]
  (if-let [elem-id (get add-this elem-prop)]
    (-> obj
        (remove-by-id vec-prop elem-prop elem-id)
        (insert-by-id vec-prop add-this))
    (throw (ex-info "Couldn't find the elem to replace."
                    {:vec-prop elem-prop :add-this add-this}))))

(defn get-object
  "The argument object obj, contains (somewhere) a key property prop having value k,
   return the object at (prop = k)."
  [obj prop kval]
  (let [found (atom nil)]
    (letfn [(somewhere [obj]
              (or @found
                  (cond  (map? obj)                (doseq [[k v] obj]
                                                     (when (and (= k prop) (= v kval))
                                                       (reset! found obj))
                                                     (somewhere v))
                         (vector? obj)             (doseq [x obj] (somewhere x)))))]
      (somewhere obj)
      @found)))
