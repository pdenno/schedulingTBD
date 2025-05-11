(ns develop.dutil
  "Tools for repl-based exploration of schedulingTBD code"
  (:require
   [clojure.set             :refer [difference]]
   [taoensso.telemere       :refer [log!]]))

(defn remove-annotations
  "Return argument with all maps of form {:comment '...' :val <some-val>} replaced by some-val."
  [obj]
  (letfn [(ca [obj]
            (cond (and (map? obj) (contains? obj :val) (contains? obj :comment))   (:val obj)
                  (map? obj)                                                       (reduce-kv (fn [m k v] (assoc m k (ca v))) {} obj)
                  (vector? obj)                                                    (mapv ca obj)
                  :else                                                            obj))]
    (ca obj)))

(defn ^:diag spec-triple
  "Return three specs implementing optional annotation for the given property and its constraints"
  [prop-name constraints]
  `[(s/def ~(symbol (str "::" prop-name)) (s/or :normal ~(keyword (str prop-name) "val") :annotated ~(symbol (str "::annotated-" prop-name))))
    (s/def ~(keyword (str prop-name) "val") ~constraints)
    (s/def ~(symbol (str "::annotated-" prop-name)) (s/keys :req-un [~(symbol "::comment") ~(keyword (str prop-name) "val")]))])

;;; ToDo: Since this was designed for EADS (that are translated into JSON), I didn't test it much with values other than strings and numbers.
;;;       Likewise, the only collections anticipated are maps and vectors.

(def schema-info
  "A map indexed by [owner prop] of maps with two keys:
    1) :types is a set of the types encountered to be interpreted as a disjunction of what is allowed,
    2) :card either :one or :many."
  (atom {}))

(defn dtype
  "Return the data type of an object, for simple types it is a keyword, for maps it is a map with :req-un,
   for vectors is is a set of dtypes of the elements."
  [obj]
  (cond (string? obj)  :type/string
        (number? obj)  :type/number
        (keyword? obj) :type/keyword
        (boolean? obj) :type/boolean
        (vector? obj)  (->> obj (map dtype) set)
        (map? obj)     {:req-un (-> obj keys set)}
        :else          (log! :warn (str "Unknown type for obj = " obj))))

(defn update-req-opt!
  "Both arguments are maps containing :req-un (mtype might also contain :opt-un).
   If dt does not contain all the keys of mtype, move the missing keys to :opt-un.
   If dt contains keys that are not in mtype, add those keys to :opt-un."
  [mtype dt]
  (let [mtype (if (empty? (:opt-un mtype)) (assoc mtype :opt-un #{}) mtype)
        mtype-not-req (difference (:req-un mtype) (:req-un dt))
        dt-new        (difference (:req-un dt) (:req-un mtype))]
    (cond-> mtype
      (not-empty mtype-not-req)   (->
                                   (update :opt-un into mtype-not-req)
                                   (update :req-un (->> mtype (remove #(mtype-not-req %)) set)))
      (not-empty dt-new)          (update :opt-un into dt-new))))

(defn update-type!
  "Update the :types property at [owner prop] to contain the type of obj.
   If the :types property contains a map (there can be only one map in the set) and the type of obj is a map
   (specifically it will be {:req-un (-> obj keys set)}) then update the :types, possibly adding :opt-un keys."
  [obj owner prop]
  (let [known-types (-> (get @schema-info [owner prop]) :types)
        mtype (some #(when (map? %) %) known-types)
        dt (dtype obj)
        dt-mspec (cond (map? dt) dt
                       (set? dt) (some #(when (map? %) %) dt))]
    (when (empty? known-types)(swap! schema-info #(assoc-in % [[owner prop] :types] #{})))
    (cond  (keyword? dt)              (swap! schema-info #(update-in % [[owner prop] :types] conj dt))
           (and dt-mspec mtype)       (update-req-opt! mtype dt-mspec)
           dt-mspec                   (swap! schema-info #(update-in % [[owner prop] :types] conj dt-mspec)))
    (when (set? dt)  (swap! schema-info #(update-in % [[owner prop] :types] into (->> dt (remove map?)))))))

(defn update-card!
  "If cardinality of owner prop is not set, set it. If it is, check that it is same as the argument."
  [owner prop card-in]
  (if-let [card (-> (get @schema-info [owner prop]) :card)]
    (when-not (= card card-in) (log! :error (str "Inconsistent cardinality at [" owner ", " prop "].")))
    (swap! schema-info #(assoc-in % [[owner prop] :card] card-in))))

(defn learn-cardinality
  "Navigate the DS setting cardinality for every map property.
   Ignore set is a set of properties that won't be processes. (Typically the reason to ignore certain properties
   is that their values are defined. For example :subprocesses are processes.)"
  [eads-ds ignore-set]
  (letfn [(lc [obj owner]
            (cond (map? obj) (doseq [[k v] obj]
                               (update-card! owner k (if (vector? v) :many :one))
                               (when-not (ignore-set k) (lc v k)))
                  (vector? obj) (doseq [x obj] (lc x owner))))]
    (lc eads-ds :eads-ds)))

(defn learn-types
  [eads-ds]
  (letfn [(lt [obj owner]
            (cond (map? obj)  (doseq [[k v] obj]
                                (update-type! v owner k)
                                (lt v k))
                  (vector? obj)  (doseq [x obj] (lt x owner))))]
    (lt eads-ds :eads-ds)))

;;; (learn-schema {:inputs ["water" {:item-id "grommets" :from "my-process-id"}]})
(defn learn-schema
  "Walk the map data structure, inferring type and structure information that could be stipulated in clojure specs."
  ([eads-ds] (learn-schema eads-ds #{}))
  ([eads-ds ignore-set]
   ;; You only get one example of the toplevel structure ([:eads-ds ;self]), so it must be as follows:
   (reset! schema-info {[:eads-ds :self] {:card :one :types [{:req-un (-> eads-ds keys set)}]}})
   (learn-cardinality eads-ds ignore-set)
   (learn-types eads-ds)
   @schema-info))
