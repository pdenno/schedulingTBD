(ns develop.dutil
  "Tools for various development-time tasks, currently especially for generating clojure specs for EADS instructions."
  (:require
   [clojure.set             :as set]
   [clojure.string          :as str]
   [taoensso.telemere       :refer [log!]]))

;;; ToDo: The naming convention used for make-specs is currently insufficient. There is a risk of collisions on :<whatever>/val specs.
;;;       A simple solution is to have a unique str prefix :<unique-string-prefix>--<whatever>/val, but I haven't yet implemented it.

(def ^:diag diag (atom nil))

(defn remove-annotations
  "Return argument with all maps of form {:comment '...' :val <some-val>} replaced by some-val."
  [obj]
  (letfn [(ca [obj]
            (cond (and (map? obj) (contains? obj :val) (contains? obj :comment))   (ca (:val obj))
                  (map? obj)                                                       (reduce-kv (fn [m k v] (assoc m k (ca v))) {} obj)
                  (vector? obj)                                                    (mapv ca obj)
                  :else                                                            obj))]
    (ca obj)))

;;; ToDo: Since this was designed for EADS (that are translated into JSON), I didn't test it much with values other than strings and numbers.
;;;       Likewise, the only collections anticipated are maps and vectors.

(def schema-info
  "A map indexed by [owner prop] of maps with two keys:
    1) :types is a set of the types encountered to be interpreted as a disjunction of what is allowed,
    2) :card either :one or :many."
  (atom {}))

(defn infer-map-type
  "Given a collection of mtypes, refer a new map type.
   Properties that are not :req-un in all examples are moved to :opt-un."
  [mtype-examples]
  (let [all-props (reduce (fn [res t] (-> res (into (:req-un t)) (into (:opt-un t)))) #{} mtype-examples)
        reqs      (reduce (fn [res t] (set/intersection res t)) all-props (map #(-> % :req-un set) mtype-examples))
        opts      (set/difference all-props reqs)]
    (cond-> {}
      (not-empty reqs)   (assoc :req-un (vec reqs))
      (not-empty opts)   (assoc :opt-un (vec opts)))))

(defn dtype
  "Return the data type of the argument object, either a keyword or a map with one key, :req-un."
  [obj]
  (cond (string? obj)  :type/string
        (number? obj)  :type/number
        (keyword? obj) :type/keyword
        (boolean? obj) :type/boolean
        (map? obj)     {:req-un (-> obj keys set)}
        (vector? obj)  (do (log! :warn (str "Vector of vectors. Hand code it. obj = " obj))
                           :VECTOR!)
        :else          (log! :warn (str "We don't handle the type of this object. obj = " obj))))

(defn dtype-vec
  "Infer types from a vector of values, returning a set of types.
   Argument v could be a mix of object and simple types.
   This looks at what is stored on schema-info as well as the arguments (not a pure function). "
  [v known-types]
  (reset! diag [v known-types])
  (let [mtype (some #(when (map? %) %) known-types)
        dtypes (remove map? known-types)
        vtypes  (-> (map dtype v) set)
        vmtypes (filter map? vtypes)
        all-mtypes (if mtype (conj vmtypes mtype) vmtypes)
        mtype (when (not-empty all-mtypes) (infer-map-type all-mtypes))]
    (if mtype
      (-> dtypes (conj mtype) set)
      (set/union vtypes (set dtypes)))))

(defn update-type!
  "Update the :types property at [owner prop] to contain the type of obj.
   If the :types property contains a map (there can be only one map in the set) and the type of obj is a map
   (specifically it will be {:req-un (-> obj keys set)}) then update the :types, possibly adding :opt-un keys."
  [obj owner prop]
  (let [known-types (or (-> (get @schema-info [owner prop]) :types) #{})
        dt-set (if (vector? obj) (dtype-vec obj known-types) (-> obj dtype vector set))
        result (set/union (->> known-types (remove map?) set) dt-set)]
    (swap! schema-info #(assoc-in % [[owner prop] :types] result))))

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
            (cond (map? obj)       (doseq [[k v] obj]
                                     (update-card! owner k (if (vector? v) :many :one))
                                     (when-not (ignore-set k) (lc v k)))
                  (vector? obj)    (doseq [x obj] (lc x owner))))]
    (lc eads-ds :eads-ds)))

(defn learn-types
  [eads-ds]
  (letfn [(lt [obj owner]
            (cond (map? obj)  (doseq [[k v] obj]
                                (update-type! v owner k)
                                (lt v k))
                  (vector? obj)  (doseq [x obj] (lt x owner))))]
    (lt eads-ds :eads-ds)))

(defn get-schema
  "Return the entry in the vector of specs that has the argument owner and prop-id."
  [schema-vec owner prop-id]
  (some #(when (= (:type-id %) [owner prop-id]) %) schema-vec))

(defn get-children
  "Return a vector of schema-vec objects that are children of the argument schema-vec object."
  [schema-vec {:keys [type-id spec]}]
  (let [[owner prop-id] type-id
        spec-slots (as-> spec ?x
                     (:types ?x)
                     (some #(when (map? %) %) ?x)
                     (cond-> []
                       (:req-un ?x) (into (:req-un ?x))
                       (:opt-un ?x) (into (:opt-un ?x)))
                     (sort ?x))]
    (if (= :self prop-id)
      (mapv #(get-schema schema-vec owner %) spec-slots)
      (mapv #(get-schema schema-vec prop-id %) spec-slots))))

(defn sort-schema
  "Using the argument map of schema indexed by [owner prop] create a vector of tuples ordered hierarchically starting [:eads-ds :self]."
  [schema-vec]
  (let [top (get-schema schema-vec :eads-ds :self)]
    (loop [result [top]
           todo (get-children schema-vec top)]
      (if (empty? todo) result
          (recur (conj result (first todo))
                 (into (-> todo rest vec) (get-children schema-vec (first todo))))))))

(defn custom-spec-defs
  "Generate spec defs (s/def) for the given property."
  [prop-id {:keys [card types]}]
  (reset! diag [prop-id {:card card :types types}])
  (letfn [(constraint [typ]
            (cond (map? typ)       (as-> typ ?t
                                     (reduce-kv (fn [r k v] (into r [k (mapv #(symbol (str ":" %)) v)])) [] ?t)
                                     `(s/keys ~@?t))
                  (keyword? typ)   (case typ
                                     :type/string 'string?
                                     :type/number 'number?
                                     :type/boolean 'boolean?
                                     :type/keyword 'keyword?
                                     :VECTOR!      'VECTOR!)))
          (prop-singular [pid]
            (when-not (str/ends-with? (name pid) "s")
              (log! :warn (str "Assumed this :many cardinality property ended in an 's': " pid)))
            (-> (re-matches #"([\w\-]+)\w" (name prop-id)) second))]
    (let [prop-val-name (symbol (str ":" (name prop-id) "/val"))
          prop-one-name (when (= card :many) (symbol (str "::" (prop-singular prop-id))))]
      (letfn [(or-choices [types] ; Trick here is that at most one map is allowed.
                (reduce (fn [res t] (into res (if (map? t)
                                                [:obj prop-val-name]
                                                [(-> t name keyword) (constraint t)])))
                        []
                        types))]
        (cond (and (== 1 (count types)) (= card :one ))   `[(s/def ~prop-val-name ~(constraint (first types)))]
              (and (== 1 (count types)) (= card :many))   `[(s/def ~prop-val-name (s/coll-of ~prop-one-name :kind ~(symbol "vector?")))
                                                            (s/def ~prop-one-name ~(constraint (first types)))]
              (= card :one)                               `[(s/def ~prop-val-name (s/or ~@(or-choices types)))]
              (= card :many)                              `[(s/def ~prop-val-name (s/coll-of ~prop-one-name :kind ~(symbol "vector?")))
                                                            (s/def ~prop-one-name (s/or ~@(or-choices types)))])))))

(defn gen-specs
  "Return specs implementing optional EADS-style annotations for the given property and its constraints.
   The topic argument is only used on for the [:eads-ds :self] object, defining a top-level spec for it."
  [{:keys [type-id spec]} topic]
  (let [[owner prop-id] type-id]
    (if (and (= owner :eads-ds) (= prop-id :self))
      `[(s/def ~(keyword topic "EADS-message") (s/keys :req-un ~(mapv #(symbol (str "::" (name %)))(-> spec :types first :req-un))))]
      (let [normal (symbol (str "::" (name prop-id)))
            prop-val-name (symbol (str ":" (name prop-id) "/val"))
            annotated (symbol (str "::annotated-" (name prop-id)))]
        `[(s/def ~normal (s/or :normal ~prop-val-name :annotated ~annotated))
          ~@(custom-spec-defs prop-id spec)
          (s/def ~(symbol (str "::annotated-" (name prop-id))) (s/keys :req-un [~(symbol "::comment") ~(keyword (name prop-id) "val")]))]))))

;;; (dutil/learn-schema {:inputs ["water" {:item-id "grommets" :from "my-process-id"}]})
(defn learn-schema!
  "Walk the map data structure, inferring type and structure information that could be stipulated in clojure specs."
  [eads-ds ignore-set]
  (let [eads-ds (remove-annotations eads-ds)]
    ;; You only get one example of the toplevel structure ([:eads-ds ;self]), so it must be as follows:
    (reset! schema-info {[:eads-ds :self] {:card :one :types [{:req-un (-> eads-ds keys set)}]}})
    (learn-cardinality eads-ds ignore-set)
    (learn-types eads-ds)))

;;; (dutil/make-specs ttable/timetabling "timetabling")
(defn ^:admin make-specs
  "This is the top-level function for making specs. It is designed to be called at the REPL.
   It return a vector of forms defining clojure specs for the argument EADS instructions."
  [eads-ds topic]
  (reset! schema-info {})
  (learn-schema! eads-ds #{}) ; Sets the schema-info atom
  (let [spec-vec (-> (reduce-kv (fn [res k v] (conj res (-> {} (assoc :type-id k) (assoc :spec v)))) [] @schema-info)
                     sort-schema)
        result-atm (atom [])]
    (doseq [spec spec-vec]
      (swap! result-atm into (gen-specs spec topic)))
    (swap! result-atm #(into [(str ";;; Created " (new java.util.Date) " using develop.dutil/make-spec.")] %))
    @result-atm))
