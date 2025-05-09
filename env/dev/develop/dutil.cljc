(ns develop.dutil
  "Tools for repl-based exploration of schedulingTBD code"
  (:require
   [clojure.pprint        :refer [cl-format]]))

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
  "A map indexed by data-structure keywords."
  (atom {:EADS {:keys [] :owner nil}}))

(defn update-info
  "Update the schema info, reporting on contradictions."
  [])

(defn data-type [obj]
  "Return a description of the data type of the object multiplicity and base type:
   {:mult #{:one :many} :base-type #{:map :vector :string :number}}."

  (if (vector? obj)   (as-> {} ?info
                        (assoc ?info :mult :many)
                        (cond (every? string? obj)  (update-info ?info :base-type :string)
                              (every? number? obj)  (assoc ?info :base-type :number)
                              :else                 :foo))
      nil))

(defn learn-schema
  "Walk the map data structure, inferring type and structure information that could be stipulated in clojure specs."
  [eads-ds]
  (letfn [(ls [obj]
            ())]))
