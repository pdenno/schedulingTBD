(ns scheduling-tbd.iviewr.domain.process.process-analysis
  "Analysis of the process interview")

(def ^:diag diag (atom nil))


;;; This will probably be used again, this time with :process/flow-shop EADS.
#_(defn ^:diag extreme-dur-span?
  "Return the vector of units used in the process if :qty/units span from from :minutes to :days or more or :hours to :weeks or more.
   This always looks at :process/interview-class = :initial-unordered." ; ToDo: Looks like code rot here! :initial-unordered?
  [pid]
  (let [units (atom #{})]
    (letfn [(get-units [obj]
              (cond (map? obj)    (doseq [[k v] (seq obj)]
                                     (when (= k :quantity/units) (swap! units conj v))
                                     (get-units v))
                    (vector? obj) (doseq [v obj] (get-units v))))]
      (let [units @units]
        (cond (and (units :minutes) (or (units :days)  (units :weeks) (units :months)))   (vec units)
              (and (units :hours)   (or (units :weeks) (units (units :months))))          (vec units))))))
