(ns scheduling-tbd.minizinc
  "Functions for creating MiniZinc from templates and executing with solvers."
  (:require
   [clojure.edn              :as edn]
   [clojure.pprint           :refer [cl-format]]
   [clojure.spec.alpha        :as s]
   [clojure.string           :as str]
   [scheduling-tbd.db        :as db]
   [taoensso.telemere.timbre        :as log]))

;;; System thoughts: Given minimal-mzn-for-process, you could then ask to
;;;   - allow parallelism where feasible
;;;   - multiple jobs with

(def ^:diag diag (atom nil))

(defn serial-precedence-constraints
  "Return constraints ensuring that the next task can't start until the previous one ends."
  [pnames]
  (reduce (fn [res i]
            (str res (format "constraint forall (p in Product) (taskEnds[p, %s] == taskStarts[p, %s]);\n"
                             (nth pnames i) (nth pnames (inc i)))))
          ""
          (range 0 (dec (count pnames)))))

(s/def ::uom (s/or :ordinary #(#{:minutes :hours :days :weeks :months} %)
                   :botched (s/keys :req [:box/string-val])))

(defn convert-duration
  "Given a process qty-map (a map with keys :duration/units and :duration/value-string),
   return a number being the value of the value-string converted to the tartget-units units of measure.
   Thus (convert-duration :hours {:duration/units :minutes :duration/value-string '30'} would return 0.50.

   The whole quantity map could be #:box{:string-val 'variable'}, in which case duration is set to -1."
  [target-units {:quantity/keys [units value-string] :as _qty-map}]
  (reset! diag {:target-units target-units :qty-map _qty-map})
  (s/assert ::uom units)
  (assert (#{:minutes :hours :days :8-hour-shifts} target-units))
  (let [src-val (edn/read-string value-string)]
    (assert (or (number? src-val) (nil? src-val)))
    (if (nil? src-val)
      -1
      (case units
        :minutes (case target-units
                   :minutes src-val
                   :hours  (/ src-val 60)
                   :8-hour-shifts (/ src-val 480)
                   :days   (/ src-val 1440))

        :hours (case target-units
                 :minutes (* src-val 60)
                 :hours src-val
                 :8-hour-shifts (/ src-val 8)
                 :days (/ src-val 24))

        :days (case target-units
                :minutes (* src-val 1440)
                :hours (* src-val 24)
                :8-hour-shifts (* src-val 3)
                :days src-val
                :weeks (* src-val 5)) ; 5-day work week.

        :weeks (case target-units
                :hours (* src-val 8 5) ; 5-day, 8 hour work week.
                :8-hour-shifts (* src-val 3 5)
                :days (* src-val 5)
                :weeks src-val)

        :months (case target-units
                  :minutes (* src-val 43200)
                  :hours (* src-val 720)
                  :8-hour-shifts (* src-val 30) ; Guessing that's what they mean!
                  :days (* src-val 30))))))

;;; ToDo: Might be silly. I think I use the -1 idea later.
(defn number-heuristic
  "Produce some kind of number out of something that might not be a number."
  [s]
  (let [val (edn/read-string s)]
    (if (number? val)
      val
      (let [st (str/lower-case s)]
        (cond
          (#{"few" "a few"} st) 3,
          (#{"several"} st)     5,
          :else                 -1)))))

(defn process-dur-avg
  "Return the argument the process definition with where :process/duration in the case where
   it has :quantity-range/low and :quantity-range/high values with the average using units of :quantity-range/low.
   Where these keys are not present, return the argument untouched."
  [{:process/keys [duration] :as process}]
  (if (contains? duration :quantity-range/low)
    (let [low-uom (-> duration :quantity-range/low :quantity/units)
          low-val (-> duration :quantity-range/low :quantity/value-string number-heuristic)
          high-val (convert-duration low-uom (:quantity-range/high duration))]
      (assoc process :process/duration {:quantity/value-string (cl-format nil "~,3f" (/ (+ high-val low-val) 2.0))
                                        :quantity/units low-uom}))
    process))

(defn extreme-dur-range?
  "Return the units found in the case that the UoM vary over a unmanageably wide range."
  [proc-maps]
  (let [units? (->> proc-maps (mapv #(-> % :process/duration :quantity/units)) set)]
    (cond (and (units? :minutes) (or (units? :days)  (units? :weeks) (units? :months))) (vec units?)
          (and (units? :hours)   (or (units? :weeks) (units? :months)))                 (vec units?))))

;;; ToDo: A candidate for an agent???
(defn best-uom
  "Apply a heuristic to return what seems to be the best common unit of measure for scheduling.
   This heuristic assumes that the process-maps don't contain quantity-ranges.
   For example, process-dur-avg has been applied to transform the range to the average."
  [proc-maps]
  (when-let [units (extreme-dur-range? proc-maps)]
    (log/warn "Extreme range of units. May want to ignore or aggregate some processes:" units))
  (let [units-found? (->> proc-maps (mapv #(-> % :process/duration :quantity/units)) set)]
    ;; ToDo: Not much of a heuristic yet!
    (cond (every? #(= % :minutes) units-found?) :minutes
          (units-found? :hours) :hours
          (units-found? :8-hour-shifts) :days ; ToDo: Needs thought. Idea is that's what they think of as a day.
          (units-found? :days) :days
          (units-found? :weeks) :days
          (units-found? :months) :days)))

(defn round-dur
  "Replace the dur-string with a rounded integer value."
  [proc]
  (update-in proc [:process/duration :quantity/value-string] #(-> % edn/read-string Math/round str)))

;;; The full line looks like this:
;;; array [Product, Task] of int: taskDuration = [|1, 2, 1, 1, 1, 0.50|];
;;; We want the part starting at 'int:' and with the ']' before the semicolon.
(defn task-durations
  "Return a map of :durs and :uom where :durs and :type where
    - :durs a vector of string representing numbers (float or int) indicating quantities of time,
    - :uom is a map describing a unit of measure, and
    - :type is either :float or :int indicating how :durs is represented.
   Argument is some vector of process object in DB format."
  [procs]
  (let [procs (map process-dur-avg procs)
        uom (best-uom procs)
        durs (mapv #(convert-duration uom %) (map :process/duration procs))
        type (if (not-every? integer? durs) :float :int)
        dur-string (if (= type :int)
                     (cl-format nil "[|~{~A~^, ~}|]" durs)
                     (cl-format nil "[|~{~,3f~^, ~}|]" durs))]
    (-> {}
        (assoc :dur-string dur-string)
        (assoc :dur-type type)
        (assoc :uom uom))))

;;; ------------------------ Templates -----------------------------------------------------
(def temp-pattern "The regex pattern used by template manipulating functions." (re-pattern #"\$\$\$([A-Z,-]+)\$\$\$"))

(defn template-line-maps
  "Return a vector of maps where each vector contains :line, a line from mzn-text, and :keys all the keys found, on the
   the line, in the order they are found. Keys is a vector of the lower-case keywords corresponding to the $$$some-key$$$."
  [{:keys [mzn-text] :as _template}]
  (reduce (fn [res s]
            (let [matcher (re-matcher temp-pattern s)
                  found (loop [r []]
                          (if-let [match (re-find matcher)]
                            (recur (conj r (-> match second str/lower-case keyword)))
                            r))]
              (conj res {:line s :keys found})))
          []
          (str/split-lines mzn-text)))

(defn sub-in-template
  "Return the template string with all keys replaced by values from the map.
   Since this uses cl-format ~A exclusively, the argument to substitute should
   be pre-formatted as needed. The subs-map should contain exactly the same
   keys as the template; no extra allowed."
  [template subs-map]
  (let [template-maps (template-line-maps template)
        subs-map-keys (-> subs-map keys set)
        template-keys (reduce (fn [res {:keys [keys]}] (into res keys)) #{} template-maps)]
    (assert (= subs-map-keys template-keys))
    (reduce (fn [res {:keys [line keys]}]
              (let [fmt-line (str/replace line temp-pattern "~A")]
                (conj res (apply cl-format nil fmt-line (map #(get subs-map %) keys)))))
            []
            template-maps)))

(defn minimal-mzn-for-process
  "Return a complete but minimal MiniZinc specification for the project's process.
   It is minimal in the following sense:
    - It is for running one product or batch (i.e. one job).
    - There is one resource of each type, the resource is named the same as the process.
    - The resources are available.
    - Parallel use of anything. It is all serial.
   The data that it must provide should be similar to that of the :test-data key found in
   the minimal-process.edn key :test-data, the keys of which are :sub-keys."
  [pid]
  (let [template (-> "data/templates/minimal-process.edn" slurp edn/read-string)
        procs (->> (db/get-process pid :initial-unordered)
                   :process/sub-processes
                   (remove #(:process/supply-chain? %))
                   vec)
        var-names (map :process/var-name procs)
        {:keys [dur-string dur-type uom]} (task-durations procs)]
    (->>
     (sub-in-template
      template
      {:dur-uom (name uom)         ; e.g. "hours"
       :dur-type (name dur-type)   ; e.g. "int"
       :task-enum-list  (cl-format nil "{~{~A~^, ~}}" var-names)  ; the variable names
       :task-duration-array dur-string ; a vector of strings representing numbers.
       :first-task-name (first var-names) ; e.g. "cutFromStock"
       :last-task-name (last var-names)   ; e.g. "packaging"
       :task-serial-constraints  (serial-precedence-constraints var-names)})
     (interpose "\n")
     (apply str))))
