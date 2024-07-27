(ns scheduling-tbd.minizinc
  "Functions for creating MiniZinc from templates and executing with solvers."
  (:require
   [clojure.edn              :as edn]
   [clojure.pprint           :refer [cl-format]]
   [scheduling-tbd.db        :as db]))

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

;;; ToDo: Make this a method on target-units.
;;; ToDo: Can also have value ranges. Maybe take the average.
(defn convert-qty
  "Given a process qty-map (a map with keys :duration/units and :duration/value-string),
   return a string representing the quantity (a number) converted to the argument units.
   Thus (convert-qty :hours {:duration/units :minutes :duration/value-string '30'} would
   return the string '0.50'."
  [target-units {:quantity/keys [units value-string] :as _qty-map}]
  (reset! diag _qty-map)
  (case target-units
    :hours (case units
             :hours value-string
             :minutes (cl-format nil "~,2f" (/ (edn/read-string value-string) 60)))))

;;; The full line looks like this:
;;; array [Product, Task] of int: taskDuration = [|1, 2, 1, 1, 1, 0.50|];
;;; We want the part starting at 'int:' and with the ']' before the semicolon.
(defn task-durations
  "Return a string such as 'int: taskDuration = <the task enum names>' or
                            float: taskDuration = <the task enum names>'
   given the enum names and durations."
  [procs uom]
  (let [durs (->> procs (map :process/duration) (map #(convert-qty uom %)))
        dur-vals (map edn/read-string durs)
        type (if (not-every? integer? dur-vals) 'float 'int)]
    (cl-format nil "array [Product, Task] of ~A: taskDuration = [|~{~A~^, ~}|]" type durs)))

(defn minimal-mzn-for-process
  "Return a complete but minimal MiniZinc specification for the projects process.
   It is minimal in the following sense:
    - It is for running one product or batch (i.e. one job).
    - There is one resource of each type, the resource is named the same as the process.
    - The resources are available.
    - Parallel use of anything. It is all serial."
  [pid]
  (let [temp (-> "data/templates/minimal-process-hours.edn" slurp edn/read-string)
        procs (->> pid
                   db/get-project
                   :project/processes
                   :process/sub-processes
                   (remove #(:process/supply-chain? %))
                   vec)]
    (cl-format nil (:template/string temp)
               (cl-format nil "{~{~A~^, ~}}" (map :process/var-name procs))
               (task-durations procs :hours) ; ToDo: Not necessarily hours
               (-> procs first :process/var-name)
               (serial-precedence-constraints (mapv :process/var-name procs))
               (-> procs last :process/var-name))))
