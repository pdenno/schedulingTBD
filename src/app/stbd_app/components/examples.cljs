(ns stbd-app.components.examples)

(declare rm-examples elena-schemas)

(defn get-example [name]
  (some #(when (= name (:name %)) %) rm-examples))

;;; ($get [["schema/name" "urn:oagis-10.8.4:Nouns:Invoice"],  ["schema-object"]])
(def rm-examples
  [{:name "Start a new project!"
    :data "Hi! Describe a scheduling challenge in a few sentences and we'll get started!"
    :code "% This is where we will define the MiniZinc for your problem."}

   {:name "Craft Brewing"
    :code "int : nProducts = 5;
set of int: Product = 1..nProducts;
set of int: Day = -36..365;
enum Task = {prep, ferment, bright};
array [Product, Task] of int: taskDuration = [|1, 20, 20 |
                                               1, 20, 20 |
                                               1, 30, 11 |
                                               1, 21, 11 |
                                               1, 18, 19 |];

array [Product] of Day: neededDate = [50, 80, 130, 140, 150];

% We assume a task uses one major resource; resources are synonymous with tasks.
array [Product, Task] of var Day: taskStarts; % 'var' means it is a decision variable
array [Product, Task] of var Day: taskEnds;

% We'll use this to ensure a resource can only be used by one product at a time.
array [Task, Day] of var Product: busyWith;

% The first two Product are already in the fermentation tank and bright tank respectively.
constraint taskStarts[1,prep] = -35;   % It is therefore in the bright tank...
constraint taskStarts[2,prep] = -5;    % ...and (IMPORTANT) will be out before this one needs to move there.

% Next task must start right after previous task ends.
constraint forall (p in Product) (taskEnds[p, prep   ] + 1 == taskStarts[p, ferment]);
constraint forall (p in Product) (taskEnds[p, ferment] + 1 == taskStarts[p, bright]);

% A task ends taskDuration days after it starts.
constraint forall (p in Product, t in Task) (taskEnds[p, t] == taskStarts[p, t] + taskDuration[p, t]);

% A resource(task) can only be used with one product at a time.
constraint forall (t in Task, p in Product, d in Day where d >= taskStarts[p,t] /\\ d <= taskEnds[p,t])
                    (busyWith[t,d] == p);

solve minimize sum (p in Product) (abs(neededDate[p] - taskEnds[p,bright]));"
}])
