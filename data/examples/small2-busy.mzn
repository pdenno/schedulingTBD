% There is a bug in this one! Second job starts late. Problem is with busyWith, I think
int : nProducts = 2;
set of int: Product = 1..nProducts;
enum Task = {nibManufacturing, barrelCapManufacturing, internalMechanisms, assembly, qualityControl, packaging};
array [Product, Task] of int: taskDuration = [|1, 1, 1, 1, 1, 1|
					       1, 1, 1, 1, 1, 1|];

int : minTime = 0;
% It shouldn't take longer than doing only one thing at a time.
int : maxTime = sum (p in Product, t in Task) (taskDuration[p, t]);

% We assume a task uses one major resource; resources are synonymous with tasks.
array [Product, Task] of var minTime..maxTime: taskStarts; % 'var' means it is a decision variable.
array [Product, Task] of var minTime..maxTime: taskEnds;

% We'll use this below to ensure a resource can only be used by one product at a time.
array [Task, minTime..maxTime] of var Product: busyWith;

% We assume no tasks are running now, so the first task of some product can start now.
constraint exists (p in Product) (taskStarts[p, nibManufacturing] = 0);

% Every other task can start after the previous task ends.
constraint forall (p in Product) (taskEnds[p, nibManufacturing] <= taskStarts[p, barrelCapManufacturing]);
constraint forall (p in Product) (taskEnds[p, barrelCapManufacturing] <= taskStarts[p, internalMechanisms]);
constraint forall (p in Product) (taskEnds[p, internalMechanisms] <= taskStarts[p, assembly]);
constraint forall (p in Product) (taskEnds[p, assembly] <= taskStarts[p, qualityControl]);
constraint forall (p in Product) (taskEnds[p, qualityControl] <= taskStarts[p, packaging]);

% A task ends taskDuration days after it starts.
constraint forall (p in Product, t in Task) (taskEnds[p, t] == taskStarts[p, t] + taskDuration[p, t]);

% A resource(task) can only be used with one product at a time.
constraint forall (t in Task, p in Product, d in minTime..maxTime where d >= taskStarts[p,t] /\ d <= taskEnds[p,t])
		    (busyWith[t,d] == p);

% Minimize time elapsed when the final task ends (maxspan).
solve minimize max (p in Product) (taskEnds[p, packaging]);
