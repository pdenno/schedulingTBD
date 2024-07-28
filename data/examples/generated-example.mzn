int : nProducts = 1;
set of int: Product = 1..nProducts;
enum Task = {nibManufacturing, barrelCapManufacturing, internalMechanisms, assembly, qualityControl, packaging};
% Durations are in units of hours and may be rounded.
array [Product, Task] of float: taskDuration = [|1.000, 2.000, 1.000, 1.000, 1.000, 0.500|];

float : minTime = 0;
% It shouldn't take longer than doing only one thing at a time.
float : maxTime = sum (p in Product, t in Task) (taskDuration[p, t]);

% We assume a task uses one major resource; resources are synonymous with tasks.
array [Product, Task] of var minTime..maxTime: taskStarts; % 'var' means it is a decision variable.
array [Product, Task] of var minTime..maxTime: taskEnds;

% This is used in the constraint below.
predicate no_overlap(var float:s1, float:d1, var float:s2, float:d2) = s1 + d1 <= s2 \/ s2 + d2 <= s1;

% Ensure that no two Products aren't using the same resource (task) at the same time.
constraint forall (t in Task)
	     (forall  (i,k in Product where i < k)
		(no_overlap (taskStarts[i,t], taskDuration[i,t], taskStarts[k,t], taskDuration[k,t])));

% We assume no tasks are running now, so the first task of some product can start now.
constraint exists (p in Product) (taskStarts[p, nibManufacturing] = 0);

% Starting some time in the past is not allowed.
constraint forall (p in Product where p > 1) (taskStarts[p, nibManufacturing] > 0);

% Every other task can start after the previous task ends.
constraint forall (p in Product) (taskEnds[p, nibManufacturing] == taskStarts[p, barrelCapManufacturing]);
constraint forall (p in Product) (taskEnds[p, barrelCapManufacturing] == taskStarts[p, internalMechanisms]);
constraint forall (p in Product) (taskEnds[p, internalMechanisms] == taskStarts[p, assembly]);
constraint forall (p in Product) (taskEnds[p, assembly] == taskStarts[p, qualityControl]);
constraint forall (p in Product) (taskEnds[p, qualityControl] == taskStarts[p, packaging]);

% A task ends taskDuration time units after it starts.
constraint forall (p in Product, t in Task) (taskEnds[p, t] == taskStarts[p, t] + taskDuration[p, t]);

% Minimize time elapsed when the final task ends (maxspan).
solve minimize max (p in Product) (taskEnds[p, packaging]);