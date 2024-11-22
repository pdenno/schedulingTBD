int : nProducts = 2;
set of int: Product = 1..nProducts;
enum Task = {nibManufacturing, barrelCapManufacturing, internalMechanisms, assembly, qualityControl, packaging};
array [Product, Task] of float: taskDuration = [|1.0, 1.0, 1.0, 1.0, 1.0, 0.50|
						 1.0, 1.0, 1.0, 1.0, 1.0, 0.50|];

float : minTime = 0.0;
% It shouldn't take longer than doing only one thing at a time.
float : maxTime = sum (p in Product, t in Task) (taskDuration[p, t]);

% We assume a task uses one major resource; resources are synonymous with tasks.
% taskStarts and taskEnds are decision variables, the values we are trying to find.
array [Product, Task] of var  minTime..maxTime : taskStarts; % 'var' means it is a decision variable.
array [Product, Task] of var  minTime..maxTime : taskEnds;

% This will be used below to ensure that products aren't trying to do the same thing at the same time.
predicate no_overlap(var float : s1, float : d1, var float : s2, float : d2) =  s1 + d1 <= s2 \/ s2 + d2 <= s1;

% ensure that two Products aren't using the same resource (task) at the same time.
constraint forall (t in Task)
	     (forall  (i,k in Product where i < k)
		(no_overlap (taskStarts[i,t], taskDuration[i,t], taskStarts[k,t], taskDuration[k,t])));

% We assume no tasks are running now, so the first task of the first (and only) job can start now.
constraint taskStarts[1, nibManufacturing] = 0.0;       % Product 1 starts immediately.

% Every other task can start after the previous task ends.
constraint forall (p in Product) (taskEnds[p, nibManufacturing] <= taskStarts[p, barrelCapManufacturing]);
constraint forall (p in Product) (taskEnds[p, barrelCapManufacturing] <= taskStarts[p, internalMechanisms]);
constraint forall (p in Product) (taskEnds[p, internalMechanisms] <= taskStarts[p, assembly]);
constraint forall (p in Product) (taskEnds[p, assembly] <= taskStarts[p, qualityControl]);
constraint forall (p in Product) (taskEnds[p, qualityControl] <= taskStarts[p, packaging]);

% A task ends taskDuration after it starts.
constraint forall (p in Product, t in Task) (taskEnds[p, t] == taskStarts[p, t] + taskDuration[p, t]);

% Minimize time elapsed when the final task ends (maxspan).
solve minimize max (p in Product) (taskEnds[p, packaging]);
