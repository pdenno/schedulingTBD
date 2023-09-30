# schedulingTBD

## Introduction

SchedulingTBD is exploratory software for writing systems that enable joint (human/AI) formulation of analytical models in an implemented domain specific language (DSL).
Currently, we are using this code to explore joint formulation of scheduling problems with [MiniZinc](https://www.minizinc.org/) as the DSL.
The idea of implementations such as our MiniZinc effort is to probe the research question
*"Can we effectively engage a person possessing little background in model formulation in a 'conversation' where we jointly formulate a model important to their work?"*
Consequently, the code (Clojure and ClojureScript) provides a web app to host conversations about scheduling work, and when schedulingTBD is ready to go, it can be used to produce MiniZinc to schedule the user's work.

As of this writing, September 2023, we are just getting start with this idea.
It is part of a NIST project, ”Human/Machine Teaming for Manufacturing Digital Twins.”
I think we want the web app to provide tools to author and capture results from teaming sessions.
The authoring capabilities provided to experts in use of the analytical tool (and its DSL) would include
(a) authoring few-shot prompts to large language models (LLMs) to analyze the user’s contribution to the conversation, and
(b) authoring plan steps to an AI planner that directs the conversation.

Currently the tool does little more than get things started.
(See the screenshot below.)
I think that the AI planning might use [SHOP3](https://github.com/shop-planner/shop3)
(and updated version of University of Maryland's Simple Hierarchical Ordered Planner (SHOP)) in some role in
an on-line partially observable Markov descision process (on-line POMDP) [1,2].
But we'll start by seeing how far we can get just using plain SHOP3 capabilities.

## ToDo
  - Explore planning with SHOP3.
  - Reference code for parsing MiniZinc and executing MiniZinc constraints [here](https://github.com/pdenno/minizinc-parser).
  - Support dynamic planning.
  - Support a UI for authoring rules and prompts.

![alt text](https://github.com/pdenno/schedulingTBD/blob/main/doc/SchedulingTBD-early.png?raw=true)


## References
  - [1] Robert P. Goldman, *Solving POMDPs online through HTN Planning and Monte Carlo Tree Search*, Proceedings of the 4th ICAPS Workshop on Hierarchical Planning (HPlan 2021).
  - [2] Mykel J. Kochenderfer, *Decision Making Under Uncertainty: Theory and Applications*, MIT Press, 2015.
