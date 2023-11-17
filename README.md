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
I think we might [SHOP3](https://github.com/shop-planner/shop3)
(and updated version of University of Maryland's Simple Hierarchical Ordered Planner (SHOP)) in
an on-line partially observable Markov descision process (on-line POMDP).
But we'll start by generating test cases running mostly static interviews.

## Building/Running (development mode)

## Set up an environment variable to find the databases
  * In your .bashrc define the following: `export SCHEDULING_TBD_DB=/opt/scheduling`
  * Note that we've only tried Linux and Macs, and judging by what I see in the data files (e.g. data/system.edn) the code will only 
    handle this particular configuration and just the linux and Mac platforms. There's a ToDo for the developers.

### The server
  * Install install a Java JDK and [Clojure](https://clojure.org/).
  * Start a server REPL by starting Clojure in your editor from anywhere in the repository and running `(start)` in the `user` namespace.
  * If you are starting from scratch, you'll need to create the system and projects databases. The files for these are data/system-db.edn and the edn files in data/projects.
 
   ```
(in-ns 'scheduling-tbd.db)
(recreate-system-db!)
(recreate-project-dbs!)
   ```

### The web app
  * Install [nodeJS/npm](https://nodejs.org/en/).
	In the repository's top-level directory run `npm install .`. Then at a shell prompt, start the shadow-cljs with `npm run stbd-client`.
	A client will start when you visit http://localhost:3300/app .
  *	As the output from [shadow-cljs](https://github.com/thheller/shadow-cljs) suggests, you can connect your editor to the running CLJS REPL at port 7002.
	A client needs to load successfully in order to use the CLJS REPL, otherwise you are likely to get a message `No available JS runtime` when you try to evaluate anything.

## ToDo
  - Explore planning with SHOP3.
  - Reference code for parsing MiniZinc and executing MiniZinc constraints [here](https://github.com/pdenno/minizinc-parser).
  - Support dynamic planning.
  - Support a UI for authoring rules and prompts.

![alt text](https://github.com/pdenno/schedulingTBD/blob/main/doc/SchedulingTBD-early.png?raw=true)
