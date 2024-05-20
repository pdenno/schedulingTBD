# schedulingTBD

## Introduction

SchedulingTBD is exploratory software to study human/AI teaming in long-running (months) cognitive work.
The cognitive work we focus on is the formulation and refinement of analytical models in a domain-specific language (DSL), [MiniZinc](https://www.minizinc.org/).
Our goal is to determine best practices in how persons having no prior experience with the DSL might learn the language to solve problems important to their work.
This DSL-based teaming model brings to focus the challenges of designing DSLs fit for this purpose, problem solving, constructionist learning, and scientific explanation.

In the software, we engage the user in chat-based conversation implemented with Large Language Models (LLMs) and a multi-agent architecture.
As of this writing, we are mid-way through the work of implementing interviewing agents to understand the goals of users.
We are far enough along to posit a few lines of MiniZinc towards formulation of a solution. We expect to be able to formulate entire solutions to some problems soon.
Currently we are testing the interviewing process with surrogate human users, LLM-based agents simulating expertise in a manufacturing domain.
We plan to test hundreds of such domain expert surrogates before enabling similar functionality for human experts.

The software is being developed as part of the NIST project [Human/AI Teaming for Manufacturing Digital Twins](https://www.nist.gov/programs-projects/humanmachine-teaming-manufacturing-digital-twins).
Feel free to contact us if this work interests you!

## Building/Running (development mode)
   These instructions have not been throroughly tested and are likely not complete. If you try have problems, write an issue or email us (see the NIST project page above).

### Set up environment variables
  * Thus far, the work has only been tested with OpenAI LLMs and on Linux and Macs.
  * In your .bashrc file `export OPENAI_API_KEY=sk-...`
  * Similarly, define the following: `export SCHEDULING_TBD_DB=/opt/scheduling` (or wherever you intend to store databases for the project).

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

![alt text](https://github.com/pdenno/schedulingTBD/blob/main/doc/stbd-screenshot-2024-05-20.png?raw=true)
