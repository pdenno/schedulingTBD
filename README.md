# schedulingTBD

## Introduction

SchedulingTBD is exploratory software to study human/AI teaming in cognitive work that might continue for weeks or months.
The cognitive work we focus on is the joint (human and AI) formulation and refinement of analytical models in a domain-specific language (DSL), [MiniZinc](https://www.minizinc.org/).
To develop and test our theory, the software is designed to help solve manufacturing production scheduling problems by leading the user to formulate a solution with MiniZinc.
Our research goal is to determine best practices in the design of systems that use a DSL unfamiliar to users in joint work to solve problems important to them.
This DSL-based teaming model brings to focus some challenges of
(1) designing DSLs fit for human/AI teaming,
(2) problem solving in technical environments,
(3) constructionist learning, and
(4) scientific explanation.

In the software, we engage the user in chat-based conversation implemented with large language models (LLMs) and a multi-agent architecture.
As of this writing, we are implementing interviewing agents to understand the goals of users and introduce them to the DSL technology.
We are far enough along to posit a few lines of MiniZinc towards formulation of a solution. We expect to be able to formulate entire solutions to some problems soon.
Currently we are testing the interviewing process with surrogate human users, LLM-based agents simulating expertise in a manufacturing domain.
We plan to test hundreds of such domain expert surrogates before enabling similar functionality for human experts.

The software is being developed as part of the NIST project [Human/AI Teaming for Manufacturing Digital Twins](https://www.nist.gov/programs-projects/humanmachine-teaming-manufacturing-digital-twins).
Feel free to contact us if this work interests you!

## Building/Running (development mode)
   These instructions have not been thoroughly tested and are likely not complete. If you have problems, write an issue or email us (see the NIST project page above).

### Set up environment variables
  * Thus far, the work has only been tested with OpenAI LLMs and on Linux and Macs.
  * In your .bashrc file define an environment variable  `export OPENAI_API_KEY=sk-...`
  * Similarly, `export SCHEDULING_TBD_DB=/opt/scheduling` (or wherever you intend to store databases for the application).

### The server
  * Install install a Java JDK and [Clojure](https://clojure.org/).
  * Start a server REPL by starting Clojure in your editor from anywhere in the repository and running `(start)` in the `user` namespace.
  * If you are starting from scratch (no databases under the `SCHEDULING_TBD_DB` directory), you'll need to create the system and projects databases.
	The backup files for these are `data/system-db.edn` and the edn files in `data/projects`. Do the following:

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
