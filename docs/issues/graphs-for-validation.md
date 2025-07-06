# Graph (diagram) presentation

We present both diagrams and tables to users to verify that we understood correctly what they told us about their processes and data.
Earlier today we got tables working.
You wrote table2.cljs based on table.cljs (which is now deleted from our iview4 branch).
The work on diagrams is analogous. Here we are starting with code that uses mermaid; and we'd like to switch to using GoJS (for which we are buying a license).
Like our work on tables, it comes down to writing something analogous to what we already have.
We wrote table2.cljs based on table.cljs. We'll write ffbd.cljs based on graph.cljs
Eventually we will want to create graphs for many different things, but today we are just replacing the functional flow block diagrams (FFBDs) from Mermaid with flow-block diagrams using GoJS.
Mermaid had a very rigid method for creating these diagrams. We made the translation from our structures to Mermaid on the server and sent the result to the client.
You might find some value in looking at ~/Documents/git/schedulingTBD/src/server/scheduling_tbd/ds2mermaid.clj, but things are apt to be so different on GoJS, that won't be of too much value.
The thing to focus on in designing a FFBD is the relationship between inputs and outputs of processes.
We create the input structures from our Summary EADS (Example Annotated Data Structures) from interviews about flow shop scheduling (and maybe others someday).
Here is such an example:
