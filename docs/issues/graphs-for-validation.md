# Graph (diagram) presentation

We present both diagrams and tables to users to verify that we understood correctly what they told us about their processes and data.
Earlier today we got tables working.
You wrote table2.cljs based on table.cljs (which is now deleted from our iview4 branch).
The work on diagrams is analogous. Here we are starting with code that uses mermaid; and we'd like to switch to using GoJS (for which we are buying a license).
Like our work on tables, it comes down to writing something analogous to what we already have.
We wrote table2.cljs based on table.cljs. We'll write ffbd.cljs based on graph.cljs
Eventually we will want to create graphs for many different things, but today we are just replacing the functional flow block diagrams (FFBDs) from Mermaid with flow-block diagrams using GoJS.
Mermaid had a very rigid method for creating these diagrams. We made the translation from our structures to Mermaid on the server and sent the result to the client.
You might find some value in looking at ~/Documents/git/schedulingTBD/src/server/scheduling_tbd/ds2mermaid.clj, but things are apt to be so different on GoJS, that that won't be of too much value.
The thing to focus on in designing a FFBD is the relationship between inputs and outputs of processes. That is how the blocks of a FFBD are strung together.
We create the input structures from our Summary EADS (Example Annotated Data Structures) from interviews about flow shop scheduling (and maybe others someday).
Here is such an example (in clojure EDN):

{:EADS-ref :process/flow-shop,
   :process-id "pencil-manufacturing",
   :inputs ["graphite" "clay" "water" "cedar wood" "metal" "eraser material" "paint"],
   :outputs [{:item-id "finished pencils", :quantity {:units "finished pencils", :value-string "100000"}}],
   :resources ["extruder" "kiln" "milling machine" "glue applicator" "shaping machine"],
   :duration {:units "hours", :value-string "4"},
   :subprocesses
   [{:process-id "graphite-core-production",
     :inputs ["graphite" "clay" "water"],
     :outputs [{:item-id "finished graphite rods", :quantity {:units "graphite cores", :value-string "100000"}}],
     :resources ["mixer" "extruder" "kiln"],
     :subprocesses
     [{:process-id "mix-graphite-and-clay",
       :inputs ["graphite" "clay" "water"],
       :outputs [{:item-id "graphite clay paste", :quantity {:units "liters", :value-string "100"}}],
       :resources ["mixer"],
       :duration {:units "hours", :value-string "1"},
       :subprocesses []}
      {:process-id "extrude-core",
       :inputs ["graphite clay paste"],
       :outputs [{:item-id "extruded graphite rods", :quantity {:units "extruded graphite core", :value-string "100000"}}],
       :resources ["extruder"],
       :duration {:units "minutes", :value-string "20"},
       :subprocesses []}
      {:process-id "dry-and-bake-core",
       :inputs ["extruded graphite rods"],
       :outputs [{:item-id "finished graphite rods", :quantity {:units "extruded graphite core", :value-string "100000"}}],
       :resources ["kiln"],
       :duration {:units "hours", :value-string "2"},
       :subprocesses []}]}
    {:process-id "wood-casing-production",
     :inputs ["cedar wood"],
     :outputs ["wood slats with grooves"],
     :resources ["milling machine"],
     :subprocess-flow "individuals-from-batch",
     :duration {:units "hours", :value-string "2"},
     :subprocesses
     [{:process-id "mill-wood-slats",
       :inputs ["cedar wood"],
       :outputs ["milled wood slats"],
       :resources ["milling machine"],
       :duration {:units "hours", :value-string "2"},
       :subprocess-flow :individuals-from-batch,
       :subprocesses []}
      {:process-id "cut-grooves-in-slats",
       :inputs ["milled wood slats"],
       :outputs ["wood slats with grooves"],
       :resources ["groove cutter"],
       :duration {:units "hours", :value-string "2"},
       :subprocesses []}]}
    {:process-id "assemble",
     :inputs
     [{:item-id "finished graphite rods", :from "graphite-core-production"}
      {:item-id "wood slats with grooves", :from "wood-casing-production"}
      "metal"
      "erasers"
      "paint"],
     :outputs ["finished pencil"],
     :resources ["glue applicator" "shaping machine"],
     :subprocesses
     [{:process-id "insert-core-into-slats",
       :inputs ["graphite core" "wood slats with grooves"],
       :outputs ["pencil blanks"],
       :resources ["glue applicator"],
       :subprocesses []}
      {:process-id "shape-and-paint-pencil",
       :inputs ["pencil blanks" "paint"],
       :outputs ["shaped and painted pencils"],
       :resources ["shaping machine" "painting station"],
       :subprocesses []}
      {:process-id "attach-eraser",
       :optional? true,
       :inputs ["shaped and painted pencils" "metal" "erasers"],
       :outputs ["finished pencils"],
       :resources ["crimping tool"],
       :subprocesses []}]}]}


You are joining this work with the system already running. I have a :sur-craft-beer with the above graph in it (yeah, I know that is about making pencils not beer!).
I'll be clicking the button for the graph modal and letting you know how things are going.
Clicking the graph button gives you a string that can be edn/read-string into the object.
I *think* you'd do the translation to something in GoJS on the client side, but you decide.  (I don't yet even have a clear idea how GoJS works!)

I know this is a really hard task. Feel free to ask questions!

## Update

You wrote the code ffbd.cljs and updated chat.cljs to call it when the button for the "FFBD Graph" modal is hit.
But the console continued to report "Error initializing GoJS diagram: ReferenceError: Must call super constructor in derived class before accessing 'this' or returning from derived constructor." and no graph is displayed.
I recommended that you try getting something simple on the screen first, which it looks like you implmented.
But check that that edit is complete, because we got disconnected in the middle of things.
We'll pick things up with getting a simple graph of any sort on the screen using GoJS.

## Second Update

We got disconnected again! You were in the middle of an edit at the time, so things are quite a mess. I moved the incomplete code to ffbd-orig.cljs.
I wrote something that looks about right but doesn't work at all; it is in ffbd.cljs.
I think we both are getting confused about how to write this as a React component with Helix.
I *think* you could write a simple example of using GoJS in Javascript and shadow-cljs will pick it up.
I don't know whether that's helpful. Hard to say where to go with this.

## Third Update

We got disconnected yet again! You cleaned up the mess from the last disconnect, but didn't make much progress.
Your work is in src/app/stbd_app/components/ffbd-orign.cljs and my more recent work is in src/app/stbd_app/components/ffbd.cljs

I think we might be failing because the simple GoJS examples we have both seen don't handle some nuanced aspects of React.
I am looking at https://github.com/NorthwoodsSoftware/gojs-react-basic/tree/master and see some things that we probably should be doing.
Can you take a look at that?

## Fourth Update

We got disconnected...
You were asking me whether I can see the simple text "FFBD Diagram will be displayed here..." Yes, I can.
