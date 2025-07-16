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

I think we might be failing because the simple GoJS examples we have both seen don't handle some nuanced aspects of React.
I am looking at https://github.com/NorthwoodsSoftware/gojs-react-basic/tree/master and see some things that we probably should be doing.
Can you take a look at that?

## MCP Agent Development Session - July 11, 2025

**Goal**: Replace Mermaid FFBD diagrams with GoJS implementation in `ffbd.cljs`

### Progress Made

1. **✅ Fixed ClojureScript Compilation Warnings**
   - Original 4 type inference warnings in lines 222, 224, 232, 242
   - Used `unchecked-set` and proper js-interop patterns
   - Compilation now clean with 0 warnings

2. **✅ EADS Data Processing Working**
   - `flatten-processes` function correctly transforms hierarchical EADS to flat process list
   - Data transformation from EADS to GoJS node/link format functional
   - ClojureScript evaluation tool confirmed data processing works correctly

3. **✅ React Integration Pattern Identified**
   - `init-diagram` function signature corrected for `ReactDiagram` component
   - Proper function parameter pattern established

### Critical Blocking Issue

**Error**: `"Must call super constructor in derived class before accessing 'this' or returning from derived constructor"`

**Location**: Inside GoJS library itself - specifically in `RelinkingTool` constructor during `Diagram` initialization

**Stack Trace**:
```
RelinkingTool @ go.js:12
ToolManager.initializeStandardTools @ go.js:57  
DL @ go.js:13
Diagram @ go.js:13
make @ go.js:14
stbd_app$components$ffbd$init_diagram @ ffbd.cljs:220
```

**Analysis**: This is NOT a ClojureScript code issue - it's happening inside GoJS itself during diagram creation. The error occurs with both:
- Complex setup using `go.GraphObject.make`
- Minimal setup using `new go/Diagram`

### Environment Details
- **GoJS Version**: 3.0.24
- **gojs-react Version**: 1.1.3
- **ClojureScript**: Shadow-CLJS compilation
- **React**: In Helix/React environment

### Attempts Made
1. **Constructor Parameter Fix**: Changed from `($ go/Diagram)` to `($ go/Diagram diagram-div)`
2. **Function Signature Fix**: Corrected `init-diagram` to take `diagram-div` parameter directly
3. **JavaScript Interop Cleanup**: Used proper `unchecked-set` and js-interop patterns
4. **Minimal Test**: Reduced to simplest possible `new go/Diagram` call

**Result**: Same constructor error in all cases, indicating fundamental GoJS/ClojureScript compatibility issue.

### Next Steps (1)
Testing with pure JavaScript GoJS examples from official sources to determine if this is:
- ClojureScript compilation issue
- Module loading problem  
- ES6 class transpilation issue
- GoJS version compatibility issue

May need to implement GoJS portion in pure JavaScript and interface from ClojureScript, or find alternative GoJS initialization approach.

### Next Steps (2) - JavaScript Integration Attempt

It appears to be the case Shadow-cljs facilitates easy integration of JavaScript files into ClojureScript projects such as ours. 
I would like to avoid a JavaScript implementation of anything in our production code, but the inclusion of a GoJS JavaScript demonstration for the purpose of investigating what has stymied our implementation in ffbd.cljs seems worthwhile.
Towards that goal of getting ffbd.cljs implemented I tried integrating an demonstration from the producers of GoJS. 
This can be found in two files in components/gojs/diagram.js and wrapper.js
I can't get these to work because I don't know JavaScript well enough. The error reported by Shadow-cljs is below. Perhaps you can get this to work, or at least learn in what important way it differs from what we've tried in ffbd.cljs.

Here is the error from Shadow-cljs:

[2025-07-11 13:12:26.042 - WARNING] :shadow.cljs.devtools.server.reload-classpath/update-failed - {:dir #object[java.io.File 0x3b7b0c93 "/home/pdenno/Documents/git/schedulingTBD/src/app"], :name "stbd_app/components/gojs/wrapper.js", :ext "js", :file #object[java.io.File 0x3318033e "/home/pdenno/Documents/git/schedulingTBD/src/app/stbd_app/components/gojs/wrapper.js"], :event :new}
ExceptionInfo parsed file had errors {:url #object[java.net.URL 0x453407ef "file:/home/pdenno/Documents/git/schedulingTBD/src/app/stbd_app/components/gojs/wrapper.js"], :resource-name "stbd_app/components/gojs/wrapper.js", :errors [{:message "primary expression expected", :line 6, :column 9}]}
        shadow.build.classpath/inspect-js (classpath.clj:110)
        shadow.build.classpath/inspect-js (classpath.clj:78)
        shadow.build.classpath/inspect-resource (classpath.clj:256)
        shadow.build.classpath/inspect-resource (classpath.clj:252)
        shadow.build.classpath/index-file-add (classpath.clj:955)
        shadow.build.classpath/index-file-add (classpath.clj:936)
        clojure.lang.Atom.swap (Atom.java:65)
        clojure.core/swap! (core.clj:2371)
        clojure.core/swap! (core.clj:2362)
        shadow.build.classpath/file-add (classpath.clj:1134)
        shadow.build.classpath/file-add (classpath.clj:1132)
        shadow.cljs.devtools.server.reload-classpath/update-classpath-index (reload_classpath.clj:40)
[2025-07-11 13:14:25.594 - WARNING] :shadow.cljs.devtools.server.reload-classpath/update-failed - {:dir #object[java.io.File 0x3b7b0c93 "/home/pdenno/Documents/git/schedulingTBD/src/app"], :name "stbd_app/components/gojs/diagram.js", :ext "js", :file #object[java.io.File 0x48fb5af9 "/home/pdenno/Documents/git/schedulingTBD/src/app/stbd_app/components/gojs/diagram.js"], :event :new}
ExceptionInfo parsed file had errors {:url #object[java.net.URL 0x1a43a2cf "file:/home/pdenno/Documents/git/schedulingTBD/src/app/stbd_app/components/gojs/diagram.js"], :resource-name "stbd_app/components/gojs/diagram.js", :errors [{:message "primary expression expected", :line 7, :column 9}]}
        shadow.build.classpath/inspect-js (classpath.clj:110)
        shadow.build.classpath/inspect-js (classpath.clj:78)
        shadow.build.classpath/inspect-resource (classpath.clj:256)
        shadow.build.classpath/inspect-resource (classpath.clj:252)
        shadow.build.classpath/index-file-add (classpath.clj:955)
        shadow.build.classpath/index-file-add (classpath.clj:936)
        clojure.lang.Atom.swap (Atom.java:65)
        clojure.core/swap! (core.clj:2371)
        clojure.core/swap! (core.clj:2362)
        shadow.build.classpath/file-add (classpath.clj:1134)
        shadow.build.classpath/file-add (classpath.clj:1132)
        shadow.cljs.devtools.server.reload-classpath/update-classpath-index (reload_classpath.clj:40)

### JavaScript Integration Results

**MCP Agent Findings**: The original JavaScript examples from GoJS used TypeScript syntax (interface definitions, type annotations) which Shadow-CLJS couldn't parse. After converting to plain JavaScript and then to Google Closure Compiler compatible syntax, we encountered module resolution issues with GoJS imports.

**Key Issues Discovered**:
1. **TypeScript vs JavaScript**: Official GoJS examples use TypeScript which requires conversion
2. **Module Import Compatibility**: Shadow-CLJS/Google Closure Compiler has specific requirements for module imports
3. **ES6 vs CommonJS**: Import syntax compatibility issues between modern JavaScript and Closure Compiler

**Module Resolution Error**: Even when using the suggested `goog:module$node_modules$gojs$release$go` syntax, we got "Invalid module path for resolution mode 'BROWSER'" errors.

**Conclusion**: JavaScript integration approach revealed that the problem likely isn't with our ClojureScript code per se, but with how GoJS interacts with the Shadow-CLJS/Google Closure compilation environment. The fundamental constructor error `"Must call super constructor in derived class before accessing 'this'"` appears to be a transpilation/compilation issue rather than a code logic issue.

### Alternative Approaches to Consider

1. **GoJS Version Compatibility**: Try downgrading GoJS to an earlier version that might be more compatible with Closure Compiler
2. **Different Diagram Library**: Consider alternatives like Cytoscape.js, D3.js force layouts, or react-flow
3. **Server-Side Generation**: Generate SVG diagrams on the server and display them (similar to current Mermaid approach)
4. **Iframe Approach**: Create a separate simple HTML page with pure JavaScript GoJS and embed it via iframe
5. **Alternative React Integration**: Try different React-GoJS integration approaches or libraries

**Recommendation**: Given the time invested and the fundamental compilation issues encountered, consider switching to a different diagramming library that has better ClojureScript/Shadow-CLJS compatibility, or implement server-side diagram generation.

## Cytoscape.js Success - July 16, 2025

**SOLUTION FOUND**: Successfully implemented working Cytoscape.js diagram in ClojureScript!

### Working Implementation

**File**: `src/app/stbd_app/components/cytoscape_demo.cljs`

**Key Success Factors**:

1. **Correct Import Pattern**:
   ```clojure
   ["cytoscape" :as cytoscape-lib]
   ```

2. **Direct Function Call** (not `.default`):
   ```clojure
   (cytoscape-lib (clj->js config-object))
   ```

3. **Proper React Integration**:
   - Use `hooks/use-ref` for DOM container
   - Use `hooks/use-effect` with `[open]` dependency
   - Call Cytoscape after Dialog opens with setTimeout for DOM readiness

### Working Features

- ✅ **Interactive Nodes**: Nodes can be repositioned by dragging
- ✅ **Dynamic Edges**: Edges follow node movements automatically  
- ✅ **Responsive Layout**: Grid layout positions nodes automatically
- ✅ **ClojureScript Compatible**: No compilation errors or JavaScript interop issues

### Technical Details

**Data Format**:
```clojure
:elements [{:data {:id "one" :label "Node 1"}}
           {:data {:id "two" :label "Node 2"}} 
           {:data {:source "one" :target "two" :label "Edge 1-2"}}]
```

**Styling**:
```clojure
:style [{:selector "node"
         :style {:background-color "#666" :label "data(label)" :width 60 :height 30}}
        {:selector "edge" 
         :style {:width 3 :line-color "#ccc" :target-arrow-color "#ccc" :target-arrow-shape "triangle"}}]
```

**Layout**: Uses `{:name "grid"}` for automatic positioning

### Next Steps for FFBD Implementation

1. **Replace ffbd.cljs GoJS code** with Cytoscape.js pattern
2. **Transform EADS data** to Cytoscape elements format  
3. **Implement FFBD-specific styling** (process boxes, flow arrows)
4. **Add interactive validation features** (click to validate subprocess details)
[2025-07-11 13:14:44.606 - WARNING] :shadow.cljs.devtools.server.reload-classpath/update-failed - {:dir #object[java.io.File 0x3b7b0c93 "/home/pdenno/Documents/git/schedulingTBD/src/app"], :name "stbd_app/components/gojs/diagram.js", :ext "js", :file #object[java.io.File 0xe561f0d "/home/pdenno/Documents/git/schedulingTBD/src/app/stbd_app/components/gojs/diagram.js"], :event :new}
ExceptionInfo parsed file had errors {:url #object[java.net.URL 0x6bb87a57 "file:/home/pdenno/Documents/git/schedulingTBD/src/app/stbd_app/components/gojs/diagram.js"], :resource-name "stbd_app/components/gojs/diagram.js", :errors [{:message "primary expression expected", :line 7, :column 9}]}
        shadow.build.classpath/inspect-js (classpath.clj:110)
        shadow.build.classpath/inspect-js (classpath.clj:78)
        shadow.build.classpath/inspect-resource (classpath.clj:256)
        shadow.build.classpath/inspect-resource (classpath.clj:252)
        shadow.build.classpath/index-file-add (classpath.clj:955)
        shadow.build.classpath/index-file-add (classpath.clj:936)
        clojure.lang.Atom.swap (Atom.java:65)
        clojure.core/swap! (core.clj:2371)
        clojure.core/swap! (core.clj:2362)
        shadow.build.classpath/file-add (classpath.clj:1134)
        shadow.build.classpath/file-add (classpath.clj:1132)
        shadow.cljs.devtools.server.reload-classpath/update-classpath-index (reload_classpath.clj:40)

