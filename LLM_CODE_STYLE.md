# LLM Code Style Guide for SchedulingTBD

This document outlines the Clojure coding conventions and style guidelines specific to the SchedulingTBD project. These conventions help maintain consistency and readability when AI coding assistants and human developers work on the codebase.

## 1. Namespace Alias Expectations and ns-setup! Macro

### Standard Namespace Aliases

Use consistent aliases throughout the project for common namespaces:

```clojure
(ns my-namespace
  (:require
   [clojure.core :as c]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [datahike.api :as d]
   [datahike.pull-api :as dp]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.db :as db]
   [scheduling-tbd.sutil :as sutil]
   [scheduling-tbd.util :as util]
   [taoensso.telemere :as tel :refer [log!]]))
```

### Using ns-setup! Macro

After starting the system with `(start)`, always establish standard namespace aliases:

```clojure
;; In the REPL after (start)
(ns-setup!)

;; If aliases become corrupted, reset them:
(undo-ns-setup!)
(ns-setup!)
```

The `ns-setup!` macro is defined in `env/dev/develop/repl.clj` and establishes consistent alias mappings for MCP-based code development.

## 2. Data Structures: Maps, Vectors, Sets Only

**Rule**: Use only maps, vectors, sets, and primitive types for data storage. **Do not use lists for storage**.

### ✅ Correct Usage

```clojure
;; Use vectors for ordered collections
(def processing-steps [:prepare :mix :bake :cool])

;; Use maps for structured data
(def project-info {:pid :craft-beer
                   :name "Craft Beer Brewery"
                   :conversations #{:process :data :resources}})

;; Use sets for unique collections
(def conversation-types #{:process :data :resources :optimality})

;; Recursive processing checks for these types only
(defn process-data [data]
  (cond
    (map? data)    (process-map data)
    (vector? data) (process-vector data)
    :else          data))
```

### ❌ Avoid

```clojure
;; Don't use lists for storage
(def processing-steps '(:prepare :mix :bake :cool))  ; Wrong!
```

## 3. Boolean, Mutating, and Diagnostic Naming Conventions

### Boolean Variables

Boolean variables and functions should end with `?`:

```clojure
(def mock? false)
(def active? true)
(def keep-db? (atom false))

(defn agent-exists? [agent-id] ...)
(defn conversation-exists? [pid cid] ...)
(defn system-agent-id? [agent-id] ...)
```

### Mutating Functions

Functions that change a database or have side effects should end with `!`:

```clojure
(defn put-agent! [agent-id agent-map] ...)
(defn add-claim! [pid claim] ...)
(defn set-execution-status! [{:keys [pid status]}] ...)
(defn recreate-dbs! [] ...)
(defn ns-setup! [] ...)
```

### Diagnostic Functions

Tag definitions used for REPL diagnostics but not part of the main system with `^:diag` metadata:

```clojure
(def ^:diag debug-info
  "Used for REPL exploration only"
  {:current-state @system-state
   :active-conversations @active-convos})

(def ^:diag agent-infos
  "REPL helper for checking agent states"
  (atom {}))
```

## 4. Atom vs Dynamic Var Rule

**Rule**: Use atoms for persistent state. Avoid dynamic variables in almost all cases.

### ✅ Correct: Use Atoms

```clojure
;; For application state
(def mock? (atom false))

;; For configuration flags
(def keep-db? (atom false))

;; For mutable state tracking
(def mocking-state
  (atom {:conversation-pos 0
         :ork-EADS nil
         :script []}))

;; Example usage
(when @mock?
  (reset! mocking-state {:conversation-pos 0 :script new-script}))
```

### ❌ Avoid: Dynamic Variables

```clojure
;; Wrong approach - don't use dynamic vars
(def ^:dynamic *mock-enabled*
  (some? (System/getenv "STBD_MOCK_LLM")))

;; Correct approach - use atom
(def mock? (atom (some? (System/getenv "STBD_MOCK_LLM"))))
```

**Exception**: Dynamic variables are acceptable only for thread-local context that must be temporarily rebound, but these cases are extremely rare.

## 5. Indentation & Spacing Examples

### Cond Alignment Rule

**Important**: Preserve the alignment in `cond` expressions. Do NOT collapse spacing to single spaces:

```clojure
;; ✅ Correct - preserve alignment
(cond
  (test-1)           1
  (much-longer-test) 2
  (another-test)     3
  :else              4)

;; ❌ Wrong - don't collapse to single spaces
(cond
  (test-1) 1
  (much-longer-test) 2
  (another-test) 3
  :else 4)
```

### Function Definition Formatting

```clojure
;; Multi-arity functions
(defn process-data
  ([data]
   (process-data data {}))
  ([data options]
   (merge default-options options)
   (transform data options)))

;; Complex parameter destructuring
(defn add-msg 
  [{:keys [pid cid from full-text table tags question-type pursuing-EADS]}]
  (let [mid (inc (max-msg-id pid cid))
        msg {:message/id mid
             :message/from from
             :message/time (now)}]
    (d/transact! conn [msg])))
```

### Let Binding Alignment

```clojure
(let [conn-atm     (sutil/connect-atm pid)
      script       (->> (d/q query @conn-atm)
                        (dp/pull-many @conn-atm '[*])
                        (sort-by :message/time))
      processed    (strip-ns script)]
  (process-script processed))
```

## 6. File Organization Hints

Organize code by functional responsibility using these file naming conventions:

### Core Database Operations
- **`db.clj`** - System and project database schemas, database initialization, core CRUD operations
- **`agent_db.clj`** - Agent-specific queries and database operations

### Business Logic
- **`interviewers.clj`** - Main interview management and execution logic
- **`ork.clj`** - Orchestrator logic for choosing EADS instructions
- **`surrogate.clj`** - Surrogate agent management for testing
- **`system_agents.clj`** - System-level agent coordination

### Utilities and Support
- **`sutil.clj`** - System utilities (database connections, configuration)
- **`util.clj`** - General utility functions
- **`specs.clj`** - Clojure.spec definitions
- **`mock.clj`** - Testing and mocking infrastructure

### Domain-Specific
- **`minizinc.clj`** - MiniZinc integration and constraint modeling
- **`llm.clj`** - LLM provider integration (OpenAI, etc.)
- **`how_made.clj`** - "How It's Made" data processing

### Web Interface
- **`web/handler.clj`** - HTTP route handling
- **`web/websockets.clj`** - WebSocket communication
- **`web/controllers/respond.clj`** - Response formatting

## 7. Testing Guidance

### Mock System Usage

Use the project's built-in mock system for testing:

```clojure
;; Enable mocking via environment variable
;; Set STBD_MOCK_LLM=true in your environment

;; In your namespace
(ns my-test-namespace
  (:require
   [scheduling-tbd.mock :as mock]))

;; Check if mocking is enabled
(when (some? (System/getenv "STBD_MOCK_LLM"))
  (reset! mock/mock? true))

;; Use mocking in tests
(deftest test-with-mocking
  (when @mock/mock?
    (mock/run-mocking :test-project)
    (is (= expected-result (my-function test-input)))))
```

### Mock System Components

The mock system provides:
- **`scheduling_tbd.mock`** - Core mocking infrastructure
- **Environment variable**: `STBD_MOCK_LLM` - Set to enable mocking
- **Mock state management**: `mocking?` and `mocking-state` atoms
- **Script-based conversation replay** for deterministic testing

### Test Structure

```clojure
(ns my-namespace-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [scheduling-tbd.mock :as mock]
   [scheduling-tbd.db :as db]))

(deftest my-test
  (testing "with mock system"
    (when (some? (System/getenv "STBD_MOCK_LLM"))
      (is (true? @mock/mock?)))
    (testing "database operations"
      (is (db/project-exists? :test-project)))))
```

## 8. Commit Etiquette

### One Namespace Per Commit

When possible, commit changes to one namespace at a time:

```bash
# Good - focused commit
git add src/server/scheduling_tbd/db.clj
git commit -m "db: Add new project validation functions

- Add project-exists? predicate
- Implement get-project-name utility
- Update schema for project metadata"

# Good - related namespace changes
git add src/server/scheduling_tbd/agent_db.clj
git add src/server/scheduling_tbd/llm.clj  
git commit -m "agent_db,llm: Integrate mock system for testing

- Update agent queries to use mock responses
- Add STBD_MOCK_LLM environment variable support"
```

### Preserve Formatting

**Critical**: Always preserve existing formatting and whitespace:

- Use MCP Clojure editing tools for precise, surgical changes
- Only modify what needs to be changed without reformatting entire sections
- Maintain alignment in `cond` expressions and let bindings
- Preserve comment formatting and spacing

### Commit Message Format

```
namespace(s): Brief description of change

- Bullet point for major changes
- Include rationale for complex changes
- Reference issue numbers when applicable

Closes #123
```

## Project-Specific Concepts

### Standard Variable Names

Always use these variable names for core concepts:

- **`pid`** - Project ID (always a keyword, e.g., `:craft-beer`)
- **`cid`** - Conversation ID (one of `#{:process :data :resources :optimality}`)
- **`aid`** - Agent ID
- **`tid`** - Thread ID
- **`eads-id`** - EADS instruction identifier

### Example Usage

```clojure
(defn start-conversation [pid cid]
  (let [agent-id (get-conversation-agent cid)
        thread-id (ensure-thread agent-id pid)]
    (put-conversation-status! pid cid :active)
    {:pid pid :cid cid :agent-id agent-id :thread-id thread-id}))
```

---

*This style guide should be followed by all AI coding assistants and human developers working on the SchedulingTBD project. When in doubt, examine existing code patterns in the core files (`db.clj`, `agent_db.clj`, `interviewers.clj`, `ork.clj`) for guidance.*
