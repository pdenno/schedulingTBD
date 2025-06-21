# CLAUDE.md - AI Coding Copilot Instructions

Essential operating instructions for any LLM agent (Claude, GPT, etc.) working on this Clojure project.

## Quick Start

### System Operations
- **Start system**: `(start)` in REPL (user namespace)
- **Restart system**: `(restart)` in REPL
- **Setup namespace**: Use `ns-setup!` for namespace initialization
- **View logs**: Check `./logs/agents-logs.edn` for system activity

The REPL starts in the user namespace where `(start)` and `(restart)` are available.
Use the clojure-MCP tool `clojure_eval` to interact with the system.

## Standard Terminology

- **pid**: Project ID (always a Clojure keyword, e.g. `:project-alpha`)
- **cid**: Conversation ID (keyword, one of `:process`, `:data`, `:resources`, `:optimality`)

## Coding Rules

### Naming Conventions
- **Boolean variables**: End with `?` (e.g. `inv/active?`, `mock?`)
- **Mutating functions**: End with `!` (e.g. `update-db!`, `reset-state!`)
- **Diagnostic definitions**: Tag with `^:diag` metadata for REPL-only usage

### Data Management
- **NEVER use dynamic variables** - there is almost never a case where they're needed
- **Prefer atoms** for persistent state instead of dynamic vars
  ```clojure
  ;; Good
  (def mock? (atom false))
  
  ;; Avoid
  (def ^:dynamic *mock-enabled* false)
  ```

### Data Structures
- Use maps, vectors, sets, and primitives to store data
- **Do NOT use lists** (sequences) for data storage
- Recursive code should check with `map?` and `vector?`

## Editing Etiquette

### Surgical Changes Only
- Make **minimal, targeted edits** - don't reformat entire functions
- **Preserve existing spacing and indentation**
- **Respect whitespace patterns** in conditional forms:
  ```clojure
  ;; Preserve this spacing pattern
  (cond
      (test-1)                      1
      (much-longer-test)           2)
  
  ;; Don't collapse to single spaces
  ```

### Code Review Standards
- Maintain existing code style and patterns
- Don't introduce unnecessary formatting changes
- Focus edits on the specific functionality being modified

## Important Files to Understand

- `db.clj` - DB maintenance for projects and system-db
- `agent_db.clj` - Agent querying interface
- `interviewers.clj` - Interview execution
- `ork.clj` - EADS instruction selection for interviews

## Agent Session Management

**Best Practice**: Document significant progress and findings as you work:
- Update `docs/issues/current-task.md` (symlink to current work) for major milestones
- Note next steps and dependencies for complex multi-step tasks
- Save intermediate findings or analysis for future reference

## Additional Resources

- **Code Style Details**: See `LLM_CODE_STYLE.md` for comprehensive coding guidelines
- **System Overview**: Check `docs/system-overview.md` for architecture details
- **Daily Logs**: Monitor `./logs/agents-log.edn` for system activity

## Quick Reference

```clojure
;; Start system
(start)

;; Check system status and explore
(clj-mcp.repl-tools/list-ns)                    ; List namespaces
(clj-mcp.repl-tools/list-vars 'some.namespace)  ; List functions
(clj-mcp.repl-tools/doc-symbol 'function-name)  ; Get documentation

;; Common patterns
(def active? (atom true))           ; Boolean atom
(defn update-state! [new-state] ..) ; Mutating function
(def ^:diag debug-helper ..)        ; Diagnostic helper
```

---
*Keep this file under 120 lines for quick loading. Last updated: $(date)*
