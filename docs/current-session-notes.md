# Current Coding Session Notes

**Date:** June 24, 2025  
**Session Start:** 11:17 UTC

## ⚠️ CRITICAL: Agent Session Limitations

**IMPORTANT**: AI agent sessions (like this one) typically crash or disconnect after approximately **10-20 minutes** of active work. This is a known limitation that affects all MCP-based coding sessions.

### Required Backup Strategy
- **Document progress frequently** in this file or `docs/` directory
- **Commit code changes** to git regularly (every 5-10 minutes)
- **Save important findings** to documentation before session ends
- **Note current working state** so next session can continue seamlessly

### Session Recovery Protocol
1. Start new session with `(start)` and `(ns-setup!)`
2. Review this documentation file for previous progress
3. Check git log for recent changes
4. Continue from documented stopping point

### Why This Matters
- Prevents loss of work when sessions unexpectedly terminate
- Enables smooth handoffs between coding sessions
- Maintains project continuity despite technical limitations
- Reduces frustration from having to restart complex debugging or development work

---

## System Status
- ✅ System started successfully using `(start)` in REPL
- ✅ Server running on port 3301 (nREPL session mode)
- ✅ Namespace aliases set up with `(ns-setup!)`
- ✅ Recent activity shows craft beer brewery flow-shop analysis in progress

## Completed Work
- The major LLM mocking system project was completed on June 20, 2025
- Full project-based mocking infrastructure implemented in `scheduling-tbd.mock` namespace
- Unit and integration tests passing (9+5 assertions)
- Mock system uses pre-recorded conversation data from `data/projects/*.edn` files

## Current System Activity
- Working on flow-shop process analysis for craft beer brewery
- Equipment availability and maintenance scheduling being analyzed
- ORM-based data structures being built for:
  - Equipment types and status
  - Task scheduling and dates
  - Maintenance schedules
- Orchestrator (ork manager) continuing with `:process` conversation

## Available Projects
- 40+ project files in `data/projects/` including various manufacturing scenarios
- Recent focus on craft beer brewing with multiple iterations
- Projects range from simple (key blanks) to complex (ice cream, carpet manufacturing)

## Current Session Work

### ✅ FIXED: Duplicate Chat Bubbles Issue
- **Problem**: Messages appearing on both sides of conversation in web client  
- **Root Cause**: Two separate `ws/send-to-client` calls for the same surrogate responses:
  1. `chat-pair-interviewees` function immediately sent `:sur-says` via WebSocket
  2. `q-and-a` function later iterated through `msgs-vec` and sent each message again
- **Solution**: Removed duplicate `ws/send-to-client` call from `chat-pair-interviewees`
- **Result**: Messages should now appear only once, on the correct side of conversation
- **Files Modified**: `src/server/scheduling_tbd/iviewr/interviewers.clj` (lines 109-110)

### Fixed `run-demo!` Client ID Issue ✅
- **Problem**: `run-demo!` function was failing with client-id validation error
- **Root Cause**: No websocket clients connected, so `(ws/recent-client!)` returned `nil`
- **Solution**: Modified function to use `(or (ws/recent-client!) :console)` as fallback
- **Result**: Demo now runs successfully from REPL using `:console` client-id

### Demo Execution Results ✅
- Successfully loaded and processed `sur-craft-beer` project with 28 messages
- Mocking system working correctly
- Processed craft beer production workflow with HTML tables
- Interview progressed through multiple EADS stages:
  - `:process/warm-up-with-challenges` 
  - `:process/scheduling-problem-type`
  - `:process/flow-shop`
- Generated comprehensive production step data with durations and resources
- Conversation completed with "exhausted" status (expected behavior)

### No `separate-table` Issues Observed
- The demo ran without getting stuck in `inv/separate-table`
- HTML table processing completed successfully
- Tables parsed correctly from embedded HTML in messages

## Next Steps
Ready to assist with:
1. System debugging and improvements
2. New feature development
3. Code analysis and refactoring
4. Testing and validation
5. Documentation updates

## Key Files to Monitor
- `logs/agents-log.edn` - Current system activity (376KB, actively growing)
- `docs/issues/current-task.md` - Current project status
- System DB and conversation states via REPL

---
*Last updated: June 24, 2025 at 12:28 UTC*  
*REMINDER: Agent sessions crash after 10-20 minutes - document progress frequently!*
