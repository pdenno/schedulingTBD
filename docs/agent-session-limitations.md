# Agent Session Limitations and Backup Strategy

## ⚠️ Critical Session Limitation

**IMPORTANT**: AI agent sessions (MCP-based coding assistants) typically **crash or disconnect after approximately 10-20 minutes** of active work. This is a known technical limitation that affects all sessions with AI coding agents in this environment.

## Impact on Development

### What Happens
- Agent sessions terminate unexpectedly without warning
- All session context and conversation history is lost
- Any unsaved work or uncommitted code changes may be lost
- Development workflow is interrupted

### When It Occurs
- After approximately 10-20 minutes of active coding work
- Regardless of task complexity or completion status
- Cannot be prevented through user actions
- Affects all types of development activities (coding, debugging, testing, documentation)

## Required Backup Strategy

### 1. Frequent Documentation
- **Update `docs/current-session-notes.md`** every 5-10 minutes
- **Document current progress** and findings immediately
- **Note the exact stopping point** and next steps
- **Record any important insights or discoveries**

### 2. Regular Git Commits
```bash
# Commit changes every 5-10 minutes
git add .
git commit -m "WIP: [brief description of current work]"
```

### 3. State Documentation
- **Current file being worked on**
- **Function or section being modified**
- **Test results or error messages**
- **REPL state if relevant** (what's loaded, current namespace)

### 4. Progress Tracking
- **Tasks completed** in current session
- **Issues encountered** and solutions attempted
- **Next logical steps** to continue work
- **Files that need attention**

## Session Recovery Protocol

### When Starting a New Session
1. **Start the system**: `(start)` in REPL
2. **Set up aliases**: `(ns-setup!)`
3. **Read session notes**: Check `docs/current-session-notes.md`
4. **Review git log**: `git log --oneline -10` to see recent changes
5. **Continue from documented stopping point**

### Recovery Checklist
- [ ] System started and running
- [ ] Previous session notes reviewed
- [ ] Git history checked for recent changes
- [ ] Current task and context understood
- [ ] Ready to continue from documented stopping point

## Best Practices

### Time Management
- **Set 5-minute reminders** to document progress
- **Commit code every 10 minutes** maximum
- **Assume session will end** after 15 minutes of work
- **Prioritize critical tasks** early in session

### Documentation Habits
- **Write as you work**, not at the end
- **Be specific** about current state and next steps
- **Include error messages** and debugging info
- **Note file paths** and line numbers for context

### Code Management
- **Small, frequent commits** rather than large changes
- **Descriptive commit messages** for easy recovery
- **Test code immediately** after writing
- **Document any REPL experiments** that work

## Why This Limitation Exists

This appears to be a technical constraint of the MCP (Model Context Protocol) environment used for AI agent interactions. The limitation is:
- **Not user-controllable**: Cannot be disabled or extended
- **Not task-dependent**: Affects all types of work equally
- **Not performance-related**: Happens regardless of system load
- **Consistent**: Occurs reliably after 10-20 minutes

## Mitigation Strategies

### Short Session Approach
- **Plan 15-minute work segments**
- **Complete discrete tasks** within timeframe
- **Document thoroughly** before each session ends
- **Use multiple short sessions** rather than long ones

### Handoff Documentation
- **Detailed session notes** for continuity
- **Clear next steps** for immediate continuation
- **Context preservation** through documentation
- **State snapshots** in comments or docs

## File Locations for Documentation

### Primary Documentation
- `docs/current-session-notes.md` - Active session tracking
- `docs/issues/current-task.md` - Current project focus
- Git commit messages - Code change tracking

### Backup Documentation
- Individual files in `docs/` directory
- Comments in code files
- Test files with documented examples
- Log files when relevant

---

## Quick Reference

**Every 5 minutes:**
- Document current progress
- Note current file and function
- Record any important findings

**Every 10 minutes:**
- Commit code changes to git
- Update session notes with detailed status
- Record next steps clearly

**Before complex operations:**
- Document current state thoroughly
- Commit any working code
- Note the specific operation being attempted

**When session ends:**
- Final commit with current state
- Complete session notes update
- Clear next steps for continuation

---

*This limitation significantly impacts development workflow but can be managed effectively through disciplined documentation and backup practices.*

**Last updated:** June 24, 2025
**Status:** Active limitation - affects all MCP agent sessions