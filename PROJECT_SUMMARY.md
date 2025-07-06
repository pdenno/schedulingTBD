# SchedulingTBD Project Summary

## Introduction & Purpose

SchedulingTBD is an experimental software platform designed to study **human/machine teaming (HMT)** in long-running cognitive tasks, specifically focusing on collaborative formulation and refinement of manufacturing production scheduling systems. The project explores how humans and AI can work together to create solutions using domain-specific languages (DSLs), particularly [MiniZinc](https://www.minizinc.org/) for constraint programming. The system is a multi-agent AI framework designed to automatically understand scheduling problems and generate MiniZinc-based solutions. The system uses a structured interview process with AI agents to elicit requirements, classify problems, and model data relationships. It serves as a web-based application running on Jetty at port 3300 with real-time WebSocket communication and comprehensive logging.

## For AI programming agents

The code is implemented in Clojure and you (AI programming agent) joint the action with a REPL already running. No need to start anything!
We run clojure-mcp (an MCP server) in which you can check that everything is running by accessing a variable scheduling-tbd.iviewr.interviewers/can-you-see-this? using the MCP server's clojure_eval tool.
If you also run (ns-setup!) in the user namespace, you can access that variable as inv/can-you-see-this? .
The value returned is the string "Yes, you can!".

## How the System Works (Orchestrator and Interviewers)

An orchestrator agent assesses the current level of discussion that has occurred through messages that pull data from the project database.
It then chooses an EADS instruction accordingly for use by a corresponding interview agent.
(Four kinds of interview agents exist: proccess, data, resources, and optimality).
The selected interview agent uses the EADS instructions to formulate questions for human experts or surrogate (LLM-based) expert.
The selected interview agent uses the answers to its questions to build a data structure conforming to the EADS instructions.
Code associated with each EADS instruction determines how to build a 'summary data structure' from the commits the interviewer makes.
Determining when to end the interview segment associated with the EADS is made by three means
  - (1) code associated with each EADS instruction determines the completeness,
  - (2) the interviewer agent itself decides that it has covered the topic adequately, or
  - (3) the budget for the asking questions associated with the EADS instructions is depleted.

### Generated Artifacts
- **MiniZinc models** - Constraint programming specifications
- **Process diagrams** - Mermaid-based visualizations
- **Summary data structures** - Formalized representations of requirements derived from the EADS templates chosen by the orchestrator.

### Conversations
Structured interview conversations with four main topics:
1. **Process** - Understanding the manufacturing workflow
2. **Data** - Identifying data sources and requirements
3. **Resources** - Cataloging available resources (people, machines)
4. **Optimality** - Defining what constitutes a "good" schedule

## Key Technologies & Namespaces

### Technologies Used
- **Clojure** for the core system with functional programming paradigms
- **OpenAI GPT models** for AI agent intelligence
- **MiniZinc** for constraint satisfaction solving
- **Object Role Modeling (ORM)** for data modeling
- **Mount** for component lifecycle management
- **Jetty** web server for HTTP/WebSocket services
- **Telemere** for comprehensive logging and telemetry
- **Helix** a means of creating React component in javascript.

### Agent Communication
- Message-based communication between agents
- Structured message types (CONVERSATION-HISTORY, SUPPLY-QUESTION, etc.)
- Comprehensive logging for transparency and debugging
- Real-time WebSocket updates for interactive sessions

### Data Structures
- Hierarchical EADS templates guide data collection
- Stripped data structures for efficient processing
- Example data generation for validation

## Where to look for Logs & Data

### Logging System
- **Console/REPL**: Real-time system activity and debugging output
- **`logs/agents-log.txt`**: Detailed agent activity logs (can grow to 400k+ lines)
- **Telemere Integration**: Streaming logs and telemetry data for comprehensive monitoring

### Data Storage
- **Datahike Database**: Conversation history and interview results
- **`data/projects/`**: Manufacturing scenario data and project configurations
- **`data/examples/`**: MiniZinc examples and generated models
- **`data/templates/`**: MiniZinc solution templates
- **EDN files**: Project configurations and structured data
- **File system**: Generated artifacts (models, diagrams, etc.)

### Monitoring & Analysis
- **Real-time tracking**: Interview progress, agent decisions, and data collection
- **Historical analysis**: Complete audit trail of system operations
- **Workflow tracking**: Interview manager communications, ORK manager decisions, agent response patterns

## Starting / Restarting the System (REPL Instructions)

 - You start the system using the clojure form `(start)` in a REPL in the user namespace.
 - We use consistent naming for namespace aliases in conversation with MCP-based code pilots. To establish these aliases, use `(ns-setup!)` after starting.
 - See `env/dev/develop/repl.clj` to see the implementation of `ns-setup!` and the correspondence between aliases and namespaces.
 - If the system gets hosed, you can often fix things with `(restart)`. If aliases get messed up (a current problem) you can do `(undo-ns-setup!)` and then `(ns-setup!)` again.
 - There are concepts for which we always use the same variable names: `pid` for project-id (a keyword); `cid` for conversation ID, one of `#{:process, :data, :resources, :optimality}`.

## Current Operational Status

- ✅ Multi-agent interview system operational
- ✅ MiniZinc integration for simple process modeling
- ✅ Web-based chat interface with real-time updates
- ✅ Surrogate agent testing framework
- 🔄 Working toward complete MiniZinc solution generation
- 🔄 Testing with hundreds of surrogate domain experts
- ⏳ Planning human expert validation studies
- Server runs on Jetty at port 3301 when running with an AI programming agent and clojure-mcp.
- OpenAI models actively used for agent processing
- Dynamic HTTP handler route updates
- Real-time conversation management
- Comprehensive agent activity logging

## Project Structure

```
schedulingTBD/
├── src/
│   ├── server/          # Clojure backend
│   └── app/             # ClojureScript frontend
├── data/
│   ├── projects/        # Manufacturing scenario data
│   ├── examples/        # MiniZinc examples
│   └── templates/       # Minizinc solution templates
├── resources/
│   ├── agents/          # Agent configuration
│   └── public/          # Static web assets
├── test/                # Test suites
├── env/                 # Environment-specific config
├── doc/                 # Documentation and screenshots
└── bin/                 # Utility scripts
```

## Future Extensions

The system architecture supports:
- Additional problem types and EADS templates
- New solution generation backends (beyond MiniZinc)
- Integration with real scheduling systems
- Multi-modal input (spreadsheets, databases, etc.)
- Advanced optimization objectives and constraints
- Enhanced web interface features
- Additional AI model integrations

---

*This summary is based on observation of the system running the music school example, analysis of the codebase structure, and examination of the comprehensive agent logging system.*
