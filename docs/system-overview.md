# SchedulingTBD System Overview

## Introduction

SchedulingTBD is an experimental software platform designed to study **human/machine teaming (HMT)** in long-running cognitive tasks, specifically focusing on collaborative formulation and refinement of manufacturing production scheduling systems. The project explores how humans and AI can work together to create solutions using domain-specific languages (DSLs), particularly [MiniZinc](https://www.minizinc.org/) for constraint programming. The system is a  multi-agent AI framework designed to automatically understand scheduling problems and generate MiniZinc-based solutions. The system uses a structured interview process with AI agents to elicit requirements, classify problems, and model data relationships. It serves as a web-based application running on Jetty at port 3300 with real-time WebSocket communication and comprehensive logging.

## Project Goals

This research aims to establish consensus and eventual standardization on best practices for:

- **AI-mediated systems integration**
- **Human/AI teaming on long-running and complex endeavors.**
- **Designing DSLs fit for human/AI teaming**
- **Running AI agent-based interviews** to elicit requirements
- **Plan recognition** in collaborative tasks
- **Constructionist learning** for adults
- **Scientific explanation** in HMT

## How the System Works

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

## Technical Implementation

### Technologies Used
- **Clojure** for the core system with functional programming paradigms
- **OpenAI GPT models** for AI agent intelligence
- **MiniZinc** for constraint satisfaction solving
- **Object Role Modeling (ORM)** for data modeling
- **Mount** for component lifecycle management
- **Jetty** web server for HTTP/WebSocket services
- **Telemere** for comprehensive logging and telemetry

### Agent Communication
- Message-based communication between agents
- Structured message types (CONVERSATION-HISTORY, SUPPLY-QUESTION, etc.)
- Comprehensive logging for transparency and debugging
- Real-time WebSocket updates for interactive sessions

### Data Structures
- Hierarchical EADS templates guide data collection
- Stripped data structures for efficient processing
- Example data generation for validation

## Logging and Monitoring

### Comprehensive Logging System
- **Telemere Integration**: Streaming logs and telemetry data
- **Agent Activity Logs**: Detailed entries in `logs/agents-log.txt` (can grow to 400k+ lines)
- **Real-time Monitoring**: Track interview progress, agent decisions, and data collection
- **Historical Analysis**: Complete audit trail of system operations

### Workflow Tracking
- Interview manager communications
- ORK manager decision points
- Agent response patterns
- EADS template progression
- Data structure refinements

- **Data Persistence**: Structured storage of interview results and models

## Current Operational Status

- **Server Configuration**: Runs on Jetty with port determined by profile:
  - **Port 3301**: nREPL sessions (MCP agent development)
  - **Port 3300**: Regular sessions (`:dev`, `:prod`, `:test` profiles)
  - Configuration located in `resources/system.edn`
- OpenAI models actively used for agent processing
- Dynamic HTTP handler route updates
- Real-time conversation management
- Comprehensive agent activity logging

## Development Workflow

 - You start the system using the clojure form (start) in a repl in the user namespace.
 - We use consistent naming for namespace aliases in conversation with MCP-based code pilots. To establish these aliases, use (ns-setup!) after starting.
 - See env/dev/develop/repl.clj to see the implementation of ns-setup! and the correspondence between aliases and namespaces.
 - If the system get hosed, you can often fix things with (restart). If aliases get messed up (a current problem) you can do (undo-ns-setup!) and then (ns-setup!) again.
 - There are concepts for which we always use the same variable names: pid for project-id (a keyword); cid for conversation ID, one of #{:process, :data, :resources, :optimality}.

### Current Status
- ✅ Multi-agent interview system operational
- ✅ MiniZinc integration for simple process modeling
- ✅ Web-based chat interface with real-time updates
- ✅ Surrogate agent testing framework
- 🔄 Working toward complete MiniZinc solution generation
- 🔄 Testing with hundreds of surrogate domain experts
- ⏳ Planning human expert validation studies

## Dependencies and Integration

### External Systems
- **OpenAI API** - For LLM capabilities
- **MiniZinc** - Constraint programming solver
- **WebSocket infrastructure** - For real-time communication

### Data Persistence
- **Datahike** database for conversation history
- **EDN files** for project configurations
- **File system** for generated artifacts


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

## Some Guidelines for AI MCP-based Coding Copilots

1. Preserve exact formatting and whitespace when making any edits.
2. Use the MCP Clojure editing tools for precise, surgical changes.
3. Only modify what needs to be changed without reformatting entire sections.
4. Allow sufficient time (30+ seconds) for LLM agent calls as noted in the documentation.

In order to observe system activity, you can watch for logging at the console (REPL), and also entries made in logs/agent-logs.txt."

---

*This overview is based on observation of the system running the music school example, analysis of the codebase structure, and examination of the comprehensive agent logging system.*
