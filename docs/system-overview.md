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

## Key Functionalities

### WebSocket Operations
- **`:start-conversation`**: Initiates a new conversation context
- **`:resume-conversation`**: Manages ongoing conversation states
- **`:load-proj`**: Loads and initializes project-related conversations
- **`:domain-expert-says`**: Handles domain expert responses
- **`:ask-llm`**: Facilitates interactions with language models for advanced insights

### Intelligent Orchestration
- The ORK manager makes dynamic decisions about which questions to ask next
- Budget management ensures efficient use of LLM resources
- Automatic transitions between different interview phases

### Structured Knowledge Capture
- EADS templates ensure consistent data collection
- ORM modeling creates precise data relationships
- Example data generation for testing and validation

### Domain Flexibility
- Surrogate agents can represent any scheduling domain
- Generic framework adapts to different problem types
- Extensible EADS templates for new domains

## Example: Music School Scheduling

When running `(orkt/music-school)`, the system:

1. **Creates a surrogate** representing a music school operator
2. **Classifies the problem** as a timetabling problem (not cyclical, not continuous)
3. **Gathers timetabling details**:
   - Individual student lessons (30/45/60 minutes)
   - Instructor room reservations
   - Room equipment requirements (pianos, drums, soundproofing)
   - Time preferences (3PM-9PM, Monday-Saturday)
   - Priority rules (student lessons > instructor reservations)
4. **Models the data**:
   - Student information (name, instrument, skill level, availability)
   - Instructor information (name, instruments taught, preferred blocks)
   - Room details (equipment, capacity, maintenance schedules)
   - Lesson schedules and constraints

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

## Benefits

1. **Automated Requirements Elicitation**: No manual analysis needed
2. **Domain Agnostic**: Works across different scheduling domains
3. **Structured Approach**: Ensures comprehensive problem understanding
4. **Executable Solutions**: Generates working MiniZinc models
5. **Transparent Process**: Detailed logging shows decision-making
6. **Iterative Refinement**: Agents can ask follow-up questions as needed
7. **Real-time Interaction**: WebSocket support for interactive sessions
8. **Scalable Architecture**: Multi-agent system handles complex workflows

## Integration Points

- **Machine Learning Models**: OpenAI integration for intelligent agent behavior
- **Constraint Solving**: MiniZinc backend for optimization
- **Web Interface**: Real-time browser-based interaction
- **Logging Infrastructure**: Telemere for monitoring and analysis
- **Data Persistence**: Structured storage of interview results and models

## Current Operational Status

- Server runs on Jetty at port 3300 or 3301, depending on configuration.
- OpenAI models actively used for agent processing
- Dynamic HTTP handler route updates
- Real-time conversation management
- Comprehensive agent activity logging

## Development Workflow

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

1. Preserve exact formatting and whitespace when making any edits
2. Use the MCP Clojure editing tools for precise, surgical changes
3. Only modify what needs to be changed without reformatting entire sections
4. Allow sufficient time (30+ seconds) for LLM agent calls as noted in the documentation

If you are asked to run functions of the system that involve calls to LLM-based agents, allow at least 30 seconds for each call to complete.
Things will timeout by themselves, you should not give up.
In order to observe system activity, you can watch for logging at the console, and also entries made in logs/agent-logs.txt."


---

*This overview is based on observation of the system running the music school example, analysis of the codebase structure, and examination of the comprehensive agent logging system.*
