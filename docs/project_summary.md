# SchedulingTBD Project Summary

## Overview

SchedulingTBD is an experimental software platform designed to study **human/machine teaming (HMT)** in long-running cognitive tasks, specifically focusing on collaborative formulation and refinement of manufacturing production scheduling systems. The project explores how humans and AI can work together to create solutions using domain-specific languages (DSLs), particularly [MiniZinc](https://www.minizinc.org/) for constraint programming.

## Project Goals

This research aims to establish consensus and eventual standardization on best practices for:

- **AI-mediated systems integration**
- **Designing DSLs fit for Human-Machine Teaming**
- **Running AI agent-based interviews** to elicit requirements
- **Plan recognition** in collaborative tasks
- **Constructionist learning** for adults
- **Scientific explanation** in HMT

## Technical Architecture

### Technology Stack

**Backend (Clojure)**
- **Clojure 1.12.0** - Core language
- **Ring/Jetty** - Web server infrastructure
- **Reitit** - HTTP routing
- **Mount** - Lifecycle management
- **Datahike** - Database layer
- **OpenAI Clojure** - LLM integration
- **Promesa** - Async programming
- **Telemere** - Logging and telemetry

**Frontend (ClojureScript)**
- **ClojureScript** with **Shadow-CLJS** build tooling
- **React 18.2.0** with **Helix** for React integration
- **Material-UI (MUI 6.4.5)** - UI components
- **CodeMirror 6** - Code editor with Clojure syntax support
- **Mermaid** - Diagram rendering
- **WebSockets** - Real-time communication

**Development Tools**
- **Kaocha** - Testing framework
- **CIDER/nREPL** - Development environment
- **clj-kondo** - Static analysis

### System Architecture

#### Multi-Agent System
The application implements a sophisticated multi-agent architecture:
1. **Interview Agents** - Conduct structured interviews with users about their manufacturing processes
2. **Surrogate Agents** - LLM-based agents that simulate domain expertise for testing
3. **System Agents** - Coordinate overall workflow and interactions
4. **Data Analysis Agents** - Process and analyze manufacturing data
5. **Process Analysis Agents** - Understand and model manufacturing processes

#### Core Components

**Server-side (`src/server/scheduling_tbd/`)**:
- `iviewr/ork.clj` - Interview orchestrator
- `iviewr/interviewers.clj` - Interview agent manager
- `surrogate.clj` - Surrogate agent management
- `core.clj` - Application bootstrap and server lifecycle
- `llm.clj` - Large Language Model integration and management
- `db.clj` - Database operations and persistence
- `web/handler.clj` - HTTP request handling
- `web/websockets.clj` - Real-time WebSocket communication
- `minizinc.clj` - MiniZinc constraint programming integration

**Client-side (`src/app/stbd_app/`)**:
- `core.cljs` - Main application entry point
- `components/chat.cljs` - Chat interface for human-AI interaction
- `components/editor.cljs` - Code editor for MiniZinc
- `components/project.cljs` - Project management interface
- `components/graph.cljs` - Process visualization
- `ws.cljs` - WebSocket client communication

### Data Model

The system manages several types of data:

#### Projects
Manufacturing scenarios stored as EDN files in `data/projects/`, including:
- Craft beer brewery scheduling
- Ice cream production
- Aluminum foil production
- Snowboard manufacturing
- Various "How It's Made" inspired scenarios

#### Conversations
Structured interview conversations with four main topics:
1. **Process** - Understanding the manufacturing workflow
2. **Data** - Identifying data sources and requirements
3. **Resources** - Cataloging available resources (people, machines)
4. **Optimality** - Defining what constitutes a "good" schedule

#### Generated Artifacts
- **MiniZinc models** - Constraint programming specifications
- **Process diagrams** - Mermaid-based visualizations
- **Data structures** - Formalized representations of requirements

## Key Features

### 1. Interactive Chat Interface
- Real-time chat with AI agents specialized in different aspects of scheduling
- Context-aware conversations that build upon previous interactions
- Support for file uploads and data sharing

### 2. Code Editor Integration
- Syntax-highlighted MiniZinc editor
- Real-time validation and error checking
- Integration with constraint solver execution

### 3. Process Visualization
- Automatic generation of process flow diagrams
- Mermaid-based rendering of manufacturing workflows
- Interactive exploration of process dependencies

### 4. Multi-Modal Learning
- Constructionist approach where users learn by building
- Scaffolded introduction to constraint programming concepts
- Real-world manufacturing scenario examples

### 5. Surrogate Testing Framework
- LLM-based surrogate users for testing interview processes
- Hundreds of test scenarios before human user engagement
- Automated validation of interview effectiveness

## Development Workflow

### Running the Application

1. **Environment Setup**:
   ```bash
   export OPENAI_API_KEY=sk-...
   export SCHEDULING_TBD_DB=/opt/scheduling
   ```

2. **Server (Backend)**:
   ```bash
   clj -M:dev
   # In REPL: (start)
   ```

3. **Client (Frontend)**:
   ```bash
   npm install
   npm run stbd-client
   # Visit http://localhost:3300/app
   ```

### Database Initialization
```clojure
(in-ns 'scheduling-tbd.db)
(recreate-system-db!)
(recreate-project-dbs!)
```

### Testing
```bash
clj -M:test  # Server-side tests
npm run kaochaSTBD  # Client-side tests
```

## Research Context

This software is being developed as part of the NIST project [Human/AI Teaming for Manufacturing Digital Twins](https://www.nist.gov/programs-projects/humanmachine-teaming-manufacturing-digital-twins). The research addresses fundamental questions about how humans and AI systems can effectively collaborate on complex, knowledge-intensive tasks.

### Current Status
- ✅ Multi-agent interview system operational
- ✅ MiniZinc integration for simple process modeling
- ✅ Web-based chat interface with real-time updates
- ✅ Surrogate agent testing framework
- 🔄 Working toward complete MiniZinc solution generation
- 🔄 Testing with hundreds of surrogate domain experts
- ⏳ Planning human expert validation studies

## Project Structure

```
schedulingTBD/
├── src/
│   ├── server/          # Clojure backend
│   └── app/             # ClojureScript frontend
├── data/
│   ├── projects/        # Manufacturing scenario data
│   ├── examples/        # MiniZinc examples
│   └── templates/       # Project templates
├── resources/
│   ├── agents/          # Agent configuration
│   └── public/          # Static web assets
├── test/                # Test suites
├── env/                 # Environment-specific config
├── doc/                 # Documentation and screenshots
└── bin/                 # Utility scripts
```

## Dependencies and Integration

### External Systems
- **OpenAI API** - For LLM capabilities
- **MiniZinc** - Constraint programming solver
- **WebSocket infrastructure** - For real-time communication

### Data Persistence
- **Datahike** database for conversation history
- **EDN files** for project configurations
- **File system** for generated artifacts

## Future Directions

The project aims to:
1. Complete end-to-end MiniZinc solution generation
2. Conduct large-scale testing with surrogate agents
3. Validate approach with human manufacturing experts
4. Contribute to standardization efforts in AI-mediated systems
5. Advance understanding of constructionist learning in technical domains

---

*This project represents cutting-edge research in human-AI collaboration, specifically targeting the complex domain of manufacturing scheduling where deep domain expertise must be combined with technical constraint programming skills.*
