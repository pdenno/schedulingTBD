# SchedulingTBD System Overview

## Introduction

The SchedulingTBD system is a sophisticated multi-agent AI framework designed to automatically understand scheduling problems and generate MiniZinc-based solutions. The system uses a structured interview process with AI agents to elicit requirements, classify problems, and model data relationships. It serves as a web-based application running on Jetty at port 3300 with real-time WebSocket communication and comprehensive logging.

## System Architecture

### Core Components

1. **Multi-Agent Interview System**
   - **Interview Manager**: Orchestrates the overall interview process
   - **Process Interviewer**: Conducts interviews about business processes
   - **Data Interviewer**: Focuses on data modeling and Object Role Modeling (ORM)
   - **ORK (Orchestration) Manager**: Decides which interview path to pursue next

2. **Surrogate Agents**
   - AI agents that roleplay as domain experts (e.g., music school operators)
   - Provide realistic, consistent responses during interviews
   - Created with specific domain knowledge and behavioral instructions

3. **EADS (Expert Analysis and Decision Support) Framework**
   - Structured data collection templates for different problem types
   - Guides the interview process through predefined inquiry areas
   - Ensures comprehensive coverage of scheduling requirements

4. **Problem Classification Engine**
   - Automatically determines the type of scheduling problem:
     - Flow Shop Scheduling
     - Job Shop Scheduling
     - Timetabling Problems
     - Project Scheduling
     - Single Machine Scheduling

5. **Web Infrastructure**
   - **Jetty Server**: Runs on port 3300
   - **WebSocket Support**: Real-time communication for interactive sessions
   - **HTTP Handler Routes**: Dynamic route management for web interface

## How the System Works

### Phase 1: Problem Initialization
1. A surrogate agent is created representing a specific domain (e.g., music school)
2. The surrogate is given detailed instructions about their role and expertise
3. Initial conversation context is established
4. OpenAI models are utilized for AI agent intelligence

### Phase 2: Problem Classification
1. The ORK manager analyzes the initial problem description
2. Determines which EADS template to pursue (e.g., `process/scheduling-problem-type`)
3. The process interviewer conducts targeted questions to classify the problem
4. Key properties are determined:
   - **Principal problem type** (e.g., TIMETABLING-PROBLEM)
   - **Problem components** (supporting problem types)
   - **Continuous?** (whether processes flow continuously)
   - **Cyclical?** (whether schedules repeat in patterns)

### Phase 3: Detailed Process Analysis
1. Once classified, the system pursues specific EADS templates (e.g., `process/timetabling`)
2. Structured interviews collect information about:
   - **Event types** and their characteristics
   - **Time slots** and availability
   - **Resource constraints** and priorities
   - **Special events** and exceptions

### Phase 4: Data Modeling
1. The system switches to data interviewing mode
2. Object Role Modeling (ORM) techniques are used to:
   - Identify key entities (students, instructors, rooms)
   - Define relationships and constraints
   - Create example data structures
   - Establish fact types and reference modes

### Phase 5: Solution Generation
1. Based on collected requirements and data models
2. Generate MiniZinc constraint satisfaction problems
3. Create executable scheduling solutions

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

- Server runs on Jetty at port 3300
- OpenAI models actively used for agent processing
- Dynamic HTTP handler route updates
- Real-time conversation management
- Comprehensive agent activity logging

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

*This overview is based on observation of the system running the music school example, analysis of the codebase structure, and examination of the comprehensive agent logging system.*