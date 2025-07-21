# Migration Strategy: From OpenAI Assistants to Generic LLM Architecture

## Current System Analysis

### OpenAI Assistants Usage

The current SchedulingTBD system heavily relies on OpenAI's Assistants API with the following key components:

1. **Assistants** (`make-assistant`): AI agents with specific instructions, tools, and vector stores
2. **Threads** (`make-thread`): Persistent conversation contexts for each agent
3. **Message Management**: Creating messages and running threads for responses
4. **Vector Stores**: File uploads for context (instruction files, response formats)
5. **Agent Types**:
   - `:system` - Shared across all projects
   - `:project` - Exclusive to one project (e.g., surrogates)
   - `:shared-assistant` - Same assistant, different threads per project

### Key Dependencies to Replace

```clojure
;; Current OpenAI-specific functions that need replacement:
- make-assistant
- make-thread
- query-on-thread-aux
- list-thread-messages
- create-message
- create-run
- retrieve-run
```

## Phase 1: Generic LLM Interface Layer

### 1.1 Create LLM Protocol

Create a protocol-based abstraction layer that can work with different LLM providers:

```clojure
(defprotocol LLMProvider
  "Generic interface for LLM operations"
  (create-conversation [provider config] "Create a new conversation context")
  (send-message [provider conversation-id message] "Send message and get response")
  (get-conversation-history [provider conversation-id] "Get message history")
  (delete-conversation [provider conversation-id] "Clean up conversation")
  (list-conversations [provider] "List active conversations"))
```

### 1.2 Conversation Management

Replace OpenAI threads with a generic conversation management system:

```clojure
(defn create-conversation!
  "Create a new conversation with system instructions"
  [{:keys [provider instructions metadata]}]
  (let [conv-id (util/generate-id)
        conversation {:id conv-id
                     :instructions instructions
                     :metadata metadata
                     :messages []
                     :created-at (now)
                     :provider provider}]
    (db/store-conversation! conversation)
    conv-id))

(defn send-message!
  "Send message to conversation and get response"
  [{:keys [conversation-id message role]}]
  (let [conversation (db/get-conversation conversation-id)
        messages (conj (:messages conversation)
                      {:role role :content message :timestamp (now)})
        response (llm-query (:provider conversation)
                           (:instructions conversation)
                           messages)
        updated-messages (conj messages response)]
    (db/update-conversation! conversation-id {:messages updated-messages})
    (:content response)))
```

### 1.3 LLM Provider Implementations

#### OpenAI Chat Completions (Transition)
```clojure
(defrecord OpenAIChatProvider [api-key model]
  LLMProvider
  (send-message [_ conv-id message]
    (let [conversation (db/get-conversation conv-id)
          messages (build-openai-messages conversation message)
          response (openai/create-chat-completion
                    {:model model
                     :messages messages
                     :temperature 0.7})]
      (extract-response-content response))))
```

#### Llama 4 Maverick Provider
```clojure
(defrecord LlamaProvider [endpoint api-key model]
  LLMProvider
  (send-message [_ conv-id message]
    (let [conversation (db/get-conversation conv-id)
          messages (build-llama-messages conversation message)
          response (http/post (str endpoint "/v1/chat/completions")
                             {:headers {"Authorization" (str "Bearer " api-key)
                                       "Content-Type" "application/json"}
                              :body (json/encode {:model model
                                                 :messages messages
                                                 :temperature 0.7})})]
      (extract-llama-response response))))
```

## Phase 2: Agent System Refactoring

### 2.1 Replace Agent Storage

Transition from OpenAI assistant IDs to internal agent management:

```clojure
(defn ensure-agent-v2!
  "New agent creation without OpenAI assistants"
  [agent-id {:keys [instructions tools vector-store-paths] :as config}]
  (let [agent {:id agent-id
               :instructions instructions
               :tools tools
               :context-files (load-context-files vector-store-paths)
               :conversation-id (create-conversation! {:instructions instructions
                                                      :metadata {:agent-id agent-id}})
               :created-at (now)
               :updated-at (now)}]
    (db/store-agent! agent)
    agent))

(defn query-agent-v2
  "Query agent using new conversation system"
  [agent-id text opts]
  (let [agent (db/get-agent agent-id)
        conversation-id (:conversation-id agent)
        enhanced-message (enhance-with-context text (:context-files agent))
        response (send-message! {:conversation-id conversation-id
                                :message enhanced-message
                                :role "user"})]
    (post-process-response response opts)))
```

### 2.2 Context Management

Replace vector stores with local context management:

```clojure
(defn load-context-files
  "Load and index context files for agent"
  [file-paths]
  (mapv (fn [path]
          {:path path
           :content (slurp path)
           :embedding (generate-embedding (slurp path)) ; Optional for semantic search
           :last-modified (.lastModified (io/file path))})
        file-paths))

(defn enhance-with-context
  "Add relevant context to user message"
  [message context-files]
  (let [relevant-context (find-relevant-context message context-files)
        context-str (str/join "\n\n" (map :content relevant-context))]
    (str "Context Information:\n" context-str "\n\nUser Query: " message)))
```

## Phase 3: Migration Execution

### 3.1 Backwards Compatibility Layer

Create a compatibility layer to ease migration:

```clojure
(defn make-assistant-v2
  "Compatibility wrapper for make-assistant"
  [{:keys [name instructions tools metadata]}]
  (let [agent-id (keyword (str/lower-case (str/replace name #"\s+" "-")))
        agent (ensure-agent-v2! agent-id {:instructions instructions
                                          :tools tools
                                          :metadata metadata})]
    {:id (:conversation-id agent)
     :name name
     :created_at (:created-at agent)}))

(defn make-thread-v2
  "Compatibility wrapper for make-thread"
  [{:keys [assistant-id metadata]}]
  ;; In the new system, each agent has its own conversation
  ;; So we can return the existing conversation-id
  (let [agent (db/find-agent-by-conversation assistant-id)]
    {:id (:conversation-id agent)
     :created_at (now)}))
```

### 3.2 Configuration Updates

```clojure
;; New configuration structure
(def llm-providers
  {:openai-chat {:type :openai-chat
                 :api-key (env/get "OPENAI_API_KEY")
                 :model "gpt-4o-2024-11-20"}
   :llama {:type :llama
           :endpoint "http://llama-server:8000"
           :api-key (env/get "LLAMA_API_KEY")
           :model "llama-4-maverick"}
   :azure-openai {:type :azure-openai
                  :api-key (env/get "AZURE_OPENAI_KEY")
                  :endpoint (env/get "AZURE_OPENAI_ENDPOINT")
                  :model "mygpt-4"}})

(defn select-provider
  "Choose LLM provider based on configuration"
  [provider-key]
  (let [config (get llm-providers provider-key)
        provider-type (:type config)]
    (case provider-type
      :openai-chat (->OpenAIChatProvider (:api-key config) (:model config))
      :llama (->LlamaProvider (:endpoint config) (:api-key config) (:model config))
      :azure-openai (->AzureOpenAIProvider (:api-key config) (:endpoint config) (:model config)))))
```

## Phase 4: MCP Server Integration

### 4.1 Human/Machine Teaming Architecture

The MCP server architecture prioritizes **human/machine teaming** where the UI serves as an intelligent MCP client that:

1. **Tracks Learning Evolution**: Monitors how human interviewees' understanding evolves
2. **MiniZinc Mentoring**: Provides progressive education on constraint satisfaction
3. **Requirements Translation**: Shows real-time mapping from interview responses to MiniZinc constructs
4. **Collaborative Development**: Enables humans and AI to co-create scheduling solutions

### 4.2 MCP Tools for Educational Collaboration

```clojure
(def mcp-tools
  ;; Core Interview Tools
  [{:name "create_surrogate"
    :description "Create a surrogate agent for a specific domain"
    :input_schema {:type "object"
                   :properties {:domain {:type "string" :description "The domain expertise"}
                               :instructions {:type "string" :description "Custom instructions"}
                               :expertise {:type "string" :description "Area of expertise"}}
                   :required ["domain"]}}

   {:name "start_interview"
    :description "Start an automated interview process with learning tracking"
    :input_schema {:type "object"
                   :properties {:surrogate_id {:type "string"}
                               :interview_type {:type "string" :enum ["process" "data" "resources"]}
                               :user_experience_level {:type "string" :enum ["beginner" "intermediate" "advanced"]}
                               :learning_objectives {:type "array" :items {:type "string"}}}
                   :required ["surrogate_id" "interview_type"]}}

   ;; MiniZinc Education Tools
   {:name "explain_minizinc_concept"
    :description "Explain how interview findings map to MiniZinc concepts"
    :input_schema {:type "object"
                   :properties {:project_id {:type "string"}
                               :concept {:type "string" :description "MiniZinc concept to explain"}
                               :interview_context {:type "string" :description "Relevant interview context"}}
                   :required ["project_id" "concept"]}}

   {:name "generate_progressive_minizinc"
    :description "Generate MiniZinc model with educational annotations and progressive complexity"
    :input_schema {:type "object"
                   :properties {:project_id {:type "string"}
                               :complexity_level {:type "string" :enum ["basic" "intermediate" "advanced"]}
                               :show_reasoning {:type "boolean" :default true}
                               :include_alternatives {:type "boolean" :default false}}
                   :required ["project_id"]}}

   ;; Learning Progress Tools
   {:name "track_learning_progress"
    :description "Track user's evolving understanding of scheduling concepts"
    :input_schema {:type "object"
                   :properties {:user_id {:type "string"}
                               :concept_understanding {:type "object"}
                               :questions_answered {:type "array"}
                               :minizinc_comfort_level {:type "integer" :minimum 1 :maximum 10}}
                   :required ["user_id"]}}

   {:name "suggest_next_learning_step"
    :description "Recommend next learning activities based on current understanding"
    :input_schema {:type "object"
                   :properties {:user_id {:type "string"}
                               :current_project {:type "string"}}
                   :required ["user_id"]}}

   ;; Collaborative Development Tools
   {:name "propose_constraint_refinement"
    :description "AI suggests refinements to user-proposed constraints"
    :input_schema {:type "object"
                   :properties {:project_id {:type "string"}
                               :user_constraint {:type "string" :description "User's constraint attempt"}
                               :context {:type "string" :description "Interview context for the constraint"}}
                   :required ["project_id" "user_constraint"]}}

   {:name "validate_user_minizinc"
    :description "Validate and provide feedback on user's MiniZinc code"
    :input_schema {:type "object"
                   :properties {:minizinc_code {:type "string"}
                               :expected_behavior {:type "string"}
                               :learning_mode {:type "boolean" :default true}}
                   :required ["minizinc_code"]}}])
```

### 4.3 Educational MCP Resources

```clojure
(def mcp-resources
  ;; Template and Learning Resources
  [{:uri "schedulingTBD://learning/minizinc-primer"
    :name "MiniZinc Learning Primer"
    :description "Progressive introduction to MiniZinc for scheduling"
    :mime_type "text/markdown"}

   {:uri "schedulingTBD://learning/constraint-patterns"
    :name "Common Scheduling Constraint Patterns"
    :description "Library of reusable MiniZinc patterns for scheduling"
    :mime_type "application/json"}

   {:uri "schedulingTBD://project/{project_id}/learning-path"
    :name "Personalized Learning Path"
    :description "User's progress and recommended next steps"
    :mime_type "application/json"}

   {:uri "schedulingTBD://project/{project_id}/interview-to-minizinc-mapping"
    :name "Interview to MiniZinc Translation"
    :description "Live mapping showing how interview responses become constraints"
    :mime_type "application/json"}

   ;; Progressive Model Generation
   {:uri "schedulingTBD://project/{project_id}/minizinc/basic"
    :name "Basic MiniZinc Model"
    :description "Simplified model focusing on core concepts"
    :mime_type "text/plain"}

   {:uri "schedulingTBD://project/{project_id}/minizinc/annotated"
    :name "Annotated MiniZinc Model"
    :description "Full model with educational comments and explanations"
    :mime_type "text/plain"}

   {:uri "schedulingTBD://project/{project_id}/minizinc/production"
    :name "Production MiniZinc Model"
    :description "Optimized model for actual scheduling use"
    :mime_type "text/plain"}])
```

### 4.4 UI as Intelligent MCP Client

The UI transforms into an educational dashboard that:

```clojure
;; Example UI state management for learning
(defn update-learning-state
  "Track user's evolving understanding"
  [user-state interview-response minizinc-concept]
  (-> user-state
      (update-in [:concept-understanding minizinc-concept]
                 #(merge % {:last-interaction (now)
                           :confidence-level (assess-confidence interview-response)
                           :examples-seen (inc (:examples-seen % 0))}))
      (update :interview-progress conj
              {:concept minizinc-concept
               :response interview-response
               :timestamp (now)
               :understanding-level (assess-understanding interview-response)})))

;; Real-time translation display
(defn show-interview-to-minizinc-translation
  "Display how interview concepts map to MiniZinc"
  [interview-concept project-context]
  {:interview-statement interview-concept
   :minizinc-equivalent (translate-to-minizinc interview-concept)
   :explanation (explain-translation interview-concept)
   :related-concepts (find-related-concepts interview-concept)
   :next-learning-opportunity (suggest-next-concept interview-concept project-context)})
```

### 4.5 Mentoring and Guidance System

```clojure
(defn generate-educational-minizinc
  "Create MiniZinc with progressive complexity and mentoring"
  [{:keys [project-id user-level show-reasoning]}]
  (let [interview-data (get-interview-results project-id)
        base-model (generate-base-model interview-data)
        annotated-model (add-educational-annotations base-model user-level)
        explanation (generate-concept-explanations base-model interview-data)]
    {:minizinc-code annotated-model
     :explanations explanation
     :learning-objectives (identify-learning-objectives base-model)
     :practice-exercises (generate-exercises base-model)
     :next-concepts (suggest-advanced-concepts user-level base-model)}))

(defn provide-constraint-mentoring
  "Guide user through constraint development"
  [user-constraint-attempt interview-context]
  {:feedback (analyze-user-attempt user-constraint-attempt)
   :suggestions (improve-constraint user-constraint-attempt)
   :explanation (explain-constraint-reasoning interview-context)
   :examples (provide-similar-examples user-constraint-attempt)
   :next-challenge (suggest-next-constraint-challenge)})
```
```

### 4.2 MCP Resources

Expose project data and templates as MCP resources:

```clojure
(def mcp-resources
  [{:uri "schedulingTBD://templates/timetabling"
    :name "Timetabling Template"
    :description "EADS template for timetabling problems"
    :mime_type "application/edn"}

   {:uri "schedulingTBD://templates/flow-shop"
    :name "Flow Shop Template"
    :description "EADS template for flow shop scheduling"
    :mime_type "application/edn"}

   {:uri "schedulingTBD://project/{project_id}/data"
    :name "Project Data"
    :description "Interview results and data models for a project"
    :mime_type "application/edn"}

   {:uri "schedulingTBD://project/{project_id}/minizinc"
    :name "Generated MiniZinc Model"
    :description "MiniZinc constraint satisfaction model"
    :mime_type "text/plain"}])

(defn handle-mcp-resource
  "Handle MCP resource requests"
  [uri]
  (cond
    (str/starts-with? uri "schedulingTBD://templates/")
    (get-template-resource uri)

    (str/includes? uri "/project/")
    (get-project-resource uri)

    :else
    {:error "Resource not found"}))
```

### 4.3 MCP Server Startup

```clojure
(defn start-mcp-server
  "Start the MCP server"
  []
  (-> (mcp/create-server)
      (mcp/add-tools mcp-tools)
      (mcp/add-resources mcp-resources)
      (mcp/set-tool-handler handle-mcp-call)
      (mcp/set-resource-handler handle-mcp-resource)
      (mcp/start)))
```

## Implementation Timeline

### Week 1-2: Foundation
- [ ] Create LLM protocol and basic implementations
- [ ] Implement conversation management system
- [ ] Create Llama 4 Maverick provider
- [ ] Test basic query/response functionality
- [ ] Design learning state tracking architecture

### Week 3-4: Agent Migration
- [ ] Refactor agent creation and storage
- [ ] Implement context management system
- [ ] Create backwards compatibility layer
- [ ] Migrate surrogate agents
- [ ] Add educational annotations to agent responses

### Week 5-6: Interview System Enhancement
- [ ] Update interview managers to use new agent system
- [ ] Test complete interview workflows
- [ ] Migrate EADS processing
- [ ] Validate with music school example
- [ ] Implement real-time interview-to-MiniZinc translation

### Week 7-8: MCP Integration and Educational Features
- [ ] Design MCP server interface with educational tools
- [ ] Implement MCP tools for learning and mentoring
- [ ] Create MCP client UI framework
- [ ] Add progressive MiniZinc generation
- [ ] Implement learning progress tracking

### Week 9-10: UI Development and Testing
- [ ] Build educational dashboard interface
- [ ] Implement real-time translation display
- [ ] Add constraint mentoring features
- [ ] Create user progress visualization
- [ ] Comprehensive testing with real users

### Week 11-12: Documentation and Refinement
- [ ] Complete user guides and documentation
- [ ] Refine educational content and examples
- [ ] Performance optimization
- [ ] Deployment and rollout planning

## Benefits of Migration

### Technical Benefits
1. **Provider Independence**: Can switch between LLM providers easily
2. **Cost Efficiency**: Use free Llama 4 Maverick for development/testing
3. **Future-Proof**: Not dependent on deprecated OpenAI assistants
4. **MCP Integration**: Standardized interface for other tools
5. **Local Control**: Full control over conversation management and context
6. **Flexibility**: Easier to customize behavior per use case

### Educational and Collaborative Benefits
7. **Progressive Learning**: Adaptive MiniZinc education based on user understanding
8. **Real-time Translation**: Live mapping from requirements to constraints
9. **Collaborative Development**: AI-human partnership in creating scheduling solutions
10. **Learning Analytics**: Track and optimize educational effectiveness
11. **Contextual Mentoring**: Personalized guidance based on domain and experience
12. **Iterative Improvement**: Continuous refinement of both understanding and models

## Risks and Mitigation

1. **Context Loss**: Current threads contain valuable conversation history
   - *Mitigation*: Export existing conversations before migration

2. **Performance Changes**: Different providers may have different response times
   - *Mitigation*: Implement timeout and retry mechanisms

3. **Capability Differences**: Llama may not match GPT-4 performance
   - *Mitigation*: Keep OpenAI as fallback option, prompt engineering

4. **Integration Complexity**: MCP server adds another layer
   - *Mitigation*: Phase implementation, maintain direct access methods

This migration strategy provides a path forward that eliminates the dependency on deprecated OpenAI assistants while adding flexibility and preparing for MCP integration.
