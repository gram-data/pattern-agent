# TODO

## 1. Foundation & Setup

- [x] Set up basic project structure and dependencies
- [x] Integrate with gram-hs Pattern abstraction
- [x] Define core `Pattern<Agent>` type 
  - [x] Type definition complete (PatternAgent type alias in Types.hs)

## 2. Basic LLM Agent

- [x] **Principle 2: Why Before How** - Document user goal and rationale for basic LLM agent implementation
  - [x] Documented in specs/002-llm-agent/spec.md with user goal and rationale
- [x] **Principle 1: Design-Driven Development** - Validate LLM agent design against user goal
  - [x] Design validated in specs/002-llm-agent/plan.md Phase 1 section
- [x] Design basic LLM agent structure similar to Google ADK's LlmAgent (Principle 4: expressive API design)
  - [x] Agent identity: name, description, model selection
  - [x] Instructions/instruction parameter for guiding agent behavior
  - [ ] Tools integration (equipping agent with capabilities)
  - [x] Basic configuration and control
- [x] Implement agent identity components (name, description, model) (Principle 4: correctness)
  - [x] Implemented in src/PatternAgent/Agent.hs with createAgent function
- [x] Implement instruction parameter (simple string for now) for guiding agent behavior (Principle 4: expressive, clear API)
  - [x] agentInstruction field in Agent type, validated in createAgent
- [ ] Implement tools integration (equipping agent with function tools) (Principle 4: correctness)
  - [ ] Tool.hs is placeholder only, not yet implemented
- [x] Create basic agent execution interface (Principle 4: expressive API)
  - [x] executeAgent function implemented in src/PatternAgent/Execution.hs
- [x] **Principle 3: Dual Testing Strategy**
  - [x] Unit tests for agent identity, instructions, and tools
    - [x] Unit tests for agent identity and instructions in tests/unit/AgentTest.hs
    - [ ] Unit tests for tools (blocked: tools not yet implemented)
  - [x] Scenario test: create and execute a basic LLM agent (similar to Google ADK example)
    - [x] Scenario test for agent creation in tests/scenario/AgentIdentityTest.hs
    - [ ] Scenario test for agent execution (conversation loop) - Main.hs shows usage but no formal test
- [x] Implement basic conversation loop for agent interaction
  - [x] ConversationContext and Message types in src/PatternAgent/Context.hs
  - [x] executeAgent accepts ConversationContext parameter
- [x] Support for agent response generation and tool execution
  - [x] Agent response generation via executeAgent returning AgentResponse
  - [ ] Tool execution (blocked: tools not yet implemented)
- [x] Document basic LLM agent usage and API
  - [x] quickstart.md with examples
  - [x] API contracts in specs/002-llm-agent/contracts/

## 3. Execution

- [x] **Principle 2: Why Before How** - Document user goal and rationale for execution environment
  - [x] Documented in specs/004-agent-execution/spec.md with user goal and rationale for scenario tests, interactive CLI, and observability
- [ ] **Principle 1: Design-Driven Development** - Validate execution environment design against user goal
  - [ ] Design validation to be completed during planning phase per Principle 1
- [x] Create basic module for calling an LLM API (Principle 4: expressive API design)
  - [x] Comprehensive LLM.hs module with createClientForModel, callLLM, sendRequest, etc.
- [x] Design execution environment that "runs the agent" (validate against user goal)
  - [x] executeAgent function in src/PatternAgent/Execution.hs
- [x] Implement execution environment for single atomic agents (Principle 4: correctness for all conditions)
  - [x] executeAgent handles single agent execution with error handling
- [x] Support for fully specifying an agent (system prompt, LLM selection) - expressive API
  - [x] Agent has agentInstruction (system prompt) and agentModel (LLM selection)
- [ ] Create early test case: simple conversational agent with no tools
  - [x] **Principle 2** - Document user goal for conversational agent
    - [x] Documented in specs/002-llm-agent/spec.md User Story 5
  - [x] **Principle 1** - Validate agent design against user goal
    - [x] Validated in specs/002-llm-agent/plan.md
  - [x] Define single agent with system prompt and LLM (Principle 4: clear, expressive specification)
    - [x] Agent type supports this via createAgent function
  - [ ] **Principle 3: Dual Testing Strategy**
    - [x] Unit tests for agent definition and LLM API module
      - [x] AgentTest.hs covers agent definition
      - [x] LLM module has comprehensive functionality (no dedicated unit tests but used in execution)
    - [ ] Scenario test: user goal satisfaction (e.g., "user can have a conversation with agent")
      - [ ] Spec created in specs/004-agent-execution/spec.md for scenario tests with zero/one/multiple tools
      - [ ] Implementation pending
  - [ ] Execute agent in conversation loop
    - [x] Infrastructure exists (Context.hs, executeAgent accepts context)
    - [ ] Interactive CLI mode spec created in specs/004-agent-execution/spec.md
    - [ ] Implementation pending
  - [ ] Validate basic conversational behavior
    - [ ] Scenario tests spec created in specs/004-agent-execution/spec.md
    - [ ] Implementation pending
- [ ] Design execution semantics for Pattern<Agent> (validate against user goals)
  - [ ] Only single agent execution exists, no pattern execution semantics
- [ ] Implement basic pattern execution engine (Principle 4: expressiveness and correctness)
  - [ ] Only single agent execution exists, no pattern execution engine
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests and scenario tests for execution engine
  - [ ] Scenario tests spec created in specs/004-agent-execution/spec.md for zero/one/multiple tools
  - [ ] Implementation pending
- [ ] Support for execution visualization
  - [ ] Observability infrastructure spec created in specs/004-agent-execution/spec.md
  - [ ] Implementation pending (researching best practices and integration options)
- [ ] Document execution semantics and DAG interpretation

## 4. Decomposition (Factorization)

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for decomposition feature
- [ ] **Principle 1: Design-Driven Development** - Validate decomposition API design against user goals
- [ ] Design decomposition API for breaking agents into sub-agents (Principle 4: expressive, intuitive API)
- [ ] Implement basic factorization strategy (e.g., task-based decomposition) (Principle 4: correctness)
- [ ] Create first concrete decomposition example:
  - [ ] **Principle 2** - Document why this specific agent needs decomposition
  - [ ] **Principle 1** - Validate decomposition design against stated goal
  - [ ] Start with a complex agent (e.g., multi-step reasoning agent)
  - [ ] Factorize into specialized sub-agents
  - [ ] Document the decomposition process and rationale
  - [ ] **Principle 3: Dual Testing Strategy**
    - [ ] Unit tests for decomposition operations
    - [ ] Scenario test: decomposed agent satisfies same user goal as original
- [ ] Support for identifying "prime agents" (atomic patterns) (Principle 4: clear, expressive API)
- [ ] Implement multiple decomposition strategies:
  - [ ] Task-based decomposition
  - [ ] Tool-based decomposition
  - [ ] Domain-based decomposition
- [ ] Validate decomposed agents maintain original capability (Principle 4: correctness)
- [ ] **Principle 3: Dual Testing Strategy** - Unit and scenario tests for each decomposition strategy
- [ ] Decomposition toolkit with factorization strategies

## 5. Composition

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for composition feature
- [ ] **Principle 1: Design-Driven Development** - Validate composition API design against user goals
- [ ] Design composition API for assembling patterns (Principle 4: expressive, intuitive API)
- [ ] Implement basic pattern assembly (DAG construction) (Principle 4: correctness)
- [ ] Create first concrete composition example:
  - [ ] **Principle 2** - Document why composition is needed for this use case
  - [ ] **Principle 1** - Validate composition design against stated goal
  - [ ] Start with library of simple pattern agents
  - [ ] Compose into a working multi-agent system
  - [ ] Document composition process and architecture
  - [ ] **Principle 3: Dual Testing Strategy**
    - [ ] Unit tests for composition operations
    - [ ] Scenario test: composed system satisfies user goal
- [ ] Support for workflow agents that coordinate sub-agents (Principle 4: expressive API)
- [ ] Pattern validation (checking DAG structure, dependencies) (Principle 4: correctness)
- [ ] Composition engine for pattern assembly and validation
- [ ] Support for parallelization opportunities in DAG structure
- [ ] Support for synchronization points in workflows
- [ ] **Principle 3: Dual Testing Strategy** - Unit and scenario tests for composition features

## 6. Pattern Library

- [ ] **Principle 2: Why Before How** - Document rationale for pattern library and catalog structure
- [ ] Create initial catalog of reusable agent patterns (Principle 4: expressive naming and organization)
- [ ] Document pattern behaviors and use cases (Principle 4: accurate documentation)
- [ ] Establish pattern naming conventions (Principle 4: expressiveness)
- [ ] Build library of reusable agent components organically
- [ ] Create pattern comparison utilities (Principle 4: expressive API, correct behavior)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests for library utilities, scenario tests for pattern usage

## 7. Equivalence & Comparison

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for equivalence feature
- [ ] **Principle 1: Design-Driven Development** - Validate equivalence design against user goals
- [ ] Design equivalence representation for patterns (Principle 4: expressive representation)
- [ ] Implement basic equivalence checking (Principle 4: correctness)
- [ ] Support for equivalence classes (different implementations, same goal)
- [ ] Pattern comparison utilities (Principle 4: expressive API, correct behavior)
- [ ] Document equivalence strategies (Principle 4: accurate documentation)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests for equivalence operations, scenario tests demonstrating equivalence validation

## 8. Tracing

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for tracing feature
- [ ] **Principle 1: Design-Driven Development** - Validate tracing design against user goals
- [ ] Design trace representation (potentially `Pattern<Trace>`) (Principle 4: expressive structure)
- [ ] Implement execution tracing (Principle 4: correctness - accurate trace capture)
- [ ] Integrate tracing with execution environment
- [ ] Support for trace analysis and inspection (Principle 4: expressive API)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests for tracing operations, scenario tests for trace capture and analysis

## 9. Evolution & Feedback

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for evolution mechanisms
- [ ] **Principle 1: Design-Driven Development** - Validate evolution design against user goals
- [ ] Design feedback loop mechanisms (Principle 4: expressive, clear design)
- [ ] Explore RL-like mechanisms for pattern improvement
- [ ] Implement pattern evolution based on traces (Principle 4: correctness)
- [ ] Evolution mechanisms with tracing and feedback loops
- [ ] Automated pattern optimization (Principle 4: correctness - valid optimizations)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests for evolution mechanisms, scenario tests for pattern improvement workflows

## 10. Theoretical Foundations

- [ ] Explore category theory connections
  - [ ] Investigate if patterns form a category
  - [ ] Define morphisms (transformations between patterns)
  - [ ] Document theoretical foundations
- [ ] Formalize bidirectional transformation properties
- [ ] Document mathematical foundations

## 11. Documentation & Examples

- [ ] Create comprehensive examples for decomposition (Principle 4: expressive, clear examples)
- [ ] Create comprehensive examples for composition (Principle 4: expressive, clear examples)
- [ ] Document emerging patterns and strategies (Principle 4: accurate documentation)
- [ ] Create tutorial/guide for using the framework (Principle 4: expressiveness)
- [ ] Document best practices for agent design
- [ ] Document user goals and rationale for each major feature (Principle 2: Why Before How)
- [ ] Document design validation process and examples (Principle 1: Design-Driven Development)
- [ ] Document testing strategy with unit and scenario test examples (Principle 3: Dual Testing Strategy)

## 12. Gram Notation Integration & Serialization

- [ ] **Principle 2: Why Before How** - Document user goal and rationale for gram notation integration
  - [ ] Document rationale: gram notation as declarative language for multi-agent systems
  - [ ] Document goal: all agentic workflows should be serializable (tool implementations are exception)
  - [ ] Document approach: tool descriptions in gram, implementations bound at execution time
- [ ] **Principle 1: Design-Driven Development** - Validate gram notation approach against user goals
  - [ ] Validate that gram notation enables declarative agent descriptions
  - [ ] Validate that tool description/implementation separation meets requirements
- [ ] Learn gram notation from gram-hs design documentation
  - [ ] Study gram-hs design docs: https://github.com/gram-data/gram-hs/tree/main/design
  - [ ] Understand Pattern serialization format
  - [ ] Understand recursive structure representation (for compound agents)
  - [ ] Understand reference/ID handling (for tool descriptions)
  - [ ] Understand type system in gram notation
- [ ] Design gram notation schema for PatternAgent
  - [ ] Explore schema requirements through under-specification approach
  - [ ] Identify gaps in initial schema design
  - [ ] Design type-to-label mapping for Agent types
  - [ ] Design field-to-property mapping for Agent fields
  - [ ] Design file-level record structure (descriptive metadata)
  - [ ] Elaborate on schema specification based on identified gaps
- [ ] Create gram-based agent notation documentation
  - [ ] Add markdown file under docs/ subdirectory
  - [ ] Document gram notation format for agents
  - [ ] Document schema conventions (type mappings, field mappings)
  - [ ] Document file-level record structure
  - [ ] Provide examples of gram-serialized agents
- [ ] Design tool description schema
  - [ ] Tool name (unique identifier)
  - [ ] Tool description (natural language - important for future self-improvement)
  - [ ] Parameter schema (names, types, required/optional)
  - [ ] Return type schema
  - [ ] Error handling contract (defer detailed work for now)
- [ ] Implement tool description type (serializable in gram)
  - [ ] ToolDescription type with natural language description
  - [ ] Parameter and return type schemas
  - [ ] Gram serialization support for ToolDescription
- [ ] Implement gram notation serialization for PatternAgent
  - [ ] Serialize atomic agents (single agent)
  - [ ] Serialize compound agents (multi-agent systems)
  - [ ] Serialize agents with tool descriptions
  - [ ] Serialize agents without tools
  - [ ] Handle edge cases (empty descriptions, optional fields)
- [ ] Implement gram notation deserialization for PatternAgent
  - [ ] Parse gram notation into PatternAgent structure
  - [ ] Validate deserialized agent structure
  - [ ] Handle schema version differences gracefully
- [ ] Create "hello world" agent example with tool call
  - [ ] Define hello world agent in gram notation
  - [ ] Include sayHello tool description (name, description, args, return type)
  - [ ] Document example as reference implementation
- [ ] Implement sayHello tool in tool library module
  - [ ] Create tool library module structure
  - [ ] Implement sayHello function (takes user name argument)
  - [ ] Register sayHello in tool library
- [ ] Design tool library interface and registry
  - [ ] Tool library registry API (simple for now)
  - [ ] Tool name to implementation mapping
  - [ ] Tool resolution logic (description → implementation)
  - [ ] Registry of "well-known tools" (starting point)
  - [ ] Keep design simple initially (composability deferred)
- [ ] Enhance execution environment with tool library binding
  - [ ] Add tool library parameter to execution environment creation
  - [ ] Implement tool resolution (match tool description to implementation)
  - [ ] Integrate tool binding with executeAgent function
  - [ ] Handle tool not found errors
  - [ ] Handle tool execution failures
  - [ ] Support both direct tool execution and tool library resolution
- [ ] Create integration test with mocked LLM provider
  - [ ] Mock LLM with two behaviors:
    - [ ] Response to any user message: call first available tool
    - [ ] Response to tool call result: stringify tool call into user message
  - [ ] Test tool library binding mechanism
  - [ ] Test tool invocation flow
  - [ ] Test error cases (tool not found, tool execution failure)
- [ ] Implement roundtrip serialization tests
  - [ ] Test: serialize agent → deserialize → execute (should work)
  - [ ] Test atomic agents roundtrip
  - [ ] Test compound agents roundtrip
  - [ ] Test agents with tools roundtrip
  - [ ] Test agents without tools roundtrip
  - [ ] Validate serialized agent structure matches original
- [ ] **Principle 3: Dual Testing Strategy**
  - [ ] Unit tests for gram serialization/deserialization
  - [ ] Unit tests for tool description schema
  - [ ] Unit tests for tool library registry
  - [ ] Unit tests for tool binding logic
  - [ ] Scenario test: hello world agent with tool call (end-to-end)
  - [ ] Scenario test: roundtrip serialization preserves agent behavior
- [ ] Document gram notation integration and usage
  - [ ] Update README with gram notation capabilities
  - [ ] Document tool library usage patterns
  - [ ] Document execution environment with tool binding

---

# Unplanned features to consider

Todo sometime:

- Pattern<Function> integration (may provide lisp-like scriptable language)
