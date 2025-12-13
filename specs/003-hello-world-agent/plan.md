# Implementation Plan: Hello World Agent with Tool Execution

**Branch**: `003-hello-world-agent` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/003-hello-world-agent/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Complete agent execution infrastructure by implementing tool creation, tool association with agents, and tool execution during agent runs. **The implementation begins with designing the gram notation format for tool specifications, including Hindley-Milner style type signatures (e.g., `(name: Text) --> IO Text`).** This design artifact (tool description format) will then be used for all subsequent implementation steps. The feature anchors the implementation in a concrete "hello world" example agent that uses the `sayHello` tool to respond to user greetings, demonstrating the complete tool execution flow end-to-end. The implementation builds upon existing execution infrastructure (Execution.hs) and LLM client (LLM.hs) to add tool support, enabling agents to extend their capabilities beyond the LLM's built-in knowledge.

## Technical Context

**Language/Version**: Haskell / GHC 2024 (GHC2024 language standard)  
**Primary Dependencies**: 
- base ^>=4.20.2.0 (Haskell base library)
- pattern (local path dependency from ../gram-hs/libs/pattern) - Pattern type for agent representation
- hashable ^>=1.4 (for Agent type instances)
- http-client ^>=0.7 (HTTP client for LLM API calls)
- http-client-tls ^>=0.3 (TLS support for HTTPS)
- aeson ^>=2.0 (JSON serialization for API requests/responses, tool schemas)
- bytestring ^>=0.11 (byte string handling)
- text ^>=2.0 (text handling)
- mtl ^>=2.3 (monad transformers for error handling)
- tasty ^>=1.4 (test framework)
- tasty-hunit ^>=0.10 (unit test support)
- tasty-quickcheck ^>=0.10 (property-based testing)

**Storage**: In-memory tool registry and conversation context (no persistence required for initial implementation)  
**Testing**: Tasty with HUnit (unit tests) and scenario tests  
**Target Platform**: Cross-platform (Haskell compiles to native binaries)  
**Project Type**: Single Haskell library with executable and test suite  
**Performance Goals**: 
- Tool creation: < 1 minute (100% success rate for valid inputs)
- Tool invocation detection and execution: < 2 seconds for typical tools (95% success rate for valid tool calls)
- Tool result integration: 95% success rate for incorporating tool results into agent responses
**Constraints**: 
- **Phase 0.5 (FIRST STEP)**: Must design gram notation format for tool specifications with Hindley-Milner style type signatures before any implementation
- Must support OpenAI function calling format (tool calls in LLM responses)
- Must generate JSON schemas automatically from gram type signatures (not manual schemas)
- Must validate tool parameters against JSON schemas before invocation
- Must handle tool execution errors gracefully
- Must support agents with zero or more tools (backward compatible with existing tool-free agents)
- Type system must ensure tool safety and correct parameter passing
**Scale/Scope**: 
- Initial scope: Single agent execution with tool support
- Support for conversation context with tool invocations across multiple exchanges
- Simple tool registry (no advanced tool composition or discovery)
- Focus on expressiveness and correctness (Principle 4)
- Hello world example as concrete demonstration and test case

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 0.5 tool description design and Phase 1 design.*

### Principle 1: Design-Driven Development
- [x] **User Goal Statement**: Clear user goal documented in spec.md - "Enable developers to create agents that can use tools during execution, demonstrated through a concrete 'hello world' example agent that uses the `sayHello` tool to respond to user greetings in friendly conversations"
- [x] **Design Validation**: Proposed design validated against user goal - **Phase 0.5 will design the tool description format (gram type signatures) as the first step**, then design artifacts (data-model.md, contracts/, quickstart.md) will demonstrate how tool creation, tool association, and tool execution satisfy the user goal
- [x] **Rationale**: Design decisions justified by user goal satisfaction - **Tool description design (Phase 0.5) establishes the foundation**, then tool type design enables tool creation (User Story 1), Agent tool specs field enables tool association (User Story 2), iterative execution loop enables tool execution (User Story 3), sayHello tool and hello world agent provide concrete example (User Story 4)

### Principle 2: Why Before How
- [x] **Why Documented**: Rationale documented in spec.md - "Agent execution infrastructure exists but lacks tool support. Without tool execution support, agents are limited to conversational responses using only the LLM's built-in knowledge. Tool support enables agents to perform actions, access external data, and extend their capabilities beyond what the LLM knows."
- [x] **Clarifying Questions**: Questions asked and answered - Spec indicates no clarifications needed
- [x] **Implementation Plan References Why**: This plan references the documented rationale in Summary section

### Principle 3: Dual Testing Strategy
- [x] **Unit Tests Planned**: Unit-level tests identified in spec.md for:
  - Tool creation with name, description, schema, invoke function
  - Tool accessors (name, description, schema retrieval)
  - Schema validation (valid and invalid parameters)
  - Tool association with agents
  - Tool retrieval from agents
  - Tool call detection in LLM responses
  - Tool invocation with parameters
  - Tool result handling
  - Error handling (tool not found, invalid parameters, execution failures)
  - Hello world agent creation
  - sayHello tool implementation
- [x] **Scenario Tests Planned**: Scenario tests identified in spec.md that simulate:
  - Creating a tool and verifying it can be accessed
  - Adding tools to an agent and verifying agent can access them
  - Executing an agent with tools and verifying tool invocation
  - Hello world agent using sayHello tool to respond to greetings
  - Multi-turn conversation with tool usage across exchanges
- [x] **Test Strategy**: Both unit and scenario testing approaches defined:
  - Unit tests: Tasty with HUnit for component-level testing
  - Scenario tests: Tasty with scenario test structure for user goal validation
  - Test organization: `tests/unit/` and `tests/scenario/` directories
  - Coverage: All tool operations, agent tool integration, execution paths, and hello world example

### Principle 4: Expressiveness and Correctness
- [x] **API Design**: APIs designed for intuitive use and clarity - **Tool specification uses gram type signatures (concise, self-documenting)**, tool creation API (createToolSpecification with type signature) is straightforward, tool association via Agent.agentToolSpecs field is clear, executeAgentWithLibrary handles tool execution automatically, contracts document API clearly
- [x] **Edge Cases**: Edge cases identified in spec.md:
  - Agent tries to invoke tool that doesn't exist
  - LLM provides invalid parameters (wrong type, missing required parameters)
  - Tool execution throws exception or fails
  - LLM requests multiple tools simultaneously
  - Tool takes too long to execute (timeout scenarios)
  - Tool invocations in conversation context
  - Agent has no tools but LLM requests tool call
  - Malformed tool call requests from LLM
- [x] **Documentation Plan**: Documentation strategy ensures accuracy and clarity - API contracts document all functions, data-model.md defines all entities, quickstart.md provides examples, research.md documents technical decisions

### Principle 5: Progressive Iteration
- [x] **Simplest Solution First**: Initial implementation will start with:
  - **Phase 0.5**: Design gram notation format for tool specifications (type signatures in Hindley-Milner style)
  - Simple tool creation API (name, description, gram type signature, invoke function)
  - Automatic JSON schema generation from gram type signatures
  - Direct tool association with agents (list of tool specifications)
  - Synchronous tool execution (tools complete before response generation continues)
  - Basic tool call detection from OpenAI function calling format
  - Simple parameter validation against JSON schemas (generated from type signatures)
  - In-memory tool registry (no advanced discovery or composition)
  - Hello world example with single sayHello tool
- [x] **Complexity Justification**: Advanced features deferred until user goals require them:
  - Tool library/discovery system - deferred (simple tool list sufficient)
  - Asynchronous tool execution - deferred (synchronous simpler, can add async later)
  - Advanced tool composition - deferred (single tool invocation sufficient)
  - Tool caching/memoization - deferred (not required by user goals)
  - Tool versioning - deferred (not required by user goals)
  - Multiple tool invocations in parallel - deferred (sequential sufficient initially)

## Implementation Phases

### Phase 0.5: Tool Description Design (FIRST STEP)

**Prerequisites**: Phase 0 research complete

**Purpose**: Design the gram notation format for tool specifications, including type signature representation in Hindley-Milner style. This design artifact will be used by all subsequent implementation steps.

**Deliverables**:
1. **Gram Type Signature Grammar**: Define the grammar for representing tool type signatures in gram notation (e.g., `(name: Text) --> IO Text`)
2. **ToolSpecification Gram Schema**: Design the complete gram schema for ToolSpecification including:
   - Tool name
   - Tool description
   - Type signature (in gram notation)
   - Schema generation rules (how to convert gram type signature to JSON schema)
3. **Type Signature Parser Design**: Design parser for gram type signatures
4. **Schema Generator Design**: Design algorithm to convert gram type signatures to JSON schemas
5. **Example ToolSpecification in Gram**: Complete example of sayHello tool in gram notation

**Output Artifacts**:
- `specs/003-hello-world-agent/tool-specification-gram.md` - Gram notation schema for tool specifications
- `specs/003-hello-world-agent/type-signature-grammar.md` - Grammar definition for type signatures
- `specs/003-hello-world-agent/examples/sayHello.gram` - Example tool specification in gram

**Dependencies**: 
- This phase must complete before any implementation begins
- All subsequent phases depend on this design

### Phase 1: Design & Contracts (Updated)

**Prerequisites**: Phase 0.5 complete (tool description design available)

**Changes**:
- ToolSpecification design now uses gram type signatures instead of manual JSON schemas
- Data model references gram type signature format
- Contracts updated to reflect gram-based tool specifications

## Project Structure

### Documentation (this feature)

```text
specs/003-hello-world-agent/
├── plan.md                        # This file (/speckit.plan command output)
├── research.md                    # Phase 0 output (/speckit.plan command)
├── tool-specification-gram.md     # Phase 0.5 output: Gram schema for tool specs
├── type-signature-grammar.md      # Phase 0.5 output: Type signature grammar
├── data-model.md                  # Phase 1 output (updated to use gram type signatures)
├── quickstart.md                  # Phase 1 output
├── contracts/                     # Phase 1 output (/speckit.plan command)
│   └── PatternAgent-Tool.md       # Updated for gram type signatures
├── examples/                      # Phase 0.5 output
│   └── sayHello.gram              # Example tool spec in gram notation
└── tasks.md                       # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
src/PatternAgent/
├── Agent.hs             # Update: Add tools field to Agent type
├── Tool.hs              # NEW: Implement tool creation and management
├── Execution.hs         # Update: Add tool execution logic
├── LLM.hs               # Update: Add tool definitions to requests, parse tool calls from responses
├── Context.hs            # Existing: Conversation context (no changes)
├── Core.hs               # Existing: Core types (no changes)
├── Types.hs              # Existing: Type aliases (no changes)
├── Env.hs                # Existing: Environment management (no changes)
└── HelloWorld.hs         # NEW: Hello world example agent and sayHello tool

tests/
├── unit/
│   ├── AgentTest.hs      # Update: Add tool association tests
│   └── ToolTest.hs       # NEW: Unit tests for tool creation and validation
└── scenario/
    ├── AgentIdentityTest.hs  # Existing (no changes)
    └── HelloWorldTest.hs     # NEW: Scenario test for hello world agent

app/
└── Main.hs               # Existing: CLI (no changes, hello world example in tests)
```

**Structure Decision**: Single Haskell library project. Tool functionality added to existing modules (Agent.hs, Execution.hs, LLM.hs) with new Tool.hs module for tool management. Hello world example implemented as separate module (HelloWorld.hs) and tested via scenario test. This structure maintains existing organization while adding tool support incrementally.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations - implementation follows Principle 5 (Progressive Iteration) by starting with simplest solution that meets user goals.
