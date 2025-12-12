# Implementation Plan: Basic LLM Agent

**Branch**: `002-llm-agent` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/002-llm-agent/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Enable developers to create and configure LLM-powered agents that can understand natural language, make decisions, generate responses, and interact with tools. This feature implements the core agent type that leverages Large Language Models for reasoning, providing the fundamental building block for all agent-based functionality including future multi-agent systems, decomposition, and composition features.

## Technical Context

**Language/Version**: Haskell / GHC 2024 (GHC2024 language standard)  
**Primary Dependencies**: 
- base ^>=4.20.2.0 (Haskell base library)
- pattern (local path dependency from ../gram-hs/libs/pattern) - Pattern type for agent representation
- hashable ^>=1.4 (for Agent type instances)
- http-client ^>=0.7 (HTTP client for LLM API calls)
- http-client-tls ^>=0.3 (TLS support for HTTPS)
- aeson ^>=2.0 (JSON serialization for API requests/responses)
- bytestring ^>=0.11 (byte string handling)
- text ^>=2.0 (text handling)
- mtl ^>=2.3 (monad transformers for error handling)
- tasty ^>=1.4 (test framework)
- tasty-hunit ^>=0.10 (unit test support)
- tasty-quickcheck ^>=0.10 (property-based testing)

**Storage**: In-memory conversation context (no persistence required for initial implementation)  
**Testing**: Tasty with HUnit (unit tests) and QuickCheck (property-based tests)  
**Target Platform**: Cross-platform (Haskell compiles to native binaries)  
**Project Type**: Single Haskell library with executable and test suite  
**Performance Goals**: 
- Agent creation: < 30 seconds (100% success rate for valid inputs)
- Response generation: < 5 seconds for 95% of typical requests
- Tool invocation: 95% success rate for valid tool calls
**Constraints**: 
- Must support multiple LLM providers (initially focus on one, design for extensibility)
- Must handle LLM API failures gracefully
- Must support both synchronous and asynchronous execution patterns
- Type system must ensure tool safety and correct parameter passing
**Scale/Scope**: 
- Initial scope: Single agent execution with basic tool support
- Support for conversation context across multiple exchanges
- Extensible design for future multi-agent features
- Focus on expressiveness and correctness (Principle 4)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Principle 1: Design-Driven Development
- [x] **User Goal Statement**: Clear user goal documented in spec.md - "Enable developers to create and configure LLM-powered agents that can understand natural language, make decisions, generate responses, and interact with tools"
- [ ] **Design Validation**: Proposed design validated against user goal (to be completed in Phase 1)
- [ ] **Rationale**: Design decisions justified by user goal satisfaction (to be completed in Phase 1)

### Principle 2: Why Before How
- [x] **Why Documented**: Rationale documented in spec.md - "The pattern-agent framework needs a core agent type that leverages LLMs for reasoning and decision-making. This is the fundamental building block that enables all agent-based functionality."
- [x] **Clarifying Questions**: Questions asked and answered - Spec indicates no clarifications needed
- [x] **Implementation Plan References Why**: This plan references the documented rationale in Summary section

### Principle 3: Dual Testing Strategy
- [x] **Unit Tests Planned**: Unit-level tests identified in spec.md for:
  - Agent creation with name, description, model
  - Instruction storage and application
  - Tool registration and discovery
  - Tool invocation with parameters
  - Response generation
  - Conversation context management
- [x] **Scenario Tests Planned**: Scenario tests identified in spec.md that simulate:
  - Creating and executing a basic conversational agent
  - Creating an agent with tools and verifying tool usage
  - Multi-turn conversation with context retention
  - Agent using multiple tools in a single interaction
- [x] **Test Strategy**: Both unit and scenario testing approaches defined:
  - Unit tests: Tasty with HUnit for component-level testing
  - Scenario tests: Tasty with scenario test structure for user goal validation
  - Test organization: `test/unit/` and `test/scenario/` directories
  - Coverage: All agent operations, tool integration, and execution paths

### Principle 4: Expressiveness and Correctness
- [ ] **API Design**: APIs designed for intuitive use and clarity (to be completed in Phase 1)
- [x] **Edge Cases**: Edge cases identified in spec.md:
  - Missing required parameters (name, model)
  - Invalid or malformed instructions
  - Tool doesn't exist or fails
  - LLM API failures or timeouts
  - Response length limits
  - Conversation context size limits
  - Duplicate agent names
  - Tool usage when no tools available
- [ ] **Documentation Plan**: Documentation strategy ensures accuracy and clarity (to be completed in Phase 1)

### Principle 5: Progressive Iteration
- [x] **Simplest Solution First**: Initial implementation will start with:
  - Simple agent creation API (name, description, model, instructions)
  - Basic tool integration (function tools only initially)
  - Synchronous execution pattern (async can be added later if needed)
  - In-memory conversation context (persistence deferred)
- [x] **Complexity Justification**: Advanced features deferred until user goals require them:
  - Multi-agent coordination (deferred to composition feature)
  - Advanced planning/thinking (deferred if not needed for basic use cases)
  - Code execution (deferred to future feature)
  - Structured input/output schemas (deferred if not needed initially)

## Project Structure

### Documentation (this feature)

```text
specs/002-llm-agent/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
src/
├── PatternAgent/
│   ├── Core.hs          # Core PatternAgent operations (from foundation)
│   ├── Types.hs         # PatternAgent type definitions (from foundation)
│   ├── Agent.hs         # NEW: LLM Agent type and operations
│   ├── Tool.hs          # NEW: Tool abstraction and function tool implementation
│   ├── Execution.hs     # NEW: Agent execution and response generation
│   └── Context.hs       # NEW: Conversation context management
│
tests/
├── unit/
│   ├── AgentTest.hs     # Unit tests for agent creation and configuration
│   ├── ToolTest.hs      # Unit tests for tool registration and invocation
│   └── ExecutionTest.hs # Unit tests for execution logic
└── scenario/
    ├── ConversationalAgentTest.hs  # Scenario: basic conversational agent
    ├── ToolAgentTest.hs            # Scenario: agent with tool integration
    └── MultiTurnConversationTest.hs # Scenario: conversation context retention
```

**Structure Decision**: Extend existing `src/PatternAgent/` module structure with new modules for LLM agent functionality. This maintains consistency with foundation setup and supports incremental expansion. Test structure follows dual testing strategy with separate unit and scenario test directories.

## Phase 0: Research Complete

**Status**: ✅ Complete  
**Output**: [research.md](research.md)

### Research Findings

All technical unknowns resolved:

1. **LLM API Client**: Use `http-client` and `http-client-tls` for HTTP requests, `aeson` for JSON serialization
2. **LLM Provider**: Start with OpenAI API, design with `LLMProvider` typeclass for extensibility
3. **Tool Representation**: `Tool` type with schema and invocation function, type-safe invocation with JSON conversion
4. **Conversation Context**: In-memory `[Message]` list, simple message-based history
5. **Error Handling**: `Either AgentError a` for recoverable errors, exceptions for programming errors

All NEEDS CLARIFICATION markers resolved. Design ready for Phase 1.

## Phase 1: Design & Contracts Complete

**Status**: ✅ Complete  
**Outputs**: 
- [data-model.md](data-model.md) - Core data structures and entities
- [contracts/PatternAgent-Agent.md](contracts/PatternAgent-Agent.md) - Agent creation and configuration API
- [contracts/PatternAgent-Execution.md](contracts/PatternAgent-Execution.md) - Agent execution API
- [contracts/PatternAgent-Tool.md](contracts/PatternAgent-Tool.md) - Tool system API
- [quickstart.md](quickstart.md) - Developer quick start guide

### Design Validation (Principle 1)

**Design Validated Against User Goal**: ✅

The proposed design enables developers to:
- ✅ Create agents with name, description, model, and instructions (FR-001, FR-002, FR-003, FR-004)
- ✅ Equip agents with tools that extend capabilities (FR-006, FR-007)
- ✅ Execute agents with user input and receive responses (FR-008, FR-009, FR-010)
- ✅ Maintain conversation context across multiple interactions (FR-011, FR-012)
- ✅ Use tools during agent execution (FR-013, FR-014, FR-015)

**Design Decisions Justified**:

1. **Simple Agent Creation API**: Direct function calls (`createAgent`) provide clear, expressive API (Principle 4: Expressiveness)
2. **Tool Type with Schema**: JSON schema approach provides flexibility while maintaining type safety (Principle 4: Correctness)
3. **In-Memory Context**: Simple list-based context sufficient for initial implementation (Principle 5: Progressive Iteration)
4. **Either for Errors**: Type-safe error handling ensures correctness (Principle 4: Correctness)
5. **Provider Abstraction**: Typeclass-based design allows extensibility without complexity (Principle 5: Progressive Iteration)

### API Design (Principle 4)

**APIs Designed for Intuitive Use**: ✅

- `createAgent` function clearly shows all required and optional parameters
- Tool creation API separates schema definition from invocation logic
- Execution API (`executeAgent`) has clear input/output types
- Context management functions (`addMessage`, `emptyContext`) are self-documenting
- Error types (`AgentError`) provide clear error categorization

### Documentation Plan (Principle 4)

**Documentation Strategy Defined**: ✅

- **API Contracts**: Complete function signatures, preconditions, postconditions, examples
- **Data Model**: All entities documented with fields, validation rules, relationships
- **Quick Start Guide**: Step-by-step examples for common use cases
- **Error Handling**: Clear error types and handling patterns documented

## Constitution Check (Post-Design)

*Re-checked after Phase 1 design completion.*

### Principle 1: Design-Driven Development
- [x] **User Goal Statement**: ✅ Clear user goal documented
- [x] **Design Validation**: ✅ Design validated against user goal (see Phase 1 section above)
- [x] **Rationale**: ✅ Design decisions justified by user goal satisfaction (see Phase 1 section above)

### Principle 4: Expressiveness and Correctness
- [x] **API Design**: ✅ APIs designed for intuitive use (see Phase 1 section above)
- [x] **Edge Cases**: ✅ Edge cases identified and handling planned
- [x] **Documentation Plan**: ✅ Documentation strategy ensures accuracy and clarity (see Phase 1 section above)

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [None identified at this time] | | |
