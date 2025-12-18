# Implementation Plan: Agent Execution with Scenario Tests, Interactive CLI, and Observability

**Branch**: `004-agent-execution` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/004-agent-execution/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Complete the execution environment by implementing comprehensive scenario tests for agents with zero, one, and multiple tools; adding an interactive CLI mode (`--interactive` or `-i` flag) for real-time conversations; and establishing observability infrastructure for execution traces and performance metrics. The implementation builds upon existing execution infrastructure (Execution.hs, Context.hs, LLM.hs) and CLI (Main.hs) to add these capabilities. Builtin tools (`getCurrentTime`, `calculate`, `formatText`) will be added to enable realistic multi-tool scenario tests. The feature enables developers to confidently verify agent execution across different tool configurations, have natural interactive conversations with agents, and understand agent behavior through observability.

## Technical Context

**Language/Version**: Haskell / GHC 2024 (GHC2024 language standard)  
**Primary Dependencies**: 
- base ^>=4.20.2.0 (Haskell base library)
- pattern (local path dependency from ../gram-hs/libs/pattern) - Pattern type for agent representation
- gram (local path dependency from ../gram-hs/libs/gram) - Gram notation parsing
- subject (local path dependency from ../gram-hs/libs/subject) - Subject type system
- hashable ^>=1.4 (for Agent type instances)
- http-client ^>=0.7 (HTTP client for LLM API calls)
- http-client-tls ^>=0.3 (TLS support for HTTPS)
- aeson ^>=2.1 (JSON serialization for API requests/responses, tool schemas, observability data export)
- aeson-pretty ^>=0.8 (Pretty JSON formatting for observability export)
- bytestring ^>=0.11 (byte string handling)
- text ^>=2.0 (text handling)
- mtl ^>=2.3 (monad transformers for error handling)
- lens ^>=5.3 (lens library for data access)
- containers ^>=0.6 (Map, Set data structures)
- vector (vector data structures)
- time (time handling for getCurrentTime tool and observability timestamps)
- tasty ^>=1.4 (test framework)
- tasty-hunit ^>=0.10 (unit test support)
- tasty-quickcheck ^>=0.10 (property-based testing)

**Storage**: In-memory for conversation context, observability data (no persistence required for initial implementation)  
**Testing**: Tasty with HUnit (unit tests) and scenario tests  
**Target Platform**: Cross-platform (Haskell compiles to native binaries)  
**Project Type**: Single Haskell library with executable and test suite  
**Performance Goals**: 
- Interactive CLI mode activation: < 5 seconds (100% success rate)
- Agent response display in interactive mode: < 3 seconds for typical requests (95% success rate)
- Observability trace capture: < 10% overhead on execution time
- Observability data access: < 1 second after execution completion (100% success rate)
**Constraints**: 
- Must support existing CLI argument parsing structure (extend parseArgs function)
- Must maintain backward compatibility with existing batch message processing mode
- Must support standard terminal input/output for interactive mode
- Must handle terminal control sequences (Ctrl+D, Ctrl+C) gracefully
- Observability must be optional (can be enabled/disabled via flags)
- Observability data must be exportable in standard formats (JSON, CSV)
- Builtin tools must be simple and complementary (getCurrentTime, calculate, formatText)
- Scenario tests must use mocked LLM providers for deterministic behavior
- Must maintain conversation context across interactive mode exchanges
**Scale/Scope**: 
- Initial scope: Single agent execution with zero/one/multiple tools
- Interactive CLI mode for single-user, single-agent sessions
- Observability for single execution traces (no aggregation across multiple executions)
- Builtin tools: 2-3 tools for multi-tool scenario testing
- Focus on expressiveness and correctness (Principle 4)
- Progressive iteration: Start simple, add complexity only when needed (Principle 5)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Principle 1: Design-Driven Development
- [x] **User Goal Statement**: Clear user goal documented in spec.md - "Enable developers to execute single agents (with zero or more tools) through comprehensive scenario tests, an interactive CLI mode for real-time conversations, and observability features that provide insight into agent execution behavior"
- [x] **Design Validation**: Proposed design validated against user goal - **Phase 1 design artifacts (data-model.md, contracts/, quickstart.md) demonstrate how scenario tests, interactive CLI mode, and observability infrastructure satisfy the user goal**
- [x] **Rationale**: Design decisions justified by user goal satisfaction - **Scenario tests enable verification across tool configurations (User Stories 1-3), interactive CLI mode enables natural conversations (User Story 4), observability enables understanding and debugging (User Story 5)**

### Principle 2: Why Before How
- [x] **Why Documented**: Rationale documented in spec.md - "The execution environment currently supports basic agent execution, but lacks comprehensive scenario tests, interactive CLI mode, and observability capabilities. Without these capabilities, developers cannot confidently verify execution, have natural interactive conversations, or understand agent behavior."
- [x] **Clarifying Questions**: Questions asked and answered - Builtin tools clarified (2-3 complementary tools), memory system deferred
- [x] **Implementation Plan References Why**: This plan references the documented rationale in Summary section

### Principle 3: Dual Testing Strategy
- [x] **Unit Tests Planned**: Unit-level tests identified in spec.md for:
  - Agent execution without tools
  - Tool detection and invocation
  - Interactive CLI mode activation and input handling
  - Observability trace capture and metric collection
  - Builtin tool implementations (getCurrentTime, calculate, formatText)
  - Error handling for all scenarios
- [x] **Scenario Tests Planned**: Scenario tests identified in spec.md that simulate:
  - Agent execution with zero tools (User Story 1)
  - Agent execution with one tool (User Story 2)
  - Agent execution with multiple tools (User Story 3)
  - Interactive CLI mode conversations (User Story 4)
  - Observability data capture and access (User Story 5)
- [x] **Test Strategy**: Both unit and scenario testing approaches defined:
  - Unit tests: Tasty with HUnit for component-level testing
  - Scenario tests: Tasty with scenario test structure for user goal validation
  - Test organization: `tests/unit/` and `tests/scenario/` directories
  - Coverage: All execution scenarios, interactive CLI flows, observability features, and builtin tools

### Principle 4: Expressiveness and Correctness
- [x] **API Design**: APIs designed for intuitive use and clarity - **Interactive CLI mode uses standard flags (`--interactive` or `-i`), observability uses simple enable/disable flags, builtin tools follow existing tool pattern, scenario tests use clear Given/When/Then structure**
- [x] **Edge Cases**: Edge cases identified in spec.md:
  - Agent with zero tools receives input requiring tools
  - Interactive CLI handles long responses, empty messages, interruptions
  - Observability handles high-frequency execution, large data, fast responses
  - Multi-tool agents invoke tools in sequence vs. parallel
  - Terminal control sequences and special characters
- [x] **Documentation Plan**: Documentation strategy ensures accuracy and clarity - **quickstart.md will demonstrate interactive CLI usage, observability features, and scenario test examples; contracts/ will document API interfaces**

### Principle 5: Progressive Iteration
- [x] **Simplest Solution First**: Initial implementation uses simplest approach:
  - Interactive CLI: Basic readline loop with standard exit commands
  - Observability: In-memory storage, simple trace structure, basic metrics
  - Builtin tools: Simple implementations (getCurrentTime, calculate, formatText)
  - Scenario tests: Direct test cases without complex test frameworks
- [x] **Complexity Justified**: Additional complexity only when user goals require:
  - Advanced terminal features (if basic readline insufficient)
  - Persistent observability storage (if in-memory insufficient)
  - Complex builtin tools (if simple tools insufficient for testing)
- [x] **User Goal Alignment**: Each feature directly addresses stated user goals without unnecessary abstraction

## Project Structure

### Documentation (this feature)

```text
specs/004-agent-execution/
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
│   ├── Language/
│   │   └── Core.hs          # Agent, Tool types (existing)
│   └── Runtime/
│       ├── Execution.hs    # executeAgent, executeAgentWithLibrary (existing, extend for observability)
│       ├── Context.hs       # ConversationContext (existing)
│       ├── LLM.hs           # LLM client (existing)
│       ├── ToolLibrary.hs   # ToolLibrary (existing)
│       ├── BuiltinTools.hs  # Builtin tool implementations (extend with getCurrentTime, calculate, formatText)
│       └── Logging.hs       # Structured logging (existing)
│
app/
└── Main.hs                  # CLI entry point (extend with interactive mode)

tests/
├── unit/
│   ├── ExecutionTest.hs     # Unit tests for execution (existing, extend)
│   ├── CLITest.hs           # Unit tests for CLI (extend with interactive mode tests)
│   └── BuiltinToolsTest.hs  # Unit tests for builtin tools (new)
└── scenario/
    ├── ZeroToolExecutionTest.hs      # Scenario test: agent with zero tools (new)
    ├── OneToolExecutionTest.hs       # Scenario test: agent with one tool (new)
    ├── MultiToolExecutionTest.hs     # Scenario test: agent with multiple tools (new)
    ├── InteractiveCLITest.hs         # Scenario test: interactive CLI mode (new)
    └── ObservabilityTest.hs          # Scenario test: observability features (new)
```

**Structure Decision**: Single Haskell project structure maintained. Interactive CLI mode extends existing Main.hs CLI. Observability extends existing Execution.hs. Builtin tools extend existing BuiltinTools.hs. Scenario tests follow existing test organization in `tests/scenario/`. This maintains consistency with existing codebase structure while adding new capabilities.

## Complexity Tracking

> **No violations - all features align with Principle 5 (Progressive Iteration)**
