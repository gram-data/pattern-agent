# Tasks: Agent Execution with Scenario Tests, Interactive CLI, and Observability

**Input**: Design documents from `/specs/004-agent-execution/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/
**Feature**: Agent Execution with Scenario Tests, Interactive CLI, and Observability

**Tests**: Per Principle 3 (Dual Testing Strategy), all features MUST include both unit tests and scenario tests. Scenario tests simulate user goal satisfaction end-to-end.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/`, `app/` at repository root
- Paths shown below assume single project structure from plan.md

## Module Structure (Language vs Runtime)

**Language Modules** (portable specification):
- `src/PatternAgent/Language/Core.hs` - Agent, Tool as Pattern Subject, lenses, creation

**Runtime Modules** (Haskell-specific implementation):
- `src/PatternAgent/Runtime/Execution.hs` - Execution engine (extend for observability)
- `src/PatternAgent/Runtime/ToolLibrary.hs` - ToolImpl, ToolLibrary, tool binding
- `src/PatternAgent/Runtime/BuiltinTools.hs` - Builtin tool implementations (extend with getCurrentTime, calculate, formatText)
- `src/PatternAgent/Runtime/LLM.hs` - LLM API client
- `src/PatternAgent/Runtime/Context.hs` - ConversationContext
- `src/PatternAgent/Runtime/Logging.hs` - Structured logging

**CLI Module**:
- `app/Main.hs` - CLI entry point (extend with interactive mode)

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and dependency setup

- [ ] T001 Update pattern-agent.cabal with time dependency if needed (verify existing dependencies are sufficient for getCurrentTime tool)
- [ ] T002 [P] Verify test directory structure exists: tests/unit/ and tests/scenario/
- [ ] T003 [P] Verify existing module structure: src/PatternAgent/Runtime/ and app/ directories exist

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T004 [P] Verify existing Execution.hs has executeAgentWithLibrary function (required for all user stories)
- [ ] T005 [P] Verify existing Context.hs has ConversationContext type and functions (required for all user stories)
- [ ] T006 [P] Verify existing BuiltinTools.hs has createToolLibraryFromAgent function (required for User Story 3)
- [ ] T007 [P] Verify existing Main.hs has parseArgs and CLIMode data type (required for User Story 4)

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Scenario Tests for Agent Execution with Zero Tools (Priority: P1) 🎯 MVP

**Goal**: Enable developers to validate execution of agents with no tools to ensure basic conversational agents work correctly.

**Independent Test**: Can be fully tested by creating an agent without tools, executing it with various inputs, and verifying it generates appropriate responses. This delivers confidence that basic agent execution works correctly.

### Tests for User Story 1 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [ ] T008 [P] [US1] Scenario test: Agent with zero tools executes and generates conversational response without tool calls in tests/scenario/ZeroToolExecutionTest.hs
- [ ] T009 [P] [US1] Scenario test: Agent with zero tools handles LLM tool call request gracefully without crashing in tests/scenario/ZeroToolExecutionTest.hs
- [ ] T010 [P] [US1] Scenario test: Agent with zero tools maintains conversation context across multiple message exchanges in tests/scenario/ZeroToolExecutionTest.hs

**Unit Tests** (component correctness):
- [ ] T011 [P] [US1] Unit test: Agent execution without tools succeeds in tests/unit/ExecutionTest.hs
- [ ] T012 [P] [US1] Unit test: Response generation using only LLM capabilities in tests/unit/ExecutionTest.hs
- [ ] T013 [P] [US1] Unit test: Execution handles tool call requests gracefully when no tools available in tests/unit/ExecutionTest.hs

### Implementation for User Story 1 (Principle 4: Expressiveness and Correctness)

- [ ] T014 [US1] Verify executeAgentWithLibrary handles agents with empty tool list correctly in src/PatternAgent/Runtime/Execution.hs
- [ ] T015 [US1] Verify execution environment gracefully handles LLM tool call requests when agent has no tools in src/PatternAgent/Runtime/Execution.hs
- [ ] T016 [US1] Ensure conversation context is maintained correctly for zero-tool agents in src/PatternAgent/Runtime/Execution.hs

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Developers can execute agents with zero tools and verify basic conversational behavior.

---

## Phase 4: User Story 2 - Scenario Tests for Agent Execution with One Tool (Priority: P1)

**Goal**: Enable developers to validate execution of agents with a single tool to ensure tool integration works correctly.

**Independent Test**: Can be fully tested by creating an agent with one tool, executing it with inputs that trigger tool usage, and verifying the tool is invoked and results are incorporated. This delivers confidence that tool execution works correctly.

### Tests for User Story 2 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [ ] T017 [P] [US2] Scenario test: Agent with one tool invokes tool and incorporates result into response in tests/scenario/OneToolExecutionTest.hs
- [ ] T018 [P] [US2] Scenario test: Agent with one tool returns tool result to LLM and generates final response in tests/scenario/OneToolExecutionTest.hs
- [ ] T019 [P] [US2] Scenario test: Agent with one tool handles tool invocation failure gracefully in tests/scenario/OneToolExecutionTest.hs

**Unit Tests** (component correctness):
- [ ] T020 [P] [US2] Unit test: Execution environment detects tool call requests from LLM in tests/unit/ExecutionTest.hs
- [ ] T021 [P] [US2] Unit test: Tools are invoked with correct parameters in tests/unit/ExecutionTest.hs
- [ ] T022 [P] [US2] Unit test: Tool results are properly formatted and returned to LLM in tests/unit/ExecutionTest.hs
- [ ] T023 [P] [US2] Unit test: Tool execution errors are caught and handled appropriately in tests/unit/ExecutionTest.hs

### Implementation for User Story 2 (Principle 4: Expressiveness and Correctness)

- [ ] T024 [US2] Verify executeAgentWithLibrary correctly detects and invokes single tool in src/PatternAgent/Runtime/Execution.hs
- [ ] T025 [US2] Verify tool results are properly integrated into agent responses in src/PatternAgent/Runtime/Execution.hs
- [ ] T026 [US2] Ensure error handling for tool execution failures works correctly in src/PatternAgent/Runtime/Execution.hs

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Developers can execute agents with zero or one tool and verify tool integration.

---

## Phase 5: User Story 3 - Scenario Tests for Agent Execution with Multiple Tools (Priority: P1)

**Goal**: Enable developers to validate execution of agents with multiple tools to ensure agents can select and use appropriate tools.

**Independent Test**: Can be fully tested by creating an agent with multiple tools, executing it with inputs that require different tools, and verifying the correct tools are selected and invoked. This delivers confidence that multi-tool agent execution works correctly.

**Note**: Builtin tools (getCurrentTime, calculate, formatText) will be provided to enable realistic multi-tool scenario tests.

### Tests for User Story 3 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [ ] T027 [P] [US3] Scenario test: Agent with multiple tools selects and invokes appropriate tool in tests/scenario/MultiToolExecutionTest.hs
- [ ] T028 [P] [US3] Scenario test: Agent with multiple tools invokes multiple tools in sequence and incorporates all results in tests/scenario/MultiToolExecutionTest.hs
- [ ] T029 [P] [US3] Scenario test: Agent with multiple tools handles tool failure without affecting other available tools in tests/scenario/MultiToolExecutionTest.hs

**Unit Tests** (component correctness):
- [ ] T030 [P] [US3] Unit test: Agents can select appropriate tools from multiple options in tests/unit/ExecutionTest.hs
- [ ] T031 [P] [US3] Unit test: Agents can invoke multiple tools in sequence in tests/unit/ExecutionTest.hs
- [ ] T032 [P] [US3] Unit test: Tool failures don't affect other available tools in tests/unit/ExecutionTest.hs
- [ ] T033 [P] [US3] Unit test: getCurrentTime tool implementation in tests/unit/BuiltinToolsTest.hs
- [ ] T034 [P] [US3] Unit test: calculate tool implementation in tests/unit/BuiltinToolsTest.hs
- [ ] T035 [P] [US3] Unit test: formatText tool implementation in tests/unit/BuiltinToolsTest.hs

### Implementation for User Story 3 (Principle 4: Expressiveness and Correctness)

**Builtin Tools Implementation**:
- [ ] T036 [US3] Implement getCurrentTimeToolImpl function in src/PatternAgent/Runtime/BuiltinTools.hs
- [ ] T037 [US3] Implement calculateToolImpl function in src/PatternAgent/Runtime/BuiltinTools.hs
- [ ] T038 [US3] Implement formatTextToolImpl function in src/PatternAgent/Runtime/BuiltinTools.hs
- [ ] T039 [US3] Extend createToolLibraryFromAgent to recognize and register getCurrentTime tool in src/PatternAgent/Runtime/BuiltinTools.hs
- [ ] T040 [US3] Extend createToolLibraryFromAgent to recognize and register calculate tool in src/PatternAgent/Runtime/BuiltinTools.hs
- [ ] T041 [US3] Extend createToolLibraryFromAgent to recognize and register formatText tool in src/PatternAgent/Runtime/BuiltinTools.hs

**Multi-Tool Execution**:
- [ ] T042 [US3] Verify executeAgentWithLibrary correctly handles agents with multiple tools in src/PatternAgent/Runtime/Execution.hs
- [ ] T043 [US3] Verify tool selection from multiple options works correctly in src/PatternAgent/Runtime/Execution.hs
- [ ] T044 [US3] Verify multiple tool invocations in sequence work correctly in src/PatternAgent/Runtime/Execution.hs
- [ ] T045 [US3] Ensure tool failures don't affect other available tools in src/PatternAgent/Runtime/Execution.hs

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently. Developers can execute agents with zero, one, or multiple tools and verify tool selection and execution.

---

## Phase 6: User Story 4 - Interactive CLI Mode (Priority: P1)

**Goal**: Enable developers to have real-time conversations with agents through an interactive CLI mode, allowing natural back-and-forth exchanges without requiring all messages to be specified upfront.

**Independent Test**: Can be fully tested by launching the CLI in interactive mode, sending messages to an agent, and verifying responses are displayed immediately with the ability to continue the conversation. This delivers a natural, real-time interaction experience.

### Tests for User Story 4 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [ ] T046 [P] [US4] Scenario test: CLI launches in interactive mode and processes messages immediately in tests/scenario/InteractiveCLITest.hs
- [ ] T047 [P] [US4] Scenario test: Interactive CLI maintains conversation context across multiple message exchanges in tests/scenario/InteractiveCLITest.hs
- [ ] T048 [P] [US4] Scenario test: Interactive CLI exits gracefully with exit commands (exit, quit, Ctrl+D) in tests/scenario/InteractiveCLITest.hs
- [ ] T049 [P] [US4] Scenario test: Interactive CLI displays tool calls and results appropriately in tests/scenario/InteractiveCLITest.hs

**Unit Tests** (component correctness):
- [ ] T050 [P] [US4] Unit test: CLI recognizes --interactive flag in tests/unit/CLITest.hs
- [ ] T051 [P] [US4] Unit test: CLI recognizes -i flag in tests/unit/CLITest.hs
- [ ] T052 [P] [US4] Unit test: CLI accepts and processes user input in interactive mode in tests/unit/CLITest.hs
- [ ] T053 [P] [US4] Unit test: CLI displays agent responses correctly in interactive mode in tests/unit/CLITest.hs
- [ ] T054 [P] [US4] Unit test: CLI handles exit commands correctly in tests/unit/CLITest.hs
- [ ] T055 [P] [US4] Unit test: Conversation context is maintained across interactive exchanges in tests/unit/CLITest.hs

### Implementation for User Story 4 (Principle 4: Expressiveness and Correctness)

**CLI Mode Extension**:
- [ ] T056 [US4] Add InteractiveMode constructor to CLIMode data type in app/Main.hs
- [ ] T057 [US4] Extend parseArgs function to recognize --interactive flag in app/Main.hs
- [ ] T058 [US4] Extend parseArgs function to recognize -i flag in app/Main.hs
- [ ] T059 [US4] Implement handleInteractiveMode function with getLine input loop in app/Main.hs
- [ ] T060 [US4] Add exit command handling (exit, quit) in handleInteractiveMode in app/Main.hs
- [ ] T061 [US4] Add EOF handling (Ctrl+D) in handleInteractiveMode in app/Main.hs
- [ ] T062 [US4] Load agent from gram file at interactive mode start in handleInteractiveMode in app/Main.hs
- [ ] T063 [US4] Create tool library from agent's tools at interactive mode start in handleInteractiveMode in app/Main.hs
- [ ] T064 [US4] Initialize empty conversation context at interactive mode start in handleInteractiveMode in app/Main.hs
- [ ] T065 [US4] Maintain conversation context across loop iterations in handleInteractiveMode in app/Main.hs
- [ ] T066 [US4] Display user messages with appropriate formatting in handleInteractiveMode in app/Main.hs
- [ ] T067 [US4] Display agent responses with appropriate formatting in handleInteractiveMode in app/Main.hs
- [ ] T068 [US4] Display tool calls and results with appropriate formatting in handleInteractiveMode in app/Main.hs
- [ ] T069 [US4] Handle empty input gracefully (skip or prompt again) in handleInteractiveMode in app/Main.hs
- [ ] T070 [US4] Update printUsage function to document --interactive and -i flags in app/Main.hs
- [ ] T071 [US4] Add error handling for gram file loading failures in handleInteractiveMode in app/Main.hs
- [ ] T072 [US4] Add error handling for agent parsing failures in handleInteractiveMode in app/Main.hs
- [ ] T073 [US4] Add error handling for tool library creation failures in handleInteractiveMode in app/Main.hs
- [ ] T074 [US4] Add error handling for agent execution failures (allow retry) in handleInteractiveMode in app/Main.hs

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should all work independently. Developers can execute agents with various tool configurations and have interactive conversations through the CLI.

---

## Phase 7: User Story 5 - Observability Infrastructure (Priority: P2)

**Goal**: Enable developers to understand agent execution behavior through execution traces and performance metrics.

**Independent Test**: Can be fully tested by executing an agent with observability enabled and verifying that execution traces, metrics, and debugging information are captured and accessible. This delivers visibility into agent execution behavior.

### Tests for User Story 5 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [ ] T075 [P] [US5] Scenario test: Execution traces are captured showing sequence of operations in tests/scenario/ObservabilityTest.hs
- [ ] T076 [P] [US5] Scenario test: Performance metrics are captured (execution time, token usage, tool invocation counts) in tests/scenario/ObservabilityTest.hs
- [ ] T077 [P] [US5] Scenario test: Execution data can be accessed programmatically in tests/scenario/ObservabilityTest.hs
- [ ] T078 [P] [US5] Scenario test: Execution data can be exported in standard formats (JSON, CSV) in tests/scenario/ObservabilityTest.hs

**Unit Tests** (component correctness):
- [ ] T079 [P] [US5] Unit test: Execution traces are captured during agent execution in tests/unit/ExecutionTest.hs
- [ ] T080 [P] [US5] Unit test: Performance metrics are collected accurately in tests/unit/ExecutionTest.hs
- [ ] T081 [P] [US5] Unit test: Execution data can be accessed programmatically in tests/unit/ExecutionTest.hs
- [ ] T082 [P] [US5] Unit test: Trace export to JSON format works correctly in tests/unit/ExecutionTest.hs
- [ ] T083 [P] [US5] Unit test: Metrics export to CSV format works correctly in tests/unit/ExecutionTest.hs

### Implementation for User Story 5 (Principle 4: Expressiveness and Correctness)

**Observability Types**:
- [ ] T084 [US5] Define TraceEvent data type (LLMCallEvent, ToolInvocationEvent, ContextUpdateEvent) in src/PatternAgent/Runtime/Execution.hs
- [ ] T085 [US5] Define ExecutionTrace data type with traceEvents, traceStartTime, traceEndTime in src/PatternAgent/Runtime/Execution.hs
- [ ] T086 [US5] Define TokenUsage data type with usagePromptTokens, usageCompletionTokens, usageTotalTokens in src/PatternAgent/Runtime/Execution.hs
- [ ] T087 [US5] Define PerformanceMetrics data type with metricsExecutionTime, metricsLLMCallCount, metricsToolInvocationCount, metricsTokenUsage, metricsContextSize in src/PatternAgent/Runtime/Execution.hs

**Observability Capture**:
- [ ] T088 [US5] Add enableObservability parameter to executeAgentWithLibrary function signature in src/PatternAgent/Runtime/Execution.hs
- [ ] T089 [US5] Create ExecutionTrace with start time when observability enabled in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [ ] T090 [US5] Capture LLMCallEvent before LLM API call in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [ ] T091 [US5] Capture LLMCallEvent after LLM API call with response in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [ ] T092 [US5] Capture ToolInvocationEvent before tool invocation in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [ ] T093 [US5] Capture ToolInvocationEvent after tool invocation with result in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [ ] T094 [US5] Capture ContextUpdateEvent after context updates in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [ ] T095 [US5] Calculate PerformanceMetrics at execution completion in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [ ] T096 [US5] Set traceEndTime when execution completes in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [ ] T097 [US5] Return ExecutionTrace and PerformanceMetrics in executeAgentWithLibrary result (extend return type) in src/PatternAgent/Runtime/Execution.hs

**Observability Export**:
- [ ] T098 [US5] Implement exportTraceToJSON function to convert ExecutionTrace to JSON Value in src/PatternAgent/Runtime/Execution.hs
- [ ] T099 [US5] Implement exportMetricsToCSV function to convert PerformanceMetrics to CSV Text in src/PatternAgent/Runtime/Execution.hs
- [ ] T100 [US5] Add ToJSON instances for TraceEvent, ExecutionTrace, TokenUsage, PerformanceMetrics in src/PatternAgent/Runtime/Execution.hs

**Checkpoint**: At this point, all User Stories (1-5) should be fully functional. Developers can execute agents with various tool configurations, have interactive conversations, and observe execution behavior through traces and metrics.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final polish, documentation, and cross-cutting concerns

### Documentation
- [ ] T101 Update README.md with interactive CLI mode usage examples
- [ ] T102 Update README.md with observability features documentation
- [ ] T103 Update README.md with builtin tools documentation

### Error Handling
- [ ] T104 Ensure all error cases from spec.md edge cases are handled appropriately
- [ ] T105 Verify error messages are clear and actionable

### Performance
- [ ] T106 Verify interactive CLI mode activation meets performance goal (< 5 seconds)
- [ ] T107 Verify agent response display meets performance goal (< 3 seconds for typical requests)
- [ ] T108 Verify observability overhead meets performance goal (< 10% overhead)

### Testing
- [ ] T109 Run all scenario tests and verify they pass
- [ ] T110 Run all unit tests and verify they pass
- [ ] T111 Verify test coverage meets requirements

### Code Quality
- [ ] T112 Review code for expressiveness and correctness (Principle 4)
- [ ] T113 Ensure all functions have appropriate type signatures
- [ ] T114 Ensure all modules export appropriate functions

---

## Dependencies & Story Completion Order

### Story Dependencies

**Independent Stories** (can be implemented in parallel):
- User Story 1 (Zero Tools): No dependencies - can start immediately after Phase 2
- User Story 2 (One Tool): No dependencies - can start immediately after Phase 2
- User Story 3 (Multiple Tools): Depends on builtin tools implementation, but can start after Phase 2
- User Story 4 (Interactive CLI): No dependencies - can start immediately after Phase 2
- User Story 5 (Observability): No dependencies - can start immediately after Phase 2

**Note**: All user stories are independent and can be implemented in parallel after Phase 2 completion. User Story 3 requires builtin tools, but those can be implemented as part of that story.

### Recommended Implementation Order

1. **Phase 2** (Foundational): Complete first - required for all user stories
2. **Phase 3** (User Story 1): MVP - simplest case, validates basic execution
3. **Phase 4** (User Story 2): Extends User Story 1 with tool support
4. **Phase 5** (User Story 3): Extends User Story 2 with multiple tools
5. **Phase 6** (User Story 4): Independent - interactive CLI mode
6. **Phase 7** (User Story 5): Independent - observability (P2 priority, can be deferred)
7. **Phase 8** (Polish): Final polish after all stories complete

### Parallel Execution Opportunities

**Within User Story 3**:
- T036, T037, T038 (builtin tool implementations) can be done in parallel
- T039, T040, T041 (tool registration) can be done in parallel after implementations

**Within User Story 4**:
- T050, T051 (flag recognition tests) can be done in parallel
- T056, T057, T058 (CLI mode extension) can be done in parallel

**Within User Story 5**:
- T084, T085, T086, T087 (type definitions) can be done in parallel
- T090, T091, T092, T093, T094 (trace event capture) can be done in parallel
- T098, T099 (export functions) can be done in parallel

**Across Stories**:
- User Stories 1, 2, 4, 5 can be implemented in parallel after Phase 2
- User Story 3 can start after Phase 2, but needs builtin tools before scenario tests

## Implementation Strategy

### MVP Scope (Minimum Viable Product)

**Recommended MVP**: User Story 1 (Zero Tools Scenario Tests)
- Simplest case
- Validates basic execution infrastructure
- Independent and testable
- Delivers immediate value (confidence in basic agent execution)

### Incremental Delivery

1. **Increment 1**: User Story 1 (Zero Tools) - Basic execution validation
2. **Increment 2**: User Story 2 (One Tool) - Tool integration validation
3. **Increment 3**: User Story 3 (Multiple Tools) - Multi-tool validation with builtin tools
4. **Increment 4**: User Story 4 (Interactive CLI) - Real-time conversation capability
5. **Increment 5**: User Story 5 (Observability) - Execution visibility (P2, can be deferred)

Each increment is independently testable and delivers value.

### Testing Strategy

- **Write tests first** (TDD approach per Principle 3)
- **Scenario tests** validate user goal satisfaction end-to-end
- **Unit tests** validate component correctness
- **All tests must pass** before moving to next phase
- **Tests serve as documentation** of expected behavior

---

## Summary

**Total Tasks**: 114 tasks across 8 phases

**Task Distribution**:
- Phase 1 (Setup): 3 tasks
- Phase 2 (Foundational): 4 tasks
- Phase 3 (User Story 1): 9 tasks (3 scenario tests, 3 unit tests, 3 implementation)
- Phase 4 (User Story 2): 10 tasks (3 scenario tests, 4 unit tests, 3 implementation)
- Phase 5 (User Story 3): 18 tasks (3 scenario tests, 6 unit tests, 9 implementation)
- Phase 6 (User Story 4): 29 tasks (4 scenario tests, 6 unit tests, 19 implementation)
- Phase 7 (User Story 5): 25 tasks (4 scenario tests, 5 unit tests, 16 implementation)
- Phase 8 (Polish): 16 tasks

**Parallel Opportunities**: Multiple tasks can be executed in parallel within and across user stories after Phase 2 completion.

**MVP Recommendation**: Start with User Story 1 (Zero Tools Scenario Tests) for fastest value delivery.

