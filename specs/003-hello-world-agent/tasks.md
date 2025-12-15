# Tasks: Hello World Agent with Tool Execution

**Input**: Design documents from `/specs/003-hello-world-agent/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/
**Feature**: Hello World Agent with Tool Execution

**Tests**: Per Principle 3 (Dual Testing Strategy), all features MUST include both unit tests and scenario tests. Scenario tests simulate user goal satisfaction end-to-end.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Paths shown below assume single project structure from plan.md

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and dependency setup

- [X] T001 Update pattern-agent.cabal with any additional dependencies if needed (verify existing dependencies are sufficient)
- [X] T002 [P] Verify test directory structure exists: tests/unit/ and tests/scenario/
- [X] T003 [P] Create new module: src/PatternAgent/Tool.hs for tool system
- [X] T004 [P] Create new module: src/PatternAgent/HelloWorld.hs for hello world example

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

**Note**: Phase 0.5 (Tool Description Design) is already complete - gram notation format designed, tool-specification-gram.md and type-signature-grammar.md exist.

- [ ] T005 [P] Define ToolSpecification type in src/PatternAgent/Tool.hs with fields: toolSpecName, toolSpecDescription, toolSpecTypeSignature, toolSpecSchema
- [ ] T006 [P] Define Tool type in src/PatternAgent/Tool.hs with fields: toolName, toolDescription, toolSchema, toolInvoke
- [ ] T007 [P] Define ToolLibrary type in src/PatternAgent/Tool.hs with libraryTools field (Map Text Tool)
- [ ] T008 [P] Define TypeSignature parsed representation type in src/PatternAgent/Tool.hs for parsed gram type signatures
- [ ] T009 [P] Implement emptyToolLibrary function in src/PatternAgent/Tool.hs
- [ ] T010 [P] Implement parseTypeSignature function in src/PatternAgent/Tool.hs to parse gram type signatures in curried form (e.g., "(personName::Text)==>(::String)")
- [ ] T011 [P] Implement typeSignatureToJSONSchema function in src/PatternAgent/Tool.hs to convert parsed type signatures to JSON schemas
- [ ] T012 [P] Implement validateToolArgs function in src/PatternAgent/Tool.hs for manual JSON schema validation
- [ ] T013 [P] Define MessageRole with FunctionRole constructor in src/PatternAgent/Context.hs for tool result messages
- [ ] T014 [P] Update Message type in src/PatternAgent/Context.hs to support FunctionRole messages with tool name

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Create and Register Tools (Priority: P1) 🎯 MVP

**Goal**: Enable developers to create tools (like `sayHello`) that can be used by agents during execution.

**Independent Test**: Can be fully tested by verifying developers can create a tool with a name, description, parameter schema, and invocation function. This delivers the ability to define reusable capabilities that agents can use.

### Tests for User Story 1 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [ ] T015 [P] [US1] Scenario test: Create tool with name, description, schema, and invocation function in tests/scenario/ToolCreationTest.hs
- [ ] T016 [P] [US1] Scenario test: Verify tool can be accessed and its properties retrieved in tests/scenario/ToolCreationTest.hs
- [ ] T017 [P] [US1] Scenario test: Verify tool parameter validation works correctly in tests/scenario/ToolCreationTest.hs

**Unit Tests** (component correctness):
- [ ] T018 [P] [US1] Unit test: ToolSpecification creation with gram type signature in tests/unit/ToolTest.hs
- [ ] T019 [P] [US1] Unit test: Tool creation with name, description, schema, invoke function in tests/unit/ToolTest.hs
- [ ] T020 [P] [US1] Unit test: ToolSpecification accessors (toolSpecName, toolSpecDescription, toolSpecTypeSignature, toolSpecSchema) in tests/unit/ToolTest.hs
- [ ] T021 [P] [US1] Unit test: Tool accessors (toolName, toolDescription, toolSchema) in tests/unit/ToolTest.hs
- [ ] T022 [P] [US1] Unit test: Schema validation for valid parameters in tests/unit/ToolTest.hs
- [ ] T023 [P] [US1] Unit test: Schema validation for invalid parameters (wrong type, missing required) in tests/unit/ToolTest.hs
- [ ] T024 [P] [US1] Unit test: Type signature parsing for simple signatures in tests/unit/ToolTest.hs
- [ ] T025 [P] [US1] Unit test: Type signature to JSON schema conversion in tests/unit/ToolTest.hs

### Implementation for User Story 1 (Principle 4: Expressiveness and Correctness)

- [ ] T026 [P] [US1] Implement createToolSpecification function in src/PatternAgent/Tool.hs with name, description, typeSignature parameters
- [ ] T027 [US1] Implement createToolSpecification to auto-generate schema from type signature in src/PatternAgent/Tool.hs
- [ ] T028 [US1] Implement createTool function in src/PatternAgent/Tool.hs with name, description, schema, invoke parameters
- [ ] T029 [P] [US1] Implement toolSpecName accessor in src/PatternAgent/Tool.hs
- [ ] T030 [P] [US1] Implement toolSpecDescription accessor in src/PatternAgent/Tool.hs
- [ ] T031 [P] [US1] Implement toolSpecTypeSignature accessor in src/PatternAgent/Tool.hs
- [ ] T032 [P] [US1] Implement toolSpecSchema accessor in src/PatternAgent/Tool.hs
- [ ] T033 [P] [US1] Implement toolName accessor in src/PatternAgent/Tool.hs
- [ ] T034 [P] [US1] Implement toolDescription accessor in src/PatternAgent/Tool.hs
- [ ] T035 [P] [US1] Implement toolSchema accessor in src/PatternAgent/Tool.hs
- [ ] T036 [US1] Add validation for non-empty name in createToolSpecification in src/PatternAgent/Tool.hs
- [ ] T037 [US1] Add validation for non-empty description in createToolSpecification in src/PatternAgent/Tool.hs
- [ ] T038 [US1] Add validation for valid gram type signature in createToolSpecification in src/PatternAgent/Tool.hs
- [ ] T039 [US1] Add validation for non-empty name in createTool in src/PatternAgent/Tool.hs
- [ ] T040 [US1] Add validation for non-empty description in createTool in src/PatternAgent/Tool.hs
- [ ] T041 [US1] Implement ToolSpecification instances (Eq, Show, Generic, ToJSON, FromJSON) in src/PatternAgent/Tool.hs
- [ ] T042 [US1] Export ToolSpecification, Tool, and related functions from PatternAgent.Tool module

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Developers can create tools with gram type signatures.

---

## Phase 4: User Story 2 - Equip Agents with Tools (Priority: P1)

**Goal**: Enable developers to associate tools with agents so agents can use them during execution.

**Independent Test**: Can be fully tested by verifying developers can add tools to an agent and the agent can access its tool list. This delivers the ability to configure agents with capabilities.

### Tests for User Story 2 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T043 [P] [US2] Scenario test: Add tool to agent and verify agent can access it in tests/scenario/AgentToolAssociationTest.hs
- [ ] T044 [P] [US2] Scenario test: Add multiple tools to agent and verify all tools are accessible in tests/scenario/AgentToolAssociationTest.hs
- [ ] T045 [P] [US2] Scenario test: Verify agent can see its available tools during request processing in tests/scenario/AgentToolAssociationTest.hs

**Unit Tests**:
- [ ] T046 [P] [US2] Unit test: Tool association with agents in tests/unit/AgentTest.hs
- [ ] T047 [P] [US2] Unit test: Tool retrieval from agents in tests/unit/AgentTest.hs
- [ ] T048 [P] [US2] Unit test: Tool name uniqueness validation within agent's tool list in tests/unit/AgentTest.hs
- [ ] T049 [P] [US2] Unit test: Agent with zero tools (backward compatibility) in tests/unit/AgentTest.hs

### Implementation for User Story 2 (Principle 4: Expressiveness and Correctness)

- [ ] T050 [US2] Add agentToolSpecs field to Agent type in src/PatternAgent/Agent.hs (list of ToolSpecification)
- [ ] T051 [US2] Update createAgent function to accept agentToolSpecs parameter in src/PatternAgent/Agent.hs
- [ ] T052 [US2] Implement agentToolSpecs accessor in src/PatternAgent/Agent.hs
- [ ] T053 [US2] Add validation for unique tool names within agentToolSpecs list in src/PatternAgent/Agent.hs
- [ ] T054 [US2] Update Agent type instances (Eq, Show, Generic, ToJSON, FromJSON) to include agentToolSpecs field in src/PatternAgent/Agent.hs
- [ ] T055 [US2] Ensure backward compatibility: agentToolSpecs defaults to empty list if not provided in src/PatternAgent/Agent.hs

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Developers can create tools and associate them with agents.

---

## Phase 5: User Story 3 - Execute Tools During Agent Execution (Priority: P1)

**Goal**: Enable developers to have agents automatically invoke tools when the LLM decides to use them during execution.

**Independent Test**: Can be fully tested by verifying that when an LLM requests a tool call, the execution environment detects it, invokes the tool, and returns results to the LLM. This delivers the fundamental tool execution capability.

### Tests for User Story 3 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T056 [P] [US3] Scenario test: Agent with tool executes and LLM requests tool call, tool is invoked in tests/scenario/ToolExecutionTest.hs
- [ ] T057 [P] [US3] Scenario test: Tool executes successfully and result is returned to LLM for response generation in tests/scenario/ToolExecutionTest.hs
- [ ] T058 [P] [US3] Scenario test: Tool invocation failure is handled gracefully and communicated to LLM in tests/scenario/ToolExecutionTest.hs
- [ ] T059 [P] [US3] Scenario test: Agent requests tool that doesn't exist, appropriate error is returned in tests/scenario/ToolExecutionTest.hs

**Unit Tests**:
- [ ] T060 [P] [US3] Unit test: Tool call detection in LLM responses in tests/unit/ExecutionTest.hs
- [ ] T061 [P] [US3] Unit test: Tool invocation with correct parameters in tests/unit/ExecutionTest.hs
- [ ] T062 [P] [US3] Unit test: Tool result handling and formatting in tests/unit/ExecutionTest.hs
- [ ] T063 [P] [US3] Unit test: Error handling for tool execution failures in tests/unit/ExecutionTest.hs
- [ ] T064 [P] [US3] Unit test: Tool binding from ToolSpecification to Tool implementation in tests/unit/ExecutionTest.hs
- [ ] T065 [P] [US3] Unit test: Tool parameter validation before invocation in tests/unit/ExecutionTest.hs
- [ ] T066 [P] [US3] Unit test: ToolLibrary registration and lookup in tests/unit/ToolTest.hs
- [ ] T067 [P] [US3] Unit test: bindTool function validates tool matches specification in tests/unit/ToolTest.hs

### Implementation for User Story 3 (Principle 4: Expressiveness and Correctness)

- [ ] T068 [P] [US3] Implement registerTool function in src/PatternAgent/Tool.hs to register tool in ToolLibrary
- [ ] T069 [P] [US3] Implement lookupTool function in src/PatternAgent/Tool.hs to lookup tool by name
- [ ] T070 [P] [US3] Implement bindTool function in src/PatternAgent/Tool.hs to bind ToolSpecification to Tool from library
- [ ] T071 [US3] Implement bindAgentTools function in src/PatternAgent/Execution.hs to bind all agent tool specs to implementations
- [ ] T072 [US3] Implement detectToolCall function in src/PatternAgent/Execution.hs to detect function_call in LLM responses
- [ ] T073 [US3] Implement invokeTool function in src/PatternAgent/Execution.hs to invoke tool with validated parameters
- [ ] T074 [US3] Update LLM.hs to add tool definitions (from ToolSpecifications) to OpenAI API requests in src/PatternAgent/LLM.hs
- [ ] T075 [US3] Update LLM.hs to parse function_call from OpenAI API responses in src/PatternAgent/LLM.hs
- [ ] T076 [US3] Implement iterative execution loop in executeAgentWithLibrary in src/PatternAgent/Execution.hs (detect tool call → validate → invoke → send result to LLM → get final response)
- [ ] T077 [US3] Add maximum iteration limit (10) to prevent infinite loops in executeAgentWithLibrary in src/PatternAgent/Execution.hs
- [ ] T078 [US3] Add tool invocation tracking to AgentResponse.responseToolsUsed in src/PatternAgent/Execution.hs
- [ ] T079 [US3] Add FunctionRole messages to conversation context for tool results in src/PatternAgent/Execution.hs
- [ ] T080 [US3] Implement executeAgentWithLibrary function signature in src/PatternAgent/Execution.hs (Agent, Text, ConversationContext, ToolLibrary → IO (Either AgentError AgentResponse))
- [ ] T081 [US3] Add error handling for tool not found in library in executeAgentWithLibrary in src/PatternAgent/Execution.hs
- [ ] T082 [US3] Add error handling for tool binding failures in executeAgentWithLibrary in src/PatternAgent/Execution.hs
- [ ] T083 [US3] Add error handling for tool parameter validation failures in executeAgentWithLibrary in src/PatternAgent/Execution.hs
- [ ] T084 [US3] Add error handling for tool execution exceptions in executeAgentWithLibrary in src/PatternAgent/Execution.hs
- [ ] T085 [US3] Add error handling for malformed tool call requests from LLM in executeAgentWithLibrary in src/PatternAgent/Execution.hs
- [ ] T086 [US3] Export executeAgentWithLibrary and related functions from PatternAgent.Execution module

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should work independently. Developers can create tools, associate them with agents, and execute agents with tool support.

---

## Phase 6: User Story 4 - Hello World Example Agent (Priority: P1)

**Goal**: Enable developers to see a concrete, working example of an agent that uses the `sayHello` tool to have friendly conversations and respond to greetings.

**Independent Test**: Can be fully tested by creating the hello world agent, executing it with greeting messages, and verifying it uses the `sayHello` tool appropriately. This delivers a complete, working example that demonstrates tool execution.

### Tests for User Story 4 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T087 [P] [US4] Scenario test: Hello world agent uses sayHello tool when responding to greetings in tests/scenario/HelloWorldTest.hs
- [ ] T088 [P] [US4] Scenario test: sayHello tool is invoked with appropriate parameters when agent processes greeting in tests/scenario/HelloWorldTest.hs
- [ ] T089 [P] [US4] Scenario test: Agent incorporates sayHello tool result into friendly response in tests/scenario/HelloWorldTest.hs
- [ ] T090 [P] [US4] Scenario test: Hello world agent responds conversationally without tool for non-greeting messages in tests/scenario/HelloWorldTest.hs

**Unit Tests**:
- [ ] T091 [P] [US4] Unit test: Hello world agent creation with sayHello tool and instructions in tests/unit/HelloWorldTest.hs
- [ ] T092 [P] [US4] Unit test: sayHello tool implementation with various inputs in tests/unit/HelloWorldTest.hs
- [ ] T093 [P] [US4] Unit test: sayHello tool specification with gram type signature in tests/unit/HelloWorldTest.hs

### Implementation for User Story 4 (Principle 4: Expressiveness and Correctness)

- [ ] T094 [US4] Create sayHelloSpec ToolSpecification in src/PatternAgent/HelloWorld.hs with name "sayHello", description, type signature "(personName::Text)==>(::String)"
- [ ] T095 [US4] Create sayHelloTool Tool implementation in src/PatternAgent/HelloWorld.hs with invoke function that extracts name and returns greeting
- [ ] T096 [US4] Create helloWorldToolLibrary ToolLibrary in src/PatternAgent/HelloWorld.hs with sayHello tool registered
- [ ] T097 [US4] Create helloWorldAgent Agent in src/PatternAgent/HelloWorld.hs with name "hello_world_agent", description, model, instruction to use sayHello tool, and agentToolSpecs = [sayHelloSpec]
- [ ] T098 [US4] Export sayHelloSpec, sayHelloTool, helloWorldToolLibrary, helloWorldAgent from PatternAgent.HelloWorld module

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should work independently. Developers can create the hello world agent and execute it with tool support.

---

## Phase 7: User Story 5 - Conversation Loop with Tool Execution (Priority: P2)

**Goal**: Enable developers to have agents maintain conversation context while using tools across multiple interactions.

**Independent Test**: Can be fully tested by verifying agents maintain conversation history and can use tools appropriately in follow-up messages. This delivers natural conversational capabilities with tool support.

### Tests for User Story 5 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T099 [P] [US5] Scenario test: Agent references previous tool usage in follow-up message in tests/scenario/MultiTurnToolConversationTest.hs
- [ ] T100 [P] [US5] Scenario test: Multi-turn conversation with tool usage maintains coherence in tests/scenario/MultiTurnToolConversationTest.hs
- [ ] T101 [P] [US5] Scenario test: Agent uses previous tool results to inform new response in tests/scenario/MultiTurnToolConversationTest.hs

**Unit Tests**:
- [ ] T102 [P] [US5] Unit test: Conversation context includes tool invocations and results in tests/unit/ContextTest.hs
- [ ] T103 [P] [US5] Unit test: Agents use conversation history including tool results when generating responses in tests/unit/ExecutionTest.hs
- [ ] T104 [P] [US5] Unit test: FunctionRole messages properly formatted in conversation context in tests/unit/ContextTest.hs

### Implementation for User Story 5 (Principle 4: Expressiveness and Correctness)

- [ ] T105 [US5] Verify conversation context includes FunctionRole messages for tool results in src/PatternAgent/Execution.hs
- [ ] T106 [US5] Verify conversation context is properly passed through iterative execution loop in src/PatternAgent/Execution.hs
- [ ] T107 [US5] Verify LLM API requests include full conversation history with tool invocations in src/PatternAgent/LLM.hs
- [ ] T108 [US5] Verify context updates include user message, assistant message with tool call, function message with tool result, and final assistant response in src/PatternAgent/Execution.hs

**Checkpoint**: At this point, all user stories should work independently. Developers can create agents with tools, execute them, and maintain conversation context with tool usage.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T109 [P] Add comprehensive error handling for all edge cases in src/PatternAgent/Execution.hs (tool timeout scenarios, multiple simultaneous tool calls, agent with no tools but LLM requests tool call)
- [ ] T110 [P] Update module exports in src/PatternAgent/Tool.hs, Execution.hs, Agent.hs, HelloWorld.hs
- [ ] T111 [P] Add Haddock documentation to all public functions in src/PatternAgent/Tool.hs, Execution.hs, Agent.hs, HelloWorld.hs
- [ ] T112 [P] Run quickstart.md examples validation
- [ ] T113 [P] Additional unit tests for edge cases in tests/unit/ (tool with no parameters, tool with optional parameters, tool with nested record parameters)
- [ ] T114 [P] Additional scenario tests for complex workflows in tests/scenario/ (multiple tools, tool chaining, error recovery)
- [ ] T115 [P] Code cleanup and refactoring across all modules
- [ ] T116 [P] Update pattern-agent.cabal exposed-modules list to include PatternAgent.Tool and PatternAgent.HelloWorld
- [ ] T117 [P] Verify backward compatibility: tool-free agents still work correctly

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2)
- **Polish (Final Phase)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Depends on User Story 1 (needs ToolSpecification type to add to Agent)
- **User Story 3 (P1)**: Depends on User Stories 1 and 2 (needs ToolSpecification, Tool, ToolLibrary, and Agent with tool specs)
- **User Story 4 (P1)**: Depends on User Stories 1, 2, and 3 (needs complete tool system and execution with tools)
- **User Story 5 (P2)**: Depends on User Story 3 (needs tool execution infrastructure for context integration)

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Types before functions
- Core functions before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational tasks marked [P] can run in parallel (within Phase 2)
- Once Foundational phase completes:
  - User Story 1 can start independently
  - After US1 completes, US2 can start (US2 extends Agent with tool specs)
  - After US2 completes, US3 can start (US3 uses Agent with tool specs)
  - After US3 completes, US4 and US5 can start (US4 uses complete tool system, US5 extends execution)
- All tests for a user story marked [P] can run in parallel
- Types within a story marked [P] can run in parallel
- Different user stories can be worked on in parallel by different team members (respecting dependencies)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together (Principle 3):
Task: "Scenario test: Create tool with name, description, schema, and invocation function in tests/scenario/ToolCreationTest.hs"
Task: "Scenario test: Verify tool can be accessed and its properties retrieved in tests/scenario/ToolCreationTest.hs"
Task: "Scenario test: Verify tool parameter validation works correctly in tests/scenario/ToolCreationTest.hs"
Task: "Unit test: ToolSpecification creation with gram type signature in tests/unit/ToolTest.hs"
Task: "Unit test: Tool creation with name, description, schema, invoke function in tests/unit/ToolTest.hs"
Task: "Unit test: ToolSpecification accessors (toolSpecName, toolSpecDescription, toolSpecTypeSignature, toolSpecSchema) in tests/unit/ToolTest.hs"
Task: "Unit test: Tool accessors (toolName, toolDescription, toolSchema) in tests/unit/ToolTest.hs"
Task: "Unit test: Schema validation for valid parameters in tests/unit/ToolTest.hs"
Task: "Unit test: Schema validation for invalid parameters (wrong type, missing required) in tests/unit/ToolTest.hs"
Task: "Unit test: Type signature parsing for simple signatures in tests/unit/ToolTest.hs"
Task: "Unit test: Type signature to JSON schema conversion in tests/unit/ToolTest.hs"

# Launch all types for User Story 1 together:
Task: "Define ToolSpecification type in src/PatternAgent/Tool.hs with fields: toolSpecName, toolSpecDescription, toolSpecTypeSignature, toolSpecSchema"
Task: "Define Tool type in src/PatternAgent/Tool.hs with fields: toolName, toolDescription, toolSchema, toolInvoke"
Task: "Define ToolLibrary type in src/PatternAgent/Tool.hs with libraryTools field (Map Text Tool)"
Task: "Define TypeSignature parsed representation type in src/PatternAgent/Tool.hs for parsed gram type signatures"
```

---

## Implementation Strategy

### MVP First (User Stories 1-4 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Create and Register Tools)
4. Complete Phase 4: User Story 2 (Equip Agents with Tools)
5. Complete Phase 5: User Story 3 (Execute Tools During Agent Execution)
6. Complete Phase 6: User Story 4 (Hello World Example Agent)
7. **STOP and VALIDATE**: Test all user stories independently
8. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (tool creation)
3. Add User Story 2 → Test independently → Deploy/Demo (tool association)
4. Add User Story 3 → Test independently → Deploy/Demo (tool execution)
5. Add User Story 4 → Test independently → Deploy/Demo (hello world example)
6. Add User Story 5 → Test independently → Deploy/Demo (conversation context with tools)
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (tool creation)
   - After US1: Developer A continues with US2, Developer B starts US3 prep
   - After US2: Developer A continues with US3, Developer B starts US4
   - After US3: Developer A continues with US4, Developer B starts US5
3. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- Total tasks: 117
- MVP scope: Phases 1-6 (User Stories 1-4) = 98 tasks
- Full feature scope: All phases = 117 tasks
- Phase 0.5 (Tool Description Design) is already complete - gram notation format designed

