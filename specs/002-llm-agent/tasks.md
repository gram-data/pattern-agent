# Tasks: Basic LLM Agent

**Input**: Design documents from `/specs/002-llm-agent/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

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

- [x] T001 Update pattern-agent.cabal with new dependencies: http-client, http-client-tls, aeson, bytestring, text, mtl
- [x] T002 Create test directory structure: tests/unit/ and tests/scenario/
- [x] T003 [P] Create module structure: src/PatternAgent/Agent.hs, src/PatternAgent/Tool.hs, src/PatternAgent/Execution.hs, src/PatternAgent/Context.hs

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 [P] Define AgentError type in src/PatternAgent/Execution.hs with constructors: LLMAPIError, ToolError, ValidationError, ConfigurationError
- [x] T005 [P] Define LLMProvider typeclass in src/PatternAgent/Execution.hs with callLLM method signature
- [x] T006 [P] Define API key configuration type/interface in src/PatternAgent/Execution.hs for provider authentication
- [x] T007 [P] Implement API key loading from environment variables in src/PatternAgent/Execution.hs (e.g., OPENAI_API_KEY)
- [x] T008 [P] Implement OpenAIProvider instance in src/PatternAgent/Execution.hs for OpenAI API integration
- [x] T009 Create HTTP client helper functions in src/PatternAgent/Execution.hs for LLM API requests with authentication headers
- [x] T010 Create JSON serialization helpers in src/PatternAgent/Execution.hs for LLM request/response handling
- [x] T011 Add error handling for missing/invalid API keys in src/PatternAgent/Execution.hs (ConfigurationError)

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Define Agent Identity (Priority: P1) 🎯 MVP

**Goal**: Enable developers to create an agent with a unique identity (name, description) and specify which LLM model powers the agent's reasoning.

**Independent Test**: Can be fully tested by verifying developers can create an agent with a name, description, and model identifier. This delivers the ability to define and identify agents in the system.

### Tests for User Story 1 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [ ] T012 [P] [US1] Scenario test: Create agent with name, description, and model in tests/scenario/AgentIdentityTest.hs
- [ ] T013 [P] [US1] Scenario test: Verify agent can be uniquely identified by name in tests/scenario/AgentIdentityTest.hs

**Unit Tests** (component correctness):
- [ ] T014 [P] [US1] Unit test: Agent creation with name, description, model in tests/unit/AgentTest.hs
- [ ] T015 [P] [US1] Unit test: Name uniqueness validation in tests/unit/AgentTest.hs
- [ ] T016 [P] [US1] Unit test: Model configuration accessors in tests/unit/AgentTest.hs

### Implementation for User Story 1 (Principle 4: Expressiveness and Correctness)

- [x] T017 [P] [US1] Define LLMProvider enum (OpenAI, Anthropic, Google) in src/PatternAgent/Agent.hs
- [x] T018 [P] [US1] Define Model type with modelId and provider fields in src/PatternAgent/Agent.hs
- [x] T019 [P] [US1] Define Agent type with name, description, model fields in src/PatternAgent/Agent.hs
- [x] T020 [US1] Implement createModel function in src/PatternAgent/Agent.hs with validation
- [x] T021 [US1] Implement createAgent function in src/PatternAgent/Agent.hs with name, description, model parameters
- [x] T022 [US1] Implement agentName accessor in src/PatternAgent/Agent.hs
- [x] T023 [US1] Implement agentDescription accessor in src/PatternAgent/Agent.hs
- [x] T024 [US1] Implement agentModel accessor in src/PatternAgent/Agent.hs
- [x] T025 [US1] Add validation for empty name in createAgent in src/PatternAgent/Agent.hs
- [x] T026 [US1] Export Agent, Model, LLMProvider types and functions from PatternAgent.Agent module

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Developers can create agents with identity.

---

## Phase 4: User Story 2 - Configure Agent Instructions (Priority: P1)

**Goal**: Enable developers to provide instructions that guide the agent's behavior, personality, constraints, and how it should use its tools.

**Independent Test**: Can be fully tested by verifying developers can provide instructions to an agent and those instructions influence the agent's responses. This delivers the ability to customize agent behavior.

### Tests for User Story 2 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T027 [P] [US2] Scenario test: Create agent with instructions and verify instructions are stored in tests/scenario/AgentInstructionsTest.hs
- [ ] T028 [P] [US2] Scenario test: Verify instructions influence agent behavior in tests/scenario/AgentInstructionsTest.hs

**Unit Tests**:
- [ ] T029 [P] [US2] Unit test: Instruction storage and retrieval in tests/unit/AgentTest.hs
- [ ] T030 [P] [US2] Unit test: Instruction validation (non-empty) in tests/unit/AgentTest.hs
- [ ] T031 [P] [US2] Unit test: agentInstruction accessor in tests/unit/AgentTest.hs

### Implementation for User Story 2 (Principle 4: Expressiveness and Correctness)

- [ ] T032 [US2] Add instruction field to Agent type in src/PatternAgent/Agent.hs
- [ ] T033 [US2] Update createAgent function to accept instruction parameter in src/PatternAgent/Agent.hs
- [ ] T034 [US2] Implement agentInstruction accessor in src/PatternAgent/Agent.hs
- [ ] T035 [US2] Add validation for empty instruction in createAgent in src/PatternAgent/Agent.hs
- [ ] T036 [US2] Update Agent type instances (Eq, Show) to include instruction field

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Developers can create agents with identity and instructions.

---

## Phase 5: User Story 4 - Execute Agent and Generate Responses (Priority: P1)

**Goal**: Enable developers to execute an agent with user input and receive responses generated by the agent.

**Independent Test**: Can be fully tested by verifying developers can send input to an agent and receive appropriate responses. This delivers the fundamental agent interaction capability.

### Tests for User Story 4 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T037 [P] [US4] Scenario test: Execute agent with user input and receive response in tests/scenario/ConversationalAgentTest.hs
- [ ] T038 [P] [US4] Scenario test: Verify agent response reflects instructions in tests/scenario/ConversationalAgentTest.hs

**Unit Tests**:
- [ ] T039 [P] [US4] Unit test: Agent execution with valid input in tests/unit/ExecutionTest.hs
- [ ] T040 [P] [US4] Unit test: Response generation and format in tests/unit/ExecutionTest.hs
- [ ] T041 [P] [US4] Unit test: Error handling for empty user input in tests/unit/ExecutionTest.hs
- [ ] T042 [P] [US4] Unit test: Error handling for LLM API failures in tests/unit/ExecutionTest.hs
- [ ] T043 [P] [US4] Unit test: Error handling for missing API key in tests/unit/ExecutionTest.hs

### Implementation for User Story 4 (Principle 4: Expressiveness and Correctness)

- [ ] T044 [P] [US4] Define MessageRole type (UserRole, AssistantRole) in src/PatternAgent/Context.hs
- [ ] T045 [P] [US4] Define Message type with role and content in src/PatternAgent/Context.hs
- [ ] T046 [P] [US4] Define ConversationContext type alias ([Message]) in src/PatternAgent/Context.hs
- [ ] T047 [P] [US4] Define AgentResponse type with responseContent and responseToolsUsed in src/PatternAgent/Execution.hs
- [ ] T048 [US4] Implement createMessage function in src/PatternAgent/Context.hs with validation
- [ ] T049 [US4] Implement emptyContext function in src/PatternAgent/Context.hs
- [ ] T050 [US4] Implement responseContent accessor in src/PatternAgent/Execution.hs
- [ ] T051 [US4] Implement responseToolsUsed accessor in src/PatternAgent/Execution.hs
- [ ] T052 [US4] Implement LLM API request building in src/PatternAgent/Execution.hs (OpenAI format) with API key authentication
- [ ] T053 [US4] Implement LLM API response parsing in src/PatternAgent/Execution.hs
- [ ] T054 [US4] Implement executeAgent function in src/PatternAgent/Execution.hs with user input and context
- [ ] T055 [US4] Add error handling for LLM API errors in executeAgent in src/PatternAgent/Execution.hs
- [ ] T056 [US4] Add validation for empty user input in executeAgent in src/PatternAgent/Execution.hs
- [ ] T057 [US4] Export execution functions from PatternAgent.Execution module
- [ ] T058 [US4] Export context functions from PatternAgent.Context module

**Checkpoint**: At this point, User Stories 1, 2, AND 4 should work independently. Developers can create agents and execute them to get responses.

---

## Phase 6: User Story 3 - Equip Agent with Tools (Priority: P2)

**Goal**: Enable developers to provide agents with tools (functions or capabilities) that extend the agent's abilities beyond the LLM's built-in knowledge.

**Independent Test**: Can be fully tested by verifying developers can add tools to an agent and the agent can invoke those tools when appropriate. This delivers the ability to extend agent capabilities.

### Tests for User Story 3 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T059 [P] [US3] Scenario test: Create agent with tool and verify tool usage in tests/scenario/ToolAgentTest.hs
- [ ] T060 [P] [US3] Scenario test: Agent selects and invokes appropriate tool in tests/scenario/ToolAgentTest.hs
- [ ] T061 [P] [US3] Scenario test: Agent uses multiple tools in single interaction in tests/scenario/ToolAgentTest.hs

**Unit Tests**:
- [ ] T062 [P] [US3] Unit test: Tool creation and registration in tests/unit/ToolTest.hs
- [ ] T063 [P] [US3] Unit test: Tool discovery and access in tests/unit/ToolTest.hs
- [ ] T064 [P] [US3] Unit test: Tool invocation with parameters in tests/unit/ToolTest.hs
- [ ] T065 [P] [US3] Unit test: Tool result handling in tests/unit/ToolTest.hs
- [ ] T066 [P] [US3] Unit test: Tool schema validation in tests/unit/ToolTest.hs
- [ ] T067 [P] [US3] Unit test: Duplicate tool name validation in tests/unit/ToolTest.hs

### Implementation for User Story 3 (Principle 4: Expressiveness and Correctness)

- [ ] T068 [P] [US3] Define Tool type with name, description, schema, invoke function in src/PatternAgent/Tool.hs
- [ ] T069 [US3] Implement createTool function in src/PatternAgent/Tool.hs
- [ ] T070 [US3] Implement createFunctionTool function in src/PatternAgent/Tool.hs (with manual schema)
- [ ] T071 [US3] Implement toolName accessor in src/PatternAgent/Tool.hs
- [ ] T072 [US3] Implement toolDescription accessor in src/PatternAgent/Tool.hs
- [ ] T073 [US3] Implement toolSchema accessor in src/PatternAgent/Tool.hs
- [ ] T074 [US3] Add tools field to Agent type in src/PatternAgent/Agent.hs
- [ ] T075 [US3] Update createAgent to accept tools parameter in src/PatternAgent/Agent.hs
- [ ] T076 [US3] Implement agentTools accessor in src/PatternAgent/Agent.hs
- [ ] T077 [US3] Add duplicate tool name validation in createAgent in src/PatternAgent/Agent.hs
- [ ] T078 [US3] Define ToolInvocation type in src/PatternAgent/Execution.hs
- [ ] T079 [US3] Implement tool invocation logic in executeAgent in src/PatternAgent/Execution.hs
- [ ] T080 [US3] Implement tool parameter schema validation in src/PatternAgent/Execution.hs
- [ ] T081 [US3] Implement tool result handling and error catching in src/PatternAgent/Execution.hs
- [ ] T082 [US3] Integrate tool results into LLM conversation flow in src/PatternAgent/Execution.hs
- [ ] T083 [US3] Add tool invocation tracking to AgentResponse in src/PatternAgent/Execution.hs
- [ ] T084 [US3] Export Tool types and functions from PatternAgent.Tool module

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should work independently. Developers can create agents with tools and execute them with tool support.

---

## Phase 7: User Story 5 - Basic Conversation Loop (Priority: P2)

**Goal**: Enable developers to maintain conversation context across multiple interactions, allowing for multi-turn conversations.

**Independent Test**: Can be fully tested by verifying agents can maintain context across multiple message exchanges and reference previous conversation history. This delivers natural conversational capabilities.

### Tests for User Story 5 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T085 [P] [US5] Scenario test: Multi-turn conversation with context retention in tests/scenario/MultiTurnConversationTest.hs
- [ ] T086 [P] [US5] Scenario test: Agent references previous conversation in follow-up in tests/scenario/MultiTurnConversationTest.hs

**Unit Tests**:
- [ ] T087 [P] [US5] Unit test: Context management (addMessage) in tests/unit/ContextTest.hs
- [ ] T088 [P] [US5] Unit test: Context application in agent execution in tests/unit/ExecutionTest.hs
- [ ] T089 [P] [US5] Unit test: Context boundaries and scoping in tests/unit/ContextTest.hs

### Implementation for User Story 5 (Principle 4: Expressiveness and Correctness)

- [x] T090 [US5] Implement addMessage function in src/PatternAgent/Context.hs
- [ ] T091 [US5] Integrate conversation context into executeAgent in src/PatternAgent/Execution.hs
- [ ] T092 [US5] Update LLM API request to include conversation history in src/PatternAgent/Execution.hs
- [ ] T093 [US5] Ensure context is passed through execution flow in src/PatternAgent/Execution.hs
- [ ] T094 [US5] Update AgentResponse to include context update guidance in src/PatternAgent/Execution.hs

**Checkpoint**: At this point, all user stories should work independently. Developers can create agents, configure them, execute them with tools, and maintain conversation context.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T095 [P] Implement GenerateContentConfig type in src/PatternAgent/Agent.hs
- [ ] T096 [P] Implement createGenerateContentConfig function in src/PatternAgent/Agent.hs with validation
- [ ] T097 [P] Add generateContentConfig field to Agent type in src/PatternAgent/Agent.hs
- [ ] T098 [P] Integrate GenerateContentConfig into LLM API requests in src/PatternAgent/Execution.hs
- [ ] T099 [P] Add comprehensive error handling for all edge cases in src/PatternAgent/Execution.hs
- [ ] T100 [P] Update module exports in src/PatternAgent/Agent.hs, Tool.hs, Execution.hs, Context.hs
- [ ] T101 [P] Add Haddock documentation to all public functions in src/PatternAgent/
- [ ] T102 [P] Run quickstart.md examples validation
- [ ] T103 [P] Additional unit tests for edge cases in tests/unit/
- [ ] T104 [P] Additional scenario tests for complex workflows in tests/scenario/
- [ ] T105 Code cleanup and refactoring across all modules
- [ ] T106 Update pattern-agent.cabal exposed-modules list

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
- **User Story 2 (P1)**: Depends on User Story 1 (extends Agent type with instruction field)
- **User Story 4 (P1)**: Depends on User Stories 1 and 2 (needs Agent with identity and instructions)
- **User Story 3 (P2)**: Depends on User Stories 1, 2, and 4 (needs execution infrastructure for tool invocation)
- **User Story 5 (P2)**: Depends on User Story 4 (needs execution infrastructure for context integration)

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
  - After US1 completes, US2 and US4 can potentially start (US2 extends US1, US4 uses US1+US2)
  - After US4 completes, US3 and US5 can start (US3 extends execution, US5 extends execution)
- All tests for a user story marked [P] can run in parallel
- Types within a story marked [P] can run in parallel
- Different user stories can be worked on in parallel by different team members (respecting dependencies)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together (Principle 3):
Task: "Scenario test: Create agent with name, description, and model in tests/scenario/AgentIdentityTest.hs"
Task: "Scenario test: Verify agent can be uniquely identified by name in tests/scenario/AgentIdentityTest.hs"
Task: "Unit test: Agent creation with name, description, model in tests/unit/AgentTest.hs"
Task: "Unit test: Name uniqueness validation in tests/unit/AgentTest.hs"
Task: "Unit test: Model configuration accessors in tests/unit/AgentTest.hs"

# Launch all types for User Story 1 together:
Task: "Define LLMProvider enum (OpenAI, Anthropic, Google) in src/PatternAgent/Agent.hs"
Task: "Define Model type with modelId and provider fields in src/PatternAgent/Agent.hs"
Task: "Define Agent type with name, description, model fields in src/PatternAgent/Agent.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test User Story 1 independently
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP - agent identity)
3. Add User Story 2 → Test independently → Deploy/Demo (agent instructions)
4. Add User Story 4 → Test independently → Deploy/Demo (agent execution)
5. Add User Story 3 → Test independently → Deploy/Demo (tool support)
6. Add User Story 5 → Test independently → Deploy/Demo (conversation context)
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (MVP)
   - After US1: Developer A continues with US2, Developer B starts US4
   - After US4: Developer A continues with US3, Developer B continues with US5
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
- Total tasks: 106
- MVP scope: Phases 1-3 (User Story 1 only) = 26 tasks
- Full feature scope: All phases = 106 tasks
