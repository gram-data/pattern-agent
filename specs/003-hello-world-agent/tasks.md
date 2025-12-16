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

## Module Structure (Language vs Runtime)

**Language Modules** (portable specification):
- `src/PatternAgent/Language/Core.hs` - Agent, Tool as Pattern Subject, lenses, creation
- `src/PatternAgent/Language/Schema.hs` - Schema validation
- `src/PatternAgent/Language/TypeSignature.hs` - Type signature parsing & JSON schema generation
- `src/PatternAgent/Language/Serialization.hs` - Gram ↔ Pattern conversion

**Runtime Modules** (Haskell-specific implementation):
- `src/PatternAgent/Runtime/Execution.hs` - Execution engine (interpretation model as default)
- `src/PatternAgent/Runtime/ToolLibrary.hs` - ToolImpl, ToolLibrary, tool binding
- `src/PatternAgent/Runtime/LLM.hs` - LLM API client
- `src/PatternAgent/Runtime/Context.hs` - ConversationContext

**Note**: Execution uses interpretation model by default (direct Pattern execution via lenses). Optional compilation to AgentRuntime is a future optimization.

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and dependency setup

- [X] T001 Update pattern-agent.cabal with any additional dependencies if needed (verify existing dependencies are sufficient)
- [X] T002 [P] Verify test directory structure exists: tests/unit/ and tests/scenario/
- [X] T003 [P] Create Language module structure: src/PatternAgent/Language/ (Core, Schema, TypeSignature, Serialization)
- [X] T004 [P] Create Runtime module structure: src/PatternAgent/Runtime/ (Execution, ToolLibrary, LLM, Context)
- [X] T004b [P] Create new module: tests/scenario/HelloWorldExample.hs for hello world example

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

**Note**: Phase 0.5 (Tool Description Design) is already complete - gram notation format designed, tool-specification-gram.md and type-signature-grammar.md exist.

- [X] T005 [P] Define Tool type alias in src/PatternAgent/Language/Core.hs (type Tool = Pattern Subject) with lenses: toolName, toolDescription, toolTypeSignature, toolSchema
- [X] T006 [P] Define ToolImpl type in src/PatternAgent/Runtime/ToolLibrary.hs with fields: toolImplName, toolImplDescription, toolImplSchema, toolImplInvoke
- [X] T007 [P] Define ToolLibrary type in src/PatternAgent/Runtime/ToolLibrary.hs with libraryTools field (Map Text ToolImpl)
- [X] T008 [P] Define TypeSignature parsed representation type in src/PatternAgent/Language/TypeSignature.hs for parsed gram type signatures
- [X] T009 [P] Implement emptyToolLibrary function in src/PatternAgent/Runtime/ToolLibrary.hs
- [X] T010 [P] ~~Implement parseTypeSignature function~~ - CANCELLED: gram-hs already parses gram files; type signatures are Pattern elements, use extractTypeSignatureFromPattern instead
- [X] T011 [P] Implement typeSignatureToJSONSchema function in src/PatternAgent/Language/TypeSignature.hs to convert parsed type signatures to JSON schemas
- [X] T012 [P] Implement validateToolArgs function in src/PatternAgent/Runtime/ToolLibrary.hs for manual JSON schema validation
- [X] T013 [P] Define MessageRole with FunctionRole constructor in src/PatternAgent/Runtime/Context.hs for tool result messages
- [X] T014 [P] Update Message type in src/PatternAgent/Runtime/Context.hs to support FunctionRole messages with tool name

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Create and Register Tools (Priority: P1) 🎯 MVP

**Goal**: Enable developers to create tools (like `sayHello`) that can be used by agents during execution.

**Independent Test**: Can be fully tested by verifying developers can create a tool with a name, description, parameter schema, and invocation function. This delivers the ability to define reusable capabilities that agents can use.

### Tests for User Story 1 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [X] T015 [P] [US1] Scenario test: Create tool with name, description, schema, and invocation function in tests/scenario/ToolCreationTest.hs
- [X] T016 [P] [US1] Scenario test: Verify tool can be accessed and its properties retrieved in tests/scenario/ToolCreationTest.hs
- [X] T017 [P] [US1] Scenario test: Verify tool parameter validation works correctly in tests/scenario/ToolCreationTest.hs

**Unit Tests** (component correctness):
- [X] T018 [P] [US1] Unit test: Tool creation with gram type signature in tests/unit/ToolTest.hs
- [X] T019 [P] [US1] Unit test: ToolImpl creation with name, description, schema, invoke function in tests/unit/ToolTest.hs
- [X] T020 [P] [US1] Unit test: Tool accessors (toolName, toolDescription, toolTypeSignature, toolSchema) via lenses in tests/unit/ToolTest.hs
- [X] T021 [P] [US1] Unit test: ToolImpl accessors (toolImplName, toolImplDescription, toolImplSchema) in tests/unit/ToolTest.hs
- [X] T022 [P] [US1] Unit test: Schema validation for valid parameters in tests/unit/ToolTest.hs
- [X] T023 [P] [US1] Unit test: Schema validation for invalid parameters (wrong type, missing required) in tests/unit/ToolTest.hs
- [X] T024 [P] [US1] Unit test: Type signature parsing for simple signatures in tests/unit/ToolTest.hs
- [X] T025 [P] [US1] Unit test: Type signature to JSON schema conversion in tests/unit/ToolTest.hs

### Implementation for User Story 1 (Principle 4: Expressiveness and Correctness)

- [X] T026 [P] [US1] Implement createTool function in src/PatternAgent/Language/Core.hs with name, description, typeSignature parameters (returns Tool Pattern)
- [X] T027 [US1] Implement createTool to auto-generate schema from type signature in src/PatternAgent/Language/Core.hs (uses Language.TypeSignature)
- [X] T028 [US1] Implement createToolImpl function in src/PatternAgent/Runtime/ToolLibrary.hs with name, description, schema, invoke parameters
- [X] T029 [P] [US1] Implement toolName lens in src/PatternAgent/Language/Core.hs
- [X] T030 [P] [US1] Implement toolDescription lens in src/PatternAgent/Language/Core.hs
- [X] T031 [P] [US1] Implement toolTypeSignature lens in src/PatternAgent/Language/Core.hs
- [X] T032 [P] [US1] Implement toolSchema lens in src/PatternAgent/Language/Core.hs
- [X] T033 [P] [US1] Implement toolImplName accessor in src/PatternAgent/Runtime/ToolLibrary.hs
- [X] T034 [P] [US1] Implement toolImplDescription accessor in src/PatternAgent/Runtime/ToolLibrary.hs
- [X] T035 [P] [US1] Implement toolImplSchema accessor in src/PatternAgent/Runtime/ToolLibrary.hs
- [X] T036 [US1] Add validation for non-empty name in createTool in src/PatternAgent/Language/Core.hs
- [X] T037 [US1] Add validation for non-empty description in createTool in src/PatternAgent/Language/Core.hs
- [X] T038 [US1] Add validation for valid gram type signature in createTool in src/PatternAgent/Language/Core.hs (uses Language.TypeSignature)
- [X] T039 [US1] Add validation for non-empty name in createToolImpl in src/PatternAgent/Runtime/ToolLibrary.hs
- [X] T040 [US1] Add validation for non-empty description in createToolImpl in src/PatternAgent/Runtime/ToolLibrary.hs
- [ ] T041 [US1] Tool is Pattern Subject (no instances needed), ToolRuntime instances (Eq, Show, Generic, ToJSON, FromJSON) in src/PatternAgent/Language/Core.hs (optional optimization)
- [X] T042 [US1] Export Tool and related functions from PatternAgent.Language.Core module, ToolImpl and ToolLibrary from PatternAgent.Runtime.ToolLibrary module

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Developers can create tools with gram type signatures.

---

## Phase 4: User Story 2 - Equip Agents with Tools (Priority: P1)

**Goal**: Enable developers to associate tools with agents so agents can use them during execution.

**Independent Test**: Can be fully tested by verifying developers can add tools to an agent and the agent can access its tool list. This delivers the ability to configure agents with capabilities.

### Tests for User Story 2 (Principle 3: Dual Testing Strategy) ✅

**Scenario Tests**:
- [x] T043 [P] [US2] Scenario test: Add tool to agent and verify agent can access it in tests/scenario/AgentToolAssociationTest.hs
- [x] T044 [P] [US2] Scenario test: Add multiple tools to agent and verify all tools are accessible in tests/scenario/AgentToolAssociationTest.hs
- [x] T045 [P] [US2] Scenario test: Verify agent can see its available tools during request processing in tests/scenario/AgentToolAssociationTest.hs
- [x] T043a [P] [US2] Scenario test: Purely conversational agent with no tools in tests/scenario/AgentToolAssociationTest.hs
- [x] T043b [P] [US2] Scenario test: Agent with one tool (hello world) in tests/scenario/AgentToolAssociationTest.hs

**Unit Tests**:
- [x] T046 [P] [US2] Unit test: Tool association with agents in tests/unit/AgentTest.hs
- [x] T047 [P] [US2] Unit test: Tool retrieval from agents in tests/unit/AgentTest.hs
- [x] T048 [P] [US2] Unit test: Tool name uniqueness validation within agent's tool list in tests/unit/AgentTest.hs
- [x] T049 [P] [US2] Unit test: Agent with zero tools (backward compatibility) in tests/unit/AgentTest.hs

### Implementation for User Story 2 (Principle 4: Expressiveness and Correctness) ✅

- [x] T050 [US2] Add agentTools lens to Agent type in src/PatternAgent/Language/Core.hs (list of Tool, Pattern elements)
- [x] T051 [US2] Update createAgent function to accept tools parameter in src/PatternAgent/Language/Core.hs
- [x] T052 [US2] Implement agentTools lens in src/PatternAgent/Language/Core.hs
- [x] T053 [US2] Add validation for unique tool names within agentTools list in src/PatternAgent/Language/Core.hs
- [x] T054 [US2] Agent is Pattern Subject (no instances needed), AgentRuntime instances (Eq, Show, Generic, ToJSON, FromJSON) to include agentRuntimeTools field in src/PatternAgent/Language/Core.hs (optional optimization)
- [x] T055 [US2] Ensure agentTools defaults to empty list if not provided in src/PatternAgent/Language/Core.hs

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Developers can create tools and associate them with agents.

---

## Phase 5: User Story 3 - Execute Tools During Agent Execution (Priority: P1)

**Goal**: Enable developers to have agents automatically invoke tools when the LLM decides to use them during execution.

**Independent Test**: Can be fully tested by verifying that when an LLM requests a tool call, the execution environment detects it, invokes the tool, and returns results to the LLM. This delivers the fundamental tool execution capability.

### Tests for User Story 3 (Principle 3: Dual Testing Strategy) ✅

**Scenario Tests**:
- [X] T056 [P] [US3] Scenario test: Agent with tool executes and LLM requests tool call, tool is invoked in tests/scenario/ToolExecutionTest.hs
- [X] T057 [P] [US3] Scenario test: Tool executes successfully and result is returned to LLM for response generation in tests/scenario/ToolExecutionTest.hs
- [X] T058 [P] [US3] Scenario test: Tool invocation failure is handled gracefully and communicated to LLM in tests/scenario/ToolExecutionTest.hs
- [X] T059 [P] [US3] Scenario test: Agent requests tool that doesn't exist, appropriate error is returned in tests/scenario/ToolExecutionTest.hs

**Unit Tests**:
- [X] T060 [P] [US3] Unit test: Tool call detection in LLM responses in tests/unit/ExecutionTest.hs
- [X] T061 [P] [US3] Unit test: Tool invocation with correct parameters in tests/unit/ExecutionTest.hs
- [X] T062 [P] [US3] Unit test: Tool result handling and formatting in tests/unit/ExecutionTest.hs
- [X] T063 [P] [US3] Unit test: Error handling for tool execution failures in tests/unit/ExecutionTest.hs
- [X] T064 [P] [US3] Unit test: Tool binding from Tool (Pattern) to ToolImpl implementation in tests/unit/ExecutionTest.hs
- [X] T065 [P] [US3] Unit test: Tool parameter validation before invocation in tests/unit/ExecutionTest.hs
- [X] T066 [P] [US3] Unit test: ToolLibrary registration and lookup in tests/unit/ToolTest.hs
- [X] T067 [P] [US3] Unit test: bindTool function validates tool matches specification in tests/unit/ToolTest.hs

### Implementation for User Story 3 (Principle 4: Expressiveness and Correctness) ✅

- [X] T068 [P] [US3] Implement registerTool function in src/PatternAgent/Runtime/ToolLibrary.hs to register tool in ToolLibrary
- [X] T069 [P] [US3] Implement lookupTool function in src/PatternAgent/Runtime/ToolLibrary.hs to lookup tool by name
- [X] T070 [P] [US3] Implement bindTool function in src/PatternAgent/Runtime/ToolLibrary.hs to bind Tool (Pattern) to ToolImpl from library
- [X] T071 [US3] Implement bindAgentTools function in src/PatternAgent/Runtime/Execution.hs to bind all agent tools to ToolImpl implementations
- [X] T072 [US3] Implement detectToolCall function in src/PatternAgent/Runtime/Execution.hs to detect function_call in LLM responses
- [X] T073 [US3] Implement invokeTool function in src/PatternAgent/Runtime/Execution.hs to invoke tool with validated parameters
- [X] T074 [US3] Update Runtime.LLM to add tool definitions (from Tools) to OpenAI API requests in src/PatternAgent/Runtime/LLM.hs
- [X] T075 [US3] Update Runtime.LLM to parse function_call from OpenAI API responses in src/PatternAgent/Runtime/LLM.hs
- [X] T076 [US3] Implement iterative execution loop in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs (detect tool call → validate → invoke → send result to LLM → get final response)
- [X] T077 [US3] Add maximum iteration limit (10) to prevent infinite loops in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [X] T078 [US3] Add tool invocation tracking to AgentResponse.responseToolsUsed in src/PatternAgent/Runtime/Execution.hs
- [X] T079 [US3] Add FunctionRole messages to conversation context for tool results in src/PatternAgent/Runtime/Execution.hs
- [X] T080 [US3] Implement executeAgentWithLibrary function signature in src/PatternAgent/Runtime/Execution.hs (Agent, Text, ConversationContext, ToolLibrary → IO (Either AgentError AgentResponse))
- [X] T081 [US3] Add error handling for tool not found in library in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [X] T082 [US3] Add error handling for tool binding failures in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [X] T083 [US3] Add error handling for tool parameter validation failures in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [X] T084 [US3] Add error handling for tool execution exceptions in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [X] T085 [US3] Add error handling for malformed tool call requests from LLM in executeAgentWithLibrary in src/PatternAgent/Runtime/Execution.hs
- [X] T086 [US3] Export executeAgentWithLibrary and related functions from PatternAgent.Runtime.Execution module

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should work independently. Developers can create tools, associate them with agents, and execute agents with tool support.

---

## Phase 6: User Story 4 - Hello World Example Agent (Priority: P1)

**Goal**: Enable developers to see a concrete, working example of an agent that uses the `sayHello` tool to have friendly conversations and respond to greetings.

**Independent Test**: Can be fully tested by creating the hello world agent, executing it with greeting messages, and verifying it uses the `sayHello` tool appropriately. This delivers a complete, working example that demonstrates tool execution.

### Tests for User Story 4 (Principle 3: Dual Testing Strategy) ✅

**Scenario Tests**:
- [X] T087 [P] [US4] Scenario test: Hello world agent uses sayHello tool when responding to greetings in tests/scenario/HelloWorldTest.hs
- [X] T088 [P] [US4] Scenario test: sayHello tool is invoked with appropriate parameters when agent processes greeting in tests/scenario/HelloWorldTest.hs
- [X] T089 [P] [US4] Scenario test: Agent incorporates sayHello tool result into friendly response in tests/scenario/HelloWorldTest.hs
- [X] T090 [P] [US4] Scenario test: Hello world agent responds conversationally without tool for non-greeting messages in tests/scenario/HelloWorldTest.hs

**Unit Tests**:
- [X] T091 [P] [US4] Unit test: Hello world agent creation with sayHello tool and instructions in tests/unit/HelloWorldTest.hs
- [X] T092 [P] [US4] Unit test: sayHello tool implementation with various inputs in tests/unit/HelloWorldTest.hs
- [X] T093 [P] [US4] Unit test: sayHello tool specification with gram type signature in tests/unit/HelloWorldTest.hs

### Implementation for User Story 4 (Principle 4: Expressiveness and Correctness) ✅

- [X] T094 [US4] Create sayHello Tool (Pattern) in tests/scenario/HelloWorldExample.hs with name "sayHello", description, type signature "(personName::Text {default:\"world\"})==>(::String)"
- [X] T095 [US4] Create sayHelloImpl ToolImpl implementation in tests/scenario/HelloWorldExample.hs with invoke function that extracts name and returns greeting
- [X] T096 [US4] Create helloWorldToolLibrary ToolLibrary in tests/scenario/HelloWorldExample.hs with sayHello ToolImpl registered
- [X] T097 [US4] Create helloWorldAgent Agent (Pattern) in tests/scenario/HelloWorldExample.hs with name "hello_world_agent", description, model, instruction to use sayHello tool, and agentTools = [sayHello]
- [X] T098 [US4] Export sayHello, sayHelloImpl, helloWorldToolLibrary, helloWorldAgent from HelloWorldExample module

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should work independently. Developers can create the hello world agent and execute it with tool support.

---

## Phase 7: User Story 5 - Conversation Loop with Tool Execution (Priority: P2)

**Goal**: Enable developers to have agents maintain conversation context while using tools across multiple interactions.

**Independent Test**: Can be fully tested by verifying agents maintain conversation history and can use tools appropriately in follow-up messages. This delivers natural conversational capabilities with tool support.

### Tests for User Story 5 (Principle 3: Dual Testing Strategy) ✅

**Scenario Tests**:
- [X] T099 [P] [US5] Scenario test: Agent references previous tool usage in follow-up message in tests/scenario/MultiTurnToolConversationTest.hs
- [X] T100 [P] [US5] Scenario test: Multi-turn conversation with tool usage maintains coherence in tests/scenario/MultiTurnToolConversationTest.hs
- [X] T101 [P] [US5] Scenario test: Agent uses previous tool results to inform new response in tests/scenario/MultiTurnToolConversationTest.hs

**Unit Tests**:
- [X] T102 [P] [US5] Unit test: Conversation context includes tool invocations and results in tests/unit/ContextTest.hs
- [X] T103 [P] [US5] Unit test: Agents use conversation history including tool results when generating responses in tests/unit/ExecutionTest.hs
- [X] T104 [P] [US5] Unit test: FunctionRole messages properly formatted in conversation context in tests/unit/ContextTest.hs

### Implementation for User Story 5 (Principle 4: Expressiveness and Correctness) ✅

- [X] T105 [US5] Verify conversation context includes FunctionRole messages for tool results in src/PatternAgent/Runtime/Execution.hs
- [X] T106 [US5] Verify conversation context is properly passed through iterative execution loop in src/PatternAgent/Runtime/Execution.hs
- [X] T107 [US5] Verify LLM API requests include full conversation history with tool invocations in src/PatternAgent/Runtime/LLM.hs
- [X] T108 [US5] Verify context updates include user message, assistant message with tool call, function message with tool result, and final assistant response in src/PatternAgent/Runtime/Execution.hs

**Checkpoint**: At this point, all user stories should work independently. Developers can create agents with tools, execute them, and maintain conversation context with tool usage.

---

## Phase 8: CLI Agent Execution

**Purpose**: Enable command-line execution of agents from gram files with tool support

**Goal**: Enable developers to execute agents from gram files via CLI using the `--agent` flag, supporting the hello world agent with sayHello tool.

**Independent Test**: Can be fully tested by verifying CLI can load a gram file, parse the agent, create tool library, and execute the agent with tool support.

### Tests for Phase 8 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [X] T118 [P] [CLI] Scenario test: CLI loads agent from gram file and executes with tool support in tests/scenario/CLIAgentExecutionTest.hs
- [X] T119 [P] [CLI] Scenario test: CLI executes hello world agent with sayHello tool and produces greeting in tests/scenario/CLIAgentExecutionTest.hs
- [X] T120 [P] [CLI] Scenario test: CLI handles missing gram file gracefully with error message in tests/scenario/CLIAgentExecutionTest.hs
- [X] T121 [P] [CLI] Scenario test: CLI handles invalid gram file format gracefully with error message in tests/scenario/CLIAgentExecutionTest.hs

**Unit Tests**:
- [X] T122 [P] [CLI] Unit test: Command line argument parsing for --agent flag in tests/unit/CLITest.hs
- [X] T123 [P] [CLI] Unit test: Gram file loading and parsing in tests/unit/CLITest.hs
- [X] T124 [P] [CLI] Unit test: Agent extraction from parsed gram file in tests/unit/CLITest.hs
- [X] T125 [P] [CLI] Unit test: Tool library creation from agent tools in tests/unit/CLITest.hs
- [X] T126 [P] [CLI] Unit test: Error handling for file not found in tests/unit/CLITest.hs
- [X] T127 [P] [CLI] Unit test: Error handling for invalid gram syntax in tests/unit/CLITest.hs

### Implementation for Phase 8 (Principle 4: Expressiveness and Correctness)

- [X] T128 [CLI] Update parseArgs function in app/Main.hs to support --agent flag with file path argument
- [X] T129 [CLI] Implement loadGramFile function in app/Main.hs to read and return gram file contents
- [X] T130 [CLI] Implement parseAgentFromGram function in app/Main.hs to parse gram file and extract Agent (Pattern)
- [X] T131 [CLI] Implement createToolLibraryFromAgent function in app/Main.hs to create ToolLibrary from agent's tools (initially supports sayHello tool for hello world agent)
- [X] T132 [CLI] Update main function in app/Main.hs to handle --agent mode: load gram file, parse agent, create tool library, execute agent
- [X] T133 [CLI] Add error handling for missing --agent file path in app/Main.hs
- [X] T134 [CLI] Add error handling for file read errors in app/Main.hs
- [X] T135 [CLI] Add error handling for gram parsing errors in app/Main.hs
- [X] T136 [CLI] Add error handling for agent execution errors in app/Main.hs
- [X] T137 [CLI] Update usage message in app/Main.hs to document --agent flag: `pattern-agent --agent <gram-file> [--debug] <message>`
- [X] T138 [CLI] Ensure --agent mode works with existing --debug flag in app/Main.hs
- [X] T139 [CLI] Add validation that gram file contains exactly one Agent pattern in app/Main.hs
- [X] T140 [CLI] Add support for hello world agent: detect sayHello tool, create ToolImpl, register in ToolLibrary in app/Main.hs

**Checkpoint**: At this point, developers can execute agents from gram files via CLI. The hello world agent with sayHello tool should work end-to-end.

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T141 [P] Add comprehensive error handling for all edge cases in src/PatternAgent/Runtime/Execution.hs (tool timeout scenarios, multiple simultaneous tool calls, agent with no tools but LLM requests tool call)
- [ ] T142 [P] Update module exports in Language modules (Core, Schema, TypeSignature, Serialization), Runtime modules (Execution, ToolLibrary, LLM, Context) (HelloWorldExample is in tests, exports already complete)
- [ ] T143 [P] Add Haddock documentation to all public functions in Language modules, Runtime modules (HelloWorldExample already has documentation)
- [ ] T144 [P] Run quickstart.md examples validation
- [ ] T145 [P] Additional unit tests for edge cases in tests/unit/ (tool with no parameters, tool with optional parameters, tool with nested record parameters)
- [ ] T146 [P] Additional scenario tests for complex workflows in tests/scenario/ (multiple tools, tool chaining, error recovery)
- [ ] T147 [P] Code cleanup and refactoring across all modules
- [X] T148 [P] Update pattern-agent.cabal exposed-modules list to include all Language modules, Runtime modules (HelloWorldExample is in tests, not exposed)
- [ ] T149 [P] Verify tool-free agents still work correctly (agents with empty tools list)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2)
- **CLI (Phase 8)**: Depends on User Stories 1-4 (needs complete agent execution with tools)
- **Polish (Final Phase)**: Depends on all desired user stories and CLI being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Depends on User Story 1 (needs Tool type to add to Agent)
- **User Story 3 (P1)**: Depends on User Stories 1 and 2 (needs Tool, ToolImpl, ToolLibrary, and Agent with tools)
- **User Story 4 (P1)**: Depends on User Stories 1, 2, and 3 (needs complete tool system and execution with tools)
- **User Story 5 (P2)**: Depends on User Story 3 (needs tool execution infrastructure for context integration)
- **Phase 8 (CLI)**: Depends on User Stories 1, 2, 3, and 4 (needs complete agent execution with tools, hello world example)

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
  - After US4 completes, Phase 8 (CLI) can start (CLI uses complete agent execution with tools)
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
Task: "Unit test: Tool creation with gram type signature in tests/unit/ToolTest.hs"
Task: "Unit test: ToolImpl creation with name, description, schema, invoke function in tests/unit/ToolTest.hs"
Task: "Unit test: Tool accessors (toolName, toolDescription, toolTypeSignature, toolSchema) via lenses in tests/unit/ToolTest.hs"
Task: "Unit test: ToolImpl accessors (toolImplName, toolImplDescription, toolImplSchema) in tests/unit/ToolTest.hs"
Task: "Unit test: Schema validation for valid parameters in tests/unit/ToolTest.hs"
Task: "Unit test: Schema validation for invalid parameters (wrong type, missing required) in tests/unit/ToolTest.hs"
Task: "Unit test: Type signature parsing for simple signatures in tests/unit/ToolTest.hs"
Task: "Unit test: Type signature to JSON schema conversion in tests/unit/ToolTest.hs"

# Launch all types for User Story 1 together:
Task: "Define Tool type alias in src/PatternAgent/Language/Core.hs (type Tool = Pattern Subject) with lenses: toolName, toolDescription, toolTypeSignature, toolSchema"
Task: "Define ToolImpl type in src/PatternAgent/Runtime/ToolLibrary.hs with fields: toolImplName, toolImplDescription, toolImplSchema, toolImplInvoke"
Task: "Define ToolLibrary type in src/PatternAgent/Runtime/ToolLibrary.hs with libraryTools field (Map Text ToolImpl)"
Task: "Define TypeSignature parsed representation type in src/PatternAgent/Language/TypeSignature.hs for parsed gram type signatures"
```

---

## Implementation Strategy

### MVP First (User Stories 1-4 + CLI)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Create and Register Tools)
4. Complete Phase 4: User Story 2 (Equip Agents with Tools)
5. Complete Phase 5: User Story 3 (Execute Tools During Agent Execution)
6. Complete Phase 6: User Story 4 (Hello World Example Agent)
7. Complete Phase 8: CLI Agent Execution (command-line interface)
8. **STOP and VALIDATE**: Test all user stories and CLI independently
9. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (tool creation)
3. Add User Story 2 → Test independently → Deploy/Demo (tool association)
4. Add User Story 3 → Test independently → Deploy/Demo (tool execution)
5. Add User Story 4 → Test independently → Deploy/Demo (hello world example)
6. Add Phase 8 (CLI) → Test independently → Deploy/Demo (CLI agent execution)
7. Add User Story 5 → Test independently → Deploy/Demo (conversation context with tools)
8. Each story/phase adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (tool creation)
   - After US1: Developer A continues with US2, Developer B starts US3 prep
   - After US2: Developer A continues with US3, Developer B starts US4
   - After US3: Developer A continues with US4, Developer B starts CLI (Phase 8)
   - After US4: Developer A continues with CLI, Developer B starts US5
3. Stories/Phases complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- Total tasks: 149
- MVP scope: Phases 1-6 (User Stories 1-4) = 98 tasks
- CLI scope: Phases 1-8 (User Stories 1-4 + CLI) = 140 tasks
- Full feature scope: All phases = 149 tasks
- Phase 0.5 (Tool Description Design) is already complete - gram notation format designed

