# Research: Agent Execution with Scenario Tests, Interactive CLI, and Observability

**Feature**: 004-agent-execution  
**Date**: 2025-01-27  
**Purpose**: Research technical approaches for interactive CLI mode, observability infrastructure, and builtin tools

## Research Questions

### 1. Interactive CLI Mode Implementation

**Question**: What are best practices for implementing interactive CLI mode in Haskell that supports real-time conversations with graceful exit handling?

**Findings**:
- **Decision**: Use simple `getLine` loop with pattern matching for exit commands (Ctrl+D, "exit", "quit")
- **Rationale**: 
  - Simple and sufficient for initial implementation (Principle 5: Progressive Iteration)
  - No external dependencies required (haskeline, readline not needed initially)
  - Standard Haskell I/O handles Ctrl+D as EOF, which can be caught gracefully
  - Exit commands ("exit", "quit") can be pattern-matched in the input loop
  - Conversation context maintained in memory across loop iterations
- **Alternatives Considered**:
  - **haskeline library**: Provides advanced readline features (history, completion) but adds dependency and complexity. Defer until user goals require these features.
  - **readline library**: C library bindings, more complex, not needed for basic interactive mode.
- **Implementation Approach**:
  - Extend `parseArgs` to recognize `--interactive` or `-i` flag
  - Add `InteractiveMode` variant to `CLIMode` data type
  - Implement `handleInteractiveMode` function with `getLine` loop
  - Pattern match on input: empty string (skip), "exit"/"quit" (exit gracefully), otherwise (process as message)
  - Handle EOF (Ctrl+D) by catching `isEOFError` or checking for empty input after EOF
  - Maintain `ConversationContext` across loop iterations
  - Display agent responses immediately after processing

### 2. Observability Infrastructure Design

**Question**: What are best practices for implementing observability (execution traces, performance metrics) in Haskell that can be exported in standard formats?

**Findings**:
- **Decision**: In-memory trace structure with simple metric collection, export to JSON/CSV
- **Rationale**:
  - In-memory storage sufficient for single execution traces (Principle 5: Progressive Iteration)
  - Simple data structures (lists, maps) for trace events and metrics
  - Aeson library (already dependency) provides JSON export
  - CSV export via simple text formatting
  - Optional feature (can be enabled/disabled via flags)
- **Alternatives Considered**:
  - **Persistent storage (database, files)**: Not needed for initial implementation. Can be added later if user goals require persistence.
  - **Structured logging frameworks**: Existing Logging.hs module sufficient. Observability is separate concern (structured data vs. log messages).
  - **External observability tools (OpenTelemetry, Prometheus)**: Too complex for initial implementation. Export formats enable future integration.
- **Implementation Approach**:
  - Create `ExecutionTrace` type: list of trace events (LLM call, tool invocation, context update)
  - Create `PerformanceMetrics` type: execution time, token usage, tool invocation counts
  - Add observability parameter to `executeAgentWithLibrary` (Bool flag)
  - Capture trace events during execution (before/after LLM calls, tool invocations)
  - Capture metrics: start/end timestamps, token counts from LLM responses, tool invocation counts
  - Return observability data in `AgentResponse` or separate return value
  - Export functions: `exportTraceToJSON`, `exportMetricsToCSV`
  - Store in memory during execution, accessible after completion

### 3. Builtin Tools Design

**Question**: What builtin tools should be implemented to enable realistic multi-tool scenario tests?

**Findings**:
- **Decision**: Implement `getCurrentTime`, `calculate`, `formatText` as builtin tools
- **Rationale**:
  - Three complementary tools provide sufficient variety for multi-tool testing
  - `getCurrentTime`: Simple, demonstrates time-based tool
  - `calculate`: Demonstrates computation tool, can handle various math expressions
  - `formatText`: Demonstrates text manipulation tool, complements other tools
  - All tools are simple to implement and test
  - Tools work well together (e.g., get time, calculate duration, format result)
- **Alternatives Considered**:
  - **More tools (4+)**: Unnecessary complexity. Three tools sufficient for multi-tool scenarios.
  - **Complex tools (database queries, API calls)**: Too complex for initial implementation. Simple tools sufficient for testing.
  - **Memory system tool**: Deferred to future feature work per clarification.
- **Implementation Approach**:
  - Follow existing `sayHello` tool pattern in `BuiltinTools.hs`
  - `getCurrentTime`: Returns current time as ISO 8601 string, no parameters
  - `calculate`: Takes expression string (e.g., "2 + 2"), evaluates safely, returns result
  - `formatText`: Takes text and format spec (e.g., "uppercase", "lowercase", "reverse"), returns formatted text
  - Register in `createToolLibraryFromAgent` function
  - Define gram type signatures for each tool
  - Implement tool functions with error handling

### 4. Terminal Input/Output Handling

**Question**: How should interactive CLI mode handle terminal control sequences, long responses, and special characters?

**Findings**:
- **Decision**: Use standard Haskell I/O with simple handling for common cases
- **Rationale**:
  - Standard `getLine` and `putStrLn` sufficient for basic interactive mode
  - Terminal width handling: Let terminal handle wrapping (most terminals do this automatically)
  - Special characters: Text type handles Unicode correctly
  - Control sequences: EOF (Ctrl+D) handled, Ctrl+C handled by default (interrupts program, acceptable for initial implementation)
- **Alternatives Considered**:
  - **Terminal width detection**: Not needed initially. Terminal handles wrapping.
  - **Advanced terminal control**: Not needed for basic interactive mode. Can be added later if user goals require.
- **Implementation Approach**:
  - Use `getLine` for input (handles most cases)
  - Use `putStrLn` for output (terminal handles wrapping)
  - Handle EOF gracefully (check for empty input or catch EOF exception)
  - Handle Ctrl+C as program interrupt (acceptable for initial implementation)
  - Empty message handling: Skip or prompt again (simple validation)

### 5. Scenario Test Structure

**Question**: How should scenario tests be structured to validate agent execution with different tool configurations?

**Findings**:
- **Decision**: Follow existing scenario test pattern with Given/When/Then structure, use mocked LLM
- **Rationale**:
  - Existing scenario tests (HelloWorldTest.hs, ToolExecutionTest.hs) provide pattern
  - Mocked LLM ensures deterministic test behavior
  - Given/When/Then structure aligns with spec acceptance scenarios
  - Separate test files for each scenario (zero tools, one tool, multiple tools)
- **Alternatives Considered**:
  - **Single comprehensive test file**: Less maintainable. Separate files better for clarity.
  - **Property-based testing**: Not needed for scenario tests. Unit tests can use property-based testing.
- **Implementation Approach**:
  - Create `ZeroToolExecutionTest.hs`: Test agent with no tools
  - Create `OneToolExecutionTest.hs`: Test agent with one tool (sayHello)
  - Create `MultiToolExecutionTest.hs`: Test agent with multiple tools (sayHello, getCurrentTime, calculate)
  - Use `MockLLM` from existing test infrastructure
  - Follow Given/When/Then structure from spec acceptance scenarios
  - Verify execution behavior, tool invocations, error handling

## Summary

All research questions resolved. Implementation approach uses simple, progressive solutions:
- Interactive CLI: Simple `getLine` loop with standard exit handling
- Observability: In-memory trace structure with JSON/CSV export
- Builtin tools: Three complementary tools (getCurrentTime, calculate, formatText)
- Terminal handling: Standard Haskell I/O with basic control sequence handling
- Scenario tests: Follow existing pattern with mocked LLM

All decisions align with Principle 5 (Progressive Iteration) - simplest solutions that meet user goals, with clear paths for future enhancement if needed.

