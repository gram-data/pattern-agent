# Data Model: Agent Execution with Scenario Tests, Interactive CLI, and Observability

**Feature**: Agent Execution with Scenario Tests, Interactive CLI, and Observability  
**Date**: 2025-01-27  
**Purpose**: Define the data structures and relationships for scenario tests, interactive CLI mode, observability infrastructure, and builtin tools

## Architecture Overview

This feature extends the existing execution infrastructure with:
1. **Scenario Tests**: Test structures for validating agent execution with different tool configurations
2. **Interactive CLI Mode**: Real-time conversation mode with terminal input/output
3. **Observability Infrastructure**: Execution traces and performance metrics capture
4. **Builtin Tools**: Additional builtin tool implementations for multi-tool scenario testing

## New Data Types

### Interactive CLI Mode

**CLIMode Extension**:
```haskell
-- Existing CLIMode (in app/Main.hs)
data CLIMode
  = StandardMode (Maybe String)      -- Existing: standard mode with optional message
  | AgentMode String [String]         -- Existing: agent mode with gram file and messages
  | InteractiveMode String            -- NEW: interactive mode with gram file
```

**Interactive Session State**:
```haskell
-- Implicit state maintained in handleInteractiveMode function
-- ConversationContext: Maintained across loop iterations
-- Agent: Loaded once at start, reused for all messages
-- ToolLibrary: Created once at start, reused for all tool invocations
```

**Key Points**:
- Interactive mode extends existing `CLIMode` data type
- No new persistent data structures needed
- Conversation context maintained in memory across loop iterations
- Agent and tool library loaded once at session start

### Observability Infrastructure

**ExecutionTrace**:
```haskell
-- NEW type in src/PatternAgent/Runtime/Execution.hs
data TraceEvent
  = LLMCallEvent
      { eventTimestamp :: UTCTime
      , eventModel :: Model
      , eventMessages :: [LLMMessage]
      , eventResponse :: LLMResponse
      }
  | ToolInvocationEvent
      { eventTimestamp :: UTCTime
      , eventToolName :: Text
      , eventArgs :: Value
      , eventResult :: Either Text Value
      }
  | ContextUpdateEvent
      { eventTimestamp :: UTCTime
      , eventContextSize :: Int  -- Number of messages in context
      }

data ExecutionTrace = ExecutionTrace
  { traceEvents :: [TraceEvent]
  , traceStartTime :: UTCTime
  , traceEndTime :: Maybe UTCTime
  }
```

**PerformanceMetrics**:
```haskell
-- NEW type in src/PatternAgent/Runtime/Execution.hs
data PerformanceMetrics = PerformanceMetrics
  { metricsExecutionTime :: NominalDiffTime  -- Total execution time
  , metricsLLMCallCount :: Int              -- Number of LLM API calls
  , metricsToolInvocationCount :: Int       -- Number of tool invocations
  , metricsTokenUsage :: Maybe TokenUsage    -- Token usage from LLM (if available)
  , metricsContextSize :: Int                -- Final context size (number of messages)
  }

data TokenUsage = TokenUsage
  { usagePromptTokens :: Int
  , usageCompletionTokens :: Int
  , usageTotalTokens :: Int
  }
```

**Observability Integration**:
```haskell
-- Extended AgentResponse (in src/PatternAgent/Runtime/Execution.hs)
-- Existing AgentResponse fields:
--   responseContent :: Text
--   responseToolsUsed :: [ToolInvocation]
-- NEW optional fields:
--   responseTrace :: Maybe ExecutionTrace
--   responseMetrics :: Maybe PerformanceMetrics
```

**Key Points**:
- Observability data is optional (can be enabled/disabled)
- Trace events captured during execution
- Metrics calculated at execution completion
- Data stored in memory, accessible after execution
- Export functions convert to JSON/CSV formats

### Builtin Tools

**New Tool Implementations** (in `src/PatternAgent/Runtime/BuiltinTools.hs`):

1. **getCurrentTime**:
   - Description: "Returns the current time as an ISO 8601 formatted string"
   - Parameters: None
   - Return Type: `String` (ISO 8601 timestamp)
   - Implementation: Uses `Data.Time` to get current time, format as ISO 8601

2. **calculate**:
   - Description: "Evaluates a mathematical expression and returns the result"
   - Parameters: `expression :: String` (e.g., "2 + 2", "10 * 5")
   - Return Type: `String` (result as string)
   - Implementation: Safe evaluation of simple math expressions (addition, subtraction, multiplication, division)

3. **formatText**:
   - Description: "Formats text according to a format specification"
   - Parameters: `text :: String`, `format :: String` (e.g., "uppercase", "lowercase", "reverse")
   - Return Type: `String` (formatted text)
   - Implementation: Text transformation based on format spec

**Tool Registration**:
```haskell
-- Extended createToolLibraryFromAgent function
createToolLibraryFromAgent :: Agent -> Either Text ToolLibrary
-- Existing: sayHello tool registration
-- NEW: getCurrentTime, calculate, formatText tool registration
```

**Key Points**:
- Builtin tools follow existing `sayHello` tool pattern
- Tools registered in `createToolLibraryFromAgent` function
- Each tool has gram type signature for serialization
- Tools are simple and complementary for multi-tool testing

## Relationships

### Interactive CLI Mode Flow

```
CLI Arguments
  → parseArgs (recognizes --interactive or -i)
  → InteractiveMode gramFile
  → handleInteractiveMode gramFile
    → Load agent from gram file
    → Create tool library
    → Loop:
      → getLine (user input)
      → Check exit commands ("exit", "quit", EOF)
      → executeAgentWithLibrary (process message)
      → Display response
      → Continue loop (maintain context)
```

### Observability Flow

```
executeAgentWithLibrary (with observability enabled)
  → Create ExecutionTrace (empty, start time)
  → Execute agent iteration:
    → Capture LLMCallEvent (before/after LLM call)
    → Capture ToolInvocationEvent (before/after tool call)
    → Capture ContextUpdateEvent (after context update)
  → Calculate PerformanceMetrics (at completion)
  → Return AgentResponse with trace and metrics
  → Export to JSON/CSV (optional)
```

### Scenario Test Structure

```
Scenario Test File (e.g., ZeroToolExecutionTest.hs)
  → Test Case:
    → Given: Create agent with zero tools
    → When: Execute agent with user input
    → Then: Verify response generated, no tool calls attempted
  → Uses MockLLM for deterministic behavior
  → Verifies execution behavior matches acceptance scenarios
```

## State Transitions

### Interactive CLI Session

1. **Session Start**:
   - Parse CLI arguments → `InteractiveMode gramFile`
   - Load agent from gram file
   - Create tool library
   - Initialize empty conversation context
   - Enter input loop

2. **Message Processing**:
   - Read user input
   - Check exit commands → Exit if matched
   - Execute agent with current context
   - Update context with user message and agent response
   - Display response
   - Continue loop

3. **Session End**:
   - User sends exit command ("exit", "quit", Ctrl+D)
   - Graceful exit (no error)

### Observability Capture

1. **Execution Start**:
   - Create `ExecutionTrace` with start time
   - Initialize `PerformanceMetrics` counters

2. **During Execution**:
   - Capture `LLMCallEvent` before/after each LLM call
   - Capture `ToolInvocationEvent` before/after each tool call
   - Capture `ContextUpdateEvent` after context updates
   - Update metrics counters

3. **Execution Complete**:
   - Set trace end time
   - Calculate final metrics (execution time, token usage)
   - Return trace and metrics in `AgentResponse`

### Builtin Tool Invocation

1. **Tool Registration**:
   - Agent loaded with tool specifications
   - `createToolLibraryFromAgent` matches tool names
   - Tool implementations registered in `ToolLibrary`

2. **Tool Invocation**:
   - LLM requests tool call
   - Execution environment looks up tool in library
   - Tool implementation invoked with parameters
   - Result returned to LLM

## Data Flow

### Interactive CLI Data Flow

```
User Input (String)
  → pack (Text)
  → executeAgentWithLibrary (Agent, Text, ConversationContext, ToolLibrary)
  → AgentResponse
  → Display responseContent
  → Update ConversationContext
  → Loop (next input)
```

### Observability Data Flow

```
Execution Start
  → ExecutionTrace (empty, start time)
  → [Trace Events captured during execution]
  → PerformanceMetrics (calculated at end)
  → AgentResponse (with trace and metrics)
  → Export (JSON/CSV) if requested
```

### Scenario Test Data Flow

```
Test Setup
  → Create agent (with/without tools)
  → Create MockLLM (deterministic responses)
  → Execute agent
  → Verify behavior (response, tool calls, errors)
```

## Validation Rules

### Interactive CLI Mode

- `--interactive` or `-i` flag must be followed by gram file path
- Gram file must exist and be readable
- Gram file must contain valid agent definition
- Exit commands: "exit", "quit", or EOF (Ctrl+D)
- Empty input: Skip or prompt again (implementation choice)

### Observability

- Observability enabled via flag/parameter
- Trace events captured in order of occurrence
- Metrics calculated accurately (timing, counts)
- Export formats: JSON (trace), CSV (metrics)

### Builtin Tools

- Tool names must match exactly ("getCurrentTime", "calculate", "formatText")
- Tool parameters validated against schema
- Tool execution errors handled gracefully
- Tool results formatted as JSON values

## Edge Cases

### Interactive CLI Mode

- **Long responses**: Terminal handles wrapping automatically
- **Empty messages**: Skip or prompt again
- **Ctrl+C**: Program interrupt (acceptable for initial implementation)
- **Special characters**: Text type handles Unicode correctly
- **EOF (Ctrl+D)**: Graceful exit

### Observability

- **High-frequency execution**: In-memory storage sufficient for single execution
- **Large trace data**: No size limits initially (can be optimized later)
- **Execution failures**: Trace captures events up to failure point
- **Fast executions**: Timing accuracy within 100ms acceptable

### Builtin Tools

- **Invalid expressions**: `calculate` returns error message
- **Invalid format spec**: `formatText` returns error message
- **Missing parameters**: Tools use default values or return errors
- **Tool not found**: Error handled in execution environment

## Summary

This feature extends existing execution infrastructure with:
- **Interactive CLI Mode**: Simple extension to `CLIMode` with input loop
- **Observability**: New trace and metrics types, optional in `AgentResponse`
- **Builtin Tools**: Three new tool implementations following existing pattern
- **Scenario Tests**: Test structure following existing pattern

All extensions maintain backward compatibility and follow Principle 5 (Progressive Iteration) - simple solutions that meet user goals.

