# API Contract: Observability Infrastructure

**Feature**: Agent Execution with Scenario Tests, Interactive CLI, and Observability  
**Module**: `src/PatternAgent/Runtime/Execution.hs`  
**Date**: 2025-01-27

## Overview

Observability infrastructure captures execution traces and performance metrics during agent execution. This enables developers to understand agent behavior, debug issues, and optimize performance.

## Types

### ExecutionTrace

**Definition**:
```haskell
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
      , eventContextSize :: Int
      }

data ExecutionTrace = ExecutionTrace
  { traceEvents :: [TraceEvent]
  , traceStartTime :: UTCTime
  , traceEndTime :: Maybe UTCTime
  }
```

**Purpose**: Record sequence of operations during agent execution

### PerformanceMetrics

**Definition**:
```haskell
data TokenUsage = TokenUsage
  { usagePromptTokens :: Int
  , usageCompletionTokens :: Int
  , usageTotalTokens :: Int
  }

data PerformanceMetrics = PerformanceMetrics
  { metricsExecutionTime :: NominalDiffTime
  , metricsLLMCallCount :: Int
  , metricsToolInvocationCount :: Int
  , metricsTokenUsage :: Maybe TokenUsage
  , metricsContextSize :: Int
  }
```

**Purpose**: Quantitative measurements of agent execution performance

## Functions

### executeAgentWithLibrary (Extended)

**Signature**:
```haskell
executeAgentWithLibrary
  :: Bool                 -- ^ debug: Enable debug logging
  -> Agent                -- ^ agent: Agent with Tools
  -> Text                 -- ^ userInput: User's input message
  -> ConversationContext -- ^ context: Previous conversation context
  -> ToolLibrary          -- ^ library: Tool library for binding
  -> Bool                 -- ^ NEW: enableObservability: Enable observability capture
  -> IO (Either AgentError (AgentResponse, Maybe ExecutionTrace, Maybe PerformanceMetrics))
```

**Extended Return Type**:
- Returns tuple: `(AgentResponse, Maybe ExecutionTrace, Maybe PerformanceMetrics)`
- Observability data is optional (None if disabled)

**Behavior**:
- If `enableObservability` is `True`:
  - Create `ExecutionTrace` with start time
  - Capture trace events during execution
  - Calculate `PerformanceMetrics` at completion
  - Return trace and metrics
- If `enableObservability` is `False`:
  - Return `Nothing` for trace and metrics
  - No performance overhead

### exportTraceToJSON

**Signature**:
```haskell
exportTraceToJSON :: ExecutionTrace -> Value
```

**Purpose**: Export execution trace to JSON format

**Output**: JSON object with trace events and timing information

### exportMetricsToCSV

**Signature**:
```haskell
exportMetricsToCSV :: PerformanceMetrics -> Text
```

**Purpose**: Export performance metrics to CSV format

**Output**: CSV string with metrics values

## AgentResponse Extension

**Extended Definition**:
```haskell
data AgentResponse = AgentResponse
  { responseContent :: Text
  , responseToolsUsed :: [ToolInvocation]
  -- NEW optional fields (for backward compatibility, may be separate return value):
  -- responseTrace :: Maybe ExecutionTrace
  -- responseMetrics :: Maybe PerformanceMetrics
  }
```

**Note**: For backward compatibility, observability data may be returned as separate values rather than extending `AgentResponse`.

## Usage Examples

### Enable Observability

```haskell
import PatternAgent.Runtime.Execution
import PatternAgent.Runtime.Observability (exportTraceToJSON, exportMetricsToCSV)

-- Execute with observability enabled
result <- executeAgentWithLibrary debug agent userInput context library True

case result of
  Right (response, Just trace, Just metrics) -> do
    -- Access trace and metrics
    let traceJSON = exportTraceToJSON trace
    let metricsCSV = exportMetricsToCSV metrics
    -- Use trace and metrics for analysis
  Right (response, Nothing, Nothing) -> do
    -- Observability disabled
  Left err -> do
    -- Handle error
```

### Export Observability Data

```haskell
-- Export trace to JSON file
traceJSON <- exportTraceToJSON trace
BL.writeFile "trace.json" (encodePretty traceJSON)

-- Export metrics to CSV file
metricsCSV <- exportMetricsToCSV metrics
TIO.writeFile "metrics.csv" metricsCSV
```

## Trace Event Capture

### LLM Call Events

**Captured**:
- Timestamp (before and after LLM call)
- Model identifier
- Input messages
- Response (including token usage if available)

**Example**:
```haskell
LLMCallEvent
  { eventTimestamp = 2025-01-27 10:30:00 UTC
  , eventModel = "gpt-4o-mini"
  , eventMessages = [userMessage, ...]
  , eventResponse = LLMResponse { ... }
  }
```

### Tool Invocation Events

**Captured**:
- Timestamp (before and after tool call)
- Tool name
- Tool arguments
- Tool result (success or error)

**Example**:
```haskell
ToolInvocationEvent
  { eventTimestamp = 2025-01-27 10:30:01 UTC
  , eventToolName = "getCurrentTime"
  , eventArgs = object []
  , eventResult = Right (String "2025-01-27T10:30:01Z")
  }
```

### Context Update Events

**Captured**:
- Timestamp (after context update)
- Context size (number of messages)

**Example**:
```haskell
ContextUpdateEvent
  { eventTimestamp = 2025-01-27 10:30:02 UTC
  , eventContextSize = 4
  }
```

## Performance Metrics Calculation

### Execution Time

- Calculated: `traceEndTime - traceStartTime`
- Accuracy: Within 100ms

### LLM Call Count

- Counted: Number of `LLMCallEvent` events

### Tool Invocation Count

- Counted: Number of `ToolInvocationEvent` events

### Token Usage

- Extracted: From `LLMResponse` if available
- May be `Nothing` if LLM provider doesn't return token usage

### Context Size

- Measured: Final number of messages in conversation context

## Error Cases

### Observability Disabled

- Returns `Nothing` for trace and metrics
- No performance overhead
- Normal execution behavior

### Execution Failure

- Trace captures events up to failure point
- Metrics calculated for partial execution
- Error returned in `Either AgentError` result

## Implementation Notes

- Observability is optional (can be enabled/disabled)
- In-memory storage (no persistence)
- Performance overhead: < 10% when enabled
- Export formats: JSON (trace), CSV (metrics)
- Future: Can add persistent storage or external tool integration

