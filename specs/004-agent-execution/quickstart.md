# Quickstart: Agent Execution with Scenario Tests, Interactive CLI, and Observability

**Feature**: Agent Execution with Scenario Tests, Interactive CLI, and Observability  
**Date**: 2025-01-27  
**Purpose**: Provide a quick start guide for using interactive CLI mode, observability features, and builtin tools

## Prerequisites

- Haskell development environment (GHC 2024)
- LLM API access (OpenAI API key)
- Pattern-agent library installed
- Understanding of basic agent creation and tool execution (see specs/003-hello-world-agent/quickstart.md)

## Example 1: Interactive CLI Mode

Launch an interactive conversation with an agent.

### Basic Usage

```bash
# Launch interactive mode
pattern-agent --interactive examples/helloAgent.gram

# Or use short flag
pattern-agent -i examples/helloAgent.gram
```

### Interactive Session Example

```bash
$ pattern-agent -i examples/helloAgent.gram
💬 Hello!
🤖 Hello! How can I help you today?
💬 What's your name?
🤖 I'm a helpful assistant. How can I assist you?
💬 exit
```

### With Tools

```bash
$ pattern-agent --interactive examples/multiToolAgent.gram
💬 What time is it?
🛠️  getCurrentTime()
🤖 The current time is 2025-01-27T10:30:00Z
💬 Calculate 15 * 7
🛠️  calculate({"expression": "15 * 7"})
🤖 15 * 7 = 105
💬 Format "Hello World" as uppercase
🛠️  formatText({"text": "Hello World", "format": "uppercase"})
🤖 HELLO WORLD
💬 quit
```

### Exit Commands

- Type `exit` and press Enter
- Type `quit` and press Enter
- Press `Ctrl+D` (EOF)

All exit commands terminate the session gracefully.

## Example 2: Using Builtin Tools

Create an agent with builtin tools for multi-tool scenario testing.

### Agent with getCurrentTime Tool

```gram
[timeAgent:Agent {
  description: "Agent that tells the current time"
  instruction: "When asked about the time, use the getCurrentTime tool to get the current time and respond with it."
  model: "OpenAI/gpt-4o-mini"
} |
  [getCurrentTime:Tool {
    description: "Returns the current time as an ISO 8601 formatted string"
  } | (::String)]
]
```

### Agent with calculate Tool

```gram
[calculatorAgent:Agent {
  description: "Agent that performs calculations"
  instruction: "When asked to calculate, use the calculate tool to evaluate mathematical expressions."
  model: "OpenAI/gpt-4o-mini"
} |
  [calculate:Tool {
    description: "Evaluates a mathematical expression and returns the result"
  } | (expression::String)==>(::String)]
]
```

### Agent with formatText Tool

```gram
[formatterAgent:Agent {
  description: "Agent that formats text"
  instruction: "When asked to format text, use the formatText tool to apply formatting."
  model: "OpenAI/gpt-4o-mini"
} |
  [formatText:Tool {
    description: "Formats text according to a format specification"
  } | (text::String, format::String)==>(::String)]
]
```

### Agent with Multiple Tools

```gram
[multiToolAgent:Agent {
  description: "Agent with multiple tools"
  instruction: "Use appropriate tools based on user requests. Use getCurrentTime for time queries, calculate for math, and formatText for text formatting."
  model: "OpenAI/gpt-4o-mini"
} |
  [getCurrentTime:Tool {
    description: "Returns the current time as an ISO 8601 formatted string"
  } | (::String)],
  [calculate:Tool {
    description: "Evaluates a mathematical expression and returns the result"
  } | (expression::String)==>(::String)],
  [formatText:Tool {
    description: "Formats text according to a format specification"
  } | (text::String, format::String)==>(::String)]
]
```

## Example 3: Observability

Enable observability to capture execution traces and performance metrics.

### Enable Observability in Code

```haskell
import PatternAgent.Runtime.Execution
import PatternAgent.Runtime.Observability (exportTraceToJSON, exportMetricsToCSV)

-- Execute with observability enabled
result <- executeAgentWithLibrary debug agent userInput context library True

case result of
  Right (response, Just trace, Just metrics) -> do
    -- Access trace and metrics
    putStrLn "Execution completed with observability data"
    
    -- Export trace to JSON
    let traceJSON = exportTraceToJSON trace
    BL.writeFile "trace.json" (encodePretty traceJSON)
    
    -- Export metrics to CSV
    let metricsCSV = exportMetricsToCSV metrics
    TIO.writeFile "metrics.csv" metricsCSV
    
  Right (response, Nothing, Nothing) -> do
    -- Observability disabled
    putStrLn "Execution completed (observability disabled)"
    
  Left err -> do
    putStrLn $ "Error: " ++ show err
```

### Trace Events

Execution trace captures:
- **LLM Call Events**: Model, messages, response, token usage
- **Tool Invocation Events**: Tool name, arguments, result
- **Context Update Events**: Context size after updates

### Performance Metrics

Performance metrics include:
- **Execution Time**: Total execution time
- **LLM Call Count**: Number of LLM API calls
- **Tool Invocation Count**: Number of tool invocations
- **Token Usage**: Prompt, completion, and total tokens (if available)
- **Context Size**: Final number of messages in context

### Export Formats

**JSON Export (Trace)**:
```json
{
  "traceEvents": [
    {
      "eventType": "LLMCallEvent",
      "eventTimestamp": "2025-01-27T10:30:00Z",
      "eventModel": "gpt-4o-mini",
      "eventMessages": [...],
      "eventResponse": {...}
    },
    {
      "eventType": "ToolInvocationEvent",
      "eventTimestamp": "2025-01-27T10:30:01Z",
      "eventToolName": "getCurrentTime",
      "eventArgs": {},
      "eventResult": {"right": "2025-01-27T10:30:01Z"}
    }
  ],
  "traceStartTime": "2025-01-27T10:30:00Z",
  "traceEndTime": "2025-01-27T10:30:02Z"
}
```

**CSV Export (Metrics)**:
```csv
execution_time,llm_call_count,tool_invocation_count,prompt_tokens,completion_tokens,total_tokens,context_size
2.5,1,1,100,50,150,4
```

## Example 4: Scenario Tests

Write scenario tests to validate agent execution with different tool configurations.

### Zero Tools Scenario Test

```haskell
import Test.Tasty
import Test.Tasty.HUnit

testZeroToolExecution :: TestTree
testZeroToolExecution = testCase "Agent with zero tools executes correctly" $ do
  -- Given: Create agent with zero tools
  let agent = createAgent
        "zeroToolAgent"
        Nothing
        (createModel "gpt-4o-mini" OpenAI)
        "You are a helpful assistant."
        []
  
  -- When: Execute agent with user input
  result <- executeAgentWithLibrary False agent "Hello!" emptyContext emptyToolLibrary
  
  -- Then: Verify response generated, no tool calls
  case result of
    Right response -> do
      assertBool "Response should not be empty" (not (T.null (responseContent response)))
      assertBool "No tools should be used" (null (responseToolsUsed response))
    Left err -> assertFailure $ "Execution failed: " ++ show err
```

### One Tool Scenario Test

```haskell
testOneToolExecution :: TestTree
testOneToolExecution = testCase "Agent with one tool executes correctly" $ do
  -- Given: Create agent with sayHello tool
  let agent = createAgent
        "oneToolAgent"
        Nothing
        (createModel "gpt-4o-mini" OpenAI)
        "Use sayHello tool to greet users"
        [sayHelloTool]
  
  -- Create tool library
  library <- createToolLibraryFromAgent agent
  
  -- When: Execute agent with greeting
  result <- executeAgentWithLibrary False agent "Hello!" emptyContext library
  
  -- Then: Verify tool invoked and response generated
  case result of
    Right response -> do
      assertBool "Response should not be empty" (not (T.null (responseContent response)))
      assertBool "Tool should be used" (not (null (responseToolsUsed response)))
      let toolName = invocationToolName (head (responseToolsUsed response))
      assertEqual "Tool name" "sayHello" toolName
    Left err -> assertFailure $ "Execution failed: " ++ show err
```

### Multiple Tools Scenario Test

```haskell
testMultiToolExecution :: TestTree
testMultiToolExecution = testCase "Agent with multiple tools executes correctly" $ do
  -- Given: Create agent with multiple tools
  let agent = createAgent
        "multiToolAgent"
        Nothing
        (createModel "gpt-4o-mini" OpenAI)
        "Use appropriate tools based on user requests"
        [getCurrentTimeTool, calculateTool, formatTextTool]
  
  -- Create tool library
  library <- createToolLibraryFromAgent agent
  
  -- When: Execute agent with request requiring specific tool
  result <- executeAgentWithLibrary False agent "What time is it?" emptyContext library
  
  -- Then: Verify correct tool selected and invoked
  case result of
    Right response -> do
      assertBool "Response should not be empty" (not (T.null (responseContent response)))
      assertBool "Tool should be used" (not (null (responseToolsUsed response)))
      let toolName = invocationToolName (head (responseToolsUsed response))
      assertEqual "Tool name" "getCurrentTime" toolName
    Left err -> assertFailure $ "Execution failed: " ++ show err
```

## Common Patterns

### Interactive Mode with Debug

```bash
pattern-agent --interactive --debug examples/agent.gram
```

Debug mode shows:
- Raw LLM request/response JSON
- Tool invocation details
- Conversation context updates

### Observability for Performance Analysis

```haskell
-- Enable observability and analyze performance
result <- executeAgentWithLibrary False agent input context library True

case result of
  Right (_, Just trace, Just metrics) -> do
    -- Analyze execution time
    putStrLn $ "Execution time: " ++ show (metricsExecutionTime metrics)
    
    -- Analyze token usage
    case metricsTokenUsage metrics of
      Just usage -> putStrLn $ "Tokens used: " ++ show (usageTotalTokens usage)
      Nothing -> putStrLn "Token usage not available"
    
    -- Analyze tool usage
    putStrLn $ "Tool invocations: " ++ show (metricsToolInvocationCount metrics)
```

### Multi-Tool Agent Testing

```haskell
-- Create agent with all builtin tools
let agent = createAgent
      "testAgent"
      Nothing
      (createModel "gpt-4o-mini" OpenAI)
      "Use tools as needed"
      [getCurrentTimeTool, calculateTool, formatTextTool]

-- Test different tool selections
testTimeQuery <- executeAgentWithLibrary False agent "What time is it?" emptyContext library
testCalculation <- executeAgentWithLibrary False agent "Calculate 10 * 5" emptyContext library
testFormatting <- executeAgentWithLibrary False agent "Format 'hello' as uppercase" emptyContext library
```

## Troubleshooting

### Interactive Mode Issues

**Problem**: Interactive mode doesn't start
- **Solution**: Check that gram file path is correct and file exists
- **Solution**: Verify gram file contains valid agent definition

**Problem**: Exit commands don't work
- **Solution**: Use exact commands: "exit", "quit", or Ctrl+D
- **Solution**: Check for input buffering issues

### Builtin Tools Issues

**Problem**: Tool not found error
- **Solution**: Verify tool name matches exactly ("getCurrentTime", "calculate", "formatText")
- **Solution**: Check tool is registered in `createToolLibraryFromAgent`

**Problem**: Tool execution fails
- **Solution**: Verify tool parameters match schema
- **Solution**: Check tool implementation for error handling

### Observability Issues

**Problem**: Observability data not captured
- **Solution**: Verify observability flag is `True` in `executeAgentWithLibrary`
- **Solution**: Check that execution completed successfully

**Problem**: Export fails
- **Solution**: Verify JSON/CSV export functions are imported
- **Solution**: Check file write permissions

## Next Steps

- See `data-model.md` for detailed data structure documentation
- See `contracts/` for API contract documentation
- See existing scenario tests for more examples
- Extend builtin tools or add custom tools as needed

