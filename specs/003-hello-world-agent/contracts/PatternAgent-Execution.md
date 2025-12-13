# API Contract: Agent Execution with Tool Support

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the API contract for executing agents with tool support, including late binding of tool descriptions to implementations

## Agent Execution

### `executeAgentWithLibrary`

Executes an agent with user input, tool library, and returns the agent's response, handling tool invocations automatically with late binding.

**Signature**:
```haskell
executeAgentWithLibrary
  :: Agent                -- agent: Agent to execute (with ToolSpecifications)
  -> Text                 -- userInput: User's input message
  -> ConversationContext  -- context: Previous conversation context
  -> ToolLibrary          -- library: Tool library for binding descriptions to implementations
  -> IO (Either AgentError AgentResponse)
```

**Preconditions**:
- `agent` must be valid (name, model, instruction non-empty)
- `userInput` must be non-empty
- `context` can be empty (new conversation)
- `library` must contain implementations for all tool descriptions in agent (or tool binding will fail)
- If agent has tool descriptions, tool names must be unique within agent's tool description list

**Postconditions**:
- Returns `Right AgentResponse` if execution succeeds
- Returns `Left AgentError` if execution fails
- `AgentResponse.responseToolsUsed` contains all tool invocations that occurred
- Conversation context updated with user message, tool calls, and agent response

**Tool Binding Flow**:
1. For each ToolSpecification in agent.agentToolSpecs:
   - Lookup Tool in ToolLibrary by name
   - Validate Tool matches ToolSpecification (name, description, schema)
   - Bind ToolSpecification to Tool
2. If any tool binding fails, return ToolError

**Tool Execution Flow**:
1. Build LLM request with agent instructions, conversation context, and tool definitions (from ToolSpecifications)
2. Send request to LLM API
3. Parse response:
   - If `function_call` present: validate and invoke bound Tool, add tool result to context, loop back to step 2
   - If text response: return final response to user
4. Maximum iterations: 10 (prevents infinite loops)

**Errors**:
- `ValidationError`: Empty user input
- `ConfigurationError`: Missing or invalid API key
- `LLMAPIError`: LLM API call failed (network, API error, etc.)
- `ToolError`: Tool execution failed (tool not found in library, binding failed, validation error, execution error)

**Example**:
```haskell
-- Create agent with sayHello tool description
let agent = Agent
      { agentName = "hello_world_agent"
      , agentModel = createModel "gpt-3.5-turbo" OpenAI
      , agentInstruction = "Use sayHello tool to greet users"
      , agentToolSpecs = [sayHelloSpec]
      }

-- Create tool library with sayHello implementation
let library = registerTool "sayHello" sayHelloTool emptyToolLibrary

-- Execute agent with tool library
result <- executeAgentWithLibrary agent "Hello!" emptyContext library

case result of
  Right response -> do
    putStrLn $ responseContent response
    -- responseToolsUsed contains sayHello invocation
  Left error -> putStrLn $ "Error: " ++ show error
```

### `executeAgent` (Legacy, for backward compatibility)

Executes an agent without tool library (for tool-free agents or direct tool binding).

**Signature**:
```haskell
executeAgent
  :: Agent                -- agent: Agent to execute
  -> Text                 -- userInput: User's input message
  -> ConversationContext  -- context: Previous conversation context
  -> IO (Either AgentError AgentResponse)
```

**Note**: This function is for backward compatibility. For agents with tools, use `executeAgentWithLibrary`. This function assumes agent has no tools or tools are pre-bound.

## Tool Binding

### `bindAgentTools`

Binds tool descriptions in an agent to tool implementations from a tool library.

**Signature**:
```haskell
bindAgentTools
  :: Agent                -- agent: Agent with ToolSpecifications
  -> ToolLibrary          -- library: Tool library
  -> Either Text [Tool]   -- Bound tools or error message
```

**Preconditions**:
- `agent` must have valid ToolSpecifications
- `library` must contain implementations for all tool specifications

**Postconditions**:
- Returns `Right [Tool]` if all tools bound successfully
- Returns `Left error` if any tool binding fails

**Binding Rules**:
- For each ToolSpecification in agent.agentToolSpecs:
  - Lookup Tool in ToolLibrary by toolSpecName
  - Validate Tool matches ToolSpecification (name, description, schema must match)
  - Add Tool to bound tools list
- If any tool not found or doesn't match, return Left error

## Tool Call Detection

### `detectToolCall`

Detects if an LLM response contains a tool call request.

**Signature**:
```haskell
detectToolCall :: LLMResponse -> Maybe (Text, Value)
  -- Returns: Just (toolName, args) if tool call detected, Nothing otherwise
```

**Preconditions**:
- `LLMResponse` must be from OpenAI API (function calling format)

**Postconditions**:
- Returns `Just (toolName, args)` if `function_call` present in response
- Returns `Nothing` if no tool call (text response)

## Tool Invocation

### `invokeTool`

Invokes a bound tool with validated parameters.

**Signature**:
```haskell
invokeTool
  :: Tool                -- tool: Bound tool to invoke
  -> Value               -- args: Validated tool arguments
  -> IO (Either Text Value)
  -- Returns: Right result or Left error message
```

**Preconditions**:
- `args` must be validated against `tool.toolSchema`
- `tool` must be valid (name, description, schema, invoke function)

**Postconditions**:
- Returns `Right result` if tool execution succeeds
- Returns `Left error` if tool execution fails

**Errors**:
- Tool execution exceptions caught and returned as `Left error`

## Parameter Validation

### `validateToolArgs`

Validates tool arguments against tool schema (re-exported from Tool module).

**Signature**:
```haskell
validateToolArgs :: Value -> Value -> Either Text Value
  -- schema: Tool's JSON schema
  -- args: Arguments to validate
  -- Returns: Right validated args or Left error message
```

## Conversation Context with Tools

Conversation context now supports function role messages for tool results.

**Message Types**:
- `UserRole`: User messages
- `AssistantRole`: Assistant messages (may include tool call requests)
- `FunctionRole Text`: Function messages with tool name (tool results)

**Context Updates**:
- User message added to context
- Assistant message with tool call added to context
- Function message with tool result added to context
- Final assistant text response added to context

## Iterative Execution

The execution loop continues until:
- LLM returns text response (no tool call)
- Maximum iterations reached (10)
- Error occurs (validation, tool binding, tool execution, API error)

**Iteration Limit**:
- Maximum 10 tool call iterations per execution
- Prevents infinite loops if LLM keeps requesting tools
- If limit reached, returns error or final response

## A/B Testing Support

**Scenario**: Test different tool implementations with the same agent specification.

**Approach**:
1. Create Agent with ToolSpecifications (same for both tests)
2. Create ToolLibrary A with implementation A
3. Create ToolLibrary B with implementation B (same tool name, different implementation)
4. Execute agent with ToolLibrary A → measure results
5. Execute agent with ToolLibrary B → measure results
6. Compare results

**Example**:
```haskell
-- Same agent specification
let agent = Agent { ..., agentToolSpecs = [sayHelloSpec] }

-- Different tool implementations
let libraryA = registerTool "sayHello" sayHelloToolA emptyToolLibrary
let libraryB = registerTool "sayHello" sayHelloToolB emptyToolLibrary

-- A/B test
resultA <- executeAgentWithLibrary agent input context libraryA
resultB <- executeAgentWithLibrary agent input context libraryB
```

## Notes

- Tool execution is synchronous (tools complete before response generation continues)
- Tool invocations are tracked in `AgentResponse.responseToolsUsed`
- Conversation context includes all tool calls and results
- Error handling ensures agent execution doesn't crash on tool failures
- Tool-free agents still supported (backward compatible)
- Tool binding happens at execution time, enabling A/B testing
- Tool specifications are serializable, tool implementations are not
- Tool library enables late binding and different implementations for same tool name
