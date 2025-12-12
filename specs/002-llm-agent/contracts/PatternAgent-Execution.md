# API Contract: Agent Execution

**Feature**: Basic LLM Agent  
**Date**: 2025-01-27  
**Purpose**: Define the API contract for executing agents and generating responses

## Agent Execution

### `executeAgent`

Executes an agent with user input and returns the agent's response.

**Signature**:
```haskell
executeAgent
  :: Agent                -- agent: Agent to execute
  -> Text                 -- userInput: User's input message
  -> ConversationContext  -- context: Previous conversation context
  -> IO (Either AgentError AgentResponse)
```

**Preconditions**:
- `agent` must be valid (created via `createAgent`)
- `userInput` must be non-empty
- `context` can be empty (new conversation) or contain previous messages

**Postconditions**:
- Returns `Right AgentResponse` on success
- Returns `Left AgentError` on failure
- Response includes agent's text and any tools used
- Conversation context is NOT modified (caller must update context)

**Errors**:
- `LLMAPIError "Network error: {details}"` - Network failure calling LLM API
- `LLMAPIError "API error: {details}"` - LLM API returned error
- `LLMAPIError "Invalid model: {modelId}"` - Model not available
- `ToolError "Tool execution failed: {details}"` - Tool invocation failed
- `ValidationError "Empty user input"` - User input is empty

**Example**:
```haskell
let agent = createAgent "capital_agent" model instruction Nothing [] Nothing
let context = []
result <- executeAgent agent "What's the capital of France?" context
case result of
  Right response -> print (responseContent response)
  Left error -> print ("Error: " ++ show error)
```

## Conversation Context Management

### `addMessage`

Adds a message to conversation context.

**Signature**:
```haskell
addMessage
  :: MessageRole  -- role: Role of the message sender
  -> Text         -- content: Message content
  -> ConversationContext  -- context: Current context
  -> ConversationContext  -- Updated context with new message
```

**Preconditions**:
- `content` must be non-empty
- `context` can be empty or contain previous messages

**Postconditions**:
- Returns new context with message appended
- Original context unchanged (pure function)

**Example**:
```haskell
let context = []
let context' = addMessage UserRole "What's the capital of France?" context
let context'' = addMessage AssistantRole "The capital of France is Paris." context'
```

### `createMessage`

Creates a message with specified role and content.

**Signature**:
```haskell
createMessage
  :: MessageRole  -- role: Role of the message sender
  -> Text        -- content: Message content
  -> Message
```

**Preconditions**:
- `content` must be non-empty

**Errors**:
- `ValidationError "Message content cannot be empty"`

**Example**:
```haskell
let msg = createMessage UserRole "What's the capital of France?"
```

### `emptyContext`

Returns an empty conversation context.

**Signature**:
```haskell
emptyContext :: ConversationContext
```

**Postconditions**:
- Returns empty list (new conversation)

**Example**:
```haskell
let context = emptyContext
```

## Response Access

### `responseContent`

Returns the text content of an agent response.

**Signature**:
```haskell
responseContent :: AgentResponse -> Text
```

### `responseToolsUsed`

Returns the list of tools invoked during response generation.

**Signature**:
```haskell
responseToolsUsed :: AgentResponse -> [ToolInvocation]
```

## Tool Invocation Access

### `invocationToolName`

Returns the name of the tool that was invoked.

**Signature**:
```haskell
invocationToolName :: ToolInvocation -> Text
```

### `invocationArgs`

Returns the arguments passed to the tool.

**Signature**:
```haskell
invocationArgs :: ToolInvocation -> Value
```

### `invocationResult`

Returns the tool result (Right) or error (Left).

**Signature**:
```haskell
invocationResult :: ToolInvocation -> Either Text Value
```

## Error Handling

### `AgentError` Type

Represents various error conditions during agent execution.

**Type Definition**:
```haskell
data AgentError
  = LLMAPIError Text      -- LLM API call failed
  | ToolError Text        -- Tool execution failed
  | ValidationError Text  -- Input validation failed
  | ConfigurationError Text  -- Agent configuration invalid
  deriving (Eq, Show)
```

**Error Message Format**:
- Errors include descriptive messages for debugging
- Error types indicate the source of the failure
- Error messages are human-readable

## Notes

- `executeAgent` is an `IO` action (may perform network calls, tool execution)
- Conversation context management is pure (no side effects)
- Error handling uses `Either AgentError` for type safety
- Context must be managed by caller (agent execution does not persist context)
- Tool invocations are included in response for transparency
