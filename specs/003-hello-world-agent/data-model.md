# Data Model: Hello World Agent with Tool Execution

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the data structures and relationships for tool creation, tool association with agents, and tool execution

## Architecture: Tool Description vs Implementation

**Key Design Decision**: Separate tool descriptions (serializable, declarative) from tool implementations (executable, bound at runtime). This enables:
- Gram notation serialization (descriptions only)
- Late binding (implementations bound at execution time)
- A/B testing (same agent specification with different tool implementations)
- Tool library pattern (registry of implementations)

**Flow**:
1. **Gram Notation** → Agent with `ToolSpecification` (name, description, schema) - serializable
2. **Deserialization** → Agent with `ToolSpecification` list
3. **Execution Environment** → Tool library/registry maps tool names to implementations
4. **Tool Binding** → At execution time, match `ToolSpecification` to `Tool` implementation from registry
5. **Execution** → Use bound `Tool` implementations to invoke tools

## Entities

### ToolSpecification

Represents a tool's declarative specification (serializable, no implementation).

**Fields**:
- `toolSpecName :: Text` - Unique name for the tool (required)
- `toolSpecDescription :: Text` - Natural language description of what the tool does (required)
- `toolSpecTypeSignature :: Text` - **NEW**: Type signature in gram notation (e.g., `(name: Text) --> IO Text`) (required)
- `toolSpecSchema :: Value` - JSON schema (auto-generated from type signature, not manually specified)

**Validation Rules**:
- `toolSpecName` must be non-empty
- `toolSpecDescription` must be non-empty
- `toolSpecTypeSignature` must be valid gram notation type signature
- `toolSpecSchema` is automatically generated from `toolSpecTypeSignature` using `typeSignatureToJSONSchema`

**Relationships**:
- ToolSpecification belongs to zero or more Agents (specifications can be shared)
- ToolSpecification is bound to Tool implementation at execution time

**Type Definition**:
```haskell
data ToolSpecification = ToolSpecification
  { toolSpecName :: Text
  , toolSpecDescription :: Text
  , toolSpecTypeSignature :: Text  -- Gram notation type signature (e.g., "(name: Text) --> IO Text")
  , toolSpecSchema :: Value  -- Auto-generated JSON schema from type signature
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

**Accessors**:
- `toolSpecName :: ToolSpecification -> Text` - Returns tool name
- `toolSpecDescription :: ToolSpecification -> Text` - Returns tool description
- `toolSpecTypeSignature :: ToolSpecification -> Text` - Returns gram type signature
- `toolSpecSchema :: ToolSpecification -> Value` - Returns auto-generated JSON schema

**Schema Generation**: JSON schema is automatically generated from gram type signature using `typeSignatureToJSONSchema` function. The schema is computed when the ToolSpecification is created, not stored separately.

**Serialization**: Fully serializable in gram notation (type signature in gram, schema generated on deserialization)

### Tool

Represents a tool with its executable implementation (bound at runtime, not serializable).

**Fields**:
- `toolName :: Text` - Unique name for the tool (required, must match ToolSpecification name)
- `toolDescription :: Text` - Natural language description (required, matches ToolSpecification)
- `toolSchema :: Value` - JSON schema (required, matches ToolSpecification schema)
- `toolInvoke :: Value -> IO Value` - Function that invokes the tool with JSON parameters and returns JSON result (required)

**Validation Rules**:
- `toolName` must match a ToolSpecification name when bound
- `toolDescription` and `toolSchema` should match ToolSpecification (validated at binding time)
- `toolInvoke` function must handle JSON parameter conversion and error cases

**Relationships**:
- Tool is bound from ToolSpecification at execution time via ToolLibrary
- Tool invocation produces ToolInvocation record

**Type Definition**:
```haskell
data Tool = Tool
  { toolName :: Text
  , toolDescription :: Text
  , toolSchema :: Value  -- Aeson Value for JSON schema
  , toolInvoke :: Value -> IO Value  -- JSON in, JSON out
  }
  deriving (Generic)  -- Note: Cannot derive Eq/Show/ToJSON/FromJSON due to function field
```

**Accessors**:
- `toolName :: Tool -> Text` - Returns tool name
- `toolDescription :: Tool -> Text` - Returns tool description
- `toolSchema :: Tool -> Value` - Returns tool parameter schema

**Binding**: Created from ToolSpecification + ToolLibrary lookup at execution time

### ToolLibrary

Registry that maps tool names to executable implementations.

**Purpose**: Enables late binding of tool descriptions to implementations, supporting A/B testing and different execution environments.

**Fields**:
- `libraryTools :: Map Text Tool` - Map from tool name to Tool implementation

**Operations**:
- `registerTool :: Text -> Tool -> ToolLibrary -> ToolLibrary` - Register a tool implementation
- `lookupTool :: Text -> ToolLibrary -> Maybe Tool` - Lookup tool by name
- `bindTool :: ToolSpecification -> ToolLibrary -> Maybe Tool` - Bind specification to implementation

**Type Definition**:
```haskell
data ToolLibrary = ToolLibrary
  { libraryTools :: Map Text Tool
  }
  deriving (Generic)

emptyToolLibrary :: ToolLibrary
emptyToolLibrary = ToolLibrary Map.empty

registerTool :: Text -> Tool -> ToolLibrary -> ToolLibrary
registerTool name tool (ToolLibrary tools) = ToolLibrary $ Map.insert name tool tools

lookupTool :: Text -> ToolLibrary -> Maybe Tool
lookupTool name (ToolLibrary tools) = Map.lookup name tools

bindTool :: ToolSpecification -> ToolLibrary -> Maybe Tool
bindTool spec library = do
  tool <- lookupTool (toolSpecName spec) library
  -- Validate that tool matches specification (optional, for safety)
  guard $ toolDescription tool == toolSpecDescription spec
  guard $ toolSchema tool == toolSpecSchema spec
  return tool
```

**A/B Testing Support**: Different ToolLibrary instances can provide different implementations for the same tool name, enabling A/B testing of tool implementations while keeping the same agent specification.

### Agent (Updated)

Represents an LLM-powered agent with identity, model, instructions, and tool descriptions.

**Fields** (existing + new):
- `agentName :: Text` - Unique agent identifier (required)
- `agentDescription :: Maybe Text` - Optional agent description
- `agentModel :: Model` - LLM model to use (required)
- `agentInstruction :: Text` - Agent behavior instructions (required)
- `agentToolSpecs :: [ToolSpecification]` - **NEW**: List of tool specifications available to the agent (optional, defaults to empty list)

**Validation Rules**:
- `agentName` must be non-empty
- `agentInstruction` must be non-empty
- `agentToolSpecs` can be empty (tool-free agents supported)
- Tool names must be unique within `agentToolSpecs` list

**Relationships**:
- Agent has zero or more ToolSpecifications
- Agent produces AgentResponse during execution
- Agent maintains ConversationContext
- Agent's tool specifications are bound to Tool implementations at execution time via ToolLibrary

**Type Definition**:
```haskell
data Agent = Agent
  { agentName :: Text
  , agentDescription :: Maybe Text
  , agentModel :: Model
  , agentInstruction :: Text
  , agentToolSpecs :: [ToolSpecification]  -- NEW: Tool specifications (serializable)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

**Serialization**: Fully serializable in gram notation (tool specifications, not implementations)

**Execution**: Tool specifications are bound to implementations at execution time via ToolLibrary parameter to executeAgent

### ToolInvocation (Existing, Enhanced)

Represents a single tool invocation during agent execution. Already defined in Execution.hs, no changes needed.

**Fields**:
- `invocationToolName :: Text` - Name of the tool that was invoked
- `invocationArgs :: Value` - JSON arguments passed to the tool
- `invocationResult :: Either Text Value` - Tool result (Right) or error message (Left)

**Validation Rules**:
- `invocationToolName` must match a tool description in the agent's tool list
- Tool implementation must exist in ToolLibrary
- `invocationArgs` must conform to tool's schema (validated before invocation)

**Relationships**:
- ToolInvocation references a ToolSpecification (by name)
- ToolInvocation uses Tool implementation from ToolLibrary
- ToolInvocation is part of AgentResponse

**Type Definition**:
```haskell
data ToolInvocation = ToolInvocation
  { invocationToolName :: Text
  , invocationArgs :: Value
  , invocationResult :: Either Text Value
  }
  deriving (Eq, Show, Generic)
```

### AgentResponse (Existing, No Changes)

Represents the response generated by an agent. Already defined in Execution.hs, no changes needed.

**Fields**:
- `responseContent :: Text` - Text content of the agent's response (required)
- `responseToolsUsed :: [ToolInvocation]` - List of tools invoked during response generation (optional)

**Validation Rules**:
- `responseContent` must be non-empty (unless error case)
- `responseToolsUsed` can be empty (no tools used)

**Relationships**:
- AgentResponse is produced by Agent execution
- AgentResponse may reference ToolInvocation results

**Type Definition**:
```haskell
data AgentResponse = AgentResponse
  { responseContent :: Text
  , responseToolsUsed :: [ToolInvocation]
  }
  deriving (Eq, Show, Generic)
```

### ConversationContext (Existing, Enhanced)

Represents the history of messages in a conversation, including tool invocations.

**Fields**:
- `contextMessages :: [Message]` - Ordered list of messages (most recent last)

**Enhancement**: Messages can now include tool invocation messages with `role: "function"` and `name: <tool_name>`.

**Validation Rules**:
- Messages should alternate between user, assistant, and function roles (not strictly enforced)
- Context can be empty (new conversation)
- Function messages must have `name` field matching tool name

**Relationships**:
- ConversationContext contains zero or more Messages
- ConversationContext is maintained by Agent during execution

**Type Definition**:
```haskell
type ConversationContext = [Message]

data Message = Message
  { messageRole :: MessageRole
  , messageContent :: Text
  }
  deriving (Eq, Show)

data MessageRole
  = UserRole
  | AssistantRole
  | FunctionRole Text  -- NEW: Function role with tool name
  deriving (Eq, Show)
```

### sayHello Tool (Concrete Example)

A concrete example tool for the hello world demonstration.

**ToolSpecification** (in gram notation):
```gram
toolSpecification: sayHello
  name: "sayHello"
  description: "Returns a friendly greeting message for the given name"
  typeSignature: "(name: Text) --> IO Text"
```

**ToolSpecification** (Haskell):
```haskell
sayHelloSpec :: ToolSpecification
sayHelloSpec = ToolSpecification
  { toolSpecName = "sayHello"
  , toolSpecDescription = "Returns a friendly greeting message for the given name"
  , toolSpecTypeSignature = "(name: Text) --> IO Text"
  , toolSpecSchema = typeSignatureToJSONSchema "(name: Text) --> IO Text"  -- Auto-generated
  }
```

**Tool Implementation**:
```haskell
sayHelloTool :: Tool
sayHelloTool = Tool
  { toolName = "sayHello"
  , toolDescription = "Returns a friendly greeting message for the given name"
  , toolSchema = typeSignatureToJSONSchema "(name: Text) --> IO Text"  -- Auto-generated from type signature
      ]
  , toolInvoke = \args -> do
      let name = args ^. key "name" . _String
      return $ String $ "Hello, " <> name <> "! Nice to meet you."
  }
```

**ToolLibrary Registration**:
```haskell
helloWorldToolLibrary :: ToolLibrary
helloWorldToolLibrary = registerTool "sayHello" sayHelloTool emptyToolLibrary
```

### Hello World Agent (Concrete Example)

A concrete example agent that uses the sayHello tool description.

**Fields** (inherited from Agent):
- `agentName = "hello_world_agent"`
- `agentDescription = Just "A friendly agent that uses the sayHello tool to greet users"`
- `agentModel = createModel "gpt-3.5-turbo" OpenAI`
- `agentInstruction = "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."`
- `agentToolSpecs = [sayHelloSpec]`

**Type Definition**:
```haskell
helloWorldAgent :: Agent
helloWorldAgent = Agent
  { agentName = "hello_world_agent"
  , agentDescription = Just "A friendly agent that uses the sayHello tool to greet users"
  , agentModel = createModel "gpt-3.5-turbo" OpenAI
  , agentInstruction = "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
  , agentToolSpecs = [sayHelloSpec]
  }
```

**Serialization**: This agent can be serialized to gram notation (tool descriptions only, no implementations)

**Execution**: Tool descriptions are bound to implementations at execution time:
```haskell
result <- executeAgentWithLibrary helloWorldAgent userInput context helloWorldToolLibrary
```

## Relationships Summary

```
Agent
  ├── has 0..* ToolSpecifications (serializable)
  ├── produces AgentResponse
  └── maintains ConversationContext

ToolSpecification (serializable)
  ├── belongs to 0..* Agents
  └── bound to Tool at execution time via ToolLibrary

Tool (executable, not serializable)
  ├── registered in ToolLibrary
  └── produces ToolInvocation

ToolLibrary
  ├── maps tool names to Tool implementations
  └── enables A/B testing (different implementations for same name)

ToolInvocation
  ├── references ToolSpecification (by name)
  ├── uses Tool from ToolLibrary
  └── part of AgentResponse

AgentResponse
  └── contains 0..* ToolInvocation

ConversationContext
  └── contains 0..* Message (user, assistant, function)
```

## State Transitions

### Tool Specification Creation
1. Developer provides: name, description, gram type signature (e.g., `(name: Text) --> IO Text`)
2. System validates: name and description non-empty, type signature valid gram notation
3. System generates: JSON schema from type signature using `typeSignatureToJSONSchema`
4. System creates: ToolSpecification instance with name, description, type signature, and generated schema
5. Result: ToolSpecification ready for serialization and agent association

### Tool Implementation Creation
1. Developer provides: name, description, schema, invoke function
2. System validates: name, description, schema match ToolSpecification (optional)
3. System creates: Tool instance
4. Result: Tool ready for ToolLibrary registration

### Tool Library Registration
1. Developer provides: Tool name and Tool implementation
2. System registers: Tool in ToolLibrary
3. Result: ToolLibrary can resolve tool name to implementation

### Tool Association with Agent
1. Developer provides: Agent and ToolSpecification
2. System validates: ToolSpecification name unique within agent's tool list
3. System updates: Agent with ToolSpecification added to agentToolSpecs list
4. Result: Agent can reference tool specification (serializable)

### Tool Binding at Execution Time
1. Execution environment receives: Agent with ToolSpecifications and ToolLibrary
2. System binds: For each ToolSpecification, lookup Tool in ToolLibrary
3. System validates: Tool matches ToolSpecification (name, description, schema)
4. Result: Bound Tools ready for execution

### Tool Execution Flow
1. Agent execution starts with user input, Agent (with ToolSpecifications), and ToolLibrary
2. Tool binding: ToolSpecifications bound to Tool implementations from ToolLibrary
3. LLM request built with tool definitions (from ToolSpecifications)
4. LLM responds with tool call request
5. System validates: Tool exists in bound Tools, parameters valid
6. System invokes: Tool with parameters
7. System sends: Tool result to LLM
8. LLM generates: Final text response
9. System returns: AgentResponse with content and tool invocations

## Validation Rules Summary

- ToolSpecification name: non-empty, unique within agent
- ToolSpecification description: non-empty
- ToolSpecification typeSignature: valid gram notation type signature
- ToolSpecification schema: auto-generated from typeSignature, must be valid JSON schema
- Tool name: must match ToolSpecification name when bound
- Tool implementation: must match ToolSpecification schema when bound (schema generated from type signature)
- Agent tool specifications: list of ToolSpecifications, names unique within list
- Tool binding: ToolSpecification must have matching Tool in ToolLibrary
- Tool invocation: tool name must exist in bound Tools, parameters must match schema
- Conversation context: function messages must have tool name

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
