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
1. **Gram Notation** → Agent with `Tool` (name, description, schema) - serializable
2. **Deserialization** → Agent with `Tool` list
3. **Execution Environment** → Tool library/registry maps tool names to implementations
4. **Tool Binding** → At execution time, match `Tool` to `ToolImpl` implementation from registry
5. **Execution** → Use bound `ToolImpl` implementations to invoke tools

## Architecture: Pattern Subject as Canonical Format

**Key Design Decision**: Use `Pattern Subject` from gram-hs as the canonical representation for all serializable entities, with lenses providing type-safe access. This hybrid approach combines the benefits of gram-native representation with type safety.

**See Also**: `ARCHITECTURE.md` for detailed discussion of Pattern as a programming language for agent workflows, execution models (interpretation vs compilation), and terminology alternatives.

### Design Principles

1. **Pattern as Source Code**: Pattern Subject is a declarative programming language for agent workflows - it's the source code, canonical and versionable
2. **Pattern Subject for Specifications**: All serializable, declarative types (Agent, Tool) are represented as `Pattern Subject`
3. **Lenses for Type Safety**: Type-safe accessors (lenses) provide compile-time guarantees about structure
4. **Execution Models**: Support both interpretation (direct Pattern execution) and compilation (Pattern → Agent optimization)
5. **Runtime Types**: Execution/runtime types (Tool, ToolLibrary, ConversationContext) remain as concrete Haskell types
6. **No Required Conversion**: If using interpretation model, no conversion targets needed - execution engine works directly with Pattern

### Type Classification

**Pattern Types** (primary/canonical - simple names):
- `Agent` - Agent workflow specification (program in Pattern language)
- `Tool` - Tool interface specification (part of workflow)
- `Model` - Simple string representation (`"OpenAI/gpt-3.5-turbo"`)

**Runtime Types** (optional optimization - longer names):
- `AgentRuntime` - Optimized runtime representation (optional compilation target)
- `ToolRuntime` - Optimized runtime representation (optional compilation target)

**Runtime-Only Types** (no Pattern equivalent - simple names):
- `ToolImpl` - Tool implementation (contains function closure, not part of language)
- `ToolLibrary` - Runtime registry mapping tool names to implementations
- `ConversationContext` - Runtime conversation state
- `AgentResponse` - Execution result (can be serialized for logging/debugging)
- `ToolInvocation` - Tool call record (can be serialized)

**Note**: Pattern types (`Agent`, `Tool`) are the canonical form (like source code) and what developers use most often. Runtime types (`AgentRuntime`, `ToolRuntime`) are optional compilation targets for performance optimization, but not required if using direct interpretation.

### Pattern Structure

**Agent Pattern Structure**:
```gram
[agentName:Agent {
  description: "Agent description",
  instruction: "Agent instructions...",
  model: "OpenAI/gpt-3.5-turbo"
} |
  [sayHello:Tool {
    description: "Returns a friendly greeting"
  } |
    (personName::Text {default:"world"})==>(::String)
  ],
  [anotherTool:Tool {...}]
]
```

**Key Points**:
- Agent name is the pattern identifier (`agentName`), not stored as a property
- Agent fields are in properties (`description`, `instruction`, `model`)
- Tool specifications are nested as pattern elements
- Type signatures are gram path notation in pattern elements

### Lens-Based API

Instead of concrete data types, use lenses for type-safe access:

```haskell
-- Simple names for primary Pattern types
type Agent = Pattern Subject
type Tool = Pattern Subject

-- Lenses for Agent access
agentName :: Lens' Agent Text
agentDescription :: Lens' Agent (Maybe Text)
agentModel :: Lens' Agent Model
agentInstruction :: Lens' Agent Text
agentTools :: Lens' Agent [Tool]  -- elements are nested patterns

-- Lenses for Tool access
toolName :: Lens' Tool Text
toolDescription :: Lens' Tool Text
toolTypeSignature :: Lens' Tool Text
toolSchema :: Lens' Tool Value  -- Auto-generated from type signature
```

### Benefits

1. **Single Source of Truth**: Gram notation is the canonical format, no conversion needed
2. **Gram-Native Operations**: Query, transform, compose using Pattern operations directly
3. **Type Safety**: Lenses provide compile-time guarantees about structure
4. **Validation**: Lenses can validate Pattern structure matches expected schema
5. **Composition**: Agents can be composed using Pattern operations
6. **No Serialization Overhead**: Already in gram format, no conversion step
7. **Flexibility**: Can use gram query operations to find, filter, transform agents

### Implementation Approach

**Lens Implementation**:
```haskell
import Control.Lens

-- Example: agentName lens (extracts from pattern identifier)
agentName :: Lens' Agent Text
agentName = lens getter setter
  where
    getter p = T.pack $ symbolToString $ identity $ value p
    setter p n = p 
      { value = (value p) 
          { identity = Symbol (T.unpack n)
          }
      }

-- Validation: ensure Pattern has "Agent" label
validateAgent :: Agent -> Either Text Agent
validateAgent p
  | "Agent" `elem` labels (value p) = Right p
  | otherwise = Left "Pattern does not have Agent label"
```

**Construction**:
```haskell
-- Create Agent from Pattern
createAgent
  :: Text           -- name (becomes pattern identifier)
  -> Maybe Text     -- description
  -> Model          -- model
  -> Text           -- instruction
  -> [Tool]         -- tool specs
  -> Agent
createAgent name desc model instruction tools =
  Pattern
    { value = Subject
        { identity = Symbol (T.unpack name)  -- Name is the pattern identifier
        , labels = Set.fromList ["Agent"]
        , properties = fromList
            [ ("instruction", VString $ T.unpack instruction)
            , ("model", VString $ T.unpack $ modelToString model)
            ] ++ maybe [] (\d -> [("description", VString $ T.unpack d)]) desc
        }
    , elements = tools
    }
```

**Access**:
```haskell
-- Type-safe access using lenses
getAgentName :: Agent -> Text
getAgentName = view agentName

setAgentName :: Text -> Agent -> Agent
setAgentName = set agentName

-- Access tools
getTools :: Agent -> [Tool]
getTools = view agentTools
```

### Conversion Between Representations

**Pattern → Concrete (for execution)**:
```haskell
-- Convert Pattern to AgentRuntime for optimized execution (optional)
compileAgent :: Agent -> Either Text AgentRuntime
compileAgent p = do
  name <- maybeToEither "Missing name" $ view agentName p
  instruction <- maybeToEither "Missing instruction" $ view agentInstruction p
  model <- parseModel =<< view agentModel p
  tools <- traverse compileTool $ view agentTools p
  return $ AgentRuntime name (view agentDescription p) model instruction tools
```

**Concrete → Pattern (for serialization)**:
```haskell
-- Convert AgentRuntime back to Pattern (for serialization)
agentRuntimeToPattern :: AgentRuntime -> Agent
agentRuntimeToPattern agent = createAgent
  (agentRuntimeName agent)
  (agentRuntimeDescription agent)
  (agentRuntimeModel agent)
  (agentRuntimeInstruction agent)
  (map toolRuntimeToPattern $ agentRuntimeTools agent)
```

### Schema Validation

Lenses can validate Pattern structure:

```haskell
-- Validate Agent pattern structure
validateAgent :: Agent -> Either Text Agent
validateAgent p = do
  -- Check required label
  unless ("Agent" `elem` labels (value p)) $
    Left "Pattern must have Agent label"
  
  -- Check pattern identifier (name)
  let ident = identity $ value p
  unless (symbolToString ident /= "") $
    Left "Agent must have a non-empty pattern identifier (name)"
  
  -- Check required properties
  let props = properties $ value p
  unless (hasProperty "instruction" props) $
    Left "Agent must have 'instruction' property"
  unless (hasProperty "model" props) $
    Left "Agent must have 'model' property"
  
  -- Validate nested tools
  forM_ (elements p) $ \tool -> do
    unless ("Tool" `elem` labels (value tool)) $
      Left "Tool must have Tool label"
  
  return p
```

### Execution Models

**Interpretation Model** (Default):
- Execution engine works directly with Pattern Subject
- Uses lenses for type-safe access
- No conversion needed
- Pattern is canonical source of truth
- Flexible, can use gram operations during execution

**Compilation Model** (Optional Optimization):
- Compile Pattern → Agent for performance
- Use concrete types for hot execution paths
- Conversion overhead, but faster execution
- Clear optimization boundary

**Hybrid Approach** (Recommended):
- Default to interpretation (Pattern is canonical)
- Optional compilation for performance-critical paths
- Fallback to interpretation if compilation fails
- See `ARCHITECTURE.md` for detailed discussion

### Performance Considerations

- **Pattern Subject**: Canonical form (source code), used for storage, versioning, sharing
- **Concrete Types**: Optional compilation target for execution optimization
- **Conversion**: Only needed if using compilation model (optional)
- **Caching**: Can cache frequently accessed Pattern fields if needed

### Migration Path

1. **Phase 1**: Implement lenses for Pattern Subject access
2. **Phase 2**: Implement interpretation model (direct Pattern execution)
3. **Phase 3**: Add optional compilation model (Pattern → Agent optimization)
4. **Phase 4**: Hybrid execution (auto-detect when to compile vs interpret)

## Entities

### Tool

Represents a tool's declarative specification (serializable, no implementation).

**Representation**: `Pattern Subject` (canonical format) with lenses for type-safe access.

**Pattern Structure**:
```gram
[toolName:Tool {
  description: "Tool description"
} |
  (paramName::Type {default:value})==>(::ReturnType)
]
```

**Fields** (accessed via lenses):
- `toolName :: Text` - Unique name for the tool (pattern identifier, required)
- `toolDescription :: Text` - Natural language description (property, required)
- `toolTypeSignature :: Text` - Type signature in gram path notation (pattern element, required)
- `toolSchema :: Value` - JSON schema (auto-generated from type signature, computed on access)

**Validation Rules**:
- Pattern must have `Tool` label
- Pattern identifier is the tool name (must be non-empty, globally unique)
- `description` property must be non-empty
- Pattern must have exactly one element (the type signature path)
- Type signature must be valid gram path notation
- `toolSchema` is automatically generated from `toolTypeSignature` using `typeSignatureToJSONSchema`

**Relationships**:
- Tool belongs to zero or more Agents (tools can be shared as nested patterns)
- Tool is bound to ToolImpl implementation at execution time

**Type Definition** (Pattern Subject with lenses - primary/canonical):
```haskell
-- Simple name for primary representation
type Tool = Pattern Subject

-- Lenses for type-safe access
toolName :: Lens' Tool Text
toolDescription :: Lens' Tool Text
toolTypeSignature :: Lens' Tool Text
toolSchema :: Lens' Tool Value  -- Computed from type signature
```

**Runtime Type** (optional optimization for execution):
```haskell
data ToolRuntime = ToolRuntime
  { toolRuntimeName :: Text
  , toolRuntimeDescription :: Text
  , toolRuntimeTypeSignature :: Text
  , toolRuntimeSchema :: Value
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

**Accessors** (lens-based):
- `toolName :: Lens' Tool Text` - Access tool name (pattern identifier)
- `toolDescription :: Lens' Tool Text` - Access description (property)
- `toolTypeSignature :: Lens' Tool Text` - Access type signature (pattern element)
- `toolSchema :: Lens' Tool Value` - Access auto-generated schema (computed)

**Schema Generation**: JSON schema is automatically generated from gram type signature using `typeSignatureToJSONSchema` function. The schema is computed when accessed via lens, not stored separately.

**Serialization**: Already in gram format (Pattern Subject), no conversion needed. Can be serialized directly to gram notation.

### ToolImpl

Represents a tool with its executable implementation (bound at runtime, not serializable).

**Representation**: Concrete Haskell type (not Pattern Subject) because it contains function closures that cannot be serialized.

**Rationale**: ToolImpl contains `toolImplInvoke :: Value -> IO Value`, which is a function closure and cannot be represented in gram notation. Tool (Pattern Subject) describes the tool interface, while ToolImpl (concrete type) provides the implementation.

**Fields**:
- `toolImplName :: Text` - Unique name for the tool (required, must match Tool name)
- `toolImplDescription :: Text` - Natural language description (required, matches Tool)
- `toolImplSchema :: Value` - JSON schema (required, matches Tool schema)
- `toolImplInvoke :: Value -> IO Value` - Function that invokes the tool with JSON parameters and returns JSON result (required)

**Validation Rules**:
- `toolImplName` must match a Tool name when bound
- `toolImplDescription` and `toolImplSchema` should match Tool (validated at binding time)
- `toolImplInvoke` function must handle JSON parameter conversion and error cases

**Relationships**:
- ToolImpl is bound from Tool at execution time via ToolLibrary
- ToolImpl invocation produces ToolInvocation record

**Type Definition**:
```haskell
data ToolImpl = ToolImpl
  { toolImplName :: Text
  , toolImplDescription :: Text
  , toolImplSchema :: Value  -- Aeson Value for JSON schema
  , toolImplInvoke :: Value -> IO Value  -- JSON in, JSON out
  }
  deriving (Generic)  -- Note: Cannot derive Eq/Show/ToJSON/FromJSON due to function field
```

**Accessors**:
- `toolImplName :: ToolImpl -> Text` - Returns tool name
- `toolImplDescription :: ToolImpl -> Text` - Returns tool description
- `toolImplSchema :: ToolImpl -> Value` - Returns tool parameter schema

**Binding**: Created from Tool + ToolLibrary lookup at execution time

### ToolLibrary

Registry that maps tool names to executable implementations.

**Purpose**: Enables late binding of tool descriptions to implementations, supporting A/B testing and different execution environments.

**Fields**:
- `libraryTools :: Map Text ToolImpl` - Map from tool name to ToolImpl implementation

**Operations**:
- `registerTool :: Text -> ToolImpl -> ToolLibrary -> ToolLibrary` - Register a tool implementation
- `lookupTool :: Text -> ToolLibrary -> Maybe ToolImpl` - Lookup tool by name
- `bindTool :: Tool -> ToolLibrary -> Maybe ToolImpl` - Bind Tool (Pattern) to ToolImpl implementation

**Type Definition**:
```haskell
data ToolLibrary = ToolLibrary
  { libraryTools :: Map Text ToolImpl
  }
  deriving (Generic)

emptyToolLibrary :: ToolLibrary
emptyToolLibrary = ToolLibrary Map.empty

registerTool :: Text -> ToolImpl -> ToolLibrary -> ToolLibrary
registerTool name toolImpl (ToolLibrary tools) = ToolLibrary $ Map.insert name toolImpl tools

lookupTool :: Text -> ToolLibrary -> Maybe ToolImpl
lookupTool name (ToolLibrary tools) = Map.lookup name tools

bindTool :: Tool -> ToolLibrary -> Maybe ToolImpl
bindTool toolPattern library = do
  toolImpl <- lookupTool (view toolName toolPattern) library
  -- Validate that tool implementation matches specification (optional, for safety)
  guard $ toolImplDescription toolImpl == view toolDescription toolPattern
  guard $ toolImplSchema toolImpl == view toolSchema toolPattern
  return toolImpl
```

**A/B Testing Support**: Different ToolLibrary instances can provide different implementations for the same tool name, enabling A/B testing of tool implementations while keeping the same agent specification.

### Agent (Updated)

Represents an LLM-powered agent with identity, model, instructions, and tool descriptions.

**Representation**: `Pattern Subject` (canonical format) with lenses for type-safe access.

**Pattern Structure**:
```gram
[agentName:Agent {
  description: "Agent description",
  instruction: "Agent instructions...",
  model: "OpenAI/gpt-3.5-turbo"
} |
  [tool1:Tool {...}],
  [tool2:Tool {...}]
]
```

**Fields** (accessed via lenses):
- `agentName :: Text` - Unique agent identifier (pattern identifier, required)
- `agentDescription :: Maybe Text` - Optional agent description (property)
- `agentModel :: Model` - LLM model to use (property, required)
- `agentInstruction :: Text` - Agent behavior instructions (property, required)
- `agentTools :: [Tool]` - List of tools (pattern elements, optional, defaults to empty list)

**Validation Rules**:
- Pattern must have `Agent` label
- Pattern identifier is the agent name (must be non-empty)
- `instruction` and `model` properties must be present
- `agentTools` can be empty (tool-free agents supported)
- Tool names must be unique within `agentTools` list (pattern identifiers)

**Relationships**:
- Agent has zero or more Tools (as nested pattern elements)
- Agent produces AgentResponse during execution
- Agent maintains ConversationContext
- Agent's tools are bound to ToolImpl implementations at execution time via ToolLibrary

**Type Definition** (Pattern Subject with lenses - primary/canonical):
```haskell
-- Simple name for primary representation
type Agent = Pattern Subject

-- Lenses for type-safe access
agentName :: Lens' Agent Text
agentDescription :: Lens' Agent (Maybe Text)
agentModel :: Lens' Agent Model
agentInstruction :: Lens' Agent Text
agentTools :: Lens' Agent [Tool]  -- elements are nested patterns
```

**Runtime Type** (optional optimization for execution):
```haskell
data AgentRuntime = AgentRuntime
  { agentRuntimeName :: Text
  , agentRuntimeDescription :: Maybe Text
  , agentRuntimeModel :: Model
  , agentRuntimeInstruction :: Text
  , agentRuntimeTools :: [ToolRuntime]  -- Runtime tools
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

**Accessors** (lens-based for Pattern):
- `agentName :: Lens' Agent Text` - Access agent name (pattern identifier)
- `agentDescription :: Lens' Agent (Maybe Text)` - Access description (property)
- `agentModel :: Lens' Agent Model` - Access model (property)
- `agentInstruction :: Lens' Agent Text` - Access instruction (property)
- `agentTools :: Lens' Agent [Tool]` - Access tools (pattern elements)

**Serialization**: Already in gram format (Pattern Subject), no conversion needed. Can be serialized directly to gram notation.

**Execution**: Pattern `Agent` can be executed directly (interpretation model) or converted to `AgentRuntime` for optimized execution (compilation model). Tools are bound to ToolImpl implementations at execution time via ToolLibrary parameter.

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
- ToolInvocation references a Tool (by name)
- ToolInvocation uses ToolImpl implementation from ToolLibrary
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

**Tool** (in gram notation):
```gram
[sayHello:Tool {
  description: "Returns a friendly greeting message for the given name"
} |
  (personName::Text {default:"world"})==>(::String)
]
```

**Note**: Tool name is the pattern identifier (`sayHello`), ensuring global uniqueness required for LLM tool calling. Parameter name `personName` is also a globally unique identifier, encouraging consistent vocabulary.

**Tool** (Pattern - primary):
```haskell
sayHello :: Tool
sayHello = createTool
  "sayHello"
  "Returns a friendly greeting message for the given name"
  "(personName::Text {default:\"world\"})==>(::String)"
```

**ToolImpl Implementation**:
```haskell
sayHelloImpl :: ToolImpl
sayHelloImpl = ToolImpl
  { toolImplName = "sayHello"
  , toolImplDescription = "Returns a friendly greeting message for the given name"
  , toolImplSchema = typeSignatureToJSONSchema "(personName::Text {default:\"world\"})==>(::String)"  -- Auto-generated from type signature
  , toolImplInvoke = \args -> do
      let name = args ^. key "personName" . _String
      return $ String $ "Hello, " <> name <> "! Nice to meet you."
  }
```

**ToolLibrary Registration**:
```haskell
helloWorldToolLibrary :: ToolLibrary
helloWorldToolLibrary = registerTool "sayHello" sayHelloImpl emptyToolLibrary
```

### Hello World Agent (Concrete Example)

A concrete example agent that uses the sayHello tool description.

**Fields** (inherited from Agent):
- `agentName = "hello_world_agent"`
- `agentDescription = Just "A friendly agent that uses the sayHello tool to greet users"`
- `agentModel = createModel "gpt-3.5-turbo" OpenAI`
- `agentInstruction = "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."`
- `agentTools = [sayHello]`

**Type Definition** (Pattern - primary):
```haskell
helloWorldAgent :: Agent
helloWorldAgent = createAgent
  "hello_world_agent"
  (Just "A friendly agent that uses the sayHello tool to greet users")
  (createModel "gpt-3.5-turbo" OpenAI)
  "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
  [sayHello]
```

**Serialization**: This agent is already in gram notation (Pattern Subject), can be serialized directly

**Execution**: Tools are bound to ToolImpl implementations at execution time:
```haskell
result <- executeAgentWithLibrary helloWorldAgent userInput context helloWorldToolLibrary
```

## Relationships Summary

```
Agent (Pattern - primary)
  ├── has 0..* Tools (serializable, nested patterns)
  ├── produces AgentResponse
  └── maintains ConversationContext

Tool (Pattern - primary, serializable)
  ├── belongs to 0..* Agents
  └── bound to ToolImpl at execution time via ToolLibrary

ToolImpl (executable, not serializable)
  ├── registered in ToolLibrary
  └── produces ToolInvocation

ToolLibrary
  ├── maps tool names to ToolImpl implementations
  └── enables A/B testing (different implementations for same name)

ToolInvocation
  ├── references Tool (by name)
  ├── uses ToolImpl from ToolLibrary
  └── part of AgentResponse

AgentResponse
  └── contains 0..* ToolInvocation

ConversationContext
  └── contains 0..* Message (user, assistant, function)
```

## State Transitions

### Tool Creation (Pattern)
1. Developer provides: name, description, gram type signature in curried form (e.g., `(personName::Text {default:"world"})==>(::String)`)
2. System validates: name and description non-empty, type signature valid gram notation
3. System generates: JSON schema from type signature using `typeSignatureToJSONSchema`
4. System creates: Tool (Pattern Subject) with name, description, type signature
5. Result: Tool ready for serialization and agent association

### ToolImpl Implementation Creation
1. Developer provides: name, description, schema, invoke function
2. System validates: name, description, schema match Tool (optional)
3. System creates: ToolImpl instance
4. Result: ToolImpl ready for ToolLibrary registration

### Tool Library Registration
1. Developer provides: Tool name and ToolImpl implementation
2. System registers: ToolImpl in ToolLibrary
3. Result: ToolLibrary can resolve tool name to implementation

### Tool Association with Agent
1. Developer provides: Agent and Tool
2. System validates: Tool name unique within agent's tool list
3. System updates: Agent with Tool added to agentTools list
4. Result: Agent can reference tool (serializable Pattern)

### Tool Binding at Execution Time
1. Execution environment receives: Agent with Tools and ToolLibrary
2. System binds: For each Tool, lookup ToolImpl in ToolLibrary
3. System validates: ToolImpl matches Tool (name, description, schema)
4. Result: Bound ToolImpls ready for execution

### Tool Execution Flow
1. Agent execution starts with user input, Agent (with Tools), and ToolLibrary
2. Tool binding: Tools bound to ToolImpl implementations from ToolLibrary
3. LLM request built with tool definitions (from Tools)
4. LLM responds with tool call request
5. System validates: ToolImpl exists in bound ToolImpls, parameters valid
6. System invokes: ToolImpl with parameters
7. System sends: Tool result to LLM
8. LLM generates: Final text response
9. System returns: AgentResponse with content and tool invocations

## Validation Rules Summary

- Tool name: non-empty, unique within agent (pattern identifier)
- Tool description: non-empty
- Tool typeSignature: valid gram notation type signature
- Tool schema: auto-generated from typeSignature, must be valid JSON schema
- ToolImpl name: must match Tool name when bound
- ToolImpl implementation: must match Tool schema when bound (schema generated from type signature)
- Agent tools: list of Tools, names unique within list
- Tool binding: Tool must have matching ToolImpl in ToolLibrary
- Tool invocation: tool name must exist in bound ToolImpls, parameters must match schema
- Conversation context: function messages must have tool name

## A/B Testing Support

**Scenario**: Test different tool implementations with the same agent specification.

**Approach**:
1. Create Agent with Tools (same for both tests)
2. Create ToolLibrary A with ToolImpl A
3. Create ToolLibrary B with ToolImpl B (same tool name, different implementation)
4. Execute agent with ToolLibrary A → measure results
5. Execute agent with ToolLibrary B → measure results
6. Compare results

**Example**:
```haskell
-- Same agent specification (Pattern)
let agent = createAgent "hello_agent" ... [sayHello]

-- Different tool implementations
let libraryA = registerTool "sayHello" sayHelloImplA emptyToolLibrary
let libraryB = registerTool "sayHello" sayHelloImplB emptyToolLibrary

-- A/B test
resultA <- executeAgentWithLibrary agent input context libraryA
resultB <- executeAgentWithLibrary agent input context libraryB
```
