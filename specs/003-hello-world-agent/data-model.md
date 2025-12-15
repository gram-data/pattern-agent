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

## Architecture: Pattern Subject as Canonical Format

**Key Design Decision**: Use `Pattern Subject` from gram-hs as the canonical representation for all serializable entities, with lenses providing type-safe access. This hybrid approach combines the benefits of gram-native representation with type safety.

**See Also**: `ARCHITECTURE.md` for detailed discussion of Pattern as a programming language for agent workflows, execution models (interpretation vs compilation), and terminology alternatives.

### Design Principles

1. **Pattern as Source Code**: Pattern Subject is a declarative programming language for agent workflows - it's the source code, canonical and versionable
2. **Pattern Subject for Specifications**: All serializable, declarative types (Agent, ToolSpecification) are represented as `Pattern Subject`
3. **Lenses for Type Safety**: Type-safe accessors (lenses) provide compile-time guarantees about structure
4. **Execution Models**: Support both interpretation (direct Pattern execution) and compilation (Pattern → Agent optimization)
5. **Runtime Types**: Execution/runtime types (Tool, ToolLibrary, ConversationContext) remain as concrete Haskell types
6. **No Required Conversion**: If using interpretation model, no conversion targets needed - execution engine works directly with Pattern

### Type Classification

**Pattern Subject Types** (source code / workflow language):
- `Agent` - Agent workflow specification (program in Pattern language)
- `ToolSpecification` - Tool interface specification (part of workflow)
- `Model` - Simple string representation (`"OpenAI/gpt-3.5-turbo"`)
- `AgentResponse` - Execution result (can be serialized for logging/debugging)
- `ToolInvocation` - Tool call record (can be serialized)

**Concrete Types** (runtime/execution environment):
- `Tool` - Tool implementation (contains function closure, not part of language)
- `ToolLibrary` - Runtime registry mapping tool names to implementations
- `ConversationContext` - Runtime conversation state

**Note**: Pattern Subject is the canonical form (like source code). Concrete types are optional compilation targets for performance optimization, but not required if using direct interpretation.

### Pattern Structure

**Agent Pattern Structure**:
```gram
[agentName:Agent {
  description: "Agent description",
  instruction: "Agent instructions...",
  model: "OpenAI/gpt-3.5-turbo"
} |
  [sayHello:ToolSpecification {
    description: "Returns a friendly greeting"
  } |
    (personName::Text {default:"world"})==>(::String)
  ],
  [anotherTool:ToolSpecification {...}]
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
-- Type aliases for clarity
type AgentPattern = Pattern Subject
type ToolSpecPattern = Pattern Subject

-- Lenses for Agent access
agentName :: Lens' AgentPattern Text
agentDescription :: Lens' AgentPattern (Maybe Text)
agentModel :: Lens' AgentPattern Model
agentInstruction :: Lens' AgentPattern Text
agentToolSpecs :: Lens' AgentPattern [ToolSpecPattern]  -- elements are nested patterns

-- Lenses for ToolSpecification access
toolSpecName :: Lens' ToolSpecPattern Text
toolSpecDescription :: Lens' ToolSpecPattern Text
toolSpecTypeSignature :: Lens' ToolSpecPattern Text
toolSpecSchema :: Lens' ToolSpecPattern Value  -- Auto-generated from type signature
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
agentName :: Lens' (Pattern Subject) Text
agentName = lens getter setter
  where
    getter p = T.pack $ symbolToString $ identity $ value p
    setter p n = p 
      { value = (value p) 
          { identity = Symbol (T.unpack n)
          }
      }

-- Validation: ensure Pattern has "Agent" label
validateAgentPattern :: Pattern Subject -> Either Text (Pattern Subject)
validateAgentPattern p
  | "Agent" `elem` labels (value p) = Right p
  | otherwise = Left "Pattern does not have Agent label"
```

**Construction**:
```haskell
-- Create Agent from Pattern
createAgentPattern 
  :: Text           -- name (becomes pattern identifier)
  -> Maybe Text     -- description
  -> Model          -- model
  -> Text           -- instruction
  -> [ToolSpecPattern]  -- tool specs
  -> AgentPattern
createAgentPattern name desc model instruction toolSpecs =
  Pattern
    { value = Subject
        { identity = Symbol (T.unpack name)  -- Name is the pattern identifier
        , labels = Set.fromList ["Agent"]
        , properties = fromList
            [ ("instruction", VString $ T.unpack instruction)
            , ("model", VString $ T.unpack $ modelToString model)
            ] ++ maybe [] (\d -> [("description", VString $ T.unpack d)]) desc
        }
    , elements = toolSpecs
    }
```

**Access**:
```haskell
-- Type-safe access using lenses
getAgentName :: AgentPattern -> Text
getAgentName = view agentName

setAgentName :: Text -> AgentPattern -> AgentPattern
setAgentName = set agentName

-- Access tool specifications
getToolSpecs :: AgentPattern -> [ToolSpecPattern]
getToolSpecs = view agentToolSpecs
```

### Conversion Between Representations

**Pattern → Concrete (for execution)**:
```haskell
-- Convert Pattern to concrete Agent type for execution
patternToAgent :: AgentPattern -> Either Text Agent
patternToAgent p = do
  name <- maybeToEither "Missing name" $ view agentName p
  instruction <- maybeToEither "Missing instruction" $ view agentInstruction p
  model <- parseModel =<< view agentModel p
  toolSpecs <- traverse patternToToolSpec $ view agentToolSpecs p
  return $ Agent name (view agentDescription p) model instruction toolSpecs
```

**Concrete → Pattern (for serialization)**:
```haskell
-- Convert concrete Agent to Pattern (already in gram format)
agentToPattern :: Agent -> AgentPattern
agentToPattern agent = createAgentPattern
  (agentName agent)
  (agentDescription agent)
  (agentModel agent)
  (agentInstruction agent)
  (map toolSpecToPattern $ agentToolSpecs agent)
```

### Schema Validation

Lenses can validate Pattern structure:

```haskell
-- Validate Agent pattern structure
validateAgentPattern :: AgentPattern -> Either Text AgentPattern
validateAgentPattern p = do
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
  
  -- Validate nested tool specifications
  forM_ (elements p) $ \toolSpec -> do
    unless ("ToolSpecification" `elem` labels (value toolSpec)) $
      Left "Tool specification must have ToolSpecification label"
  
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

### ToolSpecification

Represents a tool's declarative specification (serializable, no implementation).

**Representation**: `Pattern Subject` (canonical format) with lenses for type-safe access.

**Pattern Structure**:
```gram
[toolName:ToolSpecification {
  description: "Tool description"
} |
  (paramName::Type {default:value})==>(::ReturnType)
]
```

**Fields** (accessed via lenses):
- `toolSpecName :: Text` - Unique name for the tool (pattern identifier, required)
- `toolSpecDescription :: Text` - Natural language description (property, required)
- `toolSpecTypeSignature :: Text` - Type signature in gram path notation (pattern element, required)
- `toolSpecSchema :: Value` - JSON schema (auto-generated from type signature, computed on access)

**Validation Rules**:
- Pattern must have `ToolSpecification` label
- Pattern identifier is the tool name (must be non-empty, globally unique)
- `description` property must be non-empty
- Pattern must have exactly one element (the type signature path)
- Type signature must be valid gram path notation
- `toolSpecSchema` is automatically generated from `toolSpecTypeSignature` using `typeSignatureToJSONSchema`

**Relationships**:
- ToolSpecification belongs to zero or more Agents (specifications can be shared as nested patterns)
- ToolSpecification is bound to Tool implementation at execution time

**Type Definition** (Pattern Subject with lenses):
```haskell
type ToolSpecPattern = Pattern Subject

-- Lenses for type-safe access
toolSpecName :: Lens' ToolSpecPattern Text
toolSpecDescription :: Lens' ToolSpecPattern Text
toolSpecTypeSignature :: Lens' ToolSpecPattern Text
toolSpecSchema :: Lens' ToolSpecPattern Value  -- Computed from type signature
```

**Concrete Type** (for execution/conversion):
```haskell
data ToolSpecification = ToolSpecification
  { toolSpecName :: Text
  , toolSpecDescription :: Text
  , toolSpecTypeSignature :: Text
  , toolSpecSchema :: Value
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

**Accessors** (lens-based):
- `toolSpecName :: Lens' ToolSpecPattern Text` - Access tool name (pattern identifier)
- `toolSpecDescription :: Lens' ToolSpecPattern Text` - Access description (property)
- `toolSpecTypeSignature :: Lens' ToolSpecPattern Text` - Access type signature (pattern element)
- `toolSpecSchema :: Lens' ToolSpecPattern Value` - Access auto-generated schema (computed)

**Schema Generation**: JSON schema is automatically generated from gram type signature using `typeSignatureToJSONSchema` function. The schema is computed when accessed via lens, not stored separately.

**Serialization**: Already in gram format (Pattern Subject), no conversion needed. Can be serialized directly to gram notation.

### Tool

Represents a tool with its executable implementation (bound at runtime, not serializable).

**Representation**: Concrete Haskell type (not Pattern Subject) because it contains function closures that cannot be serialized.

**Rationale**: Tool contains `toolInvoke :: Value -> IO Value`, which is a function closure and cannot be represented in gram notation. ToolSpecification (Pattern Subject) describes the tool interface, while Tool (concrete type) provides the implementation.

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

**Representation**: `Pattern Subject` (canonical format) with lenses for type-safe access.

**Pattern Structure**:
```gram
[agentName:Agent {
  description: "Agent description",
  instruction: "Agent instructions...",
  model: "OpenAI/gpt-3.5-turbo"
} |
  [toolSpec1:ToolSpecification {...}],
  [toolSpec2:ToolSpecification {...}]
]
```

**Fields** (accessed via lenses):
- `agentName :: Text` - Unique agent identifier (pattern identifier, required)
- `agentDescription :: Maybe Text` - Optional agent description (property)
- `agentModel :: Model` - LLM model to use (property, required)
- `agentInstruction :: Text` - Agent behavior instructions (property, required)
- `agentToolSpecs :: [ToolSpecPattern]` - List of tool specifications (pattern elements, optional, defaults to empty list)

**Validation Rules**:
- Pattern must have `Agent` label
- Pattern identifier is the agent name (must be non-empty)
- `name`, `instruction`, and `model` properties must be present
- `agentToolSpecs` can be empty (tool-free agents supported)
- Tool names must be unique within `agentToolSpecs` list (pattern identifiers)

**Relationships**:
- Agent has zero or more ToolSpecifications (as nested pattern elements)
- Agent produces AgentResponse during execution
- Agent maintains ConversationContext
- Agent's tool specifications are bound to Tool implementations at execution time via ToolLibrary

**Type Definition** (Pattern Subject with lenses):
```haskell
type AgentPattern = Pattern Subject

-- Lenses for type-safe access
agentName :: Lens' AgentPattern Text
agentDescription :: Lens' AgentPattern (Maybe Text)
agentModel :: Lens' AgentPattern Model
agentInstruction :: Lens' AgentPattern Text
agentToolSpecs :: Lens' AgentPattern [ToolSpecPattern]  -- elements are nested patterns
```

**Concrete Type** (for execution/conversion):
```haskell
data Agent = Agent
  { agentName :: Text
  , agentDescription :: Maybe Text
  , agentModel :: Model
  , agentInstruction :: Text
  , agentToolSpecs :: [ToolSpecification]  -- Tool specifications (serializable)
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

**Accessors** (lens-based):
- `agentName :: Lens' AgentPattern Text` - Access agent name (pattern identifier)
- `agentDescription :: Lens' AgentPattern (Maybe Text)` - Access description (property)
- `agentModel :: Lens' AgentPattern Model` - Access model (property)
- `agentInstruction :: Lens' AgentPattern Text` - Access instruction (property)
- `agentToolSpecs :: Lens' AgentPattern [ToolSpecPattern]` - Access tool specs (pattern elements)

**Serialization**: Already in gram format (Pattern Subject), no conversion needed. Can be serialized directly to gram notation.

**Execution**: Pattern can be converted to concrete `Agent` type for execution, or execution can work directly with Pattern. Tool specifications are bound to implementations at execution time via ToolLibrary parameter to executeAgent.

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
[sayHello:ToolSpecification {
  description: "Returns a friendly greeting message for the given name"
} |
  (personName::Text)==>(::String)
]
```

**Note**: Tool name is the pattern identifier (`sayHello`), ensuring global uniqueness required for LLM tool calling. Parameter name `personName` is also a globally unique identifier, encouraging consistent vocabulary.

**ToolSpecification** (Haskell):
```haskell
sayHelloSpec :: ToolSpecification
sayHelloSpec = ToolSpecification
  { toolSpecName = "sayHello"
  , toolSpecDescription = "Returns a friendly greeting message for the given name"
  , toolSpecTypeSignature = "(personName::Text)==>(::String)"
  , toolSpecSchema = typeSignatureToJSONSchema "(personName::Text)==>(::String)"  -- Auto-generated
  }
```

**Tool Implementation**:
```haskell
sayHelloTool :: Tool
sayHelloTool = Tool
  { toolName = "sayHello"
  , toolDescription = "Returns a friendly greeting message for the given name"
  , toolSchema = typeSignatureToJSONSchema "(personName::Text)==>(::String)"  -- Auto-generated from type signature
      ]
  , toolInvoke = \args -> do
      let name = args ^. key "personName" . _String
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
1. Developer provides: name, description, gram type signature in curried form (e.g., `(personName::Text)==>(::String)`)
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
