# API Contract: Tool System

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the API contract for creating tool descriptions, tool implementations, and tool library management

## Architecture

**Key Design**: Separate tool descriptions (serializable, declarative) from tool implementations (executable, bound at runtime). This enables:
- Gram notation serialization (descriptions only)
- Late binding (implementations bound at execution time)
- A/B testing (same agent specification with different tool implementations)

## Tool Specification Creation

### `createToolSpecification`

Creates a tool specification with gram type signature (serializable, no implementation).

**Signature**:
```haskell
createToolSpecification
  :: Text              -- name: Unique tool name
  -> Text              -- description: Tool description
  -> Text              -- typeSignature: Type signature in gram path notation (curried form, e.g., "(::Text {paramName:\"name\"})==>(::String)")
  -> ToolSpecification
```

**Preconditions**:
- `name` must be non-empty
- `description` must be non-empty
- `typeSignature` must be valid gram notation type signature

**Postconditions**:
- Returns `ToolSpecification` ready for serialization and agent association
- JSON schema is automatically generated from type signature
- ToolSpecification can be added to agent's tool specification list

**Example**:
```haskell
let sayHelloSpec = createToolSpecification
      "sayHello"
      "Returns a friendly greeting message for the given name"
      -- Type signature in curried form: (::Text {paramName:"name"})==>(::String)
      -- (Implementation may parse from text or gram path notation)
```

**Type Signature Format** (Curried Form with Property Records):
- Simple: `(::Text)==>(::String)`
- Named parameter: `(::Text {paramName:"name"})==>(::String)`
- Multiple parameters: `(::Text {paramName:"name"})==>(::Int {paramName:"age"})==>(::String)`
- Optional parameter: `(::Text {paramName:"name"})==>(::Int {paramName:"age", optional:true})==>(::String)`
- Record parameter: `(::Object {paramName:"params", fields:[{name:"name", type:"Text"}, {name:"age", type:"Int"}]})==>(::String)`

## Tool Implementation Creation

### `createTool`

Creates a tool with executable implementation (not serializable).

**Signature**:
```haskell
createTool
  :: Text              -- name: Unique tool name (should match ToolSpecification name)
  -> Text              -- description: Tool description (should match ToolSpecification)
  -> Value             -- schema: JSON schema (should match ToolSpecification schema)
  -> (Value -> IO Value)  -- invoke: Tool invocation function
  -> Tool
```

**Preconditions**:
- `name` must be non-empty
- `description` must be non-empty
- `schema` must be valid JSON schema
- `invoke` function must handle JSON parameter conversion and errors

**Postconditions**:
- Returns `Tool` ready for ToolLibrary registration
- Tool can be registered in ToolLibrary for late binding

**Example**:
```haskell
let sayHelloTool = createTool
      "sayHello"
      "Returns a friendly greeting message for the given name"
      (object [ "type" .= ("object" :: Text)
              , "properties" .= object [ "name" .= object [ "type" .= ("string" :: Text) ] ]
              , "required" .= ["name"]
              ])
      (\args -> do
        let name = args ^. key "name" . _String
        return $ String $ "Hello, " <> name <> "! Nice to meet you."
      )
```

## Tool Specification Accessors

### `toolSpecName`

Returns the tool specification's name.

**Signature**:
```haskell
toolSpecName :: ToolSpecification -> Text
```

### `toolSpecDescription`

Returns the tool specification's description.

**Signature**:
```haskell
toolSpecDescription :: ToolSpecification -> Text
```

### `toolSpecTypeSignature`

Returns the tool specification's type signature in gram notation.

**Signature**:
```haskell
toolSpecTypeSignature :: ToolSpecification -> Text
```

### `toolSpecSchema`

Returns the tool specification's auto-generated JSON schema.

**Signature**:
```haskell
toolSpecSchema :: ToolSpecification -> Value
```

**Note**: Schema is automatically generated from type signature, not manually specified.

## Tool Accessors

### `toolName`

Returns the tool's name.

**Signature**:
```haskell
toolName :: Tool -> Text
```

### `toolDescription`

Returns the tool's description.

**Signature**:
```haskell
toolDescription :: Tool -> Text
```

### `toolSchema`

Returns the tool's parameter schema.

**Signature**:
```haskell
toolSchema :: Tool -> Value
```

## Tool Library Management

### `emptyToolLibrary`

Creates an empty tool library.

**Signature**:
```haskell
emptyToolLibrary :: ToolLibrary
```

### `registerTool`

Registers a tool implementation in the tool library.

**Signature**:
```haskell
registerTool
  :: Text              -- name: Tool name
  -> Tool              -- tool: Tool implementation
  -> ToolLibrary       -- library: Tool library to register in
  -> ToolLibrary       -- Updated tool library
```

**Preconditions**:
- `name` must match `tool.toolName`
- `tool` must be valid (name, description, schema, invoke function)

**Postconditions**:
- Returns ToolLibrary with tool registered
- Tool can be looked up by name
- If tool with same name already exists, it is replaced

**Example**:
```haskell
let library = registerTool "sayHello" sayHelloTool emptyToolLibrary
```

### `lookupTool`

Looks up a tool implementation by name.

**Signature**:
```haskell
lookupTool
  :: Text              -- name: Tool name
  -> ToolLibrary       -- library: Tool library to search
  -> Maybe Tool        -- Tool implementation if found
```

**Preconditions**:
- `name` must be non-empty

**Postconditions**:
- Returns `Just tool` if tool found
- Returns `Nothing` if tool not found

### `bindTool`

Binds a tool specification to a tool implementation from the library.

**Signature**:
```haskell
bindTool
  :: ToolSpecification   -- spec: Tool specification
  -> ToolLibrary        -- library: Tool library to search
  -> Maybe Tool         -- Bound tool implementation if found and matches
```

**Preconditions**:
- `spec` must be valid ToolSpecification
- `library` must be valid ToolLibrary

**Postconditions**:
- Returns `Just tool` if tool found and matches specification (name, description, schema)
- Returns `Nothing` if tool not found or doesn't match

**Validation**: Validates that tool implementation matches specification (name, description, schema must match)

## Tool Invocation

Tool invocation is handled internally by the agent execution system. Tools are invoked automatically when the LLM decides to use them based on:
- Agent instructions
- User input
- Tool descriptions and schemas

**Invocation Flow**:
1. Agent execution receives Agent (with ToolSpecifications) and ToolLibrary
2. Tool binding: ToolSpecifications bound to Tool implementations from ToolLibrary
3. LLM selects tool and provides parameters as JSON
4. Parameters validated against tool schema
5. `toolInvoke` function called with validated parameters
6. Result (or error) returned to LLM
7. LLM incorporates result into response

## Schema Definition

Tool descriptions use JSON Schema for parameter definition. The schema must define:
- Parameter types (string, number, object, array, etc.)
- Required parameters
- Parameter descriptions (for LLM understanding)

**Schema Format**:
```json
{
  "type": "object",
  "properties": {
    "paramName": {
      "type": "string",
      "description": "Parameter description"
    }
  },
  "required": ["paramName"]
}
```

## Type Signature Processing

### `typeSignatureToJSONSchema`

Generates JSON schema from gram type signature.

**Signature**:
```haskell
typeSignatureToJSONSchema
  :: Text              -- typeSignature: Gram notation type signature
  -> Either Text Value -- Right JSON schema or Left error message
```

**Preconditions**:
- `typeSignature` must be valid gram notation type signature

**Postconditions**:
- Returns `Right schema` if type signature is valid and can be converted
- Returns `Left error` if type signature is invalid or cannot be converted

**Type Mapping**:
- `Text` → JSON `"type": "string"`
- `Int` → JSON `"type": "integer"`
- `Double` → JSON `"type": "number"`
- `Bool` → JSON `"type": "boolean"`
- `Maybe T` → Optional parameter (not in required list)
- `{field1: T1, field2: T2}` → JSON object with properties
- `[T]` → JSON `"type": "array"`

**Example**:
```haskell
-- Type signature in curried form (gram path notation)
let typeSig = "(::Text {paramName:\"name\"})==>(::String)"
case typeSignatureToJSONSchema typeSig of
  Right schema -> -- Use schema
  Left error -> -- Handle error
```

### `parseTypeSignature`

Parses gram type signature to structured representation.

**Signature**:
```haskell
parseTypeSignature
  :: Text              -- typeSignature: Gram notation type signature
  -> Either Text TypeSignature -- Right parsed signature or Left error message
```

**Preconditions**:
- `typeSignature` must be valid gram notation type signature

**Postconditions**:
- Returns `Right TypeSignature` if parsing succeeds
- Returns `Left error` if parsing fails

## Parameter Validation

### `validateToolArgs`

Validates tool arguments against the tool's schema.

**Signature**:
```haskell
validateToolArgs :: Value -> Value -> Either Text Value
  -- schema: Tool's JSON schema
  -- args: Arguments to validate
  -- Returns: Right validated args or Left error message
```

**Preconditions**:
- `schema` must be valid JSON schema
- `args` must be valid JSON value

**Postconditions**:
- Returns `Right args` if validation passes
- Returns `Left error` if validation fails (missing required fields, wrong types, etc.)

**Validation Rules**:
- All required fields must be present
- Field types must match schema (string, number, boolean, object, array)
- Nested objects validated recursively

## Serialization

### ToolSpecification Serialization

ToolSpecification is fully serializable (ToJSON, FromJSON instances).

**Serialization Format** (gram notation):
```gram
[sayHello:ToolSpecification {
  description: "Returns a friendly greeting message for the given name"
} |
  (::Text {paramName:"name"})==>(::String)
]
```

**Note**: Tool name is stored as the pattern identifier (`sayHello:ToolSpecification`), not as a property. This ensures global uniqueness required for LLM tool calling.

**Note**: JSON schema is generated from type signature during deserialization, not stored.

**JSON Format** (for API compatibility):
```json
{
  "name": "sayHello",
  "description": "Returns a friendly greeting message for the given name",
  "typeSignature": "(::Text {paramName:\"name\"})==>(::String)",
  "schema": {
    "type": "object",
    "properties": {
      "name": {
        "type": "string"
      }
    },
    "required": ["name"]
  }
}
```

**Note**: The `schema` field is auto-generated from `typeSignature` (curried form gram path notation) and included for convenience, but `typeSignature` is the source of truth.

### Tool Serialization

Tool is NOT serializable (contains function closure). Tool implementations are registered in ToolLibrary at runtime, not serialized.

## Notes

- ToolSpecification creation is pure (no side effects)
- Tool creation is pure (no side effects, but contains function closure)
- ToolLibrary registration is pure (returns new ToolLibrary)
- Tool invocation uses `IO` for side effects (tools may perform I/O)
- Schema validation occurs before tool invocation
- Tool errors are caught and returned to LLM as error messages
- Tool specifications can be shared across multiple agents
- Tool names must be unique within an agent's tool specification list
- Tool names must be unique within a ToolLibrary
- Tool binding happens at execution time, enabling A/B testing
