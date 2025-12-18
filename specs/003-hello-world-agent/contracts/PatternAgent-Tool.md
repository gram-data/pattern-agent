# API Contract: Tool System

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the API contract for creating tool descriptions, tool implementations, and tool library management

## Architecture

**Key Design**: Separate tool descriptions (serializable, declarative) from tool implementations (executable, bound at runtime). This enables:
- Gram notation serialization (descriptions only)
- Late binding (implementations bound at execution time)
- A/B testing (same agent specification with different tool implementations)

## Tool Creation (Pattern)

### `createTool`

Creates a tool with gram type signature (Pattern Subject, serializable, no implementation).

**Signature**:
```haskell
createTool
  :: Text              -- name: Unique tool name
  -> Text              -- description: Tool description
  -> Text              -- typeSignature: Type signature in gram path notation (curried form, e.g., "(personName::Text {default:\"world\"})==>(::String)")
  -> Tool
```

**Preconditions**:
- `name` must be non-empty
- `description` must be non-empty
- `typeSignature` must be valid gram notation type signature

**Postconditions**:
- Returns `Tool` (Pattern Subject) ready for serialization and agent association
- JSON schema is automatically generated from type signature
- Tool can be added to agent's tools list

**Example**:
```haskell
let sayHello = createTool
      "sayHello"
      "Returns a friendly greeting message for the given name"
      -- Type signature in curried form: (personName::Text {default:"world"})==>(::String)
      -- (Implementation may parse from text or gram path notation)
```

**Type Signature Format** (Curried Form with Parameter Names as Identifiers):
- Simple (no parameters): `()==>(::String)`
- Named parameter: `(personName::Text)==>(::String)`
- Multiple parameters: `(personName::Text)==>(age::Int)==>(::String)`
- Optional parameter: `(personName::Text)==>(age::Int {default:18})==>(::String)`
- Record parameter: `(userParams::Object {fields:[{name:"name", type:"Text"}, {name:"age", type:"Int"}]})==>(::String)`

## ToolImpl Implementation Creation

### `createToolImpl`

Creates a tool implementation with executable function (not serializable).

**Signature**:
```haskell
createToolImpl
  :: Text              -- name: Unique tool name (should match Tool name)
  -> Text              -- description: Tool description (should match Tool)
  -> Value             -- schema: JSON schema (should match Tool schema)
  -> (Value -> IO Value)  -- invoke: Tool invocation function
  -> ToolImpl
```

**Preconditions**:
- `name` must be non-empty
- `description` must be non-empty
- `schema` must be valid JSON schema
- `invoke` function must handle JSON parameter conversion and errors

**Postconditions**:
- Returns `ToolImpl` ready for ToolLibrary registration
- ToolImpl can be registered in ToolLibrary for late binding

**Example**:
```haskell
let sayHelloImpl = createToolImpl
      "sayHello"
      "Returns a friendly greeting message for the given name"
      (typeSignatureToJSONSchema "(personName::Text {default:\"world\"})==>(::String)")
      (\args -> do
        let name = fromMaybe "world" $ args ^? key "personName" . _String
        return $ String $ "Hello, " <> name <> "! Nice to meet you."
      )
```

## Tool Accessors (Lens-based for Pattern)

### `toolName`

Returns the tool's name (pattern identifier).

**Signature**:
```haskell
toolName :: Lens' Tool Text
```

### `toolDescription`

Returns the tool's description.

**Signature**:
```haskell
toolDescription :: Lens' Tool Text
```

### `toolTypeSignature`

Returns the tool's type signature in gram notation.

**Signature**:
```haskell
toolTypeSignature :: Lens' Tool Text
```

### `toolSchema`

Returns the tool's auto-generated JSON schema.

**Signature**:
```haskell
toolSchema :: Lens' Tool Value
```

## ToolImpl Accessors

### `toolImplName`

Returns the tool implementation's name.

**Signature**:
```haskell
toolImplName :: ToolImpl -> Text
```

### `toolImplDescription`

Returns the tool implementation's description.

**Signature**:
```haskell
toolImplDescription :: ToolImpl -> Text
```

### `toolImplSchema`

Returns the tool implementation's parameter schema.

**Signature**:
```haskell
toolImplSchema :: ToolImpl -> Value
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
  -> ToolImpl         -- toolImpl: Tool implementation
  -> ToolLibrary      -- library: Tool library to register in
  -> ToolLibrary      -- Updated tool library
```

**Preconditions**:
- `name` must match `toolImpl.toolImplName`
- `toolImpl` must be valid (name, description, schema, invoke function)

**Postconditions**:
- Returns ToolLibrary with tool registered
- ToolImpl can be looked up by name
- If tool with same name already exists, it is replaced

**Example**:
```haskell
let library = registerTool "sayHello" sayHelloImpl emptyToolLibrary
```

### `lookupTool`

Looks up a tool implementation by name.

**Signature**:
```haskell
lookupTool
  :: Text              -- name: Tool name
  -> ToolLibrary       -- library: Tool library to search
  -> Maybe ToolImpl    -- Tool implementation if found
```

**Preconditions**:
- `name` must be non-empty

**Postconditions**:
- Returns `Just toolImpl` if tool found
- Returns `Nothing` if tool not found

### `bindTool`

Binds a Tool (Pattern) to a ToolImpl implementation from the library.

**Signature**:
```haskell
bindTool
  :: Tool              -- tool: Tool (Pattern)
  -> ToolLibrary       -- library: Tool library to search
  -> Maybe ToolImpl    -- Bound tool implementation if found and matches
```

**Preconditions**:
- `tool` must be valid Tool (Pattern)
- `library` must be valid ToolLibrary

**Postconditions**:
- Returns `Just toolImpl` if tool found and matches Tool (name, description, schema)
- Returns `Nothing` if tool not found or doesn't match

**Validation**: Validates that ToolImpl matches Tool (name, description, schema must match)

## Tool Invocation

Tool invocation is handled internally by the agent execution system. Tools are invoked automatically when the LLM decides to use them based on:
- Agent instructions
- User input
- Tool descriptions and schemas

**Invocation Flow**:
1. Agent execution receives Agent (with Tools) and ToolLibrary
2. Tool binding: Tools bound to ToolImpl implementations from ToolLibrary
3. LLM selects tool and provides parameters as JSON
4. Parameters validated against tool schema
5. `toolImplInvoke` function called with validated parameters
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
    "personName": {
      "type": "string",
      "description": "Parameter description"
    }
  },
  "required": ["personName"]
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
- `T {default:value}` → Optional parameter with default value (not in required list, default included in schema)
- `{field1: T1, field2: T2}` → JSON object with properties
- `[T]` → JSON `"type": "array"`

**Example**:
```haskell
-- Type signature in curried form (gram path notation)
let typeSig = "(personName::Text)==>(::String)"
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

### Tool Serialization (Pattern)

Tool (Pattern Subject) is fully serializable in gram notation.

**Serialization Format** (gram notation):
```gram
[sayHello:Tool {
  description: "Returns a friendly greeting message for the given name"
} |
  (personName::Text {default:"world"})==>(::String)
]
```

**Note**: Tool name is stored as the pattern identifier (`sayHello:Tool`), ensuring global uniqueness required for LLM tool calling. Parameter name `personName` is also a globally unique identifier, encouraging consistent vocabulary.

**Note**: JSON schema is generated from type signature during deserialization or when accessed via lens, not stored.

**JSON Format** (for API compatibility):
```json
{
  "name": "sayHello",
  "description": "Returns a friendly greeting message for the given name",
  "typeSignature": "(personName::Text {default:\"world\"})==>(::String)",
  "schema": {
    "type": "object",
    "properties": {
      "personName": {
        "type": "string",
        "default": "world"
      }
    },
    "required": []
  }
}
```

**Note**: The `schema` field is auto-generated from `typeSignature` (curried form gram path notation) and included for convenience, but `typeSignature` is the source of truth.

### ToolImpl Serialization

ToolImpl is NOT serializable (contains function closure). ToolImpl implementations are registered in ToolLibrary at runtime, not serialized.

## Notes

- Tool creation is pure (no side effects)
- ToolImpl creation is pure (no side effects, but contains function closure)
- ToolLibrary registration is pure (returns new ToolLibrary)
- Tool invocation uses `IO` for side effects (tools may perform I/O)
- Schema validation occurs before tool invocation
- Tool errors are caught and returned to LLM as error messages
- Tools can be shared across multiple agents
- Tool names must be unique within an agent's tools list
- Tool names must be unique within a ToolLibrary
- Tool binding happens at execution time, enabling A/B testing
