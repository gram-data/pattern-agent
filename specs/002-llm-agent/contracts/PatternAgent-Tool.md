# API Contract: Tool System

**Feature**: Basic LLM Agent  
**Date**: 2025-01-27  
**Purpose**: Define the API contract for creating and managing tools

## Tool Creation

### `createTool`

Creates a tool with specified name, description, schema, and invocation function.

**Signature**:
```haskell
createTool
  :: Text              -- name: Unique tool name
  -> Text              -- description: Tool description
  -> Value             -- schema: JSON schema for tool parameters
  -> (Value -> IO Value)  -- invoke: Tool invocation function
  -> Tool
```

**Preconditions**:
- `name` must be non-empty
- `description` must be non-empty
- `schema` must be valid JSON schema
- `invoke` function must handle JSON parameter conversion and errors

**Postconditions**:
- Returns `Tool` ready for use with agents
- Tool can be added to agent's tool list

**Errors**:
- Tool creation itself doesn't fail, but schema validation may occur during tool invocation

**Example**:
```haskell
let getCapitalCityTool = createTool
      "get_capital_city"
      "Retrieves the capital city for a given country"
      (object [ "type" .= ("object" :: Text)
              , "properties" .= object [ "country" .= object [ "type" .= ("string" :: Text) ] ]
              , "required" .= ["country"]
              ])
      (\args -> do
        let country = args ^. key "country" . _String
        let capitals = Map.fromList [("france", "Paris"), ("japan", "Tokyo")]
        return $ String $ fromMaybe "Unknown" (Map.lookup (toLower country) capitals)
      )
```

## Function Tool Creation

### `createFunctionTool`

Creates a tool from a Haskell function with automatic schema generation (simplified version - manual schema required initially).

**Signature**:
```haskell
createFunctionTool
  :: Text              -- name: Unique tool name
  -> Text              -- description: Tool description
  -> Value             -- schema: JSON schema for function parameters
  -> (Value -> IO Value)  -- invoke: Wrapper that converts JSON to function args and back
  -> Tool
```

**Preconditions**:
- `name` must be non-empty
- `description` must be non-empty
- `schema` must match function parameter types
- `invoke` function must properly convert JSON to Haskell types and back

**Note**: Full automatic schema generation from function signatures is deferred (Principle 5). For now, developers must provide schema manually.

**Example**:
```haskell
-- Haskell function
getCapitalCity :: Text -> IO Text
getCapitalCity country = do
  let capitals = Map.fromList [("france", "Paris"), ("japan", "Tokyo")]
  return $ fromMaybe "Unknown" (Map.lookup (toLower country) capitals)

-- Create tool wrapper
let tool = createFunctionTool
      "get_capital_city"
      "Retrieves the capital city for a given country"
      (object [ "type" .= ("object" :: Text)
              , "properties" .= object [ "country" .= object [ "type" .= ("string" :: Text) ] ]
              , "required" .= ["country"]
              ])
      (\jsonArgs -> do
        let country = jsonArgs ^. key "country" . _String
        result <- getCapitalCity country
        return $ String result
      )
```

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

## Tool Invocation

Tool invocation is handled internally by the agent execution system. Tools are invoked automatically when the LLM decides to use them based on:
- Agent instructions
- User input
- Tool descriptions and schemas

**Invocation Flow**:
1. LLM selects tool and provides parameters as JSON
2. Parameters validated against tool schema
3. `toolInvoke` function called with validated parameters
4. Result (or error) returned to LLM
5. LLM incorporates result into response

## Schema Definition

Tools use JSON Schema for parameter definition. The schema must define:
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

## Notes

- Tool creation is pure (no side effects)
- Tool invocation uses `IO` for side effects (tools may perform I/O)
- Schema validation occurs before tool invocation
- Tool errors are caught and returned to LLM as error messages
- Tools can be shared across multiple agents
- Tool names must be unique within an agent's tool list
