# API Contract: Builtin Tools

**Feature**: Agent Execution with Scenario Tests, Interactive CLI, and Observability  
**Module**: `src/PatternAgent/Runtime/BuiltinTools.hs`  
**Date**: 2025-01-27

## Overview

Builtin tools provide ready-to-use tool implementations for multi-tool scenario testing. Three complementary tools are provided: `getCurrentTime`, `calculate`, and `formatText`.

## Tool: getCurrentTime

### Description

Returns the current time as an ISO 8601 formatted string.

### Type Signature

```gram
[getCurrentTime:Tool {
  description: "Returns the current time as an ISO 8601 formatted string"
} | (::String)]
```

**Parameters**: None  
**Return Type**: `String` (ISO 8601 timestamp, e.g., "2025-01-27T10:30:00Z")

### Implementation

**Function**:
```haskell
getCurrentTimeToolImpl :: Tool -> Either Text ToolImpl
getCurrentTimeToolImpl tool = do
  let toolNameVal = view toolName tool
  let toolDesc = view toolDescription tool
  let toolSchemaVal = view toolSchema tool
  
  createToolImpl
    toolNameVal
    toolDesc
    toolSchemaVal
    (\_ -> do
      now <- getCurrentTime
      let iso8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      return $ String $ T.pack iso8601
    )
```

### Usage Example

```haskell
-- In agent gram file
[myAgent:Agent {
  description: "Agent that tells the time"
  instruction: "When asked about the time, use getCurrentTime tool"
  model: "OpenAI/gpt-4o-mini"
} |
  [getCurrentTime:Tool {
    description: "Returns the current time as an ISO 8601 formatted string"
  } | (::String)]
]
```

## Tool: calculate

### Description

Evaluates a mathematical expression and returns the result.

### Type Signature

```gram
[calculate:Tool {
  description: "Evaluates a mathematical expression and returns the result"
} |
  (expression::String)==>(::String)
]
```

**Parameters**:
- `expression :: String` - Mathematical expression (e.g., "2 + 2", "10 * 5", "100 / 4")

**Return Type**: `String` (result as string, e.g., "4", "50", "25")

### Implementation

**Function**:
```haskell
calculateToolImpl :: Tool -> Either Text ToolImpl
calculateToolImpl tool = do
  let toolNameVal = view toolName tool
  let toolDesc = view toolDescription tool
  let toolSchemaVal = view toolSchema tool
  
  createToolImpl
    toolNameVal
    toolDesc
    toolSchemaVal
    (\args -> do
      let parsedExpr = parseMaybe (withObject "toolArgs" $ \obj -> obj .:? "expression") args
      case parsedExpr of
        Just (Just (String expr)) -> do
          -- Safe evaluation of simple math expressions
          -- Supports: +, -, *, /, parentheses
          let result = evaluateExpression (T.unpack expr)
          return $ String $ T.pack result
        _ -> return $ String "Error: Invalid expression parameter"
    )
```

**Note**: Expression evaluation should be safe (no code execution, only math operations).

### Usage Example

```haskell
-- In agent gram file
[calculatorAgent:Agent {
  description: "Agent that performs calculations"
  instruction: "When asked to calculate, use the calculate tool"
  model: "OpenAI/gpt-4o-mini"
} |
  [calculate:Tool {
    description: "Evaluates a mathematical expression and returns the result"
  } |
    (expression::String)==>(::String)
  ]
]
```

## Tool: formatText

### Description

Formats text according to a format specification.

### Type Signature

```gram
[formatText:Tool {
  description: "Formats text according to a format specification"
} |
  (text::String, format::String)==>(::String)
]
```

**Parameters**:
- `text :: String` - Text to format
- `format :: String` - Format specification ("uppercase", "lowercase", "reverse")

**Return Type**: `String` (formatted text)

### Implementation

**Function**:
```haskell
formatTextToolImpl :: Tool -> Either Text ToolImpl
formatTextToolImpl tool = do
  let toolNameVal = view toolName tool
  let toolDesc = view toolDescription tool
  let toolSchemaVal = view toolSchema tool
  
  createToolImpl
    toolNameVal
    toolDesc
    toolSchemaVal
    (\args -> do
      let parsed = parseMaybe (withObject "toolArgs" $ \obj -> do
            textVal <- obj .:? "text"
            formatVal <- obj .:? "format"
            return (textVal, formatVal)
          ) args
      case parsed of
        Just (Just (String text), Just (String format)) -> do
          let formatted = applyFormat (T.unpack text) (T.unpack format)
          return $ String $ T.pack formatted
        _ -> return $ String "Error: Invalid text or format parameter"
    )
  where
    applyFormat text "uppercase" = map toUpper text
    applyFormat text "lowercase" = map toLower text
    applyFormat text "reverse" = reverse text
    applyFormat text _ = "Error: Unknown format specification"
```

### Usage Example

```haskell
-- In agent gram file
[formatterAgent:Agent {
  description: "Agent that formats text"
  instruction: "When asked to format text, use the formatText tool"
  model: "OpenAI/gpt-4o-mini"
} |
  [formatText:Tool {
    description: "Formats text according to a format specification"
  } |
    (text::String, format::String)==>(::String)
  ]
]
```

## Tool Registration

### createToolLibraryFromAgent (Extended)

**Signature**:
```haskell
createToolLibraryFromAgent :: Agent -> Either Text ToolLibrary
```

**Extended Behavior**:
- Recognizes `"sayHello"` (existing)
- Recognizes `"getCurrentTime"` (new)
- Recognizes `"calculate"` (new)
- Recognizes `"formatText"` (new)
- Returns error for unknown tool names

**Implementation**:
```haskell
createToolLibraryFromAgent agent = do
  let tools = view agentTools agent
  foldl registerToolFromPattern (Right emptyToolLibrary) tools
  where
    registerToolFromPattern :: Either Text ToolLibrary -> Tool -> Either Text ToolLibrary
    registerToolFromPattern (Left err) _ = Left err
    registerToolFromPattern (Right lib) tool = do
      let toolNameStr = view toolName tool
      case T.unpack toolNameStr of
        "sayHello" -> do
          impl <- sayHelloToolImpl tool
          Right $ registerTool toolNameStr impl lib
        "getCurrentTime" -> do
          impl <- getCurrentTimeToolImpl tool
          Right $ registerTool toolNameStr impl lib
        "calculate" -> do
          impl <- calculateToolImpl tool
          Right $ registerTool toolNameStr impl lib
        "formatText" -> do
          impl <- formatTextToolImpl tool
          Right $ registerTool toolNameStr impl lib
        _ -> Left $ "Unsupported tool: " <> toolNameStr
```

## Multi-Tool Usage Example

```haskell
-- Agent with multiple builtin tools
[multiToolAgent:Agent {
  description: "Agent with multiple tools"
  instruction: "Use appropriate tools based on user requests"
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

## Error Handling

### Unknown Tool Name

```haskell
createToolLibraryFromAgent agent
-- Left "Unsupported tool: unknownTool"
```

### Invalid Tool Parameters

- Tools validate parameters against schema
- Return error messages for invalid parameters
- Error messages returned as tool results (not exceptions)

## Implementation Notes

- All builtin tools follow existing `sayHello` tool pattern
- Tools are simple and complementary for multi-tool testing
- Tool implementations use safe evaluation (no code execution)
- Error handling returns error messages as tool results
- Tools registered automatically when agent is loaded

