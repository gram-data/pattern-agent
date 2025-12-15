# Quickstart: Hello World Agent with Tool Execution

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Provide a quick start guide for developers to create agents with tools and execute them

## Prerequisites

- Haskell development environment (GHC 2024)
- LLM API access (OpenAI API key for initial implementation)
- Pattern-agent library installed
- Understanding of basic agent creation (see specs/002-llm-agent/quickstart.md)

## Example 1: Create a Tool (Pattern)

Create a tool using gram type signature.

```haskell
import PatternAgent.Tool
import Data.Text

-- Define the sayHello tool with gram type signature (curried form)
sayHello :: Tool
sayHello = createTool
  "sayHello"
  "Returns a friendly greeting message for the given name"
  "(personName::Text {default:\"world\"})==>(::String)"
```

**Key Points**:
- Tool uses gram path notation type signature in curried form
- Type signature `(personName::Text {default:"world"})==>(::String)` uses curried form with parameter names as identifiers
- JSON schema is automatically generated from the type signature
- Parameter name `personName` is a globally unique identifier, encouraging consistent vocabulary
- Default value `"world"` is specified for optional parameter
- `Text` type maps to JSON `string` type automatically
- `String` is the JSON Schema return type (Haskell implementation may be `IO Text`, but gram represents JSON Schema interface)

## Example 1b: Create a ToolImpl Implementation

Create the executable tool implementation.

```haskell
import PatternAgent.Tool
import Data.Aeson
import Data.Text

-- Define the sayHello tool implementation
sayHelloImpl :: ToolImpl
sayHelloImpl = createToolImpl
  "sayHello"
  "Returns a friendly greeting message for the given name"
  (typeSignatureToJSONSchema "(personName::Text {default:\"world\"})==>(::String)")  -- Auto-generated schema
  (\args -> do
    -- Extract personName from JSON arguments (use default if missing)
    let name = fromMaybe "world" $ args ^? key "personName" . _String
    -- Return greeting message
    return $ String $ "Hello, " <> name <> "! Nice to meet you."
  )
```

**Key Points**:
- ToolImpl uses the same type signature as Tool (Pattern)
- JSON schema is generated from type signature (no manual schema needed)
- Invocation function converts JSON arguments to Haskell types and back
- Default values should be handled in implementation

## Example 2: Create an Agent with Tools

Create an agent and equip it with tools.

```haskell
import PatternAgent.Agent
import PatternAgent.LLM
import PatternAgent.Tool

-- Create agent with sayHello tool (Pattern)
let agent = createAgent
      "hello_world_agent"
      (Just "A friendly agent that uses the sayHello tool to greet users")
      (createModel "gpt-3.5-turbo" OpenAI)
      "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
      [sayHello]  -- Tools (Pattern), not implementations
```

**Key Points**:
- `agentTools` is a list of tools (Pattern, serializable) available to the agent
- Tools use gram path notation type signatures in curried form (e.g., `(personName::Text {default:"world"})==>(::String)`)
- Agent instructions should guide when and how to use tools
- Tools can be shared across multiple agents
- Agent can have zero tools (tool-free agents still supported)
- ToolImpl implementations are bound at execution time via ToolLibrary

## Example 3: Execute Agent with Tool Support

Execute an agent with tool library and see it use tools automatically.

```haskell
import PatternAgent.Execution
import PatternAgent.Context
import PatternAgent.Tool

-- Create tool library with sayHello implementation
let toolLibrary = registerTool "sayHello" sayHelloImpl emptyToolLibrary

-- Execute agent with tool library
let context = emptyContext
result <- executeAgentWithLibrary agent "Hello!" context toolLibrary

case result of
  Right response -> do
    putStrLn $ "Agent: " ++ unpack (responseContent response)
    -- Check which tools were used
    mapM_ (\invocation -> do
      putStrLn $ "Tool used: " ++ unpack (invocationToolName invocation)
      case invocationResult invocation of
        Right result -> putStrLn $ "Result: " ++ show result
        Left error -> putStrLn $ "Error: " ++ unpack error
      ) (responseToolsUsed response)
  Left error -> do
    putStrLn $ "Error: " ++ show error
```

**Expected Output**:
```
Agent: Hello, User! Nice to meet you. How can I help you today?
Tool used: sayHello
Result: String "Hello, User! Nice to meet you."
```

**Key Points**:
- Tool execution happens automatically when LLM decides to use a tool
- Tool invocations are tracked in `responseToolsUsed`
- Tool results are incorporated into the agent's final response
- Error handling ensures execution doesn't crash on tool failures

## Example 4: Multi-Turn Conversation with Tools

Maintain conversation context across multiple exchanges with tool usage.

```haskell
import PatternAgent.Context

-- First message
let context1 = emptyContext
result1 <- executeAgent agent "Hello!" context1

case result1 of
  Right response1 -> do
    putStrLn $ "Agent: " ++ unpack (responseContent response1)
    
    -- Update context with user message and agent response
    let context2 = addMessage UserRole "Hello!" context1
    let context2' = case headMay (responseToolsUsed response1) of
          Just invocation -> case invocationResult invocation of
            Right toolResult -> addMessage (FunctionRole "sayHello") (show toolResult) context2
            Left _ -> context2
          Nothing -> context2
    let context2'' = addMessage AssistantRole (responseContent response1) context2'
    
    -- Follow-up message
    result2 <- executeAgent agent "What's your name?" context2''
    case result2 of
      Right response2 -> do
        putStrLn $ "Agent: " ++ unpack (responseContent response2)
      Left error -> putStrLn $ "Error: " ++ show error
  Left error -> putStrLn $ "Error: " ++ show error
```

**Key Points**:
- Conversation context includes user messages, tool calls, tool results, and agent responses
- Function role messages (`FunctionRole toolName`) represent tool results
- Context allows agent to reference previous tool usage
- Context management is the caller's responsibility

## Example 5: Hello World Complete Example

Complete hello world example demonstrating the full tool execution flow.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import PatternAgent.Agent
import PatternAgent.Execution
import PatternAgent.Context
import PatternAgent.LLM
import PatternAgent.Tool
import Data.Aeson
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

-- Create sayHello tool (Pattern)
sayHello :: Tool
sayHello = createTool
  "sayHello"
  "Returns a friendly greeting message for the given name"
  "(personName::Text {default:\"world\"})==>(::String)"

-- Create sayHello tool implementation
sayHelloImpl :: ToolImpl
sayHelloImpl = createToolImpl
  "sayHello"
  "Returns a friendly greeting message for the given name"
  (typeSignatureToJSONSchema "(personName::Text {default:\"world\"})==>(::String)")  -- Auto-generated schema
  (\args -> do
    let name = fromMaybe "world" $ args ^? key "personName" . _String
    return $ String $ "Hello, " <> name <> "! Nice to meet you."
  )

-- Create tool library
helloWorldToolLibrary :: ToolLibrary
helloWorldToolLibrary = registerTool "sayHello" sayHelloImpl emptyToolLibrary

-- Create hello world agent (Pattern)
helloWorldAgent :: Agent
helloWorldAgent = createAgent
  "hello_world_agent"
  (Just "A friendly agent that uses the sayHello tool to greet users")
  (createModel "gpt-3.5-turbo" OpenAI)
  "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
  [sayHello]  -- Tools (Pattern)

-- Main execution
main :: IO ()
main = do
  putStrLn "Hello World Agent Example"
  putStrLn "=========================="
  putStrLn ""
  
  let context = emptyContext
  result <- executeAgentWithLibrary helloWorldAgent "Hello!" context helloWorldToolLibrary
  
  case result of
    Right response -> do
      putStrLn $ "Agent Response: " ++ unpack (responseContent response)
      putStrLn ""
      putStrLn "Tools Used:"
      mapM_ (\invocation -> do
        putStrLn $ "  - " ++ unpack (invocationToolName invocation)
        case invocationResult invocation of
          Right result -> putStrLn $ "    Result: " ++ show result
          Left error -> putStrLn $ "    Error: " ++ unpack error
        ) (responseToolsUsed response)
    Left error -> do
      putStrLn $ "Error: " ++ show error
```

**Expected Output**:
```
Hello World Agent Example
==========================

Agent Response: Hello, User! Nice to meet you. How can I help you today?

Tools Used:
  - sayHello
    Result: String "Hello, User! Nice to meet you."
```

## Common Patterns

### Type Signature Examples

Examples of gram type signatures for different tool patterns:

**Simple single parameter**:
```haskell
createTool "getTime" "Gets current time" "()==>(::String)"
```

**Named parameters** (curried form with identifiers):
```haskell
createTool "greet" "Greets a person" "(personName::Text)==>(::String)"
```

**Multiple parameters** (curried form):
```haskell
createTool "calculate" "Adds two numbers" "(a::Int)==>(b::Int)==>(::Int)"
```

**Optional parameters**:
```haskell
createTool "search" "Searches with optional limit" "(query::Text)==>(limit::Int {default:10})==>(::Array)"
```

**Record parameters**:
```haskell
createTool "createUser" "Creates a user" "(userParams::Object {fields:[{name:\"name\", type:\"Text\"}, {name:\"email\", type:\"Text\"}, {name:\"age\", type:\"Int\"}]})==>(::String)"
```

### Tool Parameter Extraction

When implementing tool invocation functions, extract parameters from JSON:

```haskell
(\args -> do
  let name = args ^. key "personName" . _String
  let age = args ^. key "age" . _Number
  -- Use parameters...
  return $ String result
)
```

### Error Handling in Tools

Handle errors gracefully in tool invocation:

```haskell
(\args -> do
  case args ^? key "personName" . _String of
    Just name -> return $ String $ "Hello, " <> name
    Nothing -> return $ String "Error: personName parameter required"
)
```

### Type Signature to Schema

Generate JSON schema from gram type signature:

```haskell
-- Type signature (curried form with parameter names as identifiers)
let typeSig = "(personName::Text)==>(age::Int {default:18})==>(::String)"

-- Generate schema
case typeSignatureToJSONSchema typeSig of
  Right schema -> -- Use schema (auto-generated)
  Left error -> -- Handle parsing error
```

### Tool Validation

Validate tool arguments before execution:

```haskell
case validateToolArgs toolSchema args of
  Right validatedArgs -> invokeTool tool validatedArgs
  Left error -> return $ Left error
```

## Next Steps

- See `specs/002-llm-agent/quickstart.md` for basic agent creation
- See `specs/003-hello-world-agent/contracts/` for detailed API documentation
- See `specs/003-hello-world-agent/data-model.md` for data structure details
- See `tests/scenario/HelloWorldTest.hs` for complete test examples

