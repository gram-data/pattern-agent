# Quickstart: Hello World Agent with Tool Execution

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Provide a quick start guide for developers to create agents with tools and execute them

## Prerequisites

- Haskell development environment (GHC 2024)
- LLM API access (OpenAI API key for initial implementation)
- Pattern-agent library installed
- Understanding of basic agent creation (see specs/002-llm-agent/quickstart.md)

## Example 1: Create a Tool Specification

Create a tool specification using gram type signature.

```haskell
import PatternAgent.Tool
import Data.Text

-- Define the sayHello tool specification with gram type signature (curried form)
sayHelloSpec :: ToolSpecification
sayHelloSpec = createToolSpecification
  "sayHello"
  "Returns a friendly greeting message for the given name"
  "(::Text {paramName:\"name\"})==>(::String)"
```

**Key Points**:
- Tool specification uses gram path notation type signature in curried form
- Type signature `(::Text {paramName:"name"})==>(::String)` uses curried form with property records for parameter names
- JSON schema is automatically generated from the type signature
- Parameter name `name` is stored in `{paramName:"name"}` property to avoid global identifier conflicts
- `Text` type maps to JSON `string` type automatically
- `String` is the JSON Schema return type (Haskell implementation may be `IO Text`, but gram represents JSON Schema interface)

## Example 1b: Create a Tool Implementation

Create the executable tool implementation.

```haskell
import PatternAgent.Tool
import Data.Aeson
import Data.Text

-- Define the sayHello tool implementation
sayHelloTool :: Tool
sayHelloTool = createTool
  "sayHello"
  "Returns a friendly greeting message for the given name"
  (typeSignatureToJSONSchema "(::Text {paramName:\"name\"})==>(::String)")  -- Auto-generated schema
  (\args -> do
    -- Extract name from JSON arguments
    let name = args ^. key "name" . _String
    -- Return greeting message
    return $ String $ "Hello, " <> name <> "! Nice to meet you."
  )
```

**Key Points**:
- Tool implementation uses the same type signature as specification
- JSON schema is generated from type signature (no manual schema needed)
- Invocation function converts JSON arguments to Haskell types and back

## Example 2: Create an Agent with Tool Specifications

Create an agent and equip it with tool specifications.

```haskell
import PatternAgent.Agent
import PatternAgent.LLM
import PatternAgent.Tool

-- Create agent with sayHello tool specification
let agent = Agent
      { agentName = "hello_world_agent"
      , agentDescription = Just "A friendly agent that uses the sayHello tool to greet users"
      , agentModel = createModel "gpt-3.5-turbo" OpenAI
      , agentInstruction = "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
      , agentToolSpecs = [sayHelloSpec]  -- Tool specifications, not implementations
      }
```

**Key Points**:
- `agentToolSpecs` is a list of tool specifications (serializable) available to the agent
- Tool specifications use gram path notation type signatures in curried form (e.g., `(::Text {paramName:"name"})==>(::String)`)
- Agent instructions should guide when and how to use tools
- Tool specifications can be shared across multiple agents
- Agent can have zero tool specifications (tool-free agents still supported)
- Tool implementations are bound at execution time via ToolLibrary

## Example 3: Execute Agent with Tool Support

Execute an agent with tool library and see it use tools automatically.

```haskell
import PatternAgent.Execution
import PatternAgent.Context
import PatternAgent.Tool

-- Create tool library with sayHello implementation
let toolLibrary = registerTool "sayHello" sayHelloTool emptyToolLibrary

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

-- Create sayHello tool specification
sayHelloSpec :: ToolSpecification
sayHelloSpec = createToolSpecification
  "sayHello"
  "Returns a friendly greeting message for the given name"
  "(::Text {paramName:\"name\"})==>(::String)"

-- Create sayHello tool implementation
sayHelloTool :: Tool
sayHelloTool = createTool
  "sayHello"
  "Returns a friendly greeting message for the given name"
  (typeSignatureToJSONSchema "(::Text {paramName:\"name\"})==>(::String)")  -- Auto-generated schema
  (\args -> do
    let name = args ^. key "name" . _String
    return $ String $ "Hello, " <> name <> "! Nice to meet you."
  )

-- Create tool library
helloWorldToolLibrary :: ToolLibrary
helloWorldToolLibrary = registerTool "sayHello" sayHelloTool emptyToolLibrary

-- Create hello world agent
helloWorldAgent :: Agent
helloWorldAgent = Agent
  { agentName = "hello_world_agent"
  , agentDescription = Just "A friendly agent that uses the sayHello tool to greet users"
  , agentModel = createModel "gpt-3.5-turbo" OpenAI
  , agentInstruction = "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
  , agentToolSpecs = [sayHelloSpec]  -- Tool specifications
  }

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
createToolSpecification "getTime" "Gets current time" "()==>(::String)"
```

**Named parameters** (curried form with property records):
```haskell
createToolSpecification "greet" "Greets a person" "(::Text {paramName:\"name\"})==>(::String)"
```

**Multiple parameters** (curried form):
```haskell
createToolSpecification "calculate" "Adds two numbers" "(::Int {paramName:\"a\"})==>(::Int {paramName:\"b\"})==>(::Int)"
```

**Optional parameters**:
```haskell
createToolSpecification "search" "Searches with optional limit" "(::Text {paramName:\"query\"})==>(::Int {paramName:\"limit\", optional:true})==>(::Array)"
```

**Record parameters**:
```haskell
createToolSpecification "createUser" "Creates a user" "(::Object {paramName:\"user\", fields:[{name:\"name\", type:\"Text\"}, {name:\"email\", type:\"Text\"}, {name:\"age\", type:\"Int\"}]})==>(::String)"
```

### Tool Parameter Extraction

When implementing tool invocation functions, extract parameters from JSON:

```haskell
(\args -> do
  let name = args ^. key "name" . _String
  let age = args ^. key "age" . _Number
  -- Use parameters...
  return $ String result
)
```

### Error Handling in Tools

Handle errors gracefully in tool invocation:

```haskell
(\args -> do
  case args ^? key "name" . _String of
    Just name -> return $ String $ "Hello, " <> name
    Nothing -> return $ String "Error: name parameter required"
)
```

### Type Signature to Schema

Generate JSON schema from gram type signature:

```haskell
-- Type signature (curried form with property records)
let typeSig = "(::Text {paramName:\"name\"})==>(::Int {paramName:\"age\", optional:true})==>(::String)"

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

