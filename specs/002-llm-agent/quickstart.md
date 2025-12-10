# Quickstart: Basic LLM Agent

**Feature**: Basic LLM Agent  
**Date**: 2025-01-27  
**Purpose**: Provide a quick start guide for developers to create and use LLM agents

## Prerequisites

- Haskell development environment (GHC 2024)
- LLM API access (OpenAI API key for initial implementation)
- Pattern-agent library installed

## Example 1: Simple Conversational Agent

Create a basic agent that answers questions using only its built-in knowledge.

```haskell
import PatternAgent.Agent
import PatternAgent.Execution

main :: IO ()
main = do
  -- Create a simple conversational agent
  let agent = createAgent
        "capital_agent"
        (createModel "gpt-4" OpenAI)
        "You are an agent that provides the capital city of a country. When a user asks for the capital, identify the country name and respond clearly with the capital city."
        (Just "Answers user questions about the capital city of a given country")
        []  -- No tools
        Nothing  -- Default generation config

  -- Execute agent with user input
  let context = emptyContext
  result <- executeAgent agent "What's the capital of France?" context

  case result of
    Right response -> do
      putStrLn "Agent Response:"
      putStrLn (unpack $ responseContent response)
    Left error -> do
      putStrLn "Error:"
      putStrLn (show error)
```

**Expected Output**:
```
Agent Response:
The capital of France is Paris.
```

## Example 2: Agent with Tool Integration

Create an agent that uses a tool to look up information.

```haskell
import PatternAgent.Agent
import PatternAgent.Execution
import PatternAgent.Tool
import Data.Aeson
import Data.Text

-- Define a tool function
getCapitalCity :: Text -> IO Text
getCapitalCity country = do
  let capitals = Map.fromList
        [ ("france", "Paris")
        , ("japan", "Tokyo")
        , ("canada", "Ottawa")
        ]
  return $ fromMaybe "Unknown" (Map.lookup (toLower country) capitals)

-- Create tool
let capitalTool = createFunctionTool
      "get_capital_city"
      "Retrieves the capital city for a given country"
      (object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "country" .= object
                [ "type" .= ("string" :: Text)
                , "description" .= ("The country name" :: Text)
                ]
            ]
        , "required" .= ["country"]
        ])
      (\jsonArgs -> do
        let country = jsonArgs ^. key "country" . _String
        result <- getCapitalCity country
        return $ String result
      )

-- Create agent with tool
let agent = createAgent
      "capital_agent_tool"
      (createModel "gpt-4" OpenAI)
      "You are a helpful agent that provides the capital city of a country using a tool. When asked about a capital: 1) Extract the country name, 2) Use the `get_capital_city` tool to find the capital, 3) Respond clearly with the result."
      (Just "Retrieves the capital city using a specific tool")
      [capitalTool]
      Nothing

-- Execute agent
let context = emptyContext
result <- executeAgent agent "What's the capital of Canada?" context

case result of
  Right response -> do
    putStrLn "Agent Response:"
    putStrLn (unpack $ responseContent response)
    putStrLn "\nTools Used:"
    mapM_ (\inv -> putStrLn $ "- " ++ unpack (invocationToolName inv)) 
          (responseToolsUsed response)
  Left error -> putStrLn $ "Error: " ++ show error
```

**Expected Output**:
```
Agent Response:
The capital of Canada is Ottawa.

Tools Used:
- get_capital_city
```

## Example 3: Multi-Turn Conversation

Create an agent that maintains conversation context across multiple exchanges.

```haskell
import PatternAgent.Agent
import PatternAgent.Execution

main :: IO ()
main = do
  -- Create agent
  let agent = createAgent
        "conversational_agent"
        (createModel "gpt-4" OpenAI)
        "You are a helpful assistant that maintains context across conversations."
        Nothing
        []
        Nothing

  -- First message
  let context1 = emptyContext
  result1 <- executeAgent agent "What's the capital of France?" context1
  
  case result1 of
    Right response1 -> do
      putStrLn "User: What's the capital of France?"
      putStrLn $ "Agent: " ++ unpack (responseContent response1)
      
      -- Update context with first exchange
      let context2 = addMessage UserRole "What's the capital of France?" context1
      let context3 = addMessage AssistantRole (responseContent response1) context2
      
      -- Second message (follow-up)
      result2 <- executeAgent agent "What about its population?" context3
      
      case result2 of
        Right response2 -> do
          putStrLn "\nUser: What about its population?"
          putStrLn $ "Agent: " ++ unpack (responseContent response2)
        Left error -> putStrLn $ "Error: " ++ show error
    Left error -> putStrLn $ "Error: " ++ show error
```

**Expected Output**:
```
User: What's the capital of France?
Agent: The capital of France is Paris.

User: What about its population?
Agent: Paris has a population of approximately 2.1 million people.
```

## Example 4: Agent with Custom Generation Config

Create an agent with custom temperature and max tokens.

```haskell
import PatternAgent.Agent

let config = createGenerateContentConfig
      (Just 0.7)   -- temperature: More creative responses
      (Just 500)   -- maxTokens: Limit response length
      Nothing      -- topP: Use default
      Nothing      -- topK: Use default

let agent = createAgent
      "creative_agent"
      (createModel "gpt-4" OpenAI)
      "You are a creative writing assistant."
      Nothing
      []
      (Just config)
```

## Error Handling

All agent operations return `Either AgentError a` for error handling:

```haskell
result <- executeAgent agent userInput context
case result of
  Right response -> 
    -- Success: use response
    putStrLn (unpack $ responseContent response)
  Left (LLMAPIError msg) ->
    -- LLM API error
    putStrLn $ "API Error: " ++ unpack msg
  Left (ToolError msg) ->
    -- Tool execution error
    putStrLn $ "Tool Error: " ++ unpack msg
  Left (ValidationError msg) ->
    -- Input validation error
    putStrLn $ "Validation Error: " ++ unpack msg
  Left (ConfigurationError msg) ->
    -- Configuration error
    putStrLn $ "Configuration Error: " ++ unpack msg
```

## Next Steps

- See [data-model.md](data-model.md) for detailed data structures
- See [contracts/](contracts/) for complete API documentation
- See [spec.md](spec.md) for feature requirements and user stories

## Notes

- Agent execution requires LLM API access (API key configuration not shown)
- Conversation context must be managed by the caller
- Tool functions must handle JSON conversion properly
- Error handling is important for production use
