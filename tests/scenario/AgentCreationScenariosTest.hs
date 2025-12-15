{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for different ways of creating agents with tools.
--
-- Tests three scenarios:
-- 1. Parsing a complete agent with tools from gram
-- 2. Assembling an agent entirely in code (no parsing)
-- 3. Mixing elements derived via parsing or programmatic creation
module AgentCreationScenariosTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Agent, Tool, createAgent, createTool, createModel, Provider(..), agentName, agentTools, toolName)
import PatternAgent.Language.Serialization (parseGram, parseAgent, parseTool)
import Control.Lens (view)
import qualified Data.Text as T
import Pattern (Pattern Subject)
import Pattern.Core (patternWith)
import Subject.Core (Subject(..), Symbol(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Subject.Value (Value(..))
import qualified Gram

-- | Scenario 1: Parse complete agent with tools from gram notation.
--
-- Given: A gram file containing a complete agent with tools
-- When: We parse it
-- Then: We get a fully functional agent with tools accessible
testParseAgentFromGram :: TestTree
testParseAgentFromGram = testCase "Parse complete agent with tools from gram" $ do
  let gramContent = T.unlines
        [ "[hello_world_agent:Agent {"
        , "  description: \"A friendly agent that uses the sayHello tool to greet users\","
        , "  instruction: \"You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting.\","
        , "  model: \"OpenAI/gpt-3.5-turbo\""
        , "} |"
        , "  [sayHello:Tool {"
        , "    description: \"Returns a friendly greeting message for the given name\""
        , "  } |"
        , "    (personName::Text {default:\"world\"})==>(::String)"
        , "  ]"
        , "]"
        ]
  
  case parseAgent gramContent of
    Right agent -> do
      -- Verify agent properties
      view agentName agent @?= "hello_world_agent"
      -- Verify agent has tool
      let tools = view agentTools agent
      length tools @?= 1
      view toolName (head tools) @?= "sayHello"
    Left err -> assertFailure $ "Failed to parse agent from gram: " ++ T.unpack err

-- | Scenario 2: Assemble agent entirely in code (no parsing).
--
-- Given: We want to create an agent programmatically
-- When: We use createAgent and createTool with Pattern Subject elements
-- Then: We get a fully functional agent without any parsing
testAssembleAgentInCode :: TestTree
testAssembleAgentInCode = testCase "Assemble agent entirely in code (no parsing)" $ do
  -- Create type signature pattern element programmatically (no parsing)
  let typeSigPattern = case Gram.fromGram "(personName::Text {default:\"world\"})==>(::String)" of
        Right p -> p
        Left _ -> error "Should parse type signature"
  
  -- Create tool programmatically using Pattern Subject element (no parsing)
  let tool = case createTool "sayHello" "Returns a friendly greeting message for the given name" typeSigPattern of
        Right t -> t
        Left err -> error $ "Should create tool: " ++ T.unpack err
  
  -- Create agent programmatically with the tool
  let model = createModel "gpt-3.5-turbo" OpenAI
  let agentResult = createAgent 
        "hello_world_agent"
        (Just "A friendly agent that uses the sayHello tool to greet users")
        model
        "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
        [tool]
  
  case agentResult of
    Right agent -> do
      -- Verify agent properties
      view agentName agent @?= "hello_world_agent"
      -- Verify agent has tool
      let tools = view agentTools agent
      length tools @?= 1
      view toolName (head tools) @?= "sayHello"
    Left err -> assertFailure $ "Failed to create agent programmatically: " ++ T.unpack err

-- | Scenario 3: Mix parsed and programmatic elements.
--
-- Given: We have some tools from gram and some created programmatically
-- When: We combine them in an agent
-- Then: The agent works with all tools regardless of origin
testMixParsedAndProgrammatic :: TestTree
testMixParsedAndProgrammatic = testCase "Mix parsed and programmatic elements" $ do
  -- Parse a tool from gram
  let toolGram = T.unlines
        [ "[sayHello:Tool {"
        , "  description: \"Returns a friendly greeting message for the given name\""
        , "} |"
        , "  (personName::Text {default:\"world\"})==>(::String)"
        , "]"
        ]
  
  let parsedTool = case parseTool toolGram of
        Right t -> t
        Left err -> error $ "Should parse tool: " ++ T.unpack err
  
  -- Create another tool programmatically
  let typeSigPattern = case Gram.fromGram "(city::Text)==>(::String)" of
        Right p -> p
        Left _ -> error "Should parse type signature"
  
  let programmaticTool = case createTool "getWeather" "Gets weather for a city" typeSigPattern of
        Right t -> t
        Left err -> error $ "Should create tool: " ++ T.unpack err
  
  -- Create agent with both tools
  let model = createModel "gpt-3.5-turbo" OpenAI
  let agentResult = createAgent
        "mixed_agent"
        (Just "Agent with mixed tools")
        model
        "You are a helpful assistant."
        [parsedTool, programmaticTool]
  
  case agentResult of
    Right agent -> do
      -- Verify agent has both tools
      let tools = view agentTools agent
      length tools @?= 2
      let toolNames = map (view toolName) tools
      "sayHello" `elem` toolNames @?= True
      "getWeather" `elem` toolNames @?= True
    Left err -> assertFailure $ "Failed to create agent with mixed tools: " ++ T.unpack err


tests :: TestTree
tests = testGroup "Agent Creation Scenarios"
  [ testParseAgentFromGram
  , testAssembleAgentInCode
  , testMixParsedAndProgrammatic
  ]

