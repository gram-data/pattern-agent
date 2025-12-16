{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for agent tool association.
--
-- These tests simulate user goal satisfaction end-to-end:
-- - Adding tools to agents
-- - Accessing agent tools
-- - Tool-free agents (purely conversational)
module AgentToolAssociationTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Agent, Tool, createAgent, createTool, createModel, Provider(..), agentName, agentTools, toolName)
import Control.Lens (view)
import qualified Data.Text as T
import Pattern (Pattern)
import Subject.Core (Subject)
import qualified Gram

type PatternSubject = Pattern Subject

-- | Helper: Parse type signature string to Pattern Subject element.
parseTypeSig :: String -> PatternSubject
parseTypeSig sig = case Gram.fromGram sig of
  Right p -> p
  Left _ -> error $ "Failed to parse type signature: " ++ sig

-- | Scenario test: Add tool to agent and verify agent can access it.
testAddToolToAgent :: TestTree
testAddToolToAgent = testCase "Add tool to agent and verify access" $ do
  -- Create a tool
  let typeSig = parseTypeSig "(name::String)==>(::String)"
  let toolResult = createTool "sayHello" "Greeting tool" typeSig
  case toolResult of
    Right tool -> do
      -- Create agent with the tool
      let model = createModel "gpt-3.5-turbo" OpenAI
      let agentResult = createAgent "test_agent" (Just "Test agent") model "You are a helpful assistant." [tool]
      
      case agentResult of
        Right agent -> do
          -- Verify agent has the tool
          let tools = view agentTools agent
          length tools @?= 1
          view toolName (head tools) @?= "sayHello"
        Left err -> assertFailure $ "Failed to create agent with tool: " ++ T.unpack err
    Left err -> assertFailure $ "Failed to create tool: " ++ T.unpack err

-- | Scenario test: Add multiple tools to agent and verify all tools are accessible.
testAddMultipleToolsToAgent :: TestTree
testAddMultipleToolsToAgent = testCase "Add multiple tools to agent" $ do
  -- Create multiple tools
  let typeSig1 = parseTypeSig "(name::String)==>(::String)"
  let typeSig2 = parseTypeSig "(city::String)==>(::String)"
  let tool1Result = createTool "sayHello" "Greeting tool" typeSig1
  let tool2Result = createTool "getWeather" "Weather tool" typeSig2
  
  case (tool1Result, tool2Result) of
    (Right tool1, Right tool2) -> do
      -- Create agent with multiple tools
      let model = createModel "gpt-3.5-turbo" OpenAI
      let agentResult = createAgent "multi_tool_agent" (Just "Agent with multiple tools") model "You are a helpful assistant." [tool1, tool2]
      
      case agentResult of
        Right agent -> do
          -- Verify agent has both tools
          let tools = view agentTools agent
          length tools @?= 2
          let toolNames = map (view toolName) tools
          "sayHello" `elem` toolNames @?= True
          "getWeather" `elem` toolNames @?= True
        Left err -> assertFailure $ "Failed to create agent with tools: " ++ T.unpack err
    _ -> assertFailure "Failed to create tools"

-- | Scenario test: Purely conversational agent with no tools.
testConversationalAgentNoTools :: TestTree
testConversationalAgentNoTools = testCase "Create purely conversational agent with no tools" $ do
  let model = createModel "gpt-3.5-turbo" OpenAI
  let agentResult = createAgent 
        "conversational_agent" 
        (Just "A friendly conversational assistant")
        model
        "You are a helpful assistant. Have friendly conversations with users."
        []  -- Empty tools list
  
  case agentResult of
    Right agent -> do
      -- Verify agent has no tools
      let tools = view agentTools agent
      length tools @?= 0
      -- Verify agent properties are correct
      view agentName agent @?= "conversational_agent"
    Left err -> assertFailure $ "Failed to create tool-free agent: " ++ T.unpack err

-- | Scenario test: Agent with one tool (hello world scenario).
testAgentWithOneTool :: TestTree
testAgentWithOneTool = testCase "Create agent with one tool (hello world)" $ do
  -- Create sayHello tool
  let typeSig = parseTypeSig "(personName::String {default:\"world\"})==>(::String)"
  let toolResult = createTool 
        "sayHello" 
        "Returns a friendly greeting message for the given name"
        typeSig
  
  case toolResult of
    Right tool -> do
      -- Create hello world agent with the tool
      let model = createModel "gpt-3.5-turbo" OpenAI
      let agentResult = createAgent
            "hello_world_agent"
            (Just "A friendly agent that uses the sayHello tool to greet users")
            model
            "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
            [tool]  -- Single tool
      
      case agentResult of
        Right agent -> do
          -- Verify agent has exactly one tool
          let tools = view agentTools agent
          length tools @?= 1
          view toolName (head tools) @?= "sayHello"
          view agentName agent @?= "hello_world_agent"
        Left err -> assertFailure $ "Failed to create hello world agent: " ++ T.unpack err
    Left err -> assertFailure $ "Failed to create sayHello tool: " ++ T.unpack err

-- | Scenario test: Verify agent can see its available tools during request processing.
-- This is a placeholder for future execution tests - for now just verifies tools are accessible.
testAgentSeesToolsDuringProcessing :: TestTree
testAgentSeesToolsDuringProcessing = testCase "Agent can access tools during processing" $ do
  let typeSig = parseTypeSig "(x::String)==>(::String)"
  let toolResult = createTool "testTool" "Test tool" typeSig
  case toolResult of
    Right tool -> do
      let model = createModel "gpt-3.5-turbo" OpenAI
      let agentResult = createAgent "test_agent" Nothing model "Test instructions" [tool]
      case agentResult of
        Right agent -> do
          -- Verify tools are accessible via lens
          let tools = view agentTools agent
          length tools @?= 1
          -- Tools should be accessible for execution processing
          return ()  -- Success
        Left err -> assertFailure $ "Failed to create agent: " ++ T.unpack err
    Left err -> assertFailure $ "Failed to create tool: " ++ T.unpack err

tests :: TestTree
tests = testGroup "Agent Tool Association Scenario Tests"
  [ testAddToolToAgent
  , testAddMultipleToolsToAgent
  , testConversationalAgentNoTools
  , testAgentWithOneTool
  , testAgentSeesToolsDuringProcessing
  ]

