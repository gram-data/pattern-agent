{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for Agent creation and configuration.
module AgentTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Agent, Tool, createAgent, createTool, createModel, Model(..), Provider(..), agentName, agentDescription, agentModel, agentInstruction, agentTools, toolName)
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

testAgentCreation :: TestTree
testAgentCreation = testGroup "Agent Creation"
  [ testCase "Create agent with name, description, and model" $ do
      let model = createModel "gpt-4" OpenAI
      let result = createAgent "test_agent" (Just "Test agent description") model "You are a helpful assistant." []
      case result of
        Right agent -> do
          view agentName agent @?= "test_agent"
          view agentDescription agent @?= Just "Test agent description"
          view agentModel agent @?= model
          view agentInstruction agent @?= "You are a helpful assistant."
        Left err -> assertFailure $ "Expected Right Agent, got Left: " ++ T.unpack err

  , testCase "Agent name cannot be empty" $ do
      let model = createModel "gpt-4" OpenAI
      let result = createAgent "" Nothing model "Some instruction" []
      case result of
        Left err -> T.isInfixOf "empty" err @?= True
        Right _ -> assertFailure "Expected Left error for empty name"

  , testCase "Agent name uniqueness validation" $ do
      -- Note: Name uniqueness is runtime-checked, not type-checked
      -- This test verifies that agents can be created with same name
      -- (uniqueness enforcement would be at a higher level)
      let model = createModel "gpt-4" OpenAI
      let agent1 = createAgent "duplicate_name" Nothing model "Instruction 1" []
      let agent2 = createAgent "duplicate_name" Nothing model "Instruction 2" []
      case (agent1, agent2) of
        (Right a1, Right a2) -> view agentName a1 @?= view agentName a2
        _ -> assertFailure "Both agents should be created successfully"
  ]

testModelConfiguration :: TestTree
testModelConfiguration = testGroup "Model Configuration"
  [ testCase "Create model with provider" $ do
      let model = createModel "gpt-4" OpenAI
      modelId model @?= "gpt-4"
      modelProvider model @?= OpenAI

  , testCase "Model accessors work correctly" $ do
      let model = createModel "gpt-3.5-turbo" OpenAI
      case createAgent "test" Nothing model "Test instruction" [] of
        Right agent -> view agentModel agent @?= model
        Left _ -> assertFailure "Should create agent successfully"
  ]

testAgentAccessors :: TestTree
testAgentAccessors = testGroup "Agent Accessors"
  [ testCase "agentName accessor" $ do
      let model = createModel "gpt-4" OpenAI
      case createAgent "my_agent" Nothing model "Test instruction" [] of
        Right agent -> view agentName agent @?= "my_agent"
        Left err -> assertFailure $ "Should create agent: " ++ T.unpack err

  , testCase "agentDescription accessor" $ do
      let model = createModel "gpt-4" OpenAI
      case createAgent "my_agent" (Just "Description") model "Test instruction" [] of
        Right agent -> view agentDescription agent @?= Just "Description"
        Left err -> assertFailure $ "Should create agent: " ++ T.unpack err

  , testCase "agentModel accessor" $ do
      let model = createModel "gpt-4" OpenAI
      case createAgent "my_agent" Nothing model "Test instruction" [] of
        Right agent -> view agentModel agent @?= model
        Left err -> assertFailure $ "Should create agent: " ++ T.unpack err
  ]

testToolAssociation :: TestTree
testToolAssociation = testGroup "Tool Association"
  [ testCase "Associate tool with agent" $ do
      let model = createModel "gpt-4" OpenAI
      let typeSig = parseTypeSig "(x::Text)==>(::String)"
      let toolResult = createTool "testTool" "Test tool" typeSig
      case toolResult of
        Right tool -> do
          let agentResult = createAgent "test_agent" Nothing model "Test instruction" [tool]
          case agentResult of
            Right agent -> do
              let tools = view agentTools agent
              length tools @?= 1
              view toolName (head tools) @?= "testTool"
            Left err -> assertFailure $ "Failed to create agent with tool: " ++ T.unpack err
        Left err -> assertFailure $ "Failed to create tool: " ++ T.unpack err
  
  , testCase "Retrieve tools from agent" $ do
      let model = createModel "gpt-4" OpenAI
      let typeSig1 = parseTypeSig "(x::Text)==>(::String)"
      let typeSig2 = parseTypeSig "(y::Int)==>(::String)"
      let tool1Result = createTool "tool1" "Tool 1" typeSig1
      let tool2Result = createTool "tool2" "Tool 2" typeSig2
      case (tool1Result, tool2Result) of
        (Right tool1, Right tool2) -> do
          let agentResult = createAgent "test_agent" Nothing model "Test instruction" [tool1, tool2]
          case agentResult of
            Right agent -> do
              let tools = view agentTools agent
              length tools @?= 2
              let toolNames = map (view toolName) tools
              "tool1" `elem` toolNames @?= True
              "tool2" `elem` toolNames @?= True
            Left err -> assertFailure $ "Failed to create agent: " ++ T.unpack err
        _ -> assertFailure "Failed to create tools"
  
  , testCase "Tool name uniqueness validation" $ do
      let model = createModel "gpt-4" OpenAI
      let typeSig1 = parseTypeSig "(x::Text)==>(::String)"
      let typeSig2 = parseTypeSig "(y::Int)==>(::String)"
      let tool1Result = createTool "duplicate" "Tool 1" typeSig1
      let tool2Result = createTool "duplicate" "Tool 2" typeSig2
      case (tool1Result, tool2Result) of
        (Right tool1, Right tool2) -> do
          -- Should fail validation when adding duplicate tool names
          let agentResult = createAgent "test_agent" Nothing model "Test instruction" [tool1, tool2]
          case agentResult of
            Left err -> T.isInfixOf "unique" err @?= True  -- Should reject duplicate names
            Right _ -> assertFailure "Should reject agents with duplicate tool names"
        _ -> assertFailure "Failed to create tools"
  
  , testCase "Agent with zero tools (backward compatibility)" $ do
      let model = createModel "gpt-4" OpenAI
      let agentResult = createAgent "conversational_agent" Nothing model "You are a helpful assistant." []
      case agentResult of
        Right agent -> do
          let tools = view agentTools agent
          length tools @?= 0  -- Should have empty tools list
          view agentName agent @?= "conversational_agent"
        Left err -> assertFailure $ "Failed to create tool-free agent: " ++ T.unpack err
  ]

tests :: TestTree
tests = testGroup "Agent Tests"
  [ testAgentCreation
  , testModelConfiguration
  , testAgentAccessors
  , testToolAssociation
  ]
