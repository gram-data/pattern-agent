{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for Agent creation and configuration.
module AgentTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core

testAgentCreation :: TestTree
testAgentCreation = testGroup "Agent Creation"
  [ testCase "Create agent with name, description, and model" $ do
      let model = createModel "gpt-4" OpenAI
      let result = createAgent "test_agent" model "You are a helpful assistant." (Just "Test agent description")
      case result of
        Right agent -> do
          agentName agent @?= "test_agent"
          agentDescription agent @?= Just "Test agent description"
          agentModel agent @?= model
          agentInstruction agent @?= "You are a helpful assistant."
        Left err -> assertFailure $ "Expected Right Agent, got Left: " ++ show err

  , testCase "Agent name cannot be empty" $ do
      let model = createModel "gpt-4" OpenAI
      let result = createAgent "" model "Some instruction" Nothing
      case result of
        Left err -> err @?= "Agent name cannot be empty"
        Right _ -> assertFailure "Expected Left error for empty name"

  , testCase "Agent name uniqueness validation" $ do
      -- Note: Name uniqueness is runtime-checked, not type-checked
      -- This test verifies that agents can be created with same name
      -- (uniqueness enforcement would be at a higher level)
      let model = createModel "gpt-4" OpenAI
      let agent1 = createAgent "duplicate_name" model "Instruction 1" Nothing
      let agent2 = createAgent "duplicate_name" model "Instruction 2" Nothing
      case (agent1, agent2) of
        (Right a1, Right a2) -> agentName a1 @?= agentName a2
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
      agentModel (case createAgent "test" model "Test instruction" Nothing of Right a -> a; Left _ -> error "Should not fail") @?= model
  ]

testAgentAccessors :: TestTree
testAgentAccessors = testGroup "Agent Accessors"
  [ testCase "agentName accessor" $ do
      let model = createModel "gpt-4" OpenAI
      let agent = case createAgent "my_agent" model "Test instruction" Nothing of Right a -> a; Left _ -> error "Should not fail"
      agentName agent @?= "my_agent"

  , testCase "agentDescription accessor" $ do
      let model = createModel "gpt-4" OpenAI
      let agent = case createAgent "my_agent" model "Test instruction" (Just "Description") of Right a -> a; Left _ -> error "Should not fail"
      agentDescription agent @?= Just "Description"

  , testCase "agentModel accessor" $ do
      let model = createModel "gpt-4" OpenAI
      let agent = case createAgent "my_agent" model "Test instruction" Nothing of Right a -> a; Left _ -> error "Should not fail"
      agentModel agent @?= model
  ]

tests :: TestTree
tests = testGroup "Agent Tests"
  [ testAgentCreation
  , testModelConfiguration
  , testAgentAccessors
  ]
