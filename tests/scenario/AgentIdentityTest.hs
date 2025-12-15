{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for agent identity (User Story 1).
--
-- These tests simulate user goal satisfaction: developers can create
-- agents with identity and identify them uniquely.
module AgentIdentityTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Agent, createAgent, createModel, Model(..), Provider(..), agentName, agentDescription, agentModel, agentInstruction)
import Control.Lens (view)

-- | Scenario: Create agent with name, description, and model
--
-- Given: A developer wants to create an agent
-- When: They specify a name, description, and model
-- Then: They can successfully create an agent instance
testCreateAgentWithIdentity :: TestTree
testCreateAgentWithIdentity = testCase "Create agent with name, description, and model" $ do
  let model = createModel "gpt-4" OpenAI
  let result = createAgent "capital_agent" (Just "Answers questions about capital cities") model "You are an agent that provides capital cities of countries." []
  
  case result of
    Right agent -> do
      -- Verify agent has correct identity
      view agentName agent @?= "capital_agent"
      view agentDescription agent @?= Just "Answers questions about capital cities"
      view agentInstruction agent @?= "You are an agent that provides capital cities of countries."
      let agentModelValue = view agentModel agent
      modelId agentModelValue @?= "gpt-4"
      modelProvider agentModelValue @?= OpenAI
    Left err -> assertFailure $ "Failed to create agent: " ++ show err

-- | Scenario: Verify agent can be uniquely identified by name
--
-- Given: An agent with a name is created
-- When: The system references the agent
-- Then: It can be uniquely identified by its name
testAgentUniquelyIdentified :: TestTree
testAgentUniquelyIdentified = testCase "Agent can be uniquely identified by name" $ do
  let model1 = createModel "gpt-4" OpenAI
  let model2 = createModel "gpt-3.5-turbo" OpenAI
  
  let agent1 = case createAgent "agent_1" (Just "First agent") model1 "You are agent 1." [] of Right a -> a; Left _ -> error "Should not fail"
  let agent2 = case createAgent "agent_2" (Just "Second agent") model2 "You are agent 2." [] of Right a -> a; Left _ -> error "Should not fail"
  
  -- Verify agents have different names
  view agentName agent1 @?= "agent_1"
  view agentName agent2 @?= "agent_2"
  view agentName agent1 /= view agentName agent2 @? "Agents should have different names"

-- | Scenario: Agent uses specified model for reasoning
--
-- Given: An agent with a model is configured
-- When: The agent processes requests
-- Then: It uses the specified model for reasoning
testAgentUsesSpecifiedModel :: TestTree
testAgentUsesSpecifiedModel = testCase "Agent uses specified model" $ do
  let model = createModel "gpt-4" OpenAI
  let agent = case createAgent "test_agent" Nothing model "You are a test agent." [] of Right a -> a; Left _ -> error "Should not fail"
  
  -- Verify model configuration
  let configuredModel = view agentModel agent
  modelId configuredModel @?= "gpt-4"
  modelProvider configuredModel @?= OpenAI

tests :: TestTree
tests = testGroup "Agent Identity Scenario Tests"
  [ testCreateAgentWithIdentity
  , testAgentUniquelyIdentified
  , testAgentUsesSpecifiedModel
  ]
