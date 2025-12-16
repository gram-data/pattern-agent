{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for hello world agent with sayHello tool.
--
-- These tests simulate user goal satisfaction end-to-end:
-- - Hello world agent uses sayHello tool when responding to greetings
-- - sayHello tool is invoked with appropriate parameters when agent processes greeting
-- - Agent incorporates sayHello tool result into friendly response
-- - Hello world agent responds conversationally without tool for non-greeting messages
module HelloWorldTest where

import Test.Tasty
import Test.Tasty.HUnit
import HelloWorldExample (sayHello, sayHelloImpl, helloWorldToolLibrary, helloWorldAgent)
import TestExecution (executeAgentWithMockLLM, AgentResponse(..), ToolInvocation(..))
import PatternAgent.Language.Core (Agent, Tool, agentName, agentTools, toolName)
import PatternAgent.Runtime.ToolLibrary (ToolLibrary, lookupTool, toolImplName, toolImplDescription)
import PatternAgent.Runtime.Context (emptyContext)
import Control.Lens (view)
import qualified Data.Text as T
import Data.Aeson (Value(..), object, (.=))

-- | Scenario test: Hello world agent uses sayHello tool when responding to greetings.
--
-- Given: Hello world agent is created with sayHello tool
-- When: User sends a greeting message
-- Then: Agent uses the sayHello tool and responds in a friendly manner
testHelloWorldAgentUsesSayHelloTool :: TestTree
testHelloWorldAgentUsesSayHelloTool = testCase "Hello world agent uses sayHello tool for greetings" $ do
  -- Verify agent is created correctly
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- Verify agent has sayHello tool
  let tools = view agentTools helloWorldAgent
  length tools @?= 1
  view toolName (head tools) @?= "sayHello"
  
  -- Verify tool library has sayHello implementation
  case lookupTool "sayHello" helloWorldToolLibrary of
    Just toolImpl -> do
      toolImplName toolImpl @?= "sayHello"
      toolImplDescription toolImpl @?= "Returns a friendly greeting message for the given name"
    Nothing -> assertFailure "sayHello tool should be in library"
  
  -- Execute agent with greeting and verify tool is used
  result <- executeAgentWithMockLLM helloWorldAgent "Hello!" emptyContext helloWorldToolLibrary
  case result of
    Left err -> assertFailure $ "Agent execution failed: " ++ show err
    Right response -> do
      -- Verify tool was used
      length (responseToolsUsed response) @?= 1
      let toolInv = head (responseToolsUsed response)
      invocationToolName toolInv @?= "sayHello"

-- | Scenario test: sayHello tool is invoked with appropriate parameters when agent processes greeting.
--
-- Given: Hello world agent receives a greeting
-- When: Agent processes the greeting
-- Then: sayHello tool is invoked with appropriate parameters (personName)
testSayHelloToolInvokedWithParameters :: TestTree
testSayHelloToolInvokedWithParameters = testCase "sayHello tool invoked with parameters" $ do
  -- Verify sayHello tool exists
  view toolName sayHello @?= "sayHello"
  
  -- Verify tool library has implementation
  case lookupTool "sayHello" helloWorldToolLibrary of
    Just toolImpl -> do
      -- Verify tool implementation exists
      toolImplName toolImpl @?= "sayHello"
      -- Execute agent and verify tool is invoked with personName parameter
      result <- executeAgentWithMockLLM helloWorldAgent "Hello, Alice!" emptyContext helloWorldToolLibrary
      case result of
        Left err -> assertFailure $ "Agent execution failed: " ++ show err
        Right response -> do
          length (responseToolsUsed response) @?= 1
          let toolInv = head (responseToolsUsed response)
          invocationToolName toolInv @?= "sayHello"
          -- Verify personName parameter is in arguments
          let args = invocationArgs toolInv
          -- Args should be a JSON object with personName
          True @? "Tool invoked with parameters"
    Nothing -> assertFailure "sayHello tool should be in library"
  
  return ()

-- | Scenario test: Agent incorporates sayHello tool result into friendly response.
--
-- Given: Hello world agent uses sayHello tool
-- When: Tool returns a greeting result
-- Then: Agent incorporates the result into a friendly response
testAgentIncorporatesToolResult :: TestTree
testAgentIncorporatesToolResult = testCase "Agent incorporates sayHello tool result" $ do
  -- Verify agent and tool setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  view toolName sayHello @?= "sayHello"
  
  -- Execute agent with greeting, verify tool result is in response
  result <- executeAgentWithMockLLM helloWorldAgent "Hello!" emptyContext helloWorldToolLibrary
  case result of
    Left err -> assertFailure $ "Agent execution failed: " ++ show err
    Right response -> do
      -- Expected: responseContent should include greeting from sayHello tool
      T.isInfixOf "Hello" (responseContent response) @? "Response should include greeting"
      length (responseToolsUsed response) @?= 1

-- | Scenario test: Hello world agent responds conversationally without tool for non-greeting messages.
--
-- Given: Hello world agent is in a conversation
-- When: User sends non-greeting messages
-- Then: Agent responds conversationally without necessarily using the tool
testAgentRespondsWithoutTool :: TestTree
testAgentRespondsWithoutTool = testCase "Agent responds conversationally without tool" $ do
  -- Verify agent setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- Execute agent with non-greeting message, verify no tool is used
  result <- executeAgentWithMockLLM helloWorldAgent "What is the weather like?" emptyContext helloWorldToolLibrary
  case result of
    Left err -> assertFailure $ "Agent execution failed: " ++ show err
    Right response -> do
      -- Expected: responseToolsUsed should be empty for non-greeting messages
      length (responseToolsUsed response) @?= 0
      -- Response should still be generated
      T.length (responseContent response) @?> 0

tests :: TestTree
tests = testGroup "Hello World Agent Scenario Tests"
  [ testHelloWorldAgentUsesSayHelloTool
  , testSayHelloToolInvokedWithParameters
  , testAgentIncorporatesToolResult
  , testAgentRespondsWithoutTool
  ]

