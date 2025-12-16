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
import PatternAgent.Language.Core (Agent, Tool, agentName, agentTools, toolName)
import PatternAgent.Runtime.ToolLibrary (ToolLibrary, lookupTool, toolImplName, toolImplDescription)
import Control.Lens (view)

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
  
  -- TODO: Execute agent with greeting and verify tool is used
  -- This will require executeAgentWithLibrary to be fully implemented
  -- For now, verify the setup is correct
  return ()

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
      -- TODO: Execute agent and verify tool is invoked with personName parameter
      -- This will require executeAgentWithLibrary to be fully implemented
      return ()
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
  
  -- TODO: Execute agent with greeting, verify tool result is in response
  -- This will require executeAgentWithLibrary to be fully implemented
  -- Expected: responseContent should include greeting from sayHello tool
  return ()

-- | Scenario test: Hello world agent responds conversationally without tool for non-greeting messages.
--
-- Given: Hello world agent is in a conversation
-- When: User sends non-greeting messages
-- Then: Agent responds conversationally without necessarily using the tool
testAgentRespondsWithoutTool :: TestTree
testAgentRespondsWithoutTool = testCase "Agent responds conversationally without tool" $ do
  -- Verify agent setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- TODO: Execute agent with non-greeting message, verify no tool is used
  -- This will require executeAgentWithLibrary to be fully implemented
  -- Expected: responseToolsUsed should be empty for non-greeting messages
  return ()

tests :: TestTree
tests = testGroup "Hello World Agent Scenario Tests"
  [ testHelloWorldAgentUsesSayHelloTool
  , testSayHelloToolInvokedWithParameters
  , testAgentIncorporatesToolResult
  , testAgentRespondsWithoutTool
  ]

