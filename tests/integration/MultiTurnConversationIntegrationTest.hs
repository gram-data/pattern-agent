{-# LANGUAGE OverloadedStrings #-}
-- | Integration tests for multi-turn conversation with tool execution.
--
-- These tests use a REAL LLM API to observe actual behavior.
-- They should be run with a valid API key set in the environment.
--
-- To run these tests:
--   export OPENAI_API_KEY=your_key_here
--   cabal test --test-option="--pattern=Integration"
--
-- These tests observe real LLM behavior so we can later create a mock
-- that accurately simulates the observed behavior.
module MultiTurnConversationIntegrationTest where

import Test.Tasty
import Test.Tasty.HUnit
import HelloWorldExample (helloWorldAgent, helloWorldToolLibrary)
import PatternAgent.Runtime.Execution (executeAgentWithLibrary, AgentError(..), AgentResponse(..), ToolInvocation(..))
import PatternAgent.Runtime.Context (emptyContext, ConversationContext, MessageRole(..), addMessage)
import PatternAgent.Language.Core (agentName)
import Control.Lens (view)
import qualified Data.Text as T
import Data.Aeson (Value(..), object, (.=), (.:?), decode, encode)
import Data.Aeson.Types (parseMaybe, withObject)
import Data.Maybe (fromMaybe)

-- | Extract personName from tool invocation arguments.
extractPersonName :: Value -> String
extractPersonName args = case parseMaybe (withObject "args" $ \obj -> obj .:? "personName") args of
  Just (Just (String name)) -> T.unpack name
  _ -> "NOT_FOUND"

-- | Integration test: Agent remembers user name from conversation history.
--
-- This test uses a REAL LLM API to observe how the agent behaves in a
-- multi-turn conversation where the user introduces themselves in the first
-- message, then greets later without re-introducing their name.
--
-- Scenario:
-- 1. User says "My name is Bob" (first message - introduces name)
-- 2. User says "the weather is nice today" (second message - smalltalk, no greeting)
-- 3. User says "oh, hello btw" (third message - greeting without re-introducing name)
--
-- Expected behavior (to be observed):
-- - Agent should remember "Bob" from the first message
-- - When user says "oh, hello btw" in the third message, agent should use sayHello tool
-- - The sayHello tool should be called with personName="Bob" (extracted from conversation history)
-- - Agent's response should include a personalized greeting using Bob's name
--
-- This test helps us understand:
-- - How the LLM extracts information from conversation history
-- - How the LLM decides when to use tools based on context
-- - What the actual tool call format looks like
-- - How the agent incorporates tool results into responses
testAgentRemembersUserInfoFromHistory :: TestTree
testAgentRemembersUserInfoFromHistory = testCase "Integration: Agent remembers user name from conversation history" $ do
  -- Verify agent setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- Turn 1: User says "My name is Bob"
  let context1 = emptyContext
  result1 <- executeAgentWithLibrary False helloWorldAgent "My name is Bob" context1 helloWorldToolLibrary
  case result1 of
    Left err -> assertFailure $ "Turn 1 failed: " ++ show err
    Right response1 -> do
      -- Verify agent responded (may or may not use tool)
      T.null (responseContent response1) @?= False
      -- Track context for next turn
      let context2 = case addMessage UserRole "My name is Bob" context1 of
            Right ctx -> case addMessage AssistantRole (responseContent response1) ctx of
              Right ctx' -> ctx'
              Left err -> error $ "Failed to add assistant message: " ++ T.unpack err
            Left err -> error $ "Failed to add user message: " ++ T.unpack err
      
      -- Turn 2: User says "the weather is nice today"
      result2 <- executeAgentWithLibrary False helloWorldAgent "the weather is nice today" context2 helloWorldToolLibrary
      case result2 of
        Left err -> assertFailure $ "Turn 2 failed: " ++ show err
        Right response2 -> do
          -- Verify agent responded to smalltalk
          T.null (responseContent response2) @?= False
          -- Track context for next turn
          let context3 = case addMessage UserRole "the weather is nice today" context2 of
                Right ctx -> case addMessage AssistantRole (responseContent response2) ctx of
                  Right ctx' -> ctx'
                  Left err -> error $ "Failed to add assistant message: " ++ T.unpack err
                Left err -> error $ "Failed to add user message: " ++ T.unpack err
          
          -- Turn 3: User says "oh, hello btw"
          result3 <- executeAgentWithLibrary False helloWorldAgent "oh, hello btw" context3 helloWorldToolLibrary
          case result3 of
            Left err -> assertFailure $ "Turn 3 failed: " ++ show err
            Right response3 -> do
              -- Verify agent responded
              T.null (responseContent response3) @?= False
              
              -- OBSERVE: Did the agent use the sayHello tool?
              let toolsUsed = responseToolsUsed response3
              length toolsUsed >= 0 @?= True  -- May or may not use tool
              
              -- OBSERVE: If tool was used, what parameters were passed?
              case toolsUsed of
                [] -> do
                  -- Agent didn't use tool - observe the response anyway
                  -- This helps us understand when the LLM decides NOT to use tools
                  putStrLn $ "Observation: Agent did not use sayHello tool. Response: " ++ T.unpack (responseContent response3)
                (invocation:_) -> do
                  -- Agent used tool - observe the behavior
                  invocationToolName invocation @?= "sayHello"
                  
                  -- OBSERVE: What arguments were passed to sayHello?
                  let args = invocationArgs invocation
                  let personName = extractPersonName args
                  putStrLn $ "Observation: sayHello called with personName=" ++ personName
                  
                  -- OBSERVE: Did the agent extract "Bob" from conversation history?
                  if personName == "Bob"
                    then putStrLn "Observation: ✓ Agent successfully extracted 'Bob' from conversation history"
                    else putStrLn $ "Observation: Agent used personName='" ++ personName ++ "' (expected 'Bob')"
                  
                  -- OBSERVE: What was the tool result?
                  case invocationResult invocation of
                    Right result -> putStrLn $ "Observation: Tool result: " ++ show result
                    Left err -> putStrLn $ "Observation: Tool error: " ++ T.unpack err
                  
                  -- OBSERVE: How did the agent incorporate the tool result into the response?
                  putStrLn $ "Observation: Final response: " ++ T.unpack (responseContent response3)

-- | Integration test: Simple greeting with tool call.
--
-- This test observes how the agent behaves when given a direct greeting.
-- It helps us understand the basic tool call flow.
testSimpleGreetingWithTool :: TestTree
testSimpleGreetingWithTool = testCase "Integration: Simple greeting with tool call" $ do
  -- Verify agent setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- Execute with a simple greeting
  result <- executeAgentWithLibrary False helloWorldAgent "Hello!" emptyContext helloWorldToolLibrary
  case result of
    Left err -> assertFailure $ "Execution failed: " ++ show err
    Right response -> do
      -- Verify agent responded
      T.null (responseContent response) @?= False
      
      -- OBSERVE: Did the agent use the sayHello tool?
      let toolsUsed = responseToolsUsed response
      case toolsUsed of
        [] -> do
          putStrLn "Observation: Agent did not use sayHello tool for direct greeting"
          putStrLn $ "Response: " ++ T.unpack (responseContent response)
        (invocation:_) -> do
          -- OBSERVE: Tool was used - observe the behavior
          invocationToolName invocation @?= "sayHello"
          
          -- OBSERVE: What arguments were passed?
          let args = invocationArgs invocation
          let personName = extractPersonName args
          putStrLn $ "Observation: sayHello called with personName=" ++ personName
          
          -- OBSERVE: Tool result
          case invocationResult invocation of
            Right result -> putStrLn $ "Observation: Tool result: " ++ show result
            Left err -> putStrLn $ "Observation: Tool error: " ++ T.unpack err
          
          -- OBSERVE: Final response
          putStrLn $ "Observation: Final response: " ++ T.unpack (responseContent response)

tests :: TestTree
tests = testGroup "Multi-Turn Conversation Integration Tests"
  [ testAgentRemembersUserInfoFromHistory
  , testSimpleGreetingWithTool
  ]

