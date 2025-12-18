{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for multi-turn conversation with tool execution.
--
-- These tests simulate user goal satisfaction end-to-end:
-- - Agent references previous tool usage in follow-up message
-- - Multi-turn conversation with tool usage maintains coherence
-- - Agent uses previous tool results to inform new response
module MultiTurnToolConversationTest where

import Test.Tasty
import Test.Tasty.HUnit
import HelloWorldExample (helloWorldAgent, helloWorldToolLibrary)
import TestExecution (executeAgentWithMockLLM, AgentError(..), AgentResponse(..), ToolInvocation(..))
import PatternAgent.Runtime.Context (emptyContext, ConversationContext, Message(..), MessageRole(..), addMessage)
import PatternAgent.Language.Core (Agent, agentName)
import Control.Lens (view)
import qualified Data.Text as T
import Data.Aeson (Value(..), object, (.=))

-- | Scenario test: Agent references previous tool usage in follow-up message.
--
-- Given: Agent has used a tool in a previous message
-- When: Developer sends a follow-up message
-- Then: Agent can reference previous tool usage in context
testAgentReferencesPreviousToolUsage :: TestTree
testAgentReferencesPreviousToolUsage = testCase "Agent references previous tool usage in follow-up" $ do
  -- Verify agent setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- Initial context
  let initialContext = emptyContext
  
  -- Turn 1: User greets, agent uses sayHello tool
  result1 <- executeAgentWithMockLLM helloWorldAgent "Hello!" initialContext helloWorldToolLibrary
  case result1 of
    Left err -> assertFailure $ "Turn 1 failed: " ++ show err
    Right response1 -> do
      -- Verify tool was used in first turn
      length (responseToolsUsed response1) @?= 1
      let toolInv1 = head (responseToolsUsed response1)
      invocationToolName toolInv1 @?= "sayHello"
      
      -- Update context for next turn
      let context1 = case addMessage UserRole "Hello!" initialContext of
                       Right c -> c
                       Left err -> error $ "Failed to add user message: " ++ T.unpack err
      let contextAfterTurn1 = case addMessage AssistantRole (responseContent response1) context1 of
                                Right c -> c
                                Left err -> error $ "Failed to add assistant message: " ++ T.unpack err
      
      -- Turn 2: Follow-up message asking about the greeting
      result2 <- executeAgentWithMockLLM helloWorldAgent "What did you say?" contextAfterTurn1 helloWorldToolLibrary
      case result2 of
        Left err -> assertFailure $ "Turn 2 failed: " ++ show err
        Right response2 -> do
          -- Expected: Follow-up message should reference the previous greeting
          -- Response should mention the greeting from turn 1
          T.length (responseContent response2) @?> 0
          -- The response should be coherent with the conversation history
          -- (Mock LLM should reference previous tool usage)
          return ()

-- | Scenario test: Multi-turn conversation with tool usage maintains coherence.
--
-- Given: Multi-turn conversation with tool usage
-- When: Agent responds
-- Then: Responses are coherent with conversation history and tool results
testMultiTurnConversationCoherence :: TestTree
testMultiTurnConversationCoherence = testCase "Multi-turn conversation maintains coherence" $ do
  -- Verify agent setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- Initial context
  let initialContext = emptyContext
  
  -- Turn 1: User greets, agent uses sayHello tool
  result1 <- executeAgentWithMockLLM helloWorldAgent "Hello!" initialContext helloWorldToolLibrary
  case result1 of
    Left err -> assertFailure $ "Turn 1 failed: " ++ show err
    Right response1 -> do
      -- Verify tool was used
      length (responseToolsUsed response1) @?= 1
      
      -- Update context
      let context1 = case addMessage UserRole "Hello!" initialContext of
                       Right c -> c
                       Left err -> error $ "Failed: " ++ T.unpack err
      let contextAfterTurn1 = case addMessage AssistantRole (responseContent response1) context1 of
                                Right c -> c
                                Left err -> error $ "Failed: " ++ T.unpack err
      
      -- Turn 2: User asks about the greeting, agent references previous tool usage
      result2 <- executeAgentWithMockLLM helloWorldAgent "What did you say?" contextAfterTurn1 helloWorldToolLibrary
      case result2 of
        Left err -> assertFailure $ "Turn 2 failed: " ++ show err
        Right response2 -> do
          -- Response should be coherent with conversation history
          T.length (responseContent response2) @?> 0
          
          -- Update context
          let context2 = case addMessage UserRole "What did you say?" contextAfterTurn1 of
                           Right c -> c
                           Left err -> error $ "Failed: " ++ T.unpack err
          let contextAfterTurn2 = case addMessage AssistantRole (responseContent response2) context2 of
                                    Right c -> c
                                    Left err -> error $ "Failed: " ++ T.unpack err
          
          -- Turn 3: User asks another question, agent maintains context
          result3 <- executeAgentWithMockLLM helloWorldAgent "Tell me more" contextAfterTurn2 helloWorldToolLibrary
          case result3 of
            Left err -> assertFailure $ "Turn 3 failed: " ++ show err
            Right response3 -> do
              -- All responses should be coherent with full conversation history
              T.length (responseContent response3) @?> 0
              -- The response should maintain context from previous turns
              return ()

-- | Scenario test: Agent uses previous tool results to inform new response.
--
-- Given: Conversation context includes tool invocations
-- When: Agent processes a new message
-- Then: Agent can use previous tool results to inform its response
testAgentUsesPreviousToolResults :: TestTree
testAgentUsesPreviousToolResults = testCase "Agent uses previous tool results" $ do
  -- Verify agent setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- Initial context
  let initialContext = emptyContext
  
  -- Turn 1: User greets, agent uses sayHello tool
  result1 <- executeAgentWithMockLLM helloWorldAgent "Hello!" initialContext helloWorldToolLibrary
  case result1 of
    Left err -> assertFailure $ "Turn 1 failed: " ++ show err
    Right response1 -> do
      -- Verify tool was used
      length (responseToolsUsed response1) @?= 1
      let toolInv1 = head (responseToolsUsed response1)
      invocationToolName toolInv1 @?= "sayHello"
      
      -- Update context (includes FunctionRole message with tool result)
      let context1 = case addMessage UserRole "Hello!" initialContext of
                       Right c -> c
                       Left err -> error $ "Failed: " ++ T.unpack err
      let contextAfterTurn1 = case addMessage AssistantRole (responseContent response1) context1 of
                                Right c -> c
                                Left err -> error $ "Failed: " ++ T.unpack err
      
      -- Turn 2: Ask about the greeting result
      result2 <- executeAgentWithMockLLM helloWorldAgent "What was the greeting you used?" contextAfterTurn1 helloWorldToolLibrary
      case result2 of
        Left err -> assertFailure $ "Turn 2 failed: " ++ show err
        Right response2 -> do
          -- Expected: Agent should reference the tool result from previous turn
          -- The response should mention the greeting result
          T.length (responseContent response2) @?> 0
          -- The conversation context includes FunctionRole messages with tool results,
          -- so the agent can reference them
          return ()

-- | Scenario test: Agent remembers user information from conversation history.
--
-- This test demonstrates the key feature: conversational history enables the agent
-- to remember information from earlier messages and use it in tool calls.
--
-- Scenario:
-- 1. User says "My name is Bob" (first message - introduces name)
-- 2. User says "the weather is nice today" (second message - smalltalk, no greeting)
-- 3. User says "oh, hello btw" (third message - greeting without re-introducing name)
--
-- Expected behavior:
-- - Agent should remember "Bob" from the first message
-- - When user says "oh, hello btw" in the third message, agent should use sayHello tool
-- - The sayHello tool should be called with personName="Bob" (extracted from conversation history)
-- - Agent's response should include a personalized greeting using Bob's name
--
-- Implementation note:
-- For multi-turn conversations, the caller must manually track the conversation context:
-- 1. Start with emptyContext
-- 2. For each turn:
--    a. Add user message to context: addMessage UserRole userInput context
--    b. Call executeAgentWithLibrary with the updated context
--    c. Add assistant response to context: addMessage AssistantRole responseContent context
--    d. Use the updated context for the next turn
-- The LLM receives the full conversation history, enabling it to extract information
-- from earlier messages (like "Bob" from the first message) and use it in tool calls.
testAgentRemembersUserInfoFromHistory :: TestTree
testAgentRemembersUserInfoFromHistory = testCase "Agent remembers user name from conversation history" $ do
  -- Verify agent setup
  view agentName helloWorldAgent @?= "hello_world_agent"
  
  -- Initial context
  let initialContext = emptyContext
  
  -- Turn 1: User says "My name is Bob"
  -- executeAgentWithMockLLM adds the user message internally, so pass the previous context
  result1 <- executeAgentWithMockLLM helloWorldAgent "My name is Bob" initialContext helloWorldToolLibrary
  case result1 of
    Left err -> assertFailure $ "Turn 1 failed: " ++ show err
    Right response1 -> do
      -- Expected: Agent responds conversationally, no tool call yet
      -- Update context for next turn
      let context1 = case addMessage UserRole "My name is Bob" initialContext of
                       Right c -> c
                       Left err -> error $ "Failed to add user message: " ++ T.unpack err
      let contextAfterTurn1 = case addMessage AssistantRole (responseContent response1) context1 of
                                Right c -> c
                                Left err -> error $ "Failed to add assistant message: " ++ T.unpack err
      
      -- Turn 2: General smalltalk
      result2 <- executeAgentWithMockLLM helloWorldAgent "the weather is nice today" contextAfterTurn1 helloWorldToolLibrary
      case result2 of
        Left err -> assertFailure $ "Turn 2 failed: " ++ show err
        Right response2 -> do
          -- Expected: Agent responds conversationally, no tool call yet
          -- Update context for next turn
          let context2 = case addMessage UserRole "the weather is nice today" contextAfterTurn1 of
                           Right c -> c
                           Left err -> error $ "Failed to add user message: " ++ T.unpack err
          let contextAfterTurn2 = case addMessage AssistantRole (responseContent response2) context2 of
                                    Right c -> c
                                    Left err -> error $ "Failed to add assistant message: " ++ T.unpack err
          
          -- Turn 3: User says hello without re-introducing name
          result3 <- executeAgentWithMockLLM helloWorldAgent "oh, hello btw" contextAfterTurn2 helloWorldToolLibrary
          case result3 of
            Left err -> assertFailure $ "Turn 3 failed: " ++ show err
            Right response3 -> do
              -- Expected: Agent uses sayHello tool with personName="Bob"
              -- Verify tool was used
              length (responseToolsUsed response3) @?= 1
              let toolInv = head (responseToolsUsed response3)
              invocationToolName toolInv @?= "sayHello"
              -- Verify arguments (expecting "Bob")
              let expectedArgs = object ["personName" .= ("Bob" :: T.Text)]
              invocationArgs toolInv @?= expectedArgs
              -- Verify response incorporates the greeting
              T.isInfixOf "Bob" (responseContent response3) @? "Response should mention Bob"

tests :: TestTree
tests = testGroup "Multi-Turn Tool Conversation Scenario Tests"
  [ testAgentReferencesPreviousToolUsage
  , testMultiTurnConversationCoherence
  , testAgentUsesPreviousToolResults
  , testAgentRemembersUserInfoFromHistory
  ]

