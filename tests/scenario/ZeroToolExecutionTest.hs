{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for agent execution with zero tools.
--
-- These tests validate that agents with no tools can execute correctly
-- and generate conversational responses without attempting tool calls.
--
-- User Story 1: Scenario Tests for Agent Execution with Zero Tools
module ZeroToolExecutionTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Agent, createAgent, createModel, Provider(..), agentTools, agentName)
import PatternAgent.Runtime.ToolLibrary (emptyToolLibrary)
import PatternAgent.Runtime.Context (emptyContext)
import TestExecution (executeAgentWithMockLLM, AgentResponse(..), ToolInvocation(..))
import PatternAgent.Runtime.Context (addMessage, MessageRole(..))
import Control.Lens (view)
import qualified Data.Text as T

-- | Create a zero-tool agent for testing.
zeroToolAgent :: Agent
zeroToolAgent = case createAgent "zero_tool_agent" "A conversational agent with no tools" (createModel "gpt-4o-mini" OpenAI) "You are a helpful assistant." [] of
  Right agent -> agent
  Left err -> error $ "Failed to create zero-tool agent: " ++ T.unpack err

-- | Scenario test: Agent with zero tools executes and generates conversational response without tool calls.
--
-- Given: An agent with zero tools is created
-- When: A developer executes it with user input
-- Then: The agent generates a conversational response without attempting tool calls
testZeroToolAgentExecutesWithoutToolCalls :: TestTree
testZeroToolAgentExecutesWithoutToolCalls = testCase "Zero-tool agent executes without tool calls" $ do
  -- Verify agent has zero tools
  let tools = view agentTools zeroToolAgent
  length tools @?= 0
  
  -- Execute agent with user input
  result <- executeAgentWithMockLLM zeroToolAgent "Hello! How are you?" emptyContext emptyToolLibrary
  
  case result of
    Left err -> assertFailure $ "Agent execution failed: " ++ show err
    Right response -> do
      -- Verify no tools were used
      length (responseToolsUsed response) @?= 0
      -- Verify response was generated
      T.length (responseContent response) @?> 0
      -- Verify response is conversational (contains some text)
      T.isInfixOf "Hello" (T.toLower (responseContent response)) || 
        T.isInfixOf "help" (T.toLower (responseContent response)) || 
        T.isInfixOf "assist" (T.toLower (responseContent response)) @? 
        "Response should be conversational"

-- | Scenario test: Agent with zero tools handles LLM tool call request gracefully without crashing.
--
-- Given: An agent with zero tools executes
-- When: The LLM attempts to request a tool call
-- Then: The execution environment handles the request gracefully and continues without crashing
testZeroToolAgentHandlesToolCallRequest :: TestTree
testZeroToolAgentHandlesToolCallRequest = testCase "Zero-tool agent handles tool call request gracefully" $ do
  -- Verify agent has zero tools
  let tools = view agentTools zeroToolAgent
  length tools @?= 0
  
  -- Execute agent with input that might trigger tool call request
  -- Note: MockLLM might request a tool call even for zero-tool agents
  -- The execution should handle this gracefully
  result <- executeAgentWithMockLLM zeroToolAgent "What time is it?" emptyContext emptyToolLibrary
  
  case result of
    Left err -> do
      -- If error occurs, it should be UnexpectedToolCallError, not a crash
      case err of
        UnexpectedToolCallError _ -> return ()  -- Expected error for zero-tool agent
        _ -> assertFailure $ "Unexpected error type: " ++ show err
    Right response -> do
      -- If execution succeeds, verify no tools were actually used
      length (responseToolsUsed response) @?= 0
      -- Verify response was generated
      T.length (responseContent response) @?> 0

-- | Scenario test: Agent with zero tools maintains conversation context across multiple message exchanges.
--
-- Given: An agent with zero tools maintains conversation context
-- When: Multiple messages are exchanged
-- Then: The agent maintains coherent conversation across all exchanges
testZeroToolAgentMaintainsContext :: TestTree
testZeroToolAgentMaintainsContext = testCase "Zero-tool agent maintains conversation context" $ do
  -- Verify agent has zero tools
  let tools = view agentTools zeroToolAgent
  length tools @?= 0
  
  -- First message exchange
  result1 <- executeAgentWithMockLLM zeroToolAgent "My name is Alice" emptyContext emptyToolLibrary
  case result1 of
    Left err -> assertFailure $ "First execution failed: " ++ show err
    Right response1 -> do
      -- Verify first response
      T.length (responseContent response1) @?> 0
      
      -- Build context from first exchange
      let context1 = case addMessage UserRole "My name is Alice" emptyContext of
            Right c -> c
            Left err -> error $ "Failed to add message: " ++ T.unpack err
      let context2 = case addMessage AssistantRole (responseContent response1) context1 of
            Right c -> c
            Left err -> error $ "Failed to add message: " ++ T.unpack err
      
      -- Second message exchange with context
      result2 <- executeAgentWithMockLLM zeroToolAgent "What's my name?" context2 emptyToolLibrary
      case result2 of
        Left err -> assertFailure $ "Second execution failed: " ++ show err
        Right response2 -> do
          -- Verify second response references context (mentions name or acknowledges previous message)
          T.length (responseContent response2) @?> 0
          -- Response should acknowledge the conversation context
          -- (MockLLM should extract name from history)
          let responseLower = T.toLower (responseContent response2)
          T.isInfixOf "alice" responseLower || 
            T.isInfixOf "name" responseLower || 
            T.isInfixOf "you" responseLower @? 
            "Response should reference conversation context"

tests :: TestTree
tests = testGroup "Zero Tool Execution Scenario Tests"
  [ testZeroToolAgentExecutesWithoutToolCalls
  , testZeroToolAgentHandlesToolCallRequest
  , testZeroToolAgentMaintainsContext
  ]
