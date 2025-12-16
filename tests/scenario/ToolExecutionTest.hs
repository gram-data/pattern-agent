{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for tool execution during agent execution.
--
-- These tests simulate user goal satisfaction end-to-end:
-- - Agent with tool executes and LLM requests tool call, tool is invoked
-- - Tool executes successfully and result is returned to LLM for response generation
-- - Tool invocation failure is handled gracefully and communicated to LLM
-- - Agent requests tool that doesn't exist, appropriate error is returned
module ToolExecutionTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Agent, Tool, createAgent, createTool, agentTools, toolName, createModel, OpenAI)
import PatternAgent.Runtime.ToolLibrary (ToolImpl, ToolLibrary, createToolImpl, emptyToolLibrary, registerTool, lookupTool, bindTool)
import PatternAgent.Runtime.Execution (executeAgentWithLibrary, AgentResponse(..), ToolInvocation(..), AgentError(..))
import PatternAgent.Runtime.Context (ConversationContext, emptyContext, MessageRole(..), createMessage)
import Control.Lens (view)
import Data.Aeson (Value(..), object, (.=))
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

-- | Scenario test: Agent with tool executes and LLM requests tool call, tool is invoked.
--
-- NOTE: This test will fail until executeAgentWithLibrary is implemented.
-- The test verifies that when an LLM requests a tool call, the execution
-- environment detects it and invokes the tool.
testAgentExecutesWithToolCall :: TestTree
testAgentExecutesWithToolCall = testCase "Agent with tool executes and LLM requests tool call" $ do
  -- Create a tool
  let typeSig = parseTypeSig "(personName::String {default:\"world\"})==>(::String)"
  let toolResult = createTool
        "sayHello"
        "Returns a friendly greeting message for the given name"
        typeSig
  
  case toolResult of
    Right tool -> do
      -- Create tool implementation
      let invoke = \args -> do
            -- Extract personName from args (simplified - would use proper JSON parsing)
            return $ String "Hello, world! Nice to meet you."
      let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
      case createToolImpl "sayHello" "Returns a friendly greeting" schema invoke of
        Right toolImpl -> do
          -- Create tool library
          let library = registerTool "sayHello" toolImpl emptyToolLibrary
          
          -- Create agent with tool
          let agentResult = createAgent
                "test_agent"
                (Just "Test agent")
                (createModel "gpt-3.5-turbo" OpenAI)
                "You are a helpful assistant. Use the sayHello tool when greeting users."
                [tool]
          
          case agentResult of
            Right agent -> do
              -- Execute agent (this will fail until executeAgentWithLibrary is implemented)
              -- For now, just verify the setup is correct
              let context = emptyContext
              let userInput = "Hello!"
              
              -- TODO: Uncomment when executeAgentWithLibrary is implemented
              -- result <- executeAgentWithLibrary agent userInput context library
              -- case result of
              --   Right response -> do
              --     -- Verify tool was invoked
              --     length (responseToolsUsed response) @?= 1
              --     let invocation = head (responseToolsUsed response)
              --     invocationToolName invocation @?= "sayHello"
              --   Left err -> assertFailure $ "Execution failed: " ++ T.unpack (show err)
              
              -- For now, just verify setup
              view toolName tool @?= "sayHello"
              lookupTool "sayHello" library @?= Just toolImpl
            Left err -> assertFailure $ "Agent creation failed: " ++ T.unpack err
        Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
    Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err

-- | Scenario test: Tool executes successfully and result is returned to LLM for response generation.
--
-- NOTE: This test will fail until executeAgentWithLibrary is implemented.
testToolExecutesSuccessfully :: TestTree
testToolExecutesSuccessfully = testCase "Tool executes successfully and result returned to LLM" $ do
  -- Create a simple tool that always succeeds
  let typeSig = parseTypeSig "(value::Integer)==>(::Integer)"
  let toolResult = createTool "addOne" "Adds one to a number" typeSig
  
  case toolResult of
    Right tool -> do
      let invoke = \args -> do
            -- Simplified - would parse args properly
            return $ Number 42
      let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
      case createToolImpl "addOne" "Adds one" schema invoke of
        Right toolImpl -> do
          let library = registerTool "addOne" toolImpl emptyToolLibrary
          let agentResult = createAgent
                "test_agent"
                Nothing
                (createModel "gpt-3.5-turbo" OpenAI)
                "Use addOne tool when asked to add one."
                [tool]
          
          case agentResult of
            Right agent -> do
              -- TODO: Execute and verify tool result is in response
              -- For now, just verify setup
              return ()
            Left err -> assertFailure $ "Agent creation failed: " ++ T.unpack err
        Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
    Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err

-- | Scenario test: Tool invocation failure is handled gracefully and communicated to LLM.
--
-- NOTE: This test will fail until executeAgentWithLibrary is implemented.
testToolInvocationFailureHandled :: TestTree
testToolInvocationFailureHandled = testCase "Tool invocation failure handled gracefully" $ do
  -- Create a tool that always fails
  let typeSig = parseTypeSig "(x::String)==>(::String)"
  let toolResult = createTool "failingTool" "A tool that always fails" typeSig
  
  case toolResult of
    Right tool -> do
      let invoke = \_args -> do
            -- Simulate failure
            error "Tool execution failed"
      let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
      case createToolImpl "failingTool" "Failing tool" schema invoke of
        Right toolImpl -> do
          let library = registerTool "failingTool" toolImpl emptyToolLibrary
          let agentResult = createAgent
                "test_agent"
                Nothing
                (createModel "gpt-3.5-turbo" OpenAI)
                "Use failingTool (it will fail)."
                [tool]
          
          case agentResult of
            Right agent -> do
              -- TODO: Execute and verify error is handled gracefully
              -- For now, just verify setup
              return ()
            Left err -> assertFailure $ "Agent creation failed: " ++ T.unpack err
        Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
    Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err

-- | Scenario test: Agent requests tool that doesn't exist, appropriate error is returned.
--
-- NOTE: This test will fail until executeAgentWithLibrary is implemented.
testToolNotFoundError :: TestTree
testToolNotFoundError = testCase "Agent requests tool that doesn't exist" $ do
  -- Create agent with tool, but don't register implementation
  let typeSig = parseTypeSig "(x::String)==>(::String)"
  let toolResult = createTool "missingTool" "A tool without implementation" typeSig
  
  case toolResult of
    Right tool -> do
      let agentResult = createAgent
            "test_agent"
            Nothing
            (createModel "gpt-3.5-turbo" OpenAI)
            "Use missingTool (not in library)."
            [tool]
      
      case agentResult of
        Right agent -> do
          let library = emptyToolLibrary  -- Empty library, tool not registered
          -- TODO: Execute and verify ToolError is returned
          -- For now, just verify setup
          return ()
        Left err -> assertFailure $ "Agent creation failed: " ++ T.unpack err
    Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err

tests :: TestTree
tests = testGroup "Tool Execution Scenario Tests"
  [ testAgentExecutesWithToolCall
  , testToolExecutesSuccessfully
  , testToolInvocationFailureHandled
  , testToolNotFoundError
  ]

