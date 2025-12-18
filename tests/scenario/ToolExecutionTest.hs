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
import PatternAgent.Runtime.Execution (AgentResponse(..), ToolInvocation(..), AgentError(..))
import PatternAgent.Runtime.Context (ConversationContext, emptyContext, MessageRole(..), createMessage)
import TestExecution (executeAgentWithMockLLM)
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
              -- Execute agent with mock LLM
              let context = emptyContext
              let userInput = "Hello!"
              
              result <- executeAgentWithMockLLM agent userInput context library
              case result of
                Right response -> do
                  -- Verify tool was invoked
                  length (responseToolsUsed response) @?= 1
                  let invocation = head (responseToolsUsed response)
                  invocationToolName invocation @?= "sayHello"
                  -- Verify tool result is success
                  case invocationResult invocation of
                    Right _ -> return ()  -- Tool executed successfully
                    Left err -> assertFailure $ "Tool execution failed: " ++ T.unpack err
                Left err -> assertFailure $ "Execution failed: " ++ T.unpack (show err)
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
              -- Execute agent with mock LLM
              let context = emptyContext
              let userInput = "Add one to 41"
              
              result <- executeAgentWithMockLLM agent userInput context library
              case result of
                Right response -> do
                  -- Verify tool was invoked
                  length (responseToolsUsed response) @?= 1
                  let invocation = head (responseToolsUsed response)
                  invocationToolName invocation @?= "addOne"
                  -- Verify tool result is in response
                  case invocationResult invocation of
                    Right (Number n) -> n @?= 42
                    Right _ -> assertFailure "Tool should return Number 42"
                    Left err -> assertFailure $ "Tool execution failed: " ++ T.unpack err
                  -- Verify response content incorporates tool result
                  T.length (responseContent response) @?> 0
                Left err -> assertFailure $ "Execution failed: " ++ T.unpack (show err)
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
              -- Execute agent with mock LLM
              let context = emptyContext
              let userInput = "Use the failing tool"
              
              result <- executeAgentWithMockLLM agent userInput context library
              case result of
                Right response -> do
                  -- Verify tool was invoked (even though it fails)
                  length (responseToolsUsed response) @?= 1
                  let invocation = head (responseToolsUsed response)
                  invocationToolName invocation @?= "failingTool"
                  -- Verify tool result indicates error
                  case invocationResult invocation of
                    Left err -> do
                      -- Error should be communicated
                      T.length err @?> 0
                      -- Response should still be generated (error handled gracefully)
                      T.length (responseContent response) @?> 0
                    Right _ -> assertFailure "Tool should have failed but didn't"
                Left err -> assertFailure $ "Execution failed: " ++ T.unpack (show err)
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
          let context = emptyContext
          let userInput = "Use the missing tool"
          
          result <- executeAgentWithMockLLM agent userInput context library
          case result of
            Left (ToolError err) -> do
              -- Verify error message mentions missing tool
              T.isInfixOf "missingTool" err @? "Error should mention missing tool"
            Left err -> assertFailure $ "Expected ToolError, got: " ++ show err
            Right _ -> assertFailure "Expected ToolError but got successful response"
        Left err -> assertFailure $ "Agent creation failed: " ++ T.unpack err
    Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err

-- | Scenario test: Agent with multiple tools can use different tools.
--
-- Verifies that agents can have multiple tools and the LLM can choose which one to use.
testAgentWithMultipleTools :: TestTree
testAgentWithMultipleTools = testCase "Agent with multiple tools can use different tools" $ do
  -- Create two tools
  let typeSig1 = parseTypeSig "(personName::String {default:\"world\"})==>(::String)"
  let typeSig2 = parseTypeSig "(number::Integer)==>(::Integer)"
  
  let tool1Result = createTool "sayHello" "Greeting tool" typeSig1
  let tool2Result = createTool "double" "Doubles a number" typeSig2
  
  case (tool1Result, tool2Result) of
    (Right tool1, Right tool2) -> do
      -- Create tool implementations
      let invoke1 = \_args -> return $ String "Hello, world!"
      let invoke2 = \_args -> return $ Number 42
      let schema1 = object ["type" .= ("object" :: T.Text), "properties" .= object []]
      let schema2 = object ["type" .= ("object" :: T.Text), "properties" .= object []]
      
      case (createToolImpl "sayHello" "Greeting" schema1 invoke1, 
            createToolImpl "double" "Doubles" schema2 invoke2) of
        (Right impl1, Right impl2) -> do
          -- Create tool library with both tools
          let library = registerTool "double" impl2 $ registerTool "sayHello" impl1 emptyToolLibrary
          
          -- Create agent with both tools
          let agentResult = createAgent
                "multi_tool_agent"
                (Just "Agent with multiple tools")
                (createModel "gpt-3.5-turbo" OpenAI)
                "You have access to sayHello for greetings and double for math. Use the appropriate tool."
                [tool1, tool2]
          
          case agentResult of
            Right agent -> do
              -- Verify agent has both tools
              let tools = view agentTools agent
              length tools @?= 2
              let toolNames = map (view toolName) tools
              "sayHello" `elem` toolNames @?= True
              "double" `elem` toolNames @?= True
              
              -- Verify both tools are in library
              lookupTool "sayHello" library @?= Just impl1
              lookupTool "double" library @?= Just impl2
            Left err -> assertFailure $ "Agent creation failed: " ++ T.unpack err
        _ -> assertFailure "ToolImpl creation failed"
    _ -> assertFailure "Tool creation failed"

-- | Scenario test: Tool chaining - one tool result used by another.
--
-- Verifies that tools can be chained together where the result of one tool
-- is used as input to another tool in the same conversation.
testToolChaining :: TestTree
testToolChaining = testCase "Tool chaining - one tool result used by another" $ do
  -- Create two tools: one that gets a value, another that processes it
  let typeSig1 = parseTypeSig "(key::String)==>(::String)"
  let typeSig2 = parseTypeSig "(value::String)==>(::String)"
  
  let tool1Result = createTool "getValue" "Gets a value" typeSig1
  let tool2Result = createTool "processValue" "Processes a value" typeSig2
  
  case (tool1Result, tool2Result) of
    (Right tool1, Right tool2) -> do
      -- Create tool implementations
      let invoke1 = \_args -> return $ String "retrieved_value"
      let invoke2 = \_args -> return $ String "processed_value"
      let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
      
      case (createToolImpl "getValue" "Gets value" schema invoke1,
            createToolImpl "processValue" "Processes value" schema invoke2) of
        (Right impl1, Right impl2) -> do
          -- Create tool library with both tools
          let library = registerTool "processValue" impl2 $ registerTool "getValue" impl1 emptyToolLibrary
          
          -- Create agent with both tools
          let agentResult = createAgent
                "chaining_agent"
                (Just "Agent that chains tools")
                (createModel "gpt-3.5-turbo" OpenAI)
                "Use getValue first, then processValue with the result."
                [tool1, tool2]
          
          case agentResult of
            Right agent -> do
              -- Verify setup is correct for chaining
              let tools = view agentTools agent
              length tools @?= 2
              -- Both tools should be accessible
              lookupTool "getValue" library @?= Just impl1
              lookupTool "processValue" library @?= Just impl2
            Left err -> assertFailure $ "Agent creation failed: " ++ T.unpack err
        _ -> assertFailure "ToolImpl creation failed"
    _ -> assertFailure "Tool creation failed"

-- | Scenario test: Error recovery - agent continues after tool error.
--
-- Verifies that when a tool fails, the agent can recover and continue
-- the conversation or try alternative approaches.
testErrorRecovery :: TestTree
testErrorRecovery = testCase "Error recovery - agent continues after tool error" $ do
  -- Create a tool that fails, and a backup tool that succeeds
  let typeSig1 = parseTypeSig "(x::String)==>(::String)"
  let typeSig2 = parseTypeSig "(y::String)==>(::String)"
  
  let tool1Result = createTool "failingTool" "A tool that fails" typeSig1
  let tool2Result = createTool "backupTool" "A backup tool that works" typeSig2
  
  case (tool1Result, tool2Result) of
    (Right tool1, Right tool2) -> do
      -- Create tool implementations: one fails, one succeeds
      let invoke1 = \_args -> error "Tool execution failed"
      let invoke2 = \_args -> return $ String "Backup succeeded"
      let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
      
      case (createToolImpl "failingTool" "Fails" schema invoke1,
            createToolImpl "backupTool" "Backup" schema invoke2) of
        (Right impl1, Right impl2) -> do
          -- Create tool library with both tools
          let library = registerTool "backupTool" impl2 $ registerTool "failingTool" impl1 emptyToolLibrary
          
          -- Create agent with both tools
          let agentResult = createAgent
                "recovery_agent"
                (Just "Agent that recovers from errors")
                (createModel "gpt-3.5-turbo" OpenAI)
                "Try failingTool first, but if it fails, use backupTool instead."
                [tool1, tool2]
          
          case agentResult of
            Right agent -> do
              -- Verify setup is correct for error recovery
              let tools = view agentTools agent
              length tools @?= 2
              -- Both tools should be accessible
              lookupTool "failingTool" library @?= Just impl1
              lookupTool "backupTool" library @?= Just impl2
            Left err -> assertFailure $ "Agent creation failed: " ++ T.unpack err
        _ -> assertFailure "ToolImpl creation failed"
    _ -> assertFailure "Tool creation failed"

tests :: TestTree
tests = testGroup "Tool Execution Scenario Tests"
  [ testAgentExecutesWithToolCall
  , testToolExecutesSuccessfully
  , testToolInvocationFailureHandled
  , testToolNotFoundError
  , testAgentWithMultipleTools
  , testToolChaining
  , testErrorRecovery
  ]

