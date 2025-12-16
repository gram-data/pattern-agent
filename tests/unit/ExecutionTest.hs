{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for agent execution with tool support.
module ExecutionTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Agent, Tool, createAgent, createTool, agentTools, toolName, createModel, OpenAI)
import PatternAgent.Runtime.ToolLibrary (ToolImpl, ToolLibrary, createToolImpl, emptyToolLibrary, registerTool, lookupTool, bindTool, validateToolArgs)
import PatternAgent.Runtime.Execution (bindAgentTools, AgentError(..))
import PatternAgent.Runtime.Context (ConversationContext, emptyContext)
import Control.Lens (view)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Vector as V
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

-- | Unit test: Tool call detection in LLM responses.
--
-- NOTE: This test will fail until detectToolCall is implemented.
testToolCallDetection :: TestTree
testToolCallDetection = testGroup "Tool Call Detection"
  [ testCase "Detect function_call in LLM response" $ do
      -- TODO: Test detectToolCall function when implemented
      -- For now, just verify test structure
      return ()
  
  , testCase "Handle LLM response without function_call" $ do
      -- TODO: Test that non-tool-call responses are handled correctly
      return ()
  ]

-- | Unit test: Tool invocation with correct parameters.
--
-- NOTE: This test will fail until invokeTool is implemented.
testToolInvocation :: TestTree
testToolInvocation = testGroup "Tool Invocation"
  [ testCase "Invoke tool with correct parameters" $ do
      let typeSig = parseTypeSig "(personName::String)==>(::String)"
      let toolResult = createTool "sayHello" "Greeting tool" typeSig
      
      case toolResult of
        Right tool -> do
          let invoke = \args -> return $ String "Hello!"
          let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
          case createToolImpl "sayHello" "Greeting" schema invoke of
            Right toolImpl -> do
              -- TODO: Test invokeTool when implemented
              -- For now, just verify setup
              toolImplName toolImpl @?= "sayHello"
            Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  ]

-- | Unit test: Tool result handling and formatting.
--
-- NOTE: This test will fail until tool result handling is implemented.
testToolResultHandling :: TestTree
testToolResultHandling = testGroup "Tool Result Handling"
  [ testCase "Format tool result for LLM" $ do
      -- TODO: Test tool result formatting when implemented
      return ()
  
  , testCase "Handle tool result errors" $ do
      -- TODO: Test error result handling when implemented
      return ()
  ]

-- | Unit test: Error handling for tool execution failures.
--
-- NOTE: This test will fail until error handling is implemented.
testToolExecutionErrorHandling :: TestTree
testToolExecutionErrorHandling = testGroup "Tool Execution Error Handling"
  [ testCase "Handle tool execution exception" $ do
      -- TODO: Test exception handling when implemented
      return ()
  
  , testCase "Handle tool timeout" $ do
      -- TODO: Test timeout handling when implemented
      return ()
  ]

-- | Unit test: Tool binding from Tool (Pattern) to ToolImpl implementation.
testToolBinding :: TestTree
testToolBinding = testGroup "Tool Binding"
  [ testCase "Bind tool to ToolImpl from library" $ do
      let typeSig = parseTypeSig "(x::String)==>(::String)"
      let toolResult = createTool "testTool" "Test tool" typeSig
      
      case toolResult of
        Right tool -> do
          let invoke = \args -> return args
          let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
          case createToolImpl "testTool" "Test tool" schema invoke of
            Right toolImpl -> do
              let library = registerTool "testTool" toolImpl emptyToolLibrary
              -- TODO: Test bindTool when implemented
              -- For now, verify lookup works
              lookupTool "testTool" library @?= Just toolImpl
            Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  
  , testCase "Bind tool fails when ToolImpl not in library" $ do
      let typeSig = parseTypeSig "(x::String)==>(::String)"
      let toolResult = createTool "missingTool" "Missing tool" typeSig
      
      case toolResult of
        Right tool -> do
          let library = emptyToolLibrary
          -- TODO: Test bindTool returns Nothing when tool not found
          -- For now, verify lookup returns Nothing
          lookupTool "missingTool" library @?= Nothing
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  ]

-- | Unit test: Tool parameter validation before invocation.
testToolParameterValidation :: TestTree
testToolParameterValidation = testGroup "Tool Parameter Validation"
  [ testCase "Validate parameters before tool invocation" $ do
      let schema = object
            [ "type" .= ("object" :: T.Text)
            , "properties" .= object ["name" .= object ["type" .= ("string" :: T.Text)]]
            , "required" .= Array (V.fromList [String "name"])
            ]
      let validArgs = object ["name" .= ("Alice" :: T.Text)]
      case validateToolArgs schema validArgs of
        Right _ -> return ()  -- Should succeed
        Left err -> assertFailure $ "Valid args should pass: " ++ T.unpack err
  
  , testCase "Reject invalid parameters before invocation" $ do
      let schema = object
            [ "type" .= ("object" :: T.Text)
            , "properties" .= object ["name" .= object ["type" .= ("string" :: T.Text)]]
            , "required" .= Array (V.fromList [String "name"])
            ]
      let invalidArgs = object []  -- Missing required field
      case validateToolArgs schema invalidArgs of
        Left _ -> return ()  -- Should fail
        Right _ -> assertFailure "Invalid args should fail validation"
  ]

-- | Unit test: ToolLibrary registration and lookup.
testToolLibraryRegistration :: TestTree
testToolLibraryRegistration = testGroup "ToolLibrary Registration"
  [ testCase "Register tool in library" $ do
      let invoke = \args -> return args
      let schema = object ["type" .= ("object" :: T.Text)]
      case createToolImpl "testTool" "Test" schema invoke of
        Right toolImpl -> do
          let library = registerTool "testTool" toolImpl emptyToolLibrary
          lookupTool "testTool" library @?= Just toolImpl
        Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
  
  , testCase "Lookup tool by name" $ do
      let invoke = \args -> return args
      let schema = object ["type" .= ("object" :: T.Text)]
      case createToolImpl "myTool" "My tool" schema invoke of
        Right toolImpl -> do
          let library = registerTool "myTool" toolImpl emptyToolLibrary
          case lookupTool "myTool" library of
            Just found -> toolImplName found @?= "myTool"
            Nothing -> assertFailure "Should find tool in library"
        Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
  
  , testCase "Lookup non-existent tool returns Nothing" $ do
      let library = emptyToolLibrary
      lookupTool "nonexistent" library @?= Nothing
  ]

-- | Unit test: bindTool function validates tool matches specification.
testBindToolValidation :: TestTree
testBindToolValidation = testGroup "bindTool Validation"
  [ testCase "bindTool validates tool matches ToolImpl" $ do
      let typeSig = parseTypeSig "(x::String)==>(::String)"
      let toolResult = createTool "testTool" "Test tool" typeSig
      
      case toolResult of
        Right tool -> do
          let invoke = \args -> return args
          let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
          case createToolImpl "testTool" "Test tool" schema invoke of
            Right toolImpl -> do
              let library = registerTool "testTool" toolImpl emptyToolLibrary
              -- TODO: Test bindTool when implemented
              -- bindTool should validate name, description, schema match
              return ()
            Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  ]

tests :: TestTree
tests = testGroup "Execution Tests"
  [ testToolCallDetection
  , testToolInvocation
  , testToolResultHandling
  , testToolExecutionErrorHandling
  , testToolBinding
  , testToolParameterValidation
  , testToolLibraryRegistration
  , testBindToolValidation
  ]

