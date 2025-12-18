{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for conversation context management.
--
-- Tests conversation context including:
-- - Tool invocations and results
-- - FunctionRole messages properly formatted
-- - Context updates with user, assistant, and function messages
module ContextTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Runtime.Context
  ( MessageRole(..)
  , Message(..)
  , ConversationContext
  , emptyContext
  , addMessage
  , createMessage
  )
import qualified Data.Text as T

-- | Unit test: Conversation context includes tool invocations and results.
testContextIncludesToolInvocations :: TestTree
testContextIncludesToolInvocations = testGroup "Context with Tool Invocations"
  [ testCase "Context includes FunctionRole message for tool result" $ do
      let context = emptyContext
      -- Add user message
      let context1 = case addMessage UserRole "Hello" context of
            Right c -> c
            Left err -> error $ "Failed to add user message: " ++ T.unpack err
      -- Add assistant message with tool call
      let context2 = case addMessage AssistantRole "Calling sayHello" context1 of
            Right c -> c
            Left err -> error $ "Failed to add assistant message: " ++ T.unpack err
      -- Add function message with tool result
      let context3 = case addMessage (FunctionRole "sayHello") "Hello, Alice!" context2 of
            Right c -> c
            Left err -> error $ "Failed to add function message: " ++ T.unpack err
      
      -- Verify context has 3 messages
      length context3 @?= 3
      
      -- Verify message roles
      messageRole (context3 !! 0) @?= UserRole
      messageRole (context3 !! 1) @?= AssistantRole
      messageRole (context3 !! 2) @?= FunctionRole "sayHello"
      
      -- Verify message contents
      messageContent (context3 !! 0) @?= "Hello"
      messageContent (context3 !! 1) @?= "Calling sayHello"
      messageContent (context3 !! 2) @?= "Hello, Alice!"
  
  , testCase "Context maintains order of messages" $ do
      let context = emptyContext
      let context1 = case addMessage UserRole "First" context of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      let context2 = case addMessage AssistantRole "Second" context1 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      let context3 = case addMessage (FunctionRole "tool1") "Third" context2 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      
      -- Verify order
      messageContent (context3 !! 0) @?= "First"
      messageContent (context3 !! 1) @?= "Second"
      messageContent (context3 !! 2) @?= "Third"
  
  , testCase "Context includes multiple tool invocations" $ do
      let context = emptyContext
      -- First tool invocation
      let context1 = case addMessage UserRole "Use tool1" context of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      let context2 = case addMessage AssistantRole "Calling tool1" context1 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      let context3 = case addMessage (FunctionRole "tool1") "Result1" context2 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      -- Second tool invocation
      let context4 = case addMessage UserRole "Use tool2" context3 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      let context5 = case addMessage AssistantRole "Calling tool2" context4 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      let context6 = case addMessage (FunctionRole "tool2") "Result2" context5 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      
      -- Verify context has 6 messages
      length context6 @?= 6
      
      -- Verify tool results are in context
      let functionMessages = filter (\msg -> case messageRole msg of
            FunctionRole _ -> True
            _ -> False) context6
      length functionMessages @?= 2
      messageContent (functionMessages !! 0) @?= "Result1"
      messageContent (functionMessages !! 1) @?= "Result2"
  ]

-- | Unit test: FunctionRole messages properly formatted in conversation context.
testFunctionRoleMessagesFormatted :: TestTree
testFunctionRoleMessagesFormatted = testGroup "FunctionRole Message Formatting"
  [ testCase "FunctionRole message includes tool name" $ do
      let msg = Message (FunctionRole "sayHello") "Hello, world!"
      messageRole msg @?= FunctionRole "sayHello"
      messageContent msg @?= "Hello, world!"
  
  , testCase "FunctionRole message can be created via addMessage" $ do
      let context = emptyContext
      let result = addMessage (FunctionRole "myTool") "Tool result" context
      case result of
        Right newContext -> do
          length newContext @?= 1
          let msg = head newContext
          messageRole msg @?= FunctionRole "myTool"
          messageContent msg @?= "Tool result"
        Left err -> assertFailure $ "Failed to add FunctionRole message: " ++ T.unpack err
  
  , testCase "FunctionRole messages preserve tool name" $ do
      let context = emptyContext
      let context1 = case addMessage (FunctionRole "toolA") "Result A" context of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      let context2 = case addMessage (FunctionRole "toolB") "Result B" context1 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      
      -- Verify tool names are preserved
      case messageRole (context2 !! 0) of
        FunctionRole name -> name @?= "toolA"
        _ -> assertFailure "Expected FunctionRole"
      case messageRole (context2 !! 1) of
        FunctionRole name -> name @?= "toolB"
        _ -> assertFailure "Expected FunctionRole"
  
  , testCase "FunctionRole message content can be tool result JSON" $ do
      let jsonResult = "{\"greeting\": \"Hello, Alice!\"}"
      let context = emptyContext
      let result = addMessage (FunctionRole "sayHello") (T.pack jsonResult) context
      case result of
        Right newContext -> do
          let msg = head newContext
          messageContent msg @?= T.pack jsonResult
        Left err -> assertFailure $ "Failed: " ++ T.unpack err
  ]

-- | Unit test: Context updates include all message types.
testContextUpdatesIncludeAllMessageTypes :: TestTree
testContextUpdatesIncludeAllMessageTypes = testGroup "Context Updates with All Message Types"
  [ testCase "Context includes user, assistant, and function messages" $ do
      let context = emptyContext
      -- User message
      let context1 = case addMessage UserRole "Hello" context of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      -- Assistant message with tool call
      let context2 = case addMessage AssistantRole "I'll call sayHello" context1 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      -- Function message with tool result
      let context3 = case addMessage (FunctionRole "sayHello") "Hello, world!" context2 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      -- Final assistant response
      let context4 = case addMessage AssistantRole "Hello, world! How can I help you?" context3 of
            Right c -> c
            Left err -> error $ "Failed: " ++ T.unpack err
      
      -- Verify all message types are present
      length context4 @?= 4
      messageRole (context4 !! 0) @?= UserRole
      messageRole (context4 !! 1) @?= AssistantRole
      messageRole (context4 !! 2) @?= FunctionRole "sayHello"
      messageRole (context4 !! 3) @?= AssistantRole
      
      -- Verify message sequence
      messageContent (context4 !! 0) @?= "Hello"
      messageContent (context4 !! 1) @?= "I'll call sayHello"
      messageContent (context4 !! 2) @?= "Hello, world!"
      messageContent (context4 !! 3) @?= "Hello, world! How can I help you?"
  ]

tests :: TestTree
tests = testGroup "Context Tests"
  [ testContextIncludesToolInvocations
  , testFunctionRoleMessagesFormatted
  , testContextUpdatesIncludeAllMessageTypes
  ]

