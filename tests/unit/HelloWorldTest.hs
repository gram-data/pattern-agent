{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for hello world agent and sayHello tool.
--
-- These tests verify component correctness:
-- - Hello world agent creation with sayHello tool and instructions
-- - sayHello tool implementation with various inputs
-- - sayHello tool specification with gram type signature
module HelloWorldTest where

import Test.Tasty
import Test.Tasty.HUnit
import HelloWorldExample (sayHello, sayHelloImpl, helloWorldToolLibrary, helloWorldAgent)
import PatternAgent.Language.Core (Agent, Tool, agentName, agentDescription, agentModel, agentInstruction, agentTools, toolName, toolDescription, toolTypeSignature, createModel, OpenAI)
import PatternAgent.Runtime.ToolLibrary (ToolImpl, ToolLibrary, toolImplName, toolImplDescription, toolImplSchema, lookupTool, validateToolArgs)
import PatternAgent.Runtime.Context (ConversationContext, emptyContext)
import Control.Lens (view)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Unit test: Hello world agent creation with sayHello tool and instructions.
testHelloWorldAgentCreation :: TestTree
testHelloWorldAgentCreation = testGroup "Hello World Agent Creation"
  [ testCase "Agent has correct name" $ do
      view agentName helloWorldAgent @?= "hello_world_agent"
  
  , testCase "Agent has description" $ do
      case view agentDescription helloWorldAgent of
        Just desc -> T.isPrefixOf "A friendly agent" desc @?= True
        Nothing -> assertFailure "Agent should have description"
  
  , testCase "Agent has correct model" $ do
      let model = view agentModel helloWorldAgent
      modelId model @?= "gpt-3.5-turbo"
      modelProvider model @?= OpenAI
  
  , testCase "Agent has instruction about sayHello tool" $ do
      let instruction = view agentInstruction helloWorldAgent
      T.isInfixOf "sayHello" instruction @?= True
      T.isInfixOf "friendly" instruction @?= True
  
  , testCase "Agent has sayHello tool" $ do
      let tools = view agentTools helloWorldAgent
      length tools @?= 1
      view toolName (head tools) @?= "sayHello"
  ]

-- | Unit test: sayHello tool implementation with various inputs.
testSayHelloToolImplementation :: TestTree
testSayHelloToolImplementation = testGroup "sayHello Tool Implementation"
  [ testCase "sayHelloImpl has correct name" $ do
      toolImplName sayHelloImpl @?= "sayHello"
  
  , testCase "sayHelloImpl has correct description" $ do
      toolImplDescription sayHelloImpl @?= "Returns a friendly greeting message for the given name"
  
  , testCase "sayHelloImpl invoke function works with default name" $ do
      -- Test with default parameter (no personName provided)
      let args = object []  -- Empty args should use default
      result <- toolImplInvoke sayHelloImpl args
      -- Result should be a string containing greeting
      case result of
        String greeting -> T.isInfixOf "Hello" greeting @?= True
        _ -> assertFailure "Tool should return string greeting"
  
  , testCase "sayHelloImpl invoke function works with custom name" $ do
      -- Test with personName parameter
      let args = object ["personName" .= ("Alice" :: T.Text)]
      result <- toolImplInvoke sayHelloImpl args
      case result of
        String greeting -> do
          T.isInfixOf "Hello" greeting @?= True
          T.isInfixOf "Alice" greeting @?= True
        _ -> assertFailure "Tool should return string greeting with name"
  
  , testCase "sayHelloImpl schema validation accepts valid parameters" $ do
      let schema = toolImplSchema sayHelloImpl
      let validArgs = object ["personName" .= ("Bob" :: T.Text)]
      case validateToolArgs schema validArgs of
        Right _ -> return ()  -- Should succeed
        Left err -> assertFailure $ "Valid args should pass: " ++ T.unpack err
  
  , testCase "sayHelloImpl schema validation accepts empty parameters (uses default)" $ do
      let schema = toolImplSchema sayHelloImpl
      let emptyArgs = object []  -- Empty should use default "world"
      case validateToolArgs schema emptyArgs of
        Right _ -> return ()  -- Should succeed (default value)
        Left err -> assertFailure $ "Empty args should pass (default): " ++ T.unpack err
  ]

-- | Unit test: sayHello tool specification with gram type signature.
testSayHelloToolSpecification :: TestTree
testSayHelloToolSpecification = testGroup "sayHello Tool Specification"
  [ testCase "sayHello tool has correct name" $ do
      view toolName sayHello @?= "sayHello"
  
  , testCase "sayHello tool has description" $ do
      let desc = view toolDescription sayHello
      T.isInfixOf "greeting" desc @?= True
  
  , testCase "sayHello tool has type signature" $ do
      let typeSig = view toolTypeSignature sayHello
      T.isInfixOf "personName" typeSig @?= True
      T.isInfixOf "Text" typeSig @?= True
      T.isInfixOf "String" typeSig @?= True
  
  , testCase "sayHello tool schema is generated from type signature" $ do
      let schema = view toolSchema sayHello
      -- Schema should be a valid JSON schema object
      case schema of
        Object _ -> return ()  -- Should be an object
        _ -> assertFailure "Tool schema should be an object"
  
  , testCase "sayHello tool is in helloWorldToolLibrary" $ do
      case lookupTool "sayHello" helloWorldToolLibrary of
        Just toolImpl -> toolImplName toolImpl @?= "sayHello"
        Nothing -> assertFailure "sayHello should be in tool library"
  ]

tests :: TestTree
tests = testGroup "Hello World Unit Tests"
  [ testHelloWorldAgentCreation
  , testSayHelloToolImplementation
  , testSayHelloToolSpecification
  ]

