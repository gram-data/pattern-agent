{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for tool creation and registration.
--
-- These tests simulate user goal satisfaction end-to-end:
-- - Creating tools with name, description, schema, and invocation function
-- - Accessing tool properties
-- - Validating tool parameters
module ToolCreationTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Tool, createTool, toolName, toolDescription, toolTypeSignature, toolSchema)
import PatternAgent.Language.TypeSignature (extractTypeSignatureFromPattern)
import PatternAgent.Runtime.ToolLibrary (ToolImpl, ToolLibrary, createToolImpl, emptyToolLibrary, registerTool, lookupTool, validateToolArgs, toolImplName, toolImplDescription, toolImplSchema)
import Control.Lens (view)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T
import Pattern (Pattern Subject)
import qualified Gram

-- | Helper: Parse type signature string to Pattern Subject element.
parseTypeSig :: String -> Pattern Subject
parseTypeSig sig = case Gram.fromGram sig of
  Right p -> p
  Left _ -> error $ "Failed to parse type signature: " ++ sig

-- | Scenario test: Create tool with name, description, schema, and invocation function.
testCreateToolWithAllProperties :: TestTree
testCreateToolWithAllProperties = testCase "Create tool with name, description, type signature" $ do
  let typeSig = parseTypeSig "(personName::Text {default:\"world\"})==>(::String)"
  let result = createTool
        "sayHello"
        "Returns a friendly greeting message for the given name"
        typeSig
  
  case result of
    Right tool -> do
      -- Verify tool was created
      view toolName tool @?= "sayHello"
      view toolDescription tool @?= "Returns a friendly greeting message for the given name"
      -- Type signature should be accessible (may be empty if extraction not fully implemented)
      let sig = view toolTypeSignature tool
      T.null sig @?= False  -- Should have type signature
    Left err -> assertFailure $ "Expected Right Tool, got Left: " ++ T.unpack err

-- | Scenario test: Verify tool can be accessed and its properties retrieved.
testToolPropertyAccess :: TestTree
testToolPropertyAccess = testCase "Access tool properties via lenses" $ do
  let typeSig = parseTypeSig "(name::Text)==>(::String)"
  let result = createTool "testTool" "Test description" typeSig
  
  case result of
    Right tool -> do
      -- Access name
      view toolName tool @?= "testTool"
      
      -- Access description
      view toolDescription tool @?= "Test description"
      
      -- Access type signature
      let sig = view toolTypeSignature tool
      T.null sig @?= False  -- Should have type signature
      
      -- Access schema (computed from type signature)
      let schema = view toolSchema tool
      -- Schema should be a JSON object
      case schema of
        Object _ -> return ()  -- Valid
        _ -> assertFailure "Schema should be a JSON object"
    Left err -> assertFailure $ "Expected Right Tool, got Left: " ++ T.unpack err

-- | Scenario test: Verify tool parameter validation works correctly.
testToolParameterValidation :: TestTree
testToolParameterValidation = testCase "Validate tool parameters against schema" $ do
  -- Create a tool with a schema
  let typeSig = parseTypeSig "(personName::Text)==>(::String)"
  let toolResult = createTool "sayHello" "Greeting tool" typeSig
  
  case toolResult of
    Right tool -> do
      let schema = view toolSchema tool
      
      -- Test valid parameters
      let validArgs = object ["personName" .= ("Alice" :: T.Text)]
      case validateToolArgs schema validArgs of
        Right _ -> return ()  -- Should succeed
        Left err -> assertFailure $ "Valid args should pass validation: " ++ T.unpack err
      
      -- Test invalid parameters (missing required field)
      let invalidArgs = object []  -- Missing personName
      case validateToolArgs schema invalidArgs of
        Left _ -> return ()  -- Should fail
        Right _ -> assertFailure "Invalid args (missing required) should fail validation"
      
      -- Test invalid parameters (wrong type)
      let wrongTypeArgs = object ["personName" .= (42 :: Int)]
      case validateToolArgs schema wrongTypeArgs of
        Left _ -> return ()  -- Should fail
        Right _ -> assertFailure "Invalid args (wrong type) should fail validation"
    Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err

tests :: TestTree
tests = testGroup "Tool Creation Scenario Tests"
  [ testCreateToolWithAllProperties
  , testToolPropertyAccess
  , testToolParameterValidation
  ]

