{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for tool creation and tool implementation.
module ToolTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Tool, createTool, toolName, toolDescription, toolTypeSignature, toolSchema)
import PatternAgent.Runtime.ToolLibrary (ToolImpl(..), createToolImpl, toolImplName, toolImplDescription, toolImplSchema, toolImplInvoke, emptyToolLibrary, registerTool, lookupTool, validateToolArgs)
import PatternAgent.Language.TypeSignature (extractTypeSignatureFromPattern, typeSignatureToJSONSchema, TypeSignature(..), Parameter(..), createTypeNode, createFunctionTypePattern)
import PatternAgent.Language.Core
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..))
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Lens (view)
import Pattern (Pattern)
import Pattern.Core (value, elements)
import Subject.Core (Subject)
import qualified Data.Set as Set
import qualified Gram

type PatternSubject = Pattern Subject

-- | Helper: Parse type signature string to Pattern Subject element.
parseTypeSig :: String -> PatternSubject
parseTypeSig sig = case Gram.fromGram sig of
  Right p -> p
  Left _ -> error $ "Failed to parse type signature: " ++ sig

-- | Unit test: Tool creation with gram type signature.
testToolCreationWithTypeSignature :: TestTree
testToolCreationWithTypeSignature = testGroup "Tool Creation"
  [ testCase "Create tool with simple type signature" $ do
      let typeSig = parseTypeSig "(personName::Text)==>(::String)"
      let result = createTool "sayHello" "Greeting tool" typeSig
      case result of
        Right tool -> do
          view toolName tool @?= "sayHello"
          view toolDescription tool @?= "Greeting tool"
        Left err -> assertFailure $ "Expected Right Tool, got Left: " ++ T.unpack err
  
  , testCase "Create tool with optional parameter" $ do
      let typeSig = parseTypeSig "(personName::Text {default:\"world\"})==>(::String)"
      let result = createTool "greet" "Greet with optional name" typeSig
      case result of
        Right tool -> view toolName tool @?= "greet"
        Left err -> assertFailure $ "Expected Right Tool, got Left: " ++ T.unpack err
  
  , testCase "Tool name cannot be empty" $ do
      let typeSig = parseTypeSig "(name::Text)==>(::String)"
      let result = createTool "" "Description" typeSig
      case result of
        Left err -> T.isInfixOf "empty" err @?= True
        Right _ -> assertFailure "Expected Left error for empty name"
  
  , testCase "Tool description cannot be empty" $ do
      let typeSig = parseTypeSig "(name::Text)==>(::String)"
      let result = createTool "toolName" "" typeSig
      case result of
        Left err -> T.isInfixOf "empty" err @?= True
        Right _ -> assertFailure "Expected Left error for empty description"
  ]

-- | Unit test: ToolImpl creation with name, description, schema, invoke function.
testToolImplCreation :: TestTree
testToolImplCreation = testGroup "ToolImpl Creation"
  [ testCase "Create ToolImpl with all fields" $ do
      let schema = object ["type" .= ("object" :: T.Text), "properties" .= object []]
      let invoke = \args -> return args  -- Simple identity function
      case createToolImpl "testTool" "Test description" schema invoke of
        Right toolImpl -> do
          toolImplName toolImpl @?= "testTool"
          toolImplDescription toolImpl @?= "Test description"
          toolImplSchema toolImpl @?= schema
        Left err -> assertFailure $ "Expected Right ToolImpl, got Left: " ++ T.unpack err
  
  , testCase "ToolImpl invoke function works" $ do
      let schema = object ["type" .= ("object" :: T.Text)]
      let invoke = \args -> return $ object ["result" .= ("success" :: T.Text)]
      case createToolImpl "test" "Test" schema invoke of
        Right toolImpl -> do
          -- Test invocation
          result <- toolImplInvoke toolImpl (object [])
          case result of
            Object obj -> return ()  -- Should return object
            _ -> assertFailure "Invoke should return object"
        Left err -> assertFailure $ "Expected Right ToolImpl, got Left: " ++ T.unpack err
  
  , testCase "ToolImpl name cannot be empty" $ do
      let schema = object []
      let invoke = \args -> return args
      case createToolImpl "" "Description" schema invoke of
        Left err -> T.isInfixOf "empty" err @?= True
        Right _ -> assertFailure "Expected Left error for empty name"
  
  , testCase "ToolImpl description cannot be empty" $ do
      let schema = object []
      let invoke = \args -> return args
      case createToolImpl "name" "" schema invoke of
        Left err -> T.isInfixOf "empty" err @?= True
        Right _ -> assertFailure "Expected Left error for empty description"
  ]

-- | Unit test: Tool accessors (toolName, toolDescription, toolTypeSignature, toolSchema) via lenses.
testToolAccessors :: TestTree
testToolAccessors = testGroup "Tool Accessors"
  [ testCase "toolName lens" $ do
      let typeSig = parseTypeSig "(x::Text)==>(::String)"
      let result = createTool "myTool" "Description" typeSig
      case result of
        Right tool -> view toolName tool @?= "myTool"
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  
  , testCase "toolDescription lens" $ do
      let typeSig = parseTypeSig "(x::Text)==>(::String)"
      let result = createTool "myTool" "My description" typeSig
      case result of
        Right tool -> view toolDescription tool @?= "My description"
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  
  , testCase "toolTypeSignature lens" $ do
      let typeSig = parseTypeSig "(name::Text)==>(::String)"
      let result = createTool "myTool" "Description" typeSig
      case result of
        Right tool -> do
          let sig = view toolTypeSignature tool
          T.null sig @?= False  -- Should have type signature
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  
  , testCase "toolSchema lens" $ do
      let typeSig = parseTypeSig "(name::Text)==>(::String)"
      let result = createTool "myTool" "Description" typeSig
      case result of
        Right tool -> do
          let schema = view toolSchema tool
          case schema of
            Object _ -> return ()  -- Should be object
            _ -> assertFailure "Schema should be JSON object"
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  ]

-- | Unit test: ToolImpl accessors (toolImplName, toolImplDescription, toolImplSchema).
testToolImplAccessors :: TestTree
testToolImplAccessors = testGroup "ToolImpl Accessors"
  [ testCase "toolImplName accessor" $ do
      case createToolImpl "test" "Desc" (object []) (\x -> return x) of
        Right toolImpl -> toolImplName toolImpl @?= "test"
        Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
  
  , testCase "toolImplDescription accessor" $ do
      case createToolImpl "test" "My description" (object []) (\x -> return x) of
        Right toolImpl -> toolImplDescription toolImpl @?= "My description"
        Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
  
  , testCase "toolImplSchema accessor" $ do
      let schema = object ["type" .= ("object" :: T.Text)]
      case createToolImpl "test" "Desc" schema (\x -> return x) of
        Right toolImpl -> toolImplSchema toolImpl @?= schema
        Left err -> assertFailure $ "ToolImpl creation failed: " ++ T.unpack err
  ]

-- | Unit test: Schema validation for valid parameters.
testSchemaValidationValid :: TestTree
testSchemaValidationValid = testGroup "Schema Validation - Valid"
  [ testCase "Validate string parameter" $ do
      let schema = object
            [ "type" .= ("object" :: T.Text)
            , "properties" .= object ["name" .= object ["type" .= ("string" :: T.Text)]]
            , "required" .= Array (V.fromList [String "name"])
            ]
      let args = object ["name" .= ("Alice" :: T.Text)]
      case validateToolArgs schema args of
        Right _ -> return ()  -- Should succeed
        Left err -> assertFailure $ "Valid args should pass: " ++ T.unpack err
  
  , testCase "Validate integer parameter" $ do
      let schema = object
            [ "type" .= ("object" :: T.Text)
            , "properties" .= object ["age" .= object ["type" .= ("integer" :: T.Text)]]
            , "required" .= Array (V.fromList [String "age"])
            ]
      let args = object ["age" .= (42 :: Int)]
      case validateToolArgs schema args of
        Right _ -> return ()  -- Should succeed
        Left err -> assertFailure $ "Valid args should pass: " ++ T.unpack err
  ]

-- | Unit test: Schema validation for invalid parameters (wrong type, missing required).
testSchemaValidationInvalid :: TestTree
testSchemaValidationInvalid = testGroup "Schema Validation - Invalid"
  [ testCase "Reject missing required field" $ do
      let schema = object
            [ "type" .= ("object" :: T.Text)
            , "properties" .= object ["name" .= object ["type" .= ("string" :: T.Text)]]
            , "required" .= Array (V.fromList [String "name"])
            ]
      let args = object []  -- Missing name
      case validateToolArgs schema args of
        Left _ -> return ()  -- Should fail
        Right _ -> assertFailure "Missing required field should fail validation"
  
  , testCase "Reject wrong type" $ do
      let schema = object
            [ "type" .= ("object" :: T.Text)
            , "properties" .= object ["name" .= object ["type" .= ("string" :: T.Text)]]
            , "required" .= Array (V.fromList [String "name"])
            ]
      let args = object ["name" .= (42 :: Int)]  -- Wrong type
      case validateToolArgs schema args of
        Left _ -> return ()  -- Should fail
        Right _ -> assertFailure "Wrong type should fail validation"
  ]

-- | Unit test: Type signature to JSON schema conversion.
testTypeSignatureToJSONSchema :: TestTree
testTypeSignatureToJSONSchema = testGroup "Type Signature to JSON Schema"
  [ testCase "Convert simple type signature" $ do
      let typeSig = TypeSignature
            [ Parameter (Just "name") "Text" Nothing ]
            (Parameter Nothing "String" Nothing)
      let schema = typeSignatureToJSONSchema typeSig
      case schema of
        Object _ -> return ()  -- Should be object
        _ -> assertFailure "Schema should be JSON object"
  
  , testCase "Convert type signature with optional parameter" $ do
      let typeSig = TypeSignature
            [ Parameter (Just "name") "Text" (Just (String "world")) ]
            (Parameter Nothing "String" Nothing)
      let schema = typeSignatureToJSONSchema typeSig
      case schema of
        Object obj -> return ()  -- Should be object
        _ -> assertFailure "Schema should be JSON object"
  ]

-- | Unit test: Verify gram-hs generates identifiers for anonymous nodes.
testAnonymousNodeIdentifiers :: TestTree
testAnonymousNodeIdentifiers = testGroup "Anonymous Node Identifiers"
  [ testCase "Anonymous node (::String) gets generated identifier" $ do
      -- Parse an anonymous node to see how gram-hs handles it
      let parsed = case Gram.fromGram "(::String)" of
            Right p -> p
            Left _ -> error "Should parse anonymous node"
      -- Verify it has an identifier (gram-hs generates one)
      let subject = value parsed
      case identity subject of
        Symbol s -> T.null (T.pack s) @?= False  -- Should have generated identifier
        _ -> assertFailure "Should have Symbol identity"
      -- Verify it has String label
      Set.member "String" (labels subject) @?= True
  ]

-- | Unit test: Programmatic type signature construction.
testProgrammaticTypeSignature :: TestTree
testProgrammaticTypeSignature = testGroup "Programmatic Type Signature Construction"
  [ testCase "Create function type pattern programmatically" $ do
      -- Create (personName::Text {default:"world"})==>(::String) programmatically
      let typeSigPattern = createFunctionTypePattern 
            (Just "personName") 
            "Text" 
            (Just (VString "world")) 
            "String"
      -- Verify it's a relationship pattern with 2 elements
      length (elements typeSigPattern) @?= 2
      -- Verify it has FunctionType label
      let subject = value typeSigPattern
      Set.member "FunctionType" (labels subject) @?= True
      -- Verify source node has personName identifier
      let sourceNode = head (elements typeSigPattern)
      case identity (value sourceNode) of
        Symbol s -> s @?= "personName"
        _ -> assertFailure "Source node should have personName identifier"
      -- Verify target node has arbString identifier (universal String type)
      let targetNode = elements typeSigPattern !! 1
      case identity (value targetNode) of
        Symbol s -> s @?= "arbString"
        _ -> assertFailure "Target node should have arbString identifier"
  ]

tests :: TestTree
tests = testGroup "Tool Tests"
  [ testToolCreationWithTypeSignature
  , testToolImplCreation
  , testToolAccessors
  , testToolImplAccessors
  , testSchemaValidationValid
  , testSchemaValidationInvalid
  , testTypeSignatureToJSONSchema
  , testAnonymousNodeIdentifiers
  , testProgrammaticTypeSignature
  ]

