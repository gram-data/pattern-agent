{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for tool creation and tool implementation.
module ToolTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Language.Core (Tool, createTool, toolName, toolDescription, toolTypeSignature, toolSchema)
import PatternAgent.Runtime.ToolLibrary (ToolImpl(..), createToolImpl, toolImplName, toolImplDescription, toolImplSchema, toolImplInvoke, emptyToolLibrary, registerTool, lookupTool, validateToolArgs, bindTool)
import HelloWorldExample (sayHello, sayHelloImpl, helloWorldToolLibrary)
import PatternAgent.Language.TypeSignature (extractTypeSignatureFromPattern, typeSignatureToJSONSchema, TypeSignature(..), Parameter(..), createTypeNode, createFunctionTypePattern)
import PatternAgent.Language.Core
import PatternAgent.Language.Serialization (parseAgent, parseTool)
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
      let typeSig = parseTypeSig "(personName::String)==>(::String)"
      let result = createTool "sayHello" "Greeting tool" typeSig
      case result of
        Right tool -> do
          view toolName tool @?= "sayHello"
          view toolDescription tool @?= "Greeting tool"
        Left err -> assertFailure $ "Expected Right Tool, got Left: " ++ T.unpack err
  
  , testCase "Create tool with optional parameter" $ do
      let typeSig = parseTypeSig "(personName::String {default:\"world\"})==>(::String)"
      let result = createTool "greet" "Greet with optional name" typeSig
      case result of
        Right tool -> view toolName tool @?= "greet"
        Left err -> assertFailure $ "Expected Right Tool, got Left: " ++ T.unpack err
  
  , testCase "Tool name cannot be empty" $ do
      let typeSig = parseTypeSig "(name::String)==>(::String)"
      let result = createTool "" "Description" typeSig
      case result of
        Left err -> T.isInfixOf "empty" err @?= True
        Right _ -> assertFailure "Expected Left error for empty name"
  
  , testCase "Tool description cannot be empty" $ do
      let typeSig = parseTypeSig "(name::String)==>(::String)"
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
      let typeSig = parseTypeSig "(x::String)==>(::String)"
      let result = createTool "myTool" "Description" typeSig
      case result of
        Right tool -> view toolName tool @?= "myTool"
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  
  , testCase "toolDescription lens" $ do
      let typeSig = parseTypeSig "(x::String)==>(::String)"
      let result = createTool "myTool" "My description" typeSig
      case result of
        Right tool -> view toolDescription tool @?= "My description"
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  
  , testCase "toolTypeSignature lens" $ do
      let typeSig = parseTypeSig "(name::String)==>(::String)"
      let result = createTool "myTool" "Description" typeSig
      case result of
        Right tool -> do
          let sig = view toolTypeSignature tool
          T.null sig @?= False  -- Should have type signature
        Left err -> assertFailure $ "Tool creation failed: " ++ T.unpack err
  
  , testCase "toolSchema lens" $ do
      let typeSig = parseTypeSig "(name::String)==>(::String)"
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
            [ Parameter (Just "name") "String" Nothing ]
            (Parameter Nothing "String" Nothing)
      let schema = typeSignatureToJSONSchema typeSig
      case schema of
        Object _ -> return ()  -- Should be object
        _ -> assertFailure "Schema should be JSON object"
  
  , testCase "Convert type signature with optional parameter" $ do
      let typeSig = TypeSignature
            [ Parameter (Just "name") "String" (Just (String "world")) ]
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

-- | Unit test: Verify parseAgent normalizes tool type signatures.
--
-- Regression test for issue where tool type signatures weren't normalized
-- when parsing agents from gram files, causing empty schemas.
testParseAgentNormalizesToolTypeSignatures :: TestTree
testParseAgentNormalizesToolTypeSignatures = testGroup "parseAgent Normalizes Tool Type Signatures"
  [ testCase "parseAgent normalizes tool type signature with FunctionType label" $ do
      let gramContent = T.unlines
            [ "[hello_world_agent:Agent {"
            , "  description: \"A friendly agent\","
            , "  instruction: \"You are a friendly assistant.\","
            , "  model: \"OpenAI/gpt-4o-mini\""
            , "} |"
            , "  [sayHello:Tool {"
            , "    description: \"Returns a friendly greeting message for the given name\""
            , "  } |"
            , "    (personName::String {default:\"world\"})==>(::String)"
            , "  ]"
            , "]"
            ]
      
      case parseAgent gramContent of
        Right agent -> do
          -- Verify agent was parsed
          view agentName agent @?= "hello_world_agent"
          
          -- Verify agent has tool
          let tools = view agentTools agent
          length tools @?= 1
          
          let tool = head tools
          view toolName tool @?= "sayHello"
          
          -- Verify tool has type signature element
          let toolElements = elements tool
          length toolElements @?= 1
          
          -- Verify type signature element has FunctionType label (normalization worked)
          let typeSigElem = head toolElements
          let typeSigSubject = value typeSigElem
          Set.member "FunctionType" (labels typeSigSubject) @?= True
          
          -- Verify type signature can be extracted
          case extractTypeSignatureFromPattern typeSigElem of
            Right typeSig -> do
              -- Verify it has the correct parameter
              length (typeParams typeSig) @?= 1
              let param = head (typeParams typeSig)
              paramName param @?= Just "personName"
              paramType param @?= "String"
              paramDefault param @?= Just (String "world")
            Left err -> assertFailure $ "Failed to extract type signature: " ++ T.unpack err
          
        Left err -> assertFailure $ "Failed to parse agent: " ++ T.unpack err
  
  , testCase "parseAgent produces tool schema with correct parameters" $ do
      let gramContent = T.unlines
            [ "[test_agent:Agent {"
            , "  description: \"Test agent\","
            , "  instruction: \"Test instruction\","
            , "  model: \"OpenAI/gpt-4o-mini\""
            , "} |"
            , "  [greet:Tool {"
            , "    description: \"Greet someone\""
            , "  } |"
            , "    (personName::String {default:\"world\"})==>(::String)"
            , "  ]"
            , "]"
            ]
      
      case parseAgent gramContent of
        Right agent -> do
          let tools = view agentTools agent
          length tools @?= 1
          
          let tool = head tools
          let schema = view toolSchema tool
          
          -- Verify schema is an object and contains expected structure
          -- We'll check by converting to string and looking for key indicators
          let schemaStr = show schema
          -- Verify schema has properties
          assertBool ("Schema should contain 'properties': " ++ schemaStr) 
            (T.isInfixOf "properties" (T.pack schemaStr))
          -- Verify schema has personName
          assertBool ("Schema should contain 'personName': " ++ schemaStr)
            (T.isInfixOf "personName" (T.pack schemaStr))
          -- Verify schema has type string for personName
          assertBool ("Schema should contain 'string' type: " ++ schemaStr)
            (T.isInfixOf "string" (T.pack schemaStr))
          -- Verify schema has default world
          assertBool ("Schema should contain default 'world': " ++ schemaStr)
            (T.isInfixOf "world" (T.pack schemaStr))
        Left err -> assertFailure $ "Failed to parse agent: " ++ T.unpack err
  
  , testCase "parseTool also normalizes type signatures" $ do
      let gramContent = T.unlines
            [ "[sayHello:Tool {"
            , "  description: \"Returns a friendly greeting message for the given name\""
            , "} |"
            , "  (personName::String {default:\"world\"})==>(::String)"
            , "]"
            ]
      
      case parseTool gramContent of
        Right tool -> do
          -- Verify tool has type signature element
          let toolElements = elements tool
          length toolElements @?= 1
          
          -- Verify type signature element has FunctionType label
          let typeSigElem = head toolElements
          let typeSigSubject = value typeSigElem
          Set.member "FunctionType" (labels typeSigSubject) @?= True
          
          -- Verify schema extraction works
          let schema = view toolSchema tool
          let schemaStr = show schema
          -- Verify schema has properties and personName
          assertBool ("Schema should contain 'properties': " ++ schemaStr)
            (T.isInfixOf "properties" (T.pack schemaStr))
          assertBool ("Schema should contain 'personName': " ++ schemaStr)
            (T.isInfixOf "personName" (T.pack schemaStr))
        Left err -> assertFailure $ "Failed to parse tool: " ++ T.unpack err
  ]

-- | Unit test: Programmatic type signature construction.
testProgrammaticTypeSignature :: TestTree
testProgrammaticTypeSignature = testGroup "Programmatic Type Signature Construction"
  [ testCase "Create function type pattern programmatically" $ do
      -- Create (personName::String {default:"world"})==>(::String) programmatically
      let typeSigPattern = createFunctionTypePattern 
            (Just "personName") 
            "String" 
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

-- | Unit test: bindTool function with sayHello tool (real example).
--
-- This test specifically tests binding the sayHello Tool to sayHelloImpl
-- to debug the integration test failure.
testBindToolSayHello :: TestTree
testBindToolSayHello = testGroup "bindTool with sayHello"
  [ testCase "bindTool binds sayHello Tool to sayHelloImpl" $ do
      -- Verify sayHello tool exists
      view toolName sayHello @?= "sayHello"
      
      -- Verify sayHelloImpl exists
      toolImplName sayHelloImpl @?= "sayHello"
      
      -- Test binding sayHello Tool to sayHelloImpl in library
      case bindTool sayHello helloWorldToolLibrary of
        Just boundImpl -> do
          -- Verify binding succeeded
          toolImplName boundImpl @?= "sayHello"
          toolImplDescription boundImpl @?= "Returns a friendly greeting message for the given name"
          
          -- Verify schemas match (this is likely where it's failing)
          let toolSchemaValue = view toolSchema sayHello
          let implSchemaValue = toolImplSchema boundImpl
          -- Print schemas for debugging if they don't match
          if toolSchemaValue == implSchemaValue
            then return ()
            else do
              putStrLn $ "Tool schema: " ++ show toolSchemaValue
              putStrLn $ "Impl schema: " ++ show implSchemaValue
              assertFailure "Schemas do not match"
        Nothing -> do
          -- Binding failed - print diagnostic information
          putStrLn $ "Tool name: " ++ T.unpack (view toolName sayHello)
          putStrLn $ "Tool description: " ++ T.unpack (view toolDescription sayHello)
          putStrLn $ "Tool schema: " ++ show (view toolSchema sayHello)
          putStrLn $ "Impl name: " ++ T.unpack (toolImplName sayHelloImpl)
          putStrLn $ "Impl description: " ++ T.unpack (toolImplDescription sayHelloImpl)
          putStrLn $ "Impl schema: " ++ show (toolImplSchema sayHelloImpl)
          
          -- Check if tool is in library
          case lookupTool "sayHello" helloWorldToolLibrary of
            Just impl -> do
              putStrLn "Tool is in library"
              putStrLn $ "Library impl name: " ++ T.unpack (toolImplName impl)
              putStrLn $ "Library impl description: " ++ T.unpack (toolImplDescription impl)
              putStrLn $ "Library impl schema: " ++ show (toolImplSchema impl)
            Nothing -> putStrLn "Tool NOT found in library"
          
          -- DIAGNOSIS: The binding failed because toolSchema returns empty schema
          -- Root cause: extractTypeSignatureFromPattern is not yet implemented
          -- It returns Left "Type signature extraction from Pattern not yet fully implemented"
          -- This causes toolSchema to return empty schema, which doesn't match implSchema
          assertFailure "bindTool returned Nothing - binding failed (extractTypeSignatureFromPattern not implemented)"
  
  , testCase "bindTool schema comparison details" $ do
      -- Extract schemas for detailed comparison
      let toolSchemaValue = view toolSchema sayHello
      let implSchemaValue = toolImplSchema sayHelloImpl
      
      -- Print both schemas for manual inspection
      putStrLn "\n=== Tool Schema (from Pattern) ==="
      putStrLn $ show toolSchemaValue
      putStrLn "\n=== Impl Schema (from typeSignatureToJSONSchema) ==="
      putStrLn $ show implSchemaValue
      putStrLn "\n=== Schema Comparison ==="
      putStrLn $ "Schemas equal: " ++ show (toolSchemaValue == implSchemaValue)
      
      -- This test always passes - it's just for debugging
      return ()
  ]

tests :: TestTree
tests = testGroup "Tool Tests"
  [ testBindToolSayHello
  , testToolCreationWithTypeSignature
  , testToolImplCreation
  , testToolAccessors
  , testToolImplAccessors
  , testSchemaValidationValid
  , testSchemaValidationInvalid
  , testTypeSignatureToJSONSchema
  , testAnonymousNodeIdentifiers
  , testProgrammaticTypeSignature
  , testParseAgentNormalizesToolTypeSignatures
  ]

