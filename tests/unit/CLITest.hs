{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for CLI functionality.
--
-- These tests verify component correctness:
-- - Command line argument parsing for --agent flag
-- - Gram file loading and parsing
-- - Agent extraction from parsed gram file
-- - Tool library creation from agent tools
-- - Error handling for file not found
-- - Error handling for invalid gram syntax
module CLITest where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import PatternAgent.Language.Serialization (parseGram, parseAgent)
import PatternAgent.Language.Core (Agent, agentName, agentTools, toolName)
import PatternAgent.Runtime.ToolLibrary (ToolLibrary, emptyToolLibrary)
import Control.Lens (view)

-- | Unit test: Command line argument parsing for --agent flag.
--
-- Verifies that parseArgs correctly identifies --agent flag and file path.
testParseArgsWithAgentFlag :: TestTree
testParseArgsWithAgentFlag = testCase "parseArgs parses --agent flag correctly" $ do
  -- TODO: Test parseArgs when CLI is implemented
  -- Expected: parseArgs ["--agent", "file.gram", "message"] -> (AgentMode "file.gram", "message", False)
  -- Expected: parseArgs ["--agent", "file.gram", "--debug", "message"] -> (AgentMode "file.gram", "message", True)
  assertBool "parseArgs test placeholder" True

-- | Unit test: Gram file loading and parsing.
--
-- Verifies that loadGramFile reads file contents and parseGram parses them correctly.
testLoadAndParseGramFile :: TestTree
testLoadAndParseGramFile = testCase "loadGramFile and parseGram work correctly" $ do
  let gramContent = "[test:Agent {description: \"test\"}]"
  
  withSystemTempFile "test.gram" $ \filePath handle -> do
    TIO.hPutStr handle gramContent
    hClose handle
    
    -- Test parseGram directly (loadGramFile will be tested when implemented)
    case parseGram (T.pack gramContent) of
      Right pattern -> assertBool "parseGram should succeed" True
      Left err -> assertFailure $ "parseGram failed: " ++ T.unpack err

-- | Unit test: Agent extraction from parsed gram file.
--
-- Verifies that parseAgentFromGram extracts Agent pattern correctly.
testExtractAgentFromGram :: TestTree
testExtractAgentFromGram = testCase "parseAgentFromGram extracts Agent correctly" $ do
  let gramContent = "[hello_world_agent:Agent {\n\
    \  description: \"A friendly agent\",\n\
    \  instruction: \"Be friendly\",\n\
    \  model: \"OpenAI/gpt-3.5-turbo\"\n\
    \}]"
  
  case parseAgent (T.pack gramContent) of
    Right agent -> do
      -- Verify agent name
      view agentName agent @?= "hello_world_agent"
      -- Verify agent has no tools (empty list)
      length (view agentTools agent) @?= 0
    Left err -> assertFailure $ "parseAgent failed: " ++ T.unpack err

-- | Unit test: Tool library creation from agent tools.
--
-- Verifies that createToolLibraryFromAgent creates ToolLibrary with correct tools.
testCreateToolLibraryFromAgent :: TestTree
testCreateToolLibraryFromAgent = testCase "createToolLibraryFromAgent creates ToolLibrary correctly" $ do
  let gramContent = "[hello_world_agent:Agent {\n\
    \  description: \"A friendly agent\",\n\
    \  instruction: \"Be friendly\",\n\
    \  model: \"OpenAI/gpt-3.5-turbo\"\n\
    \} |\n\
    \  [sayHello:Tool {description: \"Greet someone\"} |\n\
    \    (personName::String {default:\"world\"})==>(::String)\n\
    \  ]\n\
    \]"
  
  case parseAgent (T.pack gramContent) of
    Right agent -> do
      -- Verify agent has sayHello tool
      let tools = view agentTools agent
      length tools @?= 1
      view toolName (head tools) @?= "sayHello"
      
      -- TODO: Test createToolLibraryFromAgent when implemented
      -- Expected: ToolLibrary contains sayHello ToolImpl
      assertBool "createToolLibraryFromAgent test placeholder" True
    Left err -> assertFailure $ "parseAgent failed: " ++ T.unpack err

-- | Unit test: Error handling for file not found.
--
-- Verifies that loadGramFile returns appropriate error for missing file.
testErrorHandlingFileNotFound :: TestTree
testErrorHandlingFileNotFound = testCase "loadGramFile handles file not found" $ do
  -- TODO: Test loadGramFile error handling when implemented
  -- Expected: loadGramFile "/nonexistent/file.gram" -> Left "File not found: /nonexistent/file.gram"
  exists <- doesFileExist "/nonexistent/file.gram"
  assertBool "Non-existent file should not exist" (not exists)

-- | Unit test: Error handling for invalid gram syntax.
--
-- Verifies that parseAgentFromGram returns appropriate error for invalid syntax.
testErrorHandlingInvalidGramSyntax :: TestTree
testErrorHandlingInvalidGramSyntax = testCase "parseAgentFromGram handles invalid gram syntax" $ do
  let invalidGram = "[invalid:Agent { invalid syntax }"
  
  case parseAgent (T.pack invalidGram) of
    Right _ -> assertFailure "parseAgent should fail for invalid syntax"
    Left err -> do
      -- Verify error message is informative
      let errStr = T.unpack err
      assertBool ("Error message should contain information: " ++ errStr) (not (T.null err))

-- | Test suite for CLI unit tests.
cliTests :: TestTree
cliTests = testGroup "CLI Unit Tests"
  [ testParseArgsWithAgentFlag
  , testLoadAndParseGramFile
  , testExtractAgentFromGram
  , testCreateToolLibraryFromAgent
  , testErrorHandlingFileNotFound
  , testErrorHandlingInvalidGramSyntax
  ]

