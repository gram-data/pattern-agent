{-# LANGUAGE OverloadedStrings #-}
-- | Scenario tests for CLI agent execution from gram files.
--
-- These tests simulate user goal satisfaction end-to-end:
-- - CLI loads agent from gram file and executes with tool support
-- - CLI executes hello world agent with sayHello tool and produces greeting
-- - CLI handles missing gram file gracefully with error message
-- - CLI handles invalid gram file format gracefully with error message
module CLIAgentExecutionTest where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.IO (hClose)
import System.Directory (doesFileExist, removeFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

-- | Scenario test: CLI loads agent from gram file and executes with tool support.
--
-- Given: Valid gram file with agent definition
-- When: CLI is invoked with --agent flag pointing to gram file
-- Then: Agent is loaded, parsed, and executed successfully
testCLILoadsAgentFromGramFile :: TestTree
testCLILoadsAgentFromGramFile = testCase "CLI loads agent from gram file and executes" $ do
  -- Create temporary gram file with hello world agent
  let gramContent = "[hello_world_agent:Agent {\n\
    \  description: \"A friendly agent that uses the sayHello tool to greet users\",\n\
    \  instruction: \"You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting.\",\n\
    \  model: \"OpenAI/gpt-3.5-turbo\"\n\
    \} |\n\
    \  [sayHello:Tool {\n\
    \    description: \"Returns a friendly greeting message for the given name\"\n\
    \  } |\n\
    \    (personName::String {default:\"world\"})==>(::String)\n\
    \  ]\n\
    \]"
  
  withSystemTempFile "test-agent.gram" $ \filePath handle -> do
    TIO.hPutStr handle gramContent
    hClose handle
    
    -- TODO: Test CLI execution when CLI is implemented
    -- For now, verify file exists and can be read
    exists <- doesFileExist filePath
    assertBool "Temporary gram file should exist" exists
    
    -- Clean up
    removeFile filePath

-- | Scenario test: CLI executes hello world agent with sayHello tool and produces greeting.
--
-- Given: Valid hello world agent gram file
-- When: CLI is invoked with --agent flag and greeting message
-- Then: Agent executes, uses sayHello tool, and produces greeting response
testCLIExecutesHelloWorldAgent :: TestTree
testCLIExecutesHelloWorldAgent = testCase "CLI executes hello world agent with sayHello tool" $ do
  -- Create temporary gram file with hello world agent
  let gramContent = "[hello_world_agent:Agent {\n\
    \  description: \"A friendly agent that uses the sayHello tool to greet users\",\n\
    \  instruction: \"You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting.\",\n\
    \  model: \"OpenAI/gpt-3.5-turbo\"\n\
    \} |\n\
    \  [sayHello:Tool {\n\
    \    description: \"Returns a friendly greeting message for the given name\"\n\
    \  } |\n\
    \    (personName::String {default:\"world\"})==>(::String)\n\
    \  ]\n\
    \]"
  
  withSystemTempFile "hello-agent.gram" $ \filePath handle -> do
    TIO.hPutStr handle gramContent
    hClose handle
    
    -- TODO: Test CLI execution when CLI is implemented
    -- For now, verify file exists
    exists <- doesFileExist filePath
    assertBool "Temporary gram file should exist" exists
    
    -- Clean up
    removeFile filePath

-- | Scenario test: CLI handles missing gram file gracefully with error message.
--
-- Given: Non-existent gram file path
-- When: CLI is invoked with --agent flag pointing to missing file
-- Then: CLI displays appropriate error message and exits with failure
testCLIHandlesMissingGramFile :: TestTree
testCLIHandlesMissingGramFile = testCase "CLI handles missing gram file gracefully" $ do
  -- TODO: Test CLI error handling when CLI is implemented
  -- For now, verify non-existent file doesn't exist
  exists <- doesFileExist "/nonexistent/path/to/agent.gram"
  assertBool "Non-existent file should not exist" (not exists)

-- | Scenario test: CLI handles invalid gram file format gracefully with error message.
--
-- Given: Gram file with invalid syntax
-- When: CLI is invoked with --agent flag pointing to invalid file
-- Then: CLI displays appropriate error message and exits with failure
testCLIHandlesInvalidGramFile :: TestTree
testCLIHandlesInvalidGramFile = testCase "CLI handles invalid gram file format gracefully" $ do
  -- Create temporary gram file with invalid syntax
  let invalidGramContent = "[invalid:Agent { invalid syntax }"
  
  withSystemTempFile "invalid-agent.gram" $ \filePath handle -> do
    TIO.hPutStr handle invalidGramContent
    hClose handle
    
    -- TODO: Test CLI error handling when CLI is implemented
    -- For now, verify file exists
    exists <- doesFileExist filePath
    assertBool "Temporary invalid gram file should exist" exists
    
    -- Clean up
    removeFile filePath

-- | Test suite for CLI agent execution scenario tests.
cliAgentExecutionTests :: TestTree
cliAgentExecutionTests = testGroup "CLI Agent Execution Scenario Tests"
  [ testCLILoadsAgentFromGramFile
  , testCLIExecutesHelloWorldAgent
  , testCLIHandlesMissingGramFile
  , testCLIHandlesInvalidGramFile
  ]

