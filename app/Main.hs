{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified PatternAgent.Runtime.LLM as LLM
import PatternAgent.Language.Core (Model, createModel, Provider(OpenAI), Agent, agentTools, toolName, toolDescription, toolSchema)
import PatternAgent.Language.Serialization (parseAgent)
import PatternAgent.Runtime.Execution (executeAgentWithLibrary, AgentError(..), AgentResponse(..), ToolInvocation(..))
import PatternAgent.Runtime.Context (emptyContext)
import PatternAgent.Runtime.BuiltinTools (createToolLibraryFromAgent)
import PatternAgent.Runtime.Logging (logDebug, logInfo, logError, logDebugJSON, loggerCLI)
import PatternAgent.Runtime.Execution (ToolInvocation(..))
import Data.Aeson (Value(..))
import Control.Monad (when)
import Control.Lens (view)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (encode)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, catchIOError)

-- | Default LLM model to use for testing
defaultModel :: Model
defaultModel = createModel "gpt-4o-mini" OpenAI

-- | Default system instruction
defaultSystemInstruction :: Text
defaultSystemInstruction = "You are a helpful assistant."

-- | Simple icon-based output functions for normal mode
putUserMessage :: Text -> IO ()
putUserMessage msg = putStrLn $ "💬 " ++ T.unpack msg

putAgentResponse :: Text -> IO ()
putAgentResponse msg = putStrLn $ "🤖 " ++ T.unpack msg

putFunctionCall :: Text -> Text -> IO ()
putFunctionCall toolName args = putStrLn $ "🛠️  " ++ T.unpack toolName ++ "(" ++ T.unpack args ++ ")"

-- | Command line mode.
data CLIMode
  = StandardMode (Maybe String)  -- Standard mode with optional message
  | AgentMode String String  -- Agent mode: gram file, message

-- | Parse command line arguments and extract mode, debug flag, and message.
parseArgs :: [String] -> Either String (CLIMode, Bool)
parseArgs args = go False Nothing Nothing args
  where
    go debug msg agentFile [] = case (agentFile, msg) of
      (Just file, Just m) -> Right (AgentMode file m, debug)
      (Just file, Nothing) -> Left "Missing message for --agent mode"
      (Nothing, Just m) -> Right (StandardMode (Just m), debug)
      (Nothing, Nothing) -> Right (StandardMode Nothing, debug)
    go _ msg agentFile ("--agent":file:rest) = go False msg (Just file) rest
    go _ _ _ ("--agent":[]) = Left "Missing file path for --agent flag"
    go _ msg agentFile ("--debug":rest) = go True msg agentFile rest
    go debug Nothing agentFile (m:rest) = go debug (Just m) agentFile rest
    go debug (Just _) agentFile (m:rest) = go debug (Just m) agentFile rest  -- Multiple messages, take last

-- | Load gram file contents from disk.
loadGramFile :: FilePath -> IO (Either String Text)
loadGramFile filePath = do
  result <- catchIOError (Right <$> TIO.readFile filePath) $ \err ->
    if isDoesNotExistError err
      then return $ Left $ "File not found: " ++ filePath
      else return $ Left $ "Error reading file: " ++ show err
  return result

-- | Parse agent from gram file contents.
parseAgentFromGram :: Text -> Either String Agent
parseAgentFromGram gramContent = case parseAgent gramContent of
  Right agent -> Right agent
  Left err -> Left $ "Failed to parse agent from gram file: " ++ T.unpack err

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      printUsage
      exitFailure
    Right (mode, debug) -> case mode of
      StandardMode maybeMessage -> handleStandardMode debug maybeMessage
      AgentMode gramFile message -> handleAgentMode gramFile message debug

-- | Handle standard mode (direct LLM interaction).
handleStandardMode :: Bool -> Maybe String -> IO ()
handleStandardMode debug maybeMessage = case maybeMessage of
  Nothing -> do
    printUsage
    exitFailure
  Just message -> do
      let userMessage = pack message
      putStrLn "Connecting to OpenAI API..."
      putStrLn ""
      
      -- Create client (loads API key from OPENAI_API_KEY env var)
      clientResult <- LLM.createClientForModel defaultModel
      
      case clientResult of
        Left (LLM.ApiKeyNotFound err) -> do
          logError loggerCLI $ "Authentication Error: " <> err
          logError loggerCLI "Please set the OPENAI_API_KEY environment variable: export OPENAI_API_KEY=your-api-key-here"
          exitFailure
        Left (LLM.ApiKeyInvalid err) -> do
          logError loggerCLI $ "Invalid API Key: " <> err
          exitFailure
        Right client -> do
          if debug
            then logInfo loggerCLI $ "Sending message: " <> userMessage
            else putUserMessage userMessage
          
          -- Build request for debug output
          let request = LLM.buildRequest 
                defaultModel 
                defaultSystemInstruction 
                [LLM.LLMMessage "user" userMessage Nothing]
                Nothing  -- temperature (use default)
                Nothing  -- max_tokens (use default)
                Nothing  -- functions (no tools in standard mode)
          
          -- Show raw request if debug mode
          when debug $ do
            logDebug debug loggerCLI "Raw Request JSON:"
            logDebug debug loggerCLI $ T.pack $ TL.unpack $ decodeUtf8 $ encodePretty request
          
          -- Call LLM API
          (result, rawResponse) <- LLM.sendRequestWithRawResponse client request
          
          -- Show raw response if debug mode
          when debug $ do
            case rawResponse of
              Just raw -> do
                logDebug debug loggerCLI "Raw Response JSON:"
                logDebug debug loggerCLI $ T.pack $ TL.unpack $ decodeUtf8 raw
              Nothing -> return ()
          
          case result of
            Left err -> do
              logError loggerCLI $ "LLM API Error: " <> err
              logError loggerCLI "Possible causes: Network connectivity issue, OpenAI API error, Invalid API key, Rate limit exceeded"
              exitFailure
            Right response -> do
              if debug
                then logInfo loggerCLI $ "Response: " <> LLM.responseText response
                else putAgentResponse (LLM.responseText response)
              
              -- Show usage info if available
              when debug $ do
                case LLM.responseUsage response of
                  Just usage -> do
                    logDebug debug loggerCLI $ "Token Usage: prompt=" <> T.pack (show (LLM.usagePromptTokens usage)) <> ", completion=" <> T.pack (show (LLM.usageCompletionTokens usage)) <> ", total=" <> T.pack (show (LLM.usageTotalTokens usage))
                  Nothing -> return ()
                
                logDebug debug loggerCLI $ "Model: " <> LLM.responseModel response
              exitSuccess

-- | Handle agent mode (load agent from gram file and execute).
handleAgentMode :: FilePath -> String -> Bool -> IO ()
handleAgentMode gramFile message debug = do
  -- Load gram file
  gramContentResult <- loadGramFile gramFile
  case gramContentResult of
    Left err -> do
      logError loggerCLI $ "Error loading gram file: " <> T.pack err
      exitFailure
    Right gramContent -> do
      logDebug debug loggerCLI "Gram file loaded successfully"
      -- Parse agent from gram
      case parseAgentFromGram gramContent of
        Left err -> do
          logError loggerCLI $ "Error parsing agent: " <> T.pack err
          exitFailure
        Right agent -> do
          logDebug debug loggerCLI "Agent parsed successfully from gram file"
          -- Validate that gram file contains exactly one Agent pattern
          -- (parseAgent already validates this by checking for Agent label)
          
          -- Create tool library from agent's tools using built-in implementations
          case createToolLibraryFromAgent agent of
            Left err -> do
              logError loggerCLI $ "Error creating tool library: " <> err
              exitFailure
            Right toolLibrary -> do
              -- Execute agent with tool library
              when debug $ do
                logInfo loggerCLI $ "Executing agent from: " <> T.pack gramFile
                logInfo loggerCLI $ "User message: " <> pack message
              logDebug debug loggerCLI $ "Agent execution: tools=" <> T.pack (show (length (view agentTools agent)))
              
              let userMessage = pack message
              if not debug
                then putUserMessage userMessage
                else return ()
              
              result <- executeAgentWithLibrary debug agent userMessage emptyContext toolLibrary
              
              case result of
                Left (LLMAPIError err) -> do
                  logError loggerCLI $ "LLM API Error: " <> err
                  exitFailure
                Left (ToolError err) -> do
                  logError loggerCLI $ "Tool Error: " <> err
                  exitFailure
                Left (ValidationError err) -> do
                  logError loggerCLI $ "Validation Error: " <> err
                  exitFailure
                Left (ConfigurationError err) -> do
                  logError loggerCLI $ "Configuration Error: " <> err
                  logError loggerCLI "Please set the OPENAI_API_KEY environment variable: export OPENAI_API_KEY=your-api-key-here"
                  exitFailure
                Right response -> do
                  -- Show tool calls in normal mode
                  when (not debug) $ do
                    mapM_ (\invocation -> do
                      -- Convert arguments to JSON string
                      let argsJson = TL.unpack $ decodeUtf8 $ encode (invocationArgs invocation)
                      putFunctionCall (invocationToolName invocation) (T.pack argsJson)
                      ) (responseToolsUsed response)
                  
                  -- Show agent response
                  if debug
                    then logInfo loggerCLI $ "Agent response: " <> responseContent response
                    else putAgentResponse (responseContent response)
                  
                  -- Log tools used if any (debug mode only)
                  when debug $ do
                    when (not (null (responseToolsUsed response))) $ do
                      logDebug debug loggerCLI "Tools used:"
                      mapM_ (\invocation -> do
                        logDebug debug loggerCLI $ "  Tool: " <> invocationToolName invocation
                        case invocationResult invocation of
                          Right result -> logDebug debug loggerCLI $ "    Result: " <> T.pack (show result)
                          Left err -> logError loggerCLI $ "    Error: " <> err
                        ) (responseToolsUsed response)
                  
                  exitSuccess

-- | Print usage message.
printUsage :: IO ()
printUsage = do
  putStrLn "Usage: pattern-agent [--agent <gram-file>] [--debug] <message>"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --agent <file>  Load agent from gram file and execute with tool support"
  putStrLn "  --debug         Show raw request/response JSON transcript"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  pattern-agent \"What is the capital of France?\""
  putStrLn "  pattern-agent --debug \"What is the capital of France?\""
  putStrLn "  pattern-agent --agent helloAgent.gram \"Hello!\""
  putStrLn "  pattern-agent --agent helloAgent.gram --debug \"Hello!\""
