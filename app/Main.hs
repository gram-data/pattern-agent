{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified PatternAgent.LLM as LLM
import Control.Monad (when)
import Data.Aeson (encode, Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

-- | Default LLM model to use for testing
defaultModel :: LLM.Model
defaultModel = LLM.createModel "gpt-3.5-turbo" LLM.OpenAI

-- | Default system instruction
defaultSystemInstruction :: Text
defaultSystemInstruction = "You are a helpful assistant."

-- | Parse command line arguments and extract debug flag and message.
parseArgs :: [String] -> (Bool, Maybe String)
parseArgs args = go False Nothing args
  where
    go debug msg [] = (debug, msg)
    go debug msg ("--debug":rest) = go True msg rest
    go debug Nothing (m:rest) = go debug (Just m) rest
    go debug (Just _) (m:rest) = go debug (Just m) rest  -- Multiple messages, take last

main :: IO ()
main = do
  args <- getArgs
  let (debug, maybeMessage) = parseArgs args
  
  case maybeMessage of
    Nothing -> do
      putStrLn "Usage: pattern-agent [--debug] <message>"
      putStrLn ""
      putStrLn "Options:"
      putStrLn "  --debug    Show raw request/response JSON transcript"
      putStrLn ""
      putStrLn "Example: pattern-agent \"What is the capital of France?\""
      putStrLn "Example: pattern-agent --debug \"What is the capital of France?\""
      exitFailure
    Just message -> do
      let userMessage = pack message
      putStrLn "Connecting to OpenAI API..."
      putStrLn ""
      
      -- Create client (loads API key from OPENAI_API_KEY env var)
      clientResult <- LLM.createClientForModel defaultModel
      
      case clientResult of
        Left (LLM.ApiKeyNotFound err) -> do
          putStrLn "❌ Authentication Error:"
          putStrLn $ "   " ++ unpack err
          putStrLn ""
          putStrLn "Please set the OPENAI_API_KEY environment variable:"
          putStrLn "   export OPENAI_API_KEY=your-api-key-here"
          exitFailure
        Left (LLM.ApiKeyInvalid err) -> do
          putStrLn "❌ Invalid API Key:"
          putStrLn $ "   " ++ unpack err
          exitFailure
        Right client -> do
          putStrLn $ "📤 Sending message: " ++ message
          putStrLn ""
          
          -- Build request for debug output
          let request = LLM.buildRequest 
                defaultModel 
                defaultSystemInstruction 
                [LLM.Message "user" userMessage]
                Nothing  -- temperature (use default)
                Nothing  -- max_tokens (use default)
          
          -- Show raw request if debug mode
          when debug $ do
            putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            putStrLn "🔍 DEBUG: Raw Request JSON"
            putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            putStrLn $ TL.unpack $ decodeUtf8 $ encodePretty request
            putStrLn ""
          
          -- Call LLM API
          (result, rawResponse) <- LLM.sendRequestWithRawResponse client request
          
          -- Show raw response if debug mode
          when debug $ do
            case rawResponse of
              Just raw -> do
                putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                putStrLn "🔍 DEBUG: Raw Response JSON"
                putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                putStrLn $ TL.unpack $ decodeUtf8 raw
                putStrLn ""
              Nothing -> return ()
          
          case result of
            Left err -> do
              putStrLn "❌ Error:"
              putStrLn $ "   " ++ unpack err
              putStrLn ""
              putStrLn "This could be:"
              putStrLn "   - Network connectivity issue"
              putStrLn "   - OpenAI API error"
              putStrLn "   - Invalid API key"
              putStrLn "   - Rate limit exceeded"
              exitFailure
            Right response -> do
              putStrLn "✅ Response:"
              putStrLn ""
              putStrLn $ unpack (LLM.responseText response)
              putStrLn ""
              
              -- Show usage info if available
              case LLM.responseUsage response of
                Just usage -> do
                  putStrLn "📊 Token Usage:"
                  putStrLn $ "   Prompt tokens: " ++ show (LLM.usagePromptTokens usage)
                  putStrLn $ "   Completion tokens: " ++ show (LLM.usageCompletionTokens usage)
                  putStrLn $ "   Total tokens: " ++ show (LLM.usageTotalTokens usage)
                Nothing -> return ()
              
              putStrLn ""
              putStrLn $ "Model: " ++ unpack (LLM.responseModel response)
              exitSuccess
    _ -> do
      putStrLn "Error: Too many arguments"
      putStrLn "Usage: pattern-agent <message>"
      exitFailure
