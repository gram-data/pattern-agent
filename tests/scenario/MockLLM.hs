{-# LANGUAGE OverloadedStrings #-}
-- | Mock LLM implementation for scenario tests.
--
-- This module provides a mock LLM that simulates observed real LLM behavior
-- without requiring API calls. Based on observations from integration tests.
--
-- Behavior simulated:
-- - Detects greetings and calls tools appropriately
-- - Extracts names from conversation history (e.g., "My name is Bob")
-- - Calls tools with correct parameter format
-- - Generates responses that incorporate tool results
-- - Maintains conversation history across turns
module MockLLM
  ( -- * Mock Client
    MockLLMClient(..)
  , createMockClient
    -- * Mock LLM Function
  , mockCallLLM
    -- * Name Extraction
  , extractNameFromHistory
    -- * Greeting Detection
  , isGreeting
  ) where

import PatternAgent.Runtime.LLM (LLMClient(..), LLMMessage(..), LLMResponse(..), FunctionCall(..), Usage(..))
import PatternAgent.Language.Core (Model)
import Data.Aeson (Value(..), object, (.=), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (listToMaybe)
import qualified Data.List as List

-- | Mock LLM client configuration.
-- Uses a special provider to distinguish from real clients.
data MockLLMClient = MockLLMClient
  { mockClientModel :: Model
  }
  deriving (Eq, Show)

-- | Create a mock LLM client.
createMockClient :: Model -> MockLLMClient
createMockClient model = MockLLMClient { mockClientModel = model }

-- | Mock LLM call function that simulates real LLM behavior.
--
-- Based on observed behavior from integration tests:
-- - Detects greetings and calls sayHello tool
-- - Extracts names from conversation history
-- - Generates appropriate responses
mockCallLLM
  :: MockLLMClient
  -> Model
  -> Text              -- ^ systemInstruction
  -> [LLMMessage]      -- ^ messages (conversation history)
  -> Maybe Double      -- ^ temperature (ignored in mock)
  -> Maybe Int         -- ^ maxTokens (ignored in mock)
  -> Maybe [Value]     -- ^ functions (tool definitions)
  -> IO (Either Text LLMResponse)
mockCallLLM mockClient model systemInstruction messages _temperature _maxTokens functions = do
  -- Check if there's already a function result in the conversation
  -- If so, generate a final response incorporating the tool result
  let hasFunctionResult = any (\msg -> llmMessageRole msg == "function") messages
  if hasFunctionResult then do
    -- There's a function result - generate final response
    let lastFunctionMsg = List.find (\msg -> llmMessageRole msg == "function") (reverse messages)
    let toolResult = maybe "" llmMessageContent lastFunctionMsg
    let extractedName = extractNameFromHistory messages
    let responseText = case extractedName of
          Just name -> "Hello! " <> toolResult <> " How can I help you today, " <> name <> "?"
          Nothing -> "Hello! " <> toolResult <> " How can I help you today?"
    return $ Right $ LLMResponse
      { responseText = responseText
      , responseModel = T.pack $ show model
      , responseUsage = Just $ Usage 5 10 15
      , responseFunctionCall = Nothing
      }
  else do
    -- No function result yet - check if we should call a tool
    -- Get the last user message (if any)
    let lastUserMessage = List.find (\msg -> llmMessageRole msg == "user") (reverse messages)
    let lastUserContent = maybe "" llmMessageContent lastUserMessage
    
    -- Check if this is a greeting
    let isGreetingMsg = isGreeting lastUserContent
    
    -- Extract name from conversation history
    let extractedName = extractNameFromHistory messages
    
    -- Determine if we should call a tool
    case (isGreetingMsg, functions) of
      (True, Just funcs) | not (null funcs) -> do
        -- Call the first available tool (sayHello)
        let toolName = getFirstToolName funcs
        let toolArgs = createToolArgs toolName extractedName
        
        return $ Right $ LLMResponse
          { responseText = ""  -- Empty text when function call is present
          , responseModel = T.pack $ show model
          , responseUsage = Just $ Usage 10 5 15  -- Mock usage
          , responseFunctionCall = Just $ FunctionCall
              { functionCallName = toolName
              , functionCallArguments = toolArgs
              }
          }
      
      (False, _) -> do
        -- Not a greeting - generate conversational response
        let responseText = generateConversationalResponse lastUserContent extractedName messages
        return $ Right $ LLMResponse
          { responseText = responseText
          , responseModel = T.pack $ show model
          , responseUsage = Just $ Usage 5 10 15
          , responseFunctionCall = Nothing
          }
      
      (True, Nothing) -> do
        -- Greeting but no tools available - respond conversationally
        let responseText = "Hello! How can I help you?"
        return $ Right $ LLMResponse
          { responseText = responseText
          , responseModel = T.pack $ show model
          , responseUsage = Just $ Usage 5 10 15
          , responseFunctionCall = Nothing
          }
  
  where
    -- Get the first tool name from functions array
    getFirstToolName :: [Value] -> Text
    getFirstToolName funcs = case funcs of
      [] -> "sayHello"  -- Default
      (func:_) -> case func of
        Object obj -> case KM.lookup (K.fromText "name") obj of
          Just (String name) -> name
          _ -> "sayHello"
        _ -> "sayHello"
    
    -- Create tool arguments JSON string
    -- Note: functionCallArguments should be a JSON string (not double-encoded)
    createToolArgs :: Text -> Maybe Text -> Text
    createToolArgs toolName name = case toolName of
      "sayHello" -> case name of
        Just n -> T.pack $ TL.unpack $ TLE.decodeUtf8 $ encode $ object ["personName" .= n]
        Nothing -> "{}"  -- Will use default
      _ -> "{}"
    
    -- Generate conversational response
    generateConversationalResponse :: Text -> Maybe Text -> [LLMMessage] -> Text
    generateConversationalResponse userContent name _history
      | T.isInfixOf "name is" userContent || T.isInfixOf "I'm" userContent || T.isInfixOf "I am" userContent =
          case name of
            Just n -> "Nice to meet you, " <> n <> "! How can I help you?"
            Nothing -> "Nice to meet you! How can I help you?"
      | T.isInfixOf "weather" userContent =
          "Yes, the weather is nice! How can I assist you today?"
      | otherwise =
          case name of
            Just n -> "I understand, " <> n <> ". How can I help you?"
            Nothing -> "I understand. How can I help you?"

-- | Extract name from conversation history.
--
-- Looks for patterns like:
-- - "My name is X"
-- - "I'm X"
-- - "I am X"
-- - "Call me X"
extractNameFromHistory :: [LLMMessage] -> Maybe Text
extractNameFromHistory messages = 
  -- Search through all user messages for name patterns
  let userMessages = filter (\msg -> llmMessageRole msg == "user") messages
      extractFromMessage msg = extractNameFromText (llmMessageContent msg)
  in listToMaybe $ concatMap (maybeToList . extractFromMessage) userMessages
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]
    
    extractNameFromText :: Text -> Maybe Text
    extractNameFromText content = 
      -- Pattern: "My name is Bob"
      case T.splitOn "My name is " content of
        (_:rest:_) -> Just $ T.strip $ T.takeWhile (/= ' ') rest
        _ -> case T.splitOn "my name is " content of
          (_:rest:_) -> Just $ T.strip $ T.takeWhile (/= ' ') rest
          _ -> case T.splitOn "I'm " content of
            (_:rest:_) -> Just $ T.strip $ T.takeWhile (/= ' ') rest
            _ -> case T.splitOn "I am " content of
              (_:rest:_) -> Just $ T.strip $ T.takeWhile (/= ' ') rest
              _ -> case T.splitOn "call me " content of
                (_:rest:_) -> Just $ T.strip $ T.takeWhile (/= ' ') rest
                _ -> Nothing

-- | Check if a message is a greeting.
--
-- Detects common greeting patterns:
-- - "hello", "hi", "hey", "greetings"
-- - "oh, hello btw", "hello there", etc.
isGreeting :: Text -> Bool
isGreeting content =
  let lower = T.toLower content
      greetingWords = ["hello", "hi", "hey", "greetings", "good morning", "good afternoon", "good evening"]
  in any (`T.isInfixOf` lower) greetingWords

