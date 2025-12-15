{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | LLM API client (Runtime).
--
-- This module provides a standalone client for sending requests to LLM APIs.
-- It handles provider configuration, API key management, HTTP requests,
-- and response parsing.
--
-- This is part of the runtime implementation (Haskell-specific).
module PatternAgent.Runtime.LLM
  ( -- * Types
    LLMClient(..)
  , LLMRequest(..)
  , LLMResponse(..)
  , LLMMessage(..)
  , FunctionCall(..)
  , Usage(..)
    -- * Re-exported from Language
  , Provider(..)
  , Model(..)
  , createModel
    -- * Client Creation
  , createOpenAIClient
  , createClientForModel
    -- * API Key Management
  , loadApiKeyFromEnv
  , ApiKeyError(..)
    -- * LLM API Calls
  , callLLM
  , sendRequest
  , sendRequestWithRawResponse
    -- * Request Building
  , buildRequest
  , buildOpenAIRequest
    -- * Response Parsing
  , parseResponse
  , parseOpenAIResponse
    -- * Response Accessors
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types (status200)
import PatternAgent.Language.Core (Provider(..), Model(..), createModel)
import PatternAgent.Env (loadEnvFile)
import System.Environment (lookupEnv)

-- | LLM Client configuration.
data LLMClient = LLMClient
  { clientProvider :: Provider
  , clientApiKey :: Text
  , clientBaseURL :: Text
  }
  deriving (Eq, Show, Generic)

-- | LLM API request payload.
data LLMRequest = LLMRequest
  { requestModel :: Text
  , requestMessages :: [LLMMessage]
  , requestTemperature :: Maybe Double
  , requestMaxTokens :: Maybe Int
  , requestFunctions :: Maybe [Value]  -- ^ OpenAI functions array (tool definitions)
  }
  deriving (Eq, Show, Generic)

-- | Message in LLM request (runtime format, different from Language).
data LLMMessage = LLMMessage
  { llmMessageRole :: Text  -- "user", "assistant", "system", "function"
  , llmMessageContent :: Text
  , llmMessageName :: Maybe Text  -- ^ Tool name for function role messages
  }
  deriving (Eq, Show, Generic)

-- | Function call from LLM response.
data FunctionCall = FunctionCall
  { functionCallName :: Text      -- ^ Tool name to invoke
  , functionCallArguments :: Text -- ^ JSON string of arguments
  }
  deriving (Eq, Show, Generic)

-- | LLM API response.
data LLMResponse = LLMResponse
  { responseText :: Text
  , responseModel :: Text
  , responseUsage :: Maybe Usage
  , responseFunctionCall :: Maybe FunctionCall  -- ^ Function call if LLM wants to invoke a tool
  }
  deriving (Eq, Show, Generic)

-- | Token usage information.
data Usage = Usage
  { usagePromptTokens :: Int
  , usageCompletionTokens :: Int
  , usageTotalTokens :: Int
  }
  deriving (Eq, Show, Generic)

-- | API key loading error.
data ApiKeyError
  = ApiKeyNotFound Text
  | ApiKeyInvalid Text
  deriving (Eq, Show)

-- | Create an OpenAI client from API key.
createOpenAIClient :: Text -> LLMClient
createOpenAIClient apiKey = LLMClient
  { clientProvider = OpenAI
  , clientApiKey = apiKey
  , clientBaseURL = "https://api.openai.com/v1"
  }

-- | Create a client for the specified model, loading API key from environment or .env file.
createClientForModel :: Model -> IO (Either ApiKeyError LLMClient)
createClientForModel model = do
  -- Try to load .env file first (if it exists)
  -- Ignore parse errors (file might be malformed but env vars could still be set directly)
  _ <- loadEnvFile
  
  let (envVar, providerName, baseURL) = case modelProvider model of
        OpenAI -> ("OPENAI_API_KEY", "OpenAI", "https://api.openai.com/v1")
        Anthropic -> ("ANTHROPIC_API_KEY", "Anthropic", "https://api.anthropic.com/v1")
        Google -> ("GOOGLE_API_KEY", "Google", "https://generativelanguage.googleapis.com/v1")
  
  maybeKey <- lookupEnv envVar
  case maybeKey of
    Just key -> return $ Right $ LLMClient
      { clientProvider = modelProvider model
      , clientApiKey = pack key
      , clientBaseURL = baseURL
      }
    Nothing -> return $ Left $ ApiKeyNotFound $
      pack envVar <> " environment variable not set for " <> pack providerName <> " provider (checked both environment and .env file)"

-- | Load API key from environment variable or .env file.
loadApiKeyFromEnv :: Text -> IO (Either ApiKeyError Text)
loadApiKeyFromEnv envVar = do
  -- Try to load .env file first (if it exists)
  -- Ignore parse errors (file might be malformed but env vars could still be set directly)
  _ <- loadEnvFile
  
  maybeKey <- lookupEnv (unpack envVar)
  case maybeKey of
    Just key -> return $ Right $ pack key
    Nothing -> return $ Left $ ApiKeyNotFound $ 
      envVar <> " environment variable not set (checked both environment and .env file)"

-- | Build an LLM request for OpenAI format.
buildOpenAIRequest :: Model -> Text -> [LLMMessage] -> Maybe Double -> Maybe Int -> Maybe [Value] -> LLMRequest
buildOpenAIRequest model systemInstruction messages temperature maxTokens functions = LLMRequest
  { requestModel = modelId model
  , requestMessages = LLMMessage "system" systemInstruction Nothing : messages
  , requestTemperature = temperature
  , requestMaxTokens = maxTokens
  , requestFunctions = functions
  }

-- | Build an LLM request (generic, delegates to provider-specific builder).
buildRequest :: Model -> Text -> [LLMMessage] -> Maybe Double -> Maybe Int -> Maybe [Value] -> LLMRequest
buildRequest = buildOpenAIRequest  -- For now, default to OpenAI format

-- | Send a request to the LLM API.
sendRequest :: LLMClient -> LLMRequest -> IO (Either Text LLMResponse)
sendRequest client request = do
  result <- sendRequestWithRawResponse client request
  return $ fst result

-- | Send a request to the LLM API and return both parsed response and raw JSON.
sendRequestWithRawResponse :: LLMClient -> LLMRequest -> IO (Either Text LLMResponse, Maybe ByteString)
sendRequestWithRawResponse client request = do
  case clientProvider client of
    OpenAI -> sendOpenAIRequestWithRaw client request
    Anthropic -> return (Left "Anthropic provider not yet implemented", Nothing)
    Google -> return (Left "Google provider not yet implemented", Nothing)

-- | Send request to OpenAI API and return both parsed response and raw JSON.
sendOpenAIRequestWithRaw :: LLMClient -> LLMRequest -> IO (Either Text LLMResponse, Maybe ByteString)
sendOpenAIRequestWithRaw client request = do
  manager <- newManager tlsManagerSettings
  
  -- Create HTTP request (using ToJSON instance which correctly omits Nothing values)
  initialRequest <- parseRequest $ unpack (clientBaseURL client <> "/chat/completions")
  let httpRequest = initialRequest
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 (clientApiKey client))
            , ("Content-Type", "application/json")
            ]
        , requestBody = RequestBodyLBS $ encode request
        }
  
  -- Execute request
  response <- try (httpLbs httpRequest manager) :: IO (Either SomeException (Response ByteString))
  case response of
    Left err -> return (Left $ "Network error: " <> pack (show err), Nothing)
    Right httpResponse
      | responseStatus httpResponse == status200 -> do
          let rawResponse = responseBody httpResponse
          case decode rawResponse of
            Just responseValue -> case parseOpenAIResponse responseValue of
              Right llmResponse -> return (Right llmResponse, Just rawResponse)
              Left err -> return (Left err, Just rawResponse)
            Nothing -> return (Left "Failed to parse response JSON", Just rawResponse)
      | otherwise -> do
          let rawResponse = responseBody httpResponse
          let errorBody = TL.toStrict $ decodeUtf8 rawResponse
          return (Left $ "API error: " <> pack (show (responseStatus httpResponse)) <> " - " <> errorBody, Just rawResponse)

-- | Parse OpenAI API response.
parseOpenAIResponse :: Value -> Either Text LLMResponse
parseOpenAIResponse value = case parseMaybe parseOpenAIResponse' value of
  Just response -> Right response
  Nothing -> Left "Failed to parse OpenAI response structure"
  where
    parseOpenAIResponse' = withObject "OpenAIResponse" $ \obj -> do
      choices <- obj .: "choices"
      firstChoice <- case choices of
        [] -> fail "No choices in response"
        (c:_) -> return c
      message <- firstChoice .: "message"
      content <- message .:? "content"  -- content may be null for function calls
      let contentText = case content of
            Just (String s) -> s
            _ -> ""  -- Empty content when function_call is present
      model <- obj .: "model"
      usage <- obj .:? "usage"
      usageData <- case usage of
        Just u -> do
          promptTokens <- u .: "prompt_tokens"
          completionTokens <- u .: "completion_tokens"
          totalTokens <- u .: "total_tokens"
          return $ Just $ Usage promptTokens completionTokens totalTokens
        Nothing -> return Nothing
      -- Parse function_call if present
      functionCall <- message .:? "function_call"
      functionCallData <- case functionCall of
        Just fc -> do
          name <- fc .: "name"
          arguments <- fc .: "arguments"
          return $ Just $ FunctionCall name arguments
        Nothing -> return Nothing
      return $ LLMResponse contentText model usageData functionCallData

-- | Parse LLM response (generic, delegates to provider-specific parser).
parseResponse :: Provider -> Value -> Either Text LLMResponse
parseResponse provider value = case provider of
  OpenAI -> parseOpenAIResponse value
  Anthropic -> Left "Anthropic response parsing not yet implemented"
  Google -> Left "Google response parsing not yet implemented"

-- | Call LLM API (high-level interface).
callLLM :: LLMClient -> Model -> Text -> [LLMMessage] -> Maybe Double -> Maybe Int -> Maybe [Value] -> IO (Either Text LLMResponse)
callLLM client model systemInstruction messages temperature maxTokens functions = do
  let request = buildRequest model systemInstruction messages temperature maxTokens functions
  sendRequest client request

-- Aeson instances for JSON serialization
instance ToJSON LLMMessage where
  toJSON msg = object $
    [ "role" .= llmMessageRole msg
    , "content" .= llmMessageContent msg
    ] ++
    maybe [] (\name -> ["name" .= name]) (llmMessageName msg)

instance FromJSON LLMMessage where
  parseJSON = withObject "LLMMessage" $ \obj -> do
    role <- obj .: "role"
    content <- obj .: "content"
    name <- obj .:? "name"
    return $ LLMMessage role content name

instance ToJSON LLMRequest where
  toJSON req = object $
    [ "model" .= requestModel req
    , "messages" .= requestMessages req
    ] ++
    maybe [] (\t -> ["temperature" .= t]) (requestTemperature req) ++
    maybe [] (\m -> ["max_tokens" .= m]) (requestMaxTokens req) ++
    maybe [] (\f -> ["functions" .= f]) (requestFunctions req)

instance ToJSON FunctionCall where
  toJSON fc = object
    [ "name" .= functionCallName fc
    , "arguments" .= functionCallArguments fc
    ]

instance FromJSON FunctionCall where
  parseJSON = withObject "FunctionCall" $ \obj -> do
    name <- obj .: "name"
    arguments <- obj .: "arguments"
    return $ FunctionCall name arguments

instance ToJSON Usage where
  toJSON usage = object
    [ "prompt_tokens" .= usagePromptTokens usage
    , "completion_tokens" .= usageCompletionTokens usage
    , "total_tokens" .= usageTotalTokens usage
    ]

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \obj -> do
    promptTokens <- obj .: "prompt_tokens"
    completionTokens <- obj .: "completion_tokens"
    totalTokens <- obj .: "total_tokens"
    return $ Usage promptTokens completionTokens totalTokens
