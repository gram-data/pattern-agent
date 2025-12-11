{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Standalone LLM API client.
--
-- This module provides a standalone client for sending requests to LLM APIs.
-- It handles provider configuration, API key management, HTTP requests,
-- and response parsing.
module PatternAgent.LLM
  ( -- * Types
    Provider(..)
  , Model(..)
  , LLMClient(..)
  , LLMRequest(..)
  , LLMResponse(..)
  , Message(..)
  , Usage(..)
    -- * Model Creation
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
  , responseText
  , responseModel
  , responseUsage
    -- * Usage Accessors
  , Usage(..)
  , usagePromptTokens
  , usageCompletionTokens
  , usageTotalTokens
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import Data.Aeson.Types (parseMaybe, withObject, (.:), (.:?))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types (status200)
import PatternAgent.Env (loadEnvFile)
import System.Environment (lookupEnv)

-- | LLM Provider enumeration.
data Provider
  = OpenAI
  | Anthropic
  | Google
  deriving (Eq, Show, Generic)

-- | Model type representing an LLM model identifier and provider.
data Model = Model
  { modelId :: Text
  , modelProvider :: Provider
  }
  deriving (Eq, Show, Generic)

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
  , requestMessages :: [Message]
  , requestTemperature :: Maybe Double
  , requestMaxTokens :: Maybe Int
  }
  deriving (Eq, Show, Generic)

-- | Message in LLM request.
data Message = Message
  { messageRole :: Text  -- "user", "assistant", "system"
  , messageContent :: Text
  }
  deriving (Eq, Show, Generic)

-- | LLM API response.
data LLMResponse = LLMResponse
  { responseText :: Text
  , responseModel :: Text
  , responseUsage :: Maybe Usage
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

-- | Create a model identifier for a specific provider.
createModel :: Text -> Provider -> Model
createModel modelId provider = Model { modelId = modelId, modelProvider = provider }

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
  loadEnvFile
  
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
  loadEnvFile
  
  maybeKey <- lookupEnv (unpack envVar)
  case maybeKey of
    Just key -> return $ Right $ pack key
    Nothing -> return $ Left $ ApiKeyNotFound $ 
      envVar <> " environment variable not set (checked both environment and .env file)"

-- | Build an LLM request for OpenAI format.
buildOpenAIRequest :: Model -> Text -> [Message] -> Maybe Double -> Maybe Int -> LLMRequest
buildOpenAIRequest model systemInstruction messages temperature maxTokens = LLMRequest
  { requestModel = modelId model
  , requestMessages = Message "system" systemInstruction : messages
  , requestTemperature = temperature
  , requestMaxTokens = maxTokens
  }

-- | Build an LLM request (generic, delegates to provider-specific builder).
buildRequest :: Model -> Text -> [Message] -> Maybe Double -> Maybe Int -> LLMRequest
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

-- | Send request to OpenAI API.
sendOpenAIRequest :: LLMClient -> LLMRequest -> IO (Either Text LLMResponse)
sendOpenAIRequest client request = do
  (result, _) <- sendOpenAIRequestWithRaw client request
  return result

-- | Send request to OpenAI API and return both parsed response and raw JSON.
sendOpenAIRequestWithRaw :: LLMClient -> LLMRequest -> IO (Either Text LLMResponse, Maybe ByteString)
sendOpenAIRequestWithRaw client request = do
  manager <- newManager tlsManagerSettings
  
  -- Build request body
  let requestBody = object
        [ "model" .= requestModel request
        , "messages" .= requestMessages request
        , "temperature" .= requestTemperature request
        , "max_tokens" .= requestMaxTokens request
        ]
  
  -- Create HTTP request
  initialRequest <- parseRequest $ unpack (clientBaseURL client <> "/chat/completions")
  let httpRequest = initialRequest
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 (clientApiKey client))
            , ("Content-Type", "application/json")
            ]
        , requestBody = RequestBodyLBS $ encode requestBody
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
      content <- message .: "content"
      model <- obj .: "model"
      usage <- obj .:? "usage"
      usageData <- case usage of
        Just u -> do
          promptTokens <- u .: "prompt_tokens"
          completionTokens <- u .: "completion_tokens"
          totalTokens <- u .: "total_tokens"
          return $ Just $ Usage promptTokens completionTokens totalTokens
        Nothing -> return Nothing
      return $ LLMResponse content model usageData

-- | Parse LLM response (generic, delegates to provider-specific parser).
parseResponse :: Provider -> Value -> Either Text LLMResponse
parseResponse provider value = case provider of
  OpenAI -> parseOpenAIResponse value
  Anthropic -> Left "Anthropic response parsing not yet implemented"
  Google -> Left "Google response parsing not yet implemented"

-- | Call LLM API (high-level interface).
callLLM :: LLMClient -> Model -> Text -> [Message] -> Maybe Double -> Maybe Int -> IO (Either Text LLMResponse)
callLLM client model systemInstruction messages temperature maxTokens = do
  let request = buildRequest model systemInstruction messages temperature maxTokens
  sendRequest client request

-- Aeson instances for JSON serialization
instance ToJSON Message where
  toJSON msg = object
    [ "role" .= messageRole msg
    , "content" .= messageContent msg
    ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \obj -> do
    role <- obj .: "role"
    content <- obj .: "content"
    return $ Message role content

instance ToJSON LLMRequest where
  toJSON req = object $
    [ "model" .= requestModel req
    , "messages" .= requestMessages req
    ] ++
    maybe [] (\t -> ["temperature" .= t]) (requestTemperature req) ++
    maybe [] (\m -> ["max_tokens" .= m]) (requestMaxTokens req)

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
