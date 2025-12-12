{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Agent execution and LLM API integration.
--
-- This module provides the core execution infrastructure for LLM agents,
-- including error handling and agent execution logic that uses the
-- standalone LLM client module.
module PatternAgent.Execution
  ( -- * Error Types
    AgentError(..)
    -- * Agent Execution
  , AgentResponse(..)
  , ToolInvocation(..)
  , executeAgent
  ) where

import PatternAgent.Agent (Agent(..), agentModel, agentInstruction)
import qualified PatternAgent.LLM as LLM
import PatternAgent.Context
  ( ConversationContext
  , Message(..)
  , MessageRole(..)
  )

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Error types for agent execution.
data AgentError
  = LLMAPIError Text      -- ^ LLM API call failed
  | ToolError Text        -- ^ Tool execution failed
  | ValidationError Text   -- ^ Input validation failed
  | ConfigurationError Text  -- ^ Agent configuration invalid (e.g., missing API key)
  deriving (Eq, Show, Generic)

-- | Agent response type.
data AgentResponse = AgentResponse
  { responseContent :: Text
  , responseToolsUsed :: [ToolInvocation]
  }
  deriving (Eq, Show, Generic)

-- | Tool invocation record.
data ToolInvocation = ToolInvocation
  { invocationToolName :: Text
  , invocationArgs :: Value
  , invocationResult :: Either Text Value
  }
  deriving (Eq, Show, Generic)

-- | Convert Context.Message to LLM.Message format.
contextToLLMMessage :: Message -> LLM.Message
contextToLLMMessage (Message role content) = LLM.Message
  { LLM.messageRole = case role of
      UserRole -> "user"
      AssistantRole -> "assistant"
  , LLM.messageContent = content
  }

-- | Convert conversation context to LLM message list.
contextToLLMMessages :: ConversationContext -> [LLM.Message]
contextToLLMMessages = map contextToLLMMessage

-- | Execute an agent with user input and return the agent's response.
executeAgent
  :: Agent                -- ^ agent: Agent to execute
  -> Text                 -- ^ userInput: User's input message
  -> ConversationContext  -- ^ context: Previous conversation context
  -> IO (Either AgentError AgentResponse)
executeAgent agent userInput context
  | T.null userInput = return $ Left $ ValidationError "Empty user input"
  | otherwise = do
      -- Create LLM client for the agent's model
      clientResult <- LLM.createClientForModel (agentModel agent)
      case clientResult of
        Left (LLM.ApiKeyNotFound msg) -> return $ Left $ ConfigurationError msg
        Left (LLM.ApiKeyInvalid msg) -> return $ Left $ ConfigurationError msg
        Right client -> do
          -- Convert context to LLM messages
          let contextMessages = contextToLLMMessages context
          -- Add user input as a new message
          let userMessage = LLM.Message "user" userInput
          let allMessages = contextMessages ++ [userMessage]
          
          -- Call LLM API
          llmResult <- LLM.callLLM client (agentModel agent) (agentInstruction agent) allMessages Nothing Nothing
          case llmResult of
            Left err -> return $ Left $ LLMAPIError err
            Right llmResponse -> return $ Right $ AgentResponse
              { responseContent = LLM.responseText llmResponse
              , responseToolsUsed = []  -- TODO: Extract tool invocations from response
              }
