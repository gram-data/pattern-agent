{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Agent execution engine (Runtime).
--
-- This module provides the core execution infrastructure for LLM agents,
-- including error handling and agent execution logic that uses the
-- standalone LLM client module.
--
-- This is part of the runtime implementation (Haskell-specific).
module PatternAgent.Runtime.Execution
  ( -- * Error Types
    AgentError(..)
    -- * Agent Execution
  , AgentResponse(..)
  , ToolInvocation(..)
  , executeAgent
  , executeAgentWithLibrary
    -- * Tool Binding
  , bindAgentTools
  ) where

import PatternAgent.Language.Core (Agent, Tool, agentModel, agentInstruction, agentTools)
import PatternAgent.Runtime.LLM (LLMClient, LLMMessage(..), callLLM, createClientForModel, ApiKeyError(..))
import PatternAgent.Runtime.Context
  ( ConversationContext
  , Message(..)
  , MessageRole(..)
  )
import PatternAgent.Runtime.ToolLibrary (ToolLibrary, ToolImpl, bindTool, lookupTool)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Error types for agent execution.
data AgentError
  = LLMAPIError Text      -- ^ LLM API call failed
  | ToolError Text        -- ^ Tool execution failed
  | ValidationError Text  -- ^ Input validation failed
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

-- | Convert Context.Message to LLM.LLMMessage format.
contextToLLMMessage :: Message -> LLMMessage
contextToLLMMessage (Message role content) = LLMMessage
  { llmMessageRole = case role of
      UserRole -> "user"
      AssistantRole -> "assistant"
      FunctionRole toolName -> "function"  -- TODO: Handle function role properly
  , llmMessageContent = content
  }

-- | Convert conversation context to LLM message list.
contextToLLMMessages :: ConversationContext -> [LLMMessage]
contextToLLMMessages = map contextToLLMMessage

-- | Execute an agent with user input and return the agent's response.
executeAgent
  :: Agent                -- ^ agent: Agent to execute (Pattern)
  -> Text                 -- ^ userInput: User's input message
  -> ConversationContext  -- ^ context: Previous conversation context
  -> IO (Either AgentError AgentResponse)
executeAgent agent userInput context
  | T.null userInput = return $ Left $ ValidationError "Empty user input"
  | otherwise = do
      -- TODO: Access agent fields using lenses
      -- For now, this is a placeholder that will be implemented when
      -- Pattern Subject structure and lenses are fully defined
      undefined

-- | Execute an agent with tool library support.
--
-- Binds tools from agent to implementations in library, then executes.
executeAgentWithLibrary
  :: Agent                -- ^ agent: Agent with Tools (Pattern)
  -> Text                 -- ^ userInput: User's input message
  -> ConversationContext  -- ^ context: Previous conversation context
  -> ToolLibrary          -- ^ library: Tool library for binding
  -> IO (Either AgentError AgentResponse)
executeAgentWithLibrary agent userInput context library = undefined -- TODO: Implement

-- | Bind all agent tools to implementations from library.
bindAgentTools
  :: Agent                -- ^ agent: Agent with Tools (Pattern)
  -> ToolLibrary          -- ^ library: Tool library
  -> Either Text [ToolImpl]   -- ^ Bound tool implementations or error
bindAgentTools agent library = undefined -- TODO: Implement
