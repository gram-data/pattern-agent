{-# LANGUAGE OverloadedStrings #-}
-- | Conversation context management.
--
-- This module provides types and functions for managing conversation
-- context (message history) in multi-turn conversations.
module PatternAgent.Context
  ( -- * Types
    MessageRole(..)
  , Message(..)
  , ConversationContext
    -- * Message Creation
  , createMessage
    -- * Context Management
  , emptyContext
  , addMessage
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Role of a message sender.
data MessageRole
  = UserRole
  | AssistantRole
  deriving (Eq, Show)

-- | A single message in a conversation.
data Message = Message
  { messageRole :: MessageRole
  , messageContent :: Text
  }
  deriving (Eq, Show)

-- | Conversation context: ordered list of messages (most recent last).
type ConversationContext = [Message]

-- | Create a message with specified role and content.
--
-- Validates that content is non-empty.
createMessage :: MessageRole -> Text -> Either Text Message
createMessage role content
  | T.null content = Left "Message content cannot be empty"
  | otherwise = Right $ Message { messageRole = role, messageContent = content }

-- | Returns an empty conversation context (new conversation).
emptyContext :: ConversationContext
emptyContext = []

-- | Adds a message to conversation context.
--
-- Returns Right with new context if message is valid, Left with error otherwise.
-- Original context unchanged (pure function).
addMessage :: MessageRole -> Text -> ConversationContext -> Either Text ConversationContext
addMessage role content context = case createMessage role content of
  Right msg -> Right $ context ++ [msg]
  Left err -> Left err
