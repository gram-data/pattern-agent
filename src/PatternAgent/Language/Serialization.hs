{-# LANGUAGE OverloadedStrings #-}
-- | Serialization for Pattern Agent language.
--
-- This module provides conversion between gram notation and Pattern Subject,
-- enabling serialization and deserialization of agent workflows.
--
-- This is part of the portable language specification.
module PatternAgent.Language.Serialization
  ( -- * Gram to Pattern
    parseGram
  , parseAgent
  , parseTool
    -- * Pattern to Gram
  , toGram
  , agentToGram
  , toolToGram
    -- * JSON Serialization (for API compatibility)
  , agentToJSON
  , toolToJSON
  , agentFromJSON
  , toolFromJSON
  ) where

import PatternAgent.Language.Core (Agent, Tool)
import Pattern (Pattern Subject)
import Data.Aeson (Value)
import Data.Text (Text)

-- | Parse gram notation string to Pattern Subject.
parseGram :: Text -> Either Text (Pattern Subject)
parseGram gram = undefined -- TODO: Implement gram notation parser

-- | Parse agent from gram notation.
parseAgent :: Text -> Either Text Agent
parseAgent gram = undefined -- TODO: Implement agent parsing

-- | Parse tool from gram notation.
parseTool :: Text -> Either Text Tool
parseTool gram = undefined -- TODO: Implement tool parsing

-- | Convert Pattern Subject to gram notation string.
toGram :: Pattern Subject -> Text
toGram pattern = undefined -- TODO: Implement gram notation serializer

-- | Convert agent to gram notation.
agentToGram :: Agent -> Text
agentToGram agent = undefined -- TODO: Implement agent serialization

-- | Convert tool to gram notation.
toolToGram :: Tool -> Text
toolToGram tool = undefined -- TODO: Implement tool serialization

-- | Convert agent to JSON (for API compatibility).
agentToJSON :: Agent -> Value
agentToJSON agent = undefined -- TODO: Implement agent JSON serialization

-- | Convert tool to JSON (for API compatibility).
toolToJSON :: Tool -> Value
toolToJSON tool = undefined -- TODO: Implement tool JSON serialization

-- | Parse agent from JSON.
agentFromJSON :: Value -> Either Text Agent
agentFromJSON json = undefined -- TODO: Implement agent JSON deserialization

-- | Parse tool from JSON.
toolFromJSON :: Value -> Either Text Tool
toolFromJSON json = undefined -- TODO: Implement tool JSON deserialization
