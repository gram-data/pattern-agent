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

import PatternAgent.Language.Core (Agent, Tool, normalizeTypeSignaturePattern)
import Pattern (Pattern)
import Pattern.Core (value, elements, patternWith)
import Subject.Core (Subject(..), Symbol(..))
import qualified Gram
import Gram.Parse (ParseError(..))
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Control.Monad (unless)

type PatternSubject = Pattern Subject

-- | Parse gram notation string to Pattern Subject.
-- Uses gram-hs parser to convert gram notation text to Pattern Subject.
parseGram :: Text -> Either Text PatternSubject
parseGram gram = case Gram.fromGram (T.unpack gram) of
  Left (ParseError err) -> Left $ T.pack err
  Right pattern -> Right pattern

-- | Parse agent from gram notation.
--
-- Parses a gram string and validates it represents an Agent pattern.
-- The parsed pattern must have the "Agent" label.
-- Tool elements are normalized to infer FunctionType labels when missing.
parseAgent :: Text -> Either Text Agent
parseAgent gram = do
  pattern <- parseGram gram
  -- Validate that the pattern has "Agent" label
  let subject = value pattern
  unless ("Agent" `Set.member` labels subject) $
    Left "Parsed pattern does not have 'Agent' label"
  
  -- Normalize type signature elements in tool patterns by inferring FunctionType labels
  let normalizedElements = map normalizeToolPattern (elements pattern)
  let normalizedPattern = pattern { elements = normalizedElements }
  
  Right normalizedPattern
  where
    -- Normalize a tool pattern by normalizing its type signature elements
    normalizeToolPattern :: PatternSubject -> PatternSubject
    normalizeToolPattern toolPattern =
      let normalizedElements = map normalizeTypeSignaturePattern (elements toolPattern)
      in toolPattern { elements = normalizedElements }

-- | Parse tool from gram notation.
--
-- Parses a gram string and validates it represents a Tool pattern.
-- The parsed pattern must have the "Tool" label.
-- Type signature elements are normalized to infer FunctionType labels when missing.
parseTool :: Text -> Either Text Tool
parseTool gram = do
  pattern <- parseGram gram
  -- Validate that the pattern has "Tool" label
  let subject = value pattern
  unless ("Tool" `Set.member` labels subject) $
    Left "Parsed pattern does not have 'Tool' label"
  
  -- Normalize type signature elements by inferring FunctionType labels
  let normalizedElements = map normalizeTypeSignaturePattern (elements pattern)
  let normalizedPattern = pattern { elements = normalizedElements }
  
  Right normalizedPattern

-- | Convert Pattern Subject to gram notation string.
-- Uses gram-hs serializer to convert Pattern Subject to gram notation text.
toGram :: PatternSubject -> Text
toGram pattern = T.pack $ Gram.toGram pattern

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
