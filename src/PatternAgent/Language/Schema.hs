{-# LANGUAGE OverloadedStrings #-}
-- | Schema validation for Pattern Agent language.
--
-- This module provides validation rules for gram notation structures,
-- ensuring that Agent and Tool patterns conform to the language specification.
--
-- This is part of the portable language specification.
module PatternAgent.Language.Schema
  ( -- * Schema Validation
    validateAgentSchema
  , validateToolSchema
  , validatePatternStructure
    -- * Validation Errors
  , ValidationError(..)
  ) where

import PatternAgent.Language.Core (Agent, Tool)
import Pattern (Pattern Subject)
import Data.Text (Text)

-- | Validation error type.
data ValidationError
  = MissingRequiredField Text
  | InvalidFieldType Text Text
  | InvalidStructure Text
  | DuplicateIdentifier Text
  deriving (Eq, Show)

-- | Validate agent schema structure.
--
-- Checks that the agent pattern conforms to the gram notation schema:
-- - Has pattern identifier (agent name)
-- - Has required properties (instruction, model)
-- - Has valid nested tool patterns (if any)
validateAgentSchema :: Agent -> Either ValidationError ()
validateAgentSchema agent = undefined -- TODO: Implement schema validation

-- | Validate tool schema structure.
--
-- Checks that the tool pattern conforms to the gram notation schema:
-- - Has pattern identifier (tool name)
-- - Has required properties (description)
-- - Has valid type signature in elements
validateToolSchema :: Tool -> Either ValidationError ()
validateToolSchema tool = undefined -- TODO: Implement schema validation

-- | Validate general pattern structure.
--
-- Checks that a Pattern Subject has valid structure (identifier, properties, elements).
validatePatternStructure :: Pattern Subject -> Either ValidationError ()
validatePatternStructure pattern = undefined -- TODO: Implement pattern structure validation
