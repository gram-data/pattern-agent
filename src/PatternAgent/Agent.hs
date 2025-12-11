{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | LLM Agent type and operations.
--
-- This module provides the core Agent type for LLM-powered agents,
-- including agent identity (name, description, model) and configuration.
module PatternAgent.Agent
  ( -- * Types
    Agent(..)
  , Model(..)
  , Provider(..)
    -- * Agent Creation
  , createAgent
    -- * Model Creation (re-exported from LLM)
  , createModel
    -- * Accessors
  , agentName
  , agentDescription
  , agentModel
  , agentInstruction
  ) where

import PatternAgent.LLM (Model(..), Provider(..), createModel)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Agent type representing an LLM-powered agent.
--
-- This is the core type for LLM agents. Includes identity fields
-- (name, description, model) and instruction field. Additional fields
-- (tools, config) will be added in subsequent user stories.
data Agent = Agent
  { agentName :: Text
  , agentDescription :: Maybe Text
  , agentModel :: Model
  , agentInstruction :: Text
  }
  deriving (Eq, Show, Generic)

-- | Create an agent with the specified configuration.
--
-- Validates that name and instruction are non-empty. Returns Left with error message
-- if validation fails.
createAgent
  :: Text                    -- ^ name: Unique agent identifier
  -> Model                   -- ^ model: LLM model to use
  -> Text                    -- ^ instruction: Agent behavior instructions
  -> Maybe Text              -- ^ description: Optional agent description
  -> Either Text Agent       -- ^ Returns Right Agent or Left error message
createAgent name model instruction description
  | T.null name = Left "Agent name cannot be empty"
  | T.null instruction = Left "Agent instruction cannot be empty"
  | otherwise = Right $ Agent
      { agentName = name
      , agentDescription = description
      , agentModel = model
      , agentInstruction = instruction
      }
