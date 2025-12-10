{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Type aliases and supporting type definitions for PatternAgent.
--
-- This module provides the core type definitions needed for the PatternAgent
-- framework, including the Agent value type and PatternAgent type alias.
module PatternAgent.Types
  ( -- * Type Aliases
    PatternAgent
    -- * Supporting Types
  , Agent(..)
  ) where

import Pattern (Pattern)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | Main type alias for Pattern<Agent>.
-- 
-- PatternAgent represents an agent system that can be either:
-- - Atomic: A single agent with no sub-agents (elements == [])
-- - Compound: A multi-agent system with sub-agents (elements /= [])
type PatternAgent = Pattern Agent

-- | Agent value type (minimal definition for foundation).
--
-- This is a placeholder definition to enable type checking.
-- The full Agent structure will be defined in the execution feature.
-- For now, it provides basic fields that represent agent capability:
--
-- * @prompt@: System prompt for the agent
-- * @tools@: Available tools/functions the agent can use
-- * @constraints@: Behavioral constraints or parameters
data Agent = Agent
  { prompt :: String
  , tools :: [String]
  , constraints :: [String]
  }
  deriving (Eq, Show, Ord, Generic, Hashable)

