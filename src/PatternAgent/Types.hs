-- | Type aliases and supporting type definitions for PatternAgent.
--
-- This module provides the core type definitions needed for the PatternAgent
-- framework, including the PatternAgent type alias.
module PatternAgent.Types
  ( -- * Type Aliases
    PatternAgent
    -- * Re-exported Types
  , Agent
  ) where

import Pattern (Pattern)
import PatternAgent.Language.Core (Agent)

-- | Main type alias for Pattern<Agent>.
-- 
-- PatternAgent represents an agent system that can be either:
-- - Atomic: A single agent with no sub-agents (elements == [])
-- - Compound: A multi-agent system with sub-agents (elements /= [])
--
-- The Agent type is defined in PatternAgent.Language.Core and represents
-- an LLM-powered agent workflow specification (Pattern Subject).
type PatternAgent = Pattern Agent

