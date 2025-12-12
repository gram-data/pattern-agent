-- | Core PatternAgent type definitions and basic operations.
--
-- This module provides the core PatternAgent API by re-exporting
-- the PatternAgent type alias and Agent type from PatternAgent.Types.
--
-- The PatternAgent type alias (Pattern<Agent>) is fully implemented
-- and ready for use. Additional operations (construction, query, validation)
-- will be added in future user stories.
module PatternAgent.Core
  ( -- Re-export types from Types module
    module PatternAgent.Types
  ) where

import PatternAgent.Types

