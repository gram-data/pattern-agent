{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tool module (deprecated - use Language/Runtime modules instead).
--
-- This module is a placeholder. Tool functionality is now split across:
-- - PatternAgent.Language.Core: Tool (Pattern Subject) creation and lenses
-- - PatternAgent.Runtime.ToolLibrary: ToolImpl and ToolLibrary
--
-- @deprecated Use PatternAgent.Language.Core and PatternAgent.Runtime.ToolLibrary
module PatternAgent.Tool
  ( -- Re-export from Language and Runtime modules
    module PatternAgent.Language.Core
  , module PatternAgent.Runtime.ToolLibrary
  ) where

import PatternAgent.Language.Core
import PatternAgent.Runtime.ToolLibrary
