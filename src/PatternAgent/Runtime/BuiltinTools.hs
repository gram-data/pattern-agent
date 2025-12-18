{-# LANGUAGE OverloadedStrings #-}
-- | Built-in tool implementations registry.
--
-- This module provides a registry of built-in tool implementations that can be
-- automatically created from tool specifications in agents.
--
-- This is part of the runtime implementation (Haskell-specific).
module PatternAgent.Runtime.BuiltinTools
  ( -- * Tool Registry
    createToolLibraryFromAgent
    -- * Built-in Tool Implementations
  , sayHelloToolImpl
  ) where

import PatternAgent.Language.Core (Agent, Tool, agentTools, toolName, toolDescription, toolSchema)
import PatternAgent.Runtime.ToolLibrary (ToolLibrary, ToolImpl, createToolImpl, emptyToolLibrary, registerTool)
import PatternAgent.Language.TypeSignature (TypeSignature(..), Parameter(..), typeSignatureToJSONSchema)
import Data.Aeson (Value(..), toJSON, (.:?))
import Data.Aeson.Types (parseMaybe, withObject)
import Control.Lens (view)
import Data.Text (Text)
import qualified Data.Text as T

-- | Create a ToolLibrary from an agent's tools using built-in implementations.
--
-- This function looks up built-in tool implementations for each tool in the agent
-- and registers them in a ToolLibrary. Only tools with known built-in implementations
-- are supported.
--
-- Currently supports:
-- - sayHello: Returns a friendly greeting message
createToolLibraryFromAgent :: Agent -> Either Text ToolLibrary
createToolLibraryFromAgent agent = do
  let tools = view agentTools agent
  foldl registerToolFromPattern (Right emptyToolLibrary) tools
  where
    registerToolFromPattern :: Either Text ToolLibrary -> Tool -> Either Text ToolLibrary
    registerToolFromPattern (Left err) _ = Left err
    registerToolFromPattern (Right lib) tool = do
      let toolNameStr = view toolName tool
      case T.unpack toolNameStr of
        "sayHello" -> do
          impl <- sayHelloToolImpl tool
          Right $ registerTool toolNameStr impl lib
        _ -> Left $ "Unsupported tool: " <> toolNameStr <> " (only sayHello is currently supported)"

-- | Create sayHello ToolImpl from a Tool specification.
--
-- Extracts the tool's description and schema, then creates the implementation
-- with the sayHello greeting logic.
sayHelloToolImpl :: Tool -> Either Text ToolImpl
sayHelloToolImpl tool = do
  let toolNameVal = view toolName tool
  let toolDesc = view toolDescription tool
  let toolSchemaVal = view toolSchema tool
  
  createToolImpl
    toolNameVal
    toolDesc
    toolSchemaVal
    (\args -> do
      -- Extract personName from args, default to "world" if not provided
      -- args is already the parsed JSON object from function call: {"personName": "ABK"}
      let parsedName = parseMaybe (withObject "toolArgs" $ \obj -> obj .:? "personName") args
      
      let name = case parsedName of
            Just (Just (String n)) -> T.unpack n
            _ -> "world"
      
      let result = String $ "Hello, " <> T.pack name <> "! Nice to meet you."
      return result
    )

