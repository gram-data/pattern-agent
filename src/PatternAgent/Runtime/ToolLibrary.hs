{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tool library and tool implementation (Runtime).
--
-- This module provides runtime tool implementations and tool library management.
-- ToolImpl contains executable function closures (not serializable).
-- ToolLibrary is a runtime registry mapping tool names to implementations.
--
-- This is part of the runtime implementation (Haskell-specific).
module PatternAgent.Runtime.ToolLibrary
  ( -- * Types
    ToolImpl(..)
  , ToolLibrary(..)
    -- * ToolImpl Creation
  , createToolImpl
    -- * ToolImpl Accessors
  , toolImplName
  , toolImplDescription
  , toolImplSchema
  , toolImplInvoke
    -- * ToolLibrary Management
  , emptyToolLibrary
  , registerTool
  , lookupTool
    -- * Tool Binding
  , bindTool
    -- * Validation
  , validateToolArgs
  ) where

import PatternAgent.Language.Core (Tool, toolName, toolDescription, toolSchema)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.Monad (unless, when, guard)
import Control.Lens (view)

-- | Tool implementation with executable function.
--
-- This is NOT serializable (contains function closure).
-- Registered in ToolLibrary at runtime.
data ToolImpl = ToolImpl
  { toolImplName :: Text
  , toolImplDescription :: Text
  , toolImplSchema :: Value
  , toolImplInvoke :: Value -> IO Value  -- ^ Tool invocation function
  }
  deriving (Generic)

-- | Tool library - runtime registry of tool implementations.
data ToolLibrary = ToolLibrary
  { libraryTools :: Map Text ToolImpl
  }
  deriving (Generic)

-- | Create a tool implementation.
--
-- Validates that name and description are non-empty.
createToolImpl
  :: Text              -- ^ name: Tool name
  -> Text              -- ^ description: Tool description
  -> Value             -- ^ schema: JSON schema
  -> (Value -> IO Value)  -- ^ invoke: Tool invocation function
  -> Either Text ToolImpl  -- ^ Returns Right ToolImpl or Left error message
createToolImpl name description schema invoke
  | T.null name = Left "ToolImpl name cannot be empty"
  | T.null description = Left "ToolImpl description cannot be empty"
  | otherwise = Right $ ToolImpl
      { toolImplName = name
      , toolImplDescription = description
      , toolImplSchema = schema
      , toolImplInvoke = invoke
      }

-- | Create an empty tool library.
emptyToolLibrary :: ToolLibrary
emptyToolLibrary = ToolLibrary { libraryTools = Map.empty }

-- | Register a tool implementation in the library.
registerTool
  :: Text              -- ^ name: Tool name
  -> ToolImpl          -- ^ toolImpl: Tool implementation
  -> ToolLibrary       -- ^ library: Tool library
  -> ToolLibrary       -- ^ Updated tool library
registerTool name toolImpl (ToolLibrary tools) = ToolLibrary
  { libraryTools = Map.insert name toolImpl tools
  }

-- | Lookup a tool implementation by name.
lookupTool
  :: Text              -- ^ name: Tool name
  -> ToolLibrary       -- ^ library: Tool library
  -> Maybe ToolImpl    -- ^ Tool implementation if found
lookupTool name (ToolLibrary tools) = Map.lookup name tools

-- | Bind a Tool (Pattern) to a ToolImpl from the library.
--
-- Validates that ToolImpl matches Tool (name, description, schema).
bindTool
  :: Tool              -- ^ tool: Tool specification (Pattern)
  -> ToolLibrary       -- ^ library: Tool library
  -> Maybe ToolImpl    -- ^ Bound tool implementation if found and matches
bindTool tool library = do
  -- Lookup tool by name
  toolImpl <- lookupTool (view toolName tool) library
  
  -- Validate that ToolImpl matches Tool specification
  -- Check name matches (already verified by lookup)
  guard $ toolImplName toolImpl == view toolName tool
  
  -- Check description matches
  guard $ toolImplDescription toolImpl == view toolDescription tool
  
  -- Check schema matches (schema is computed from type signature, so compare)
  guard $ toolImplSchema toolImpl == view toolSchema tool
  
  return toolImpl

-- | Validate tool arguments against JSON schema.
--
-- Manual JSON schema validation for tool parameters.
-- Validates required fields, field types, and structure.
-- Returns Right with validated arguments or Left with error message.
validateToolArgs
  :: Value             -- ^ schema: JSON schema for tool parameters
  -> Value             -- ^ args: Tool arguments to validate
  -> Either Text Value -- ^ Validated arguments or error message
validateToolArgs schema args = case (schema, args) of
  (Object schemaMap, Object argMap) -> do
    -- Extract properties and required fields from schema
    let properties = case KM.lookup (K.fromText "properties") schemaMap of
          Just (Object props) -> props
          _ -> KM.empty
    let required = case KM.lookup (K.fromText "required") schemaMap of
          Just (Array req) -> 
            let mapMaybe f = foldr (\x acc -> case f x of Just y -> y:acc; Nothing -> acc) []
            in mapMaybe extractString (V.toList req)
          _ -> []
    
    -- Check all required fields are present
    forM_ required $ \reqField -> do
      let key = K.fromText reqField
      unless (KM.member key argMap) $
        Left $ "Missing required field: " <> reqField
    
    -- Validate field types
    KM.foldrWithKey (\key val acc -> do
      validated <- acc
      let fieldName = K.toText key
      case KM.lookup key properties of
        Just fieldSchema -> do
          _ <- validateFieldType fieldSchema val
          return validated
        Nothing -> Left $ "Unknown field: " <> fieldName
      ) (Right args) argMap
    
  (_, Object _) -> Left "Schema must be an object"
  (_, _) -> Left "Arguments must be an object"
  where
    validateFieldType fieldSchema fieldValue = case fieldSchema of
      Object fieldMap -> case KM.lookup (K.fromText "type") fieldMap of
        Just (String t) -> validateType t fieldValue
        _ -> Right ()  -- No type specified, allow it
      _ -> Right ()  -- Not an object schema, allow it
    
    validateType expectedType fieldValue = case (expectedType, fieldValue) of
      ("string", String _) -> Right ()
      ("string", _) -> Left "Field must be a string"
      ("integer", Number _) -> Right ()  -- JSON numbers can be integers
      ("integer", _) -> Left "Field must be an integer"
      ("number", Number _) -> Right ()
      ("number", _) -> Left "Field must be a number"
      ("boolean", Bool _) -> Right ()
      ("boolean", _) -> Left "Field must be a boolean"
      ("object", Object _) -> Right ()
      ("object", _) -> Left "Field must be an object"
      ("array", Array _) -> Right ()
      ("array", _) -> Left "Field must be an array"
      _ -> Right ()  -- Unknown type, allow it
    
    extractString (String s) = Just s
    extractString _ = Nothing
    
    forM_ [] _ = Right ()
    forM_ (x:xs) f = case f x of
      Left err -> Left err
      Right _ -> forM_ xs f
