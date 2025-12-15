{-# LANGUAGE OverloadedStrings #-}
-- | Type signature parsing and JSON schema generation.
--
-- This module provides parsing of gram path notation type signatures
-- and conversion to JSON schemas for LLM API compatibility.
--
-- This is part of the portable language specification.
module PatternAgent.Language.TypeSignature
  ( -- * Type Signature Extraction
    extractTypeSignatureFromPattern
  , TypeSignature(..)
  , Parameter(..)
    -- * JSON Schema Generation
  , typeSignatureToJSONSchema
  , parameterToJSONSchema
    -- * Validation
  , validateTypeSignature
  ) where

import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Pattern (Pattern Subject)
import Pattern.Core (value, elements)
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Parsed type signature representation.
data TypeSignature = TypeSignature
  { typeParams :: [Parameter]
  , typeReturn :: Parameter
  }
  deriving (Eq, Show)

-- | Function parameter representation.
data Parameter = Parameter
  { paramName :: Maybe Text      -- ^ Parameter name (from identifier)
  , paramType :: Text             -- ^ Type label (Text, Int, etc.)
  , paramDefault :: Maybe Value   -- ^ Default value (for optional parameters)
  }
  deriving (Eq, Show)

-- | Extract type signature from Pattern element.
--
-- The type signature is already parsed by gram-hs as a path notation element.
-- This function extracts parameter and return type information from the Pattern structure.
--
-- Example: Pattern element representing "(personName::Text {default:\"world\"})==>(::String)"
-- Returns parsed representation or error message.
extractTypeSignatureFromPattern :: Pattern Subject -> Either Text TypeSignature
extractTypeSignatureFromPattern patternElem = do
  -- Type signature is a path notation element with nodes connected by arrows
  -- For now, return a placeholder - full implementation will traverse the path structure
  -- This is a foundational implementation that will be expanded
  Left "Type signature extraction from Pattern not yet fully implemented"

-- | Convert parsed type signature to JSON schema.
--
-- Generates JSON schema compatible with LLM function calling APIs.
typeSignatureToJSONSchema :: TypeSignature -> Value
typeSignatureToJSONSchema (TypeSignature params returnType) =
  object
    [ "type" .= ("object" :: Text)
    , "properties" .= object (mapMaybe paramToProperty params)
    , "required" .= Array (map String $ requiredParamNames params)
    ]
  where
    paramToProperty (Parameter (Just name) typeLabel defaultVal) =
      Just (T.unpack name, parameterToJSONSchema (Parameter (Just name) typeLabel defaultVal))
    paramToProperty _ = Nothing
    
    requiredParamNames = mapMaybe (\(Parameter name _ defaultVal) ->
      if isNothing defaultVal then fmap T.unpack name else Nothing)
    
    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y:acc; Nothing -> acc) []
    
    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing _ = False

-- | Convert a parameter to JSON schema property.
parameterToJSONSchema :: Parameter -> Value
parameterToJSONSchema (Parameter _ typeLabel defaultVal) =
  let baseSchema = object ["type" .= typeLabelToJSONType typeLabel]
      withDefault = case defaultVal of
        Just val -> baseSchema <> object ["default" .= val]
        Nothing -> baseSchema
  in withDefault

-- | Convert gram type label to JSON Schema type string.
typeLabelToJSONType :: Text -> Text
typeLabelToJSONType label
  | label == "Text" || label == "String" = "string"
  | label == "Int" = "integer"
  | label == "Double" = "number"
  | label == "Bool" = "boolean"
  | label == "Object" = "object"
  | label == "Array" = "array"
  | otherwise = "string" -- Default to string for unknown types

-- | Validate a type signature Pattern element.
--
-- Checks that the Pattern element represents a valid type signature.
validateTypeSignature :: Pattern Subject -> Either Text ()
validateTypeSignature patternElem = do
  -- Basic validation - ensure it's a path notation element
  -- Full validation will check path structure, node types, etc.
  Right ()  -- Placeholder - will be expanded
