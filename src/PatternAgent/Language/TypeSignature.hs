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
    -- * Programmatic Construction
  , createTypeNode
  , createFunctionTypePattern
    -- * JSON Schema Generation
  , typeSignatureToJSONSchema
  , parameterToJSONSchema
    -- * Validation
  , validateTypeSignature
  ) where

import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Pattern (Pattern(..))
import Pattern.Core (value, elements, patternWith)
import Subject.Core (Subject(..), Symbol(..))
import qualified Subject.Value as SubjectValue
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (unless)

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
-- Example: Pattern element representing "(personName::String {default:\"world\"})==>(::String)"
-- Returns parsed representation or error message.
--
-- The Pattern structure created by createFunctionTypePattern:
-- - Relationship pattern with "FunctionType" label
-- - Two elements: [sourceNode (parameter), targetNode (return type)]
extractTypeSignatureFromPattern :: Pattern Subject -> Either Text TypeSignature
extractTypeSignatureFromPattern patternElem = do
  -- Check if this is a FunctionType pattern
  let subject = value patternElem
  unless ("FunctionType" `Set.member` labels subject) $
    Left "Pattern element must have FunctionType label"
  
  -- Extract source and target nodes
  case elements patternElem of
    [sourceNode, targetNode] -> do
      -- Extract parameter from source node
      param <- extractParameterFromNode sourceNode
      
      -- Extract return type from target node
      returnType <- extractReturnTypeFromNode targetNode
      
      return $ TypeSignature [param] returnType
    _ -> Left "FunctionType pattern must have exactly 2 elements (source and target nodes)"
  
  where
    -- Extract parameter information from a type node
    extractParameterFromNode :: Pattern Subject -> Either Text Parameter
    extractParameterFromNode node = do
      let nodeSubject = value node
      let nodeLabels = labels nodeSubject
      
      -- Extract type label (should be one of: String, Integer, Number, Boolean, Object, Array)
      typeLabel <- case Set.toList nodeLabels of
        [label] -> Right $ T.pack label
        _ -> Left "Type node must have exactly one type label"
      
      -- Extract parameter name from identity
      let paramName = case identity nodeSubject of
            Symbol "" -> Nothing  -- Anonymous parameter
            Symbol name -> Just (T.pack name)
      
      -- Extract default value from properties
      -- Note: Subject.Value supports VString, VInteger, VBoolean, VSymbol, VRange
      -- For JSON schema, we convert to Aeson.Value
      let defaultVal = case Map.lookup "default" (properties nodeSubject) of
            Just (SubjectValue.VString s) -> Just (String (T.pack s))
            Just (SubjectValue.VInteger i) -> Just (Number (fromIntegral i))
            Just (SubjectValue.VBoolean b) -> Just (Bool b)
            -- For other types (VSymbol, VRange), we could convert to string if needed
            _ -> Nothing
      
      return $ Parameter paramName typeLabel defaultVal
    
    -- Extract return type information from a type node
    extractReturnTypeFromNode :: Pattern Subject -> Either Text Parameter
    extractReturnTypeFromNode node = do
      let nodeSubject = value node
      let nodeLabels = labels nodeSubject
      
      -- Extract type label
      typeLabel <- case Set.toList nodeLabels of
        [label] -> Right $ T.pack label
        _ -> Left "Return type node must have exactly one type label"
      
      -- Return type is always anonymous (no parameter name)
      return $ Parameter Nothing typeLabel Nothing

-- | Convert parsed type signature to JSON schema.
--
-- Generates JSON schema compatible with LLM function calling APIs.
typeSignatureToJSONSchema :: TypeSignature -> Value
typeSignatureToJSONSchema (TypeSignature params returnType) =
  object
    [ "type" .= ("object" :: Text)
    , "properties" .= object (mapMaybe paramToProperty params)
    , "required" .= Array (V.fromList $ map (toJSON . T.pack) $ requiredParamNames params)
    ]
  where
    paramToProperty (Parameter (Just name) typeLabel defaultVal) =
      Just (Key.fromString (T.unpack name), parameterToJSONSchema (Parameter (Just name) typeLabel defaultVal))
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
  case defaultVal of
    Just val -> object ["type" .= typeLabelToJSONType typeLabel, "default" .= val]
    Nothing -> object ["type" .= typeLabelToJSONType typeLabel]

-- | Convert gram type label to JSON Schema type string.
--
-- Type labels use capitalized JSON Schema type names (Gram label convention):
-- String, Integer, Number, Boolean, Object, Array
-- These map directly to JSON Schema types: string, integer, number, boolean, object, array
typeLabelToJSONType :: Text -> Text
typeLabelToJSONType label
  | label == "String" = "string"
  | label == "Integer" = "integer"
  | label == "Number" = "number"
  | label == "Boolean" = "boolean"
  | label == "Object" = "object"
  | label == "Array" = "array"
  | otherwise = error $ "Unsupported type label: " ++ T.unpack label ++ ". Supported types: String, Integer, Number, Boolean, Object, Array"

-- | Create a type node pattern programmatically.
--
-- Creates a Pattern Subject representing a type node (parameter or return type).
-- For universal type nodes (like String, Integer), use conventional identifiers
-- so all functions sharing the same return type reference the same node.
--
-- Type labels use capitalized JSON Schema type names: String, Integer, Number, Boolean, Object, Array
--
-- Examples:
-- - Parameter node: createTypeNode (Just "personName") "String" (Just (VString "world"))
-- - Return type node: createTypeNode Nothing "String" Nothing
createTypeNode
  :: Maybe Text        -- ^ Parameter name (Nothing for return types or anonymous)
  -> Text              -- ^ Type label (String, Integer, Number, Boolean, Object, Array)
  -> Maybe SubjectValue.Value  -- ^ Default value (for optional parameters, using Subject.Value)
  -> Pattern Subject   -- ^ Pattern Subject representing the type node
createTypeNode paramName typeLabel defaultVal =
  let identity = case paramName of
        Just name -> Symbol (T.unpack name)
        Nothing -> Symbol ""  -- Anonymous - will need identifier for uniqueness
      labels = Set.fromList [T.unpack typeLabel]
      properties = case defaultVal of
        Just val -> Map.fromList [("default", val)]
        Nothing -> Map.empty
      subject = Subject { identity = identity, labels = labels, properties = properties }
  in patternWith subject []

-- | Create a function type pattern programmatically (simple, single arrow).
--
-- Creates a Pattern Subject representing a function type signature.
-- The relationship pattern has FunctionType label and contains source and target nodes.
--
-- Type labels use capitalized JSON Schema type names: String, Integer, Number, Boolean, Object, Array
--
-- Example: createFunctionTypePattern (Just "personName") "String" (Just (SubjectValue.VString "world")) "String"
-- Creates: (personName::String {default:"world"})==>(arbString::String)
--
-- Note: For curried functions (multiple parameters), this is a future enhancement.
-- For now, this handles simple single-parameter functions.
createFunctionTypePattern
  :: Maybe Text        -- ^ Parameter name (Nothing for anonymous parameter)
  -> Text              -- ^ Parameter type label (String, Integer, Number, Boolean, Object, Array)
  -> Maybe SubjectValue.Value  -- ^ Default value (for optional parameters, using Subject.Value)
  -> Text              -- ^ Return type label (String, Integer, Number, Boolean, Object, Array)
  -> Pattern Subject   -- ^ Pattern Subject representing the function type
createFunctionTypePattern paramName paramType defaultVal returnType =
  -- Create source node (parameter)
  let sourceNode = createTypeNode paramName paramType defaultVal
      -- Create target node (return type) - use universal identifier convention
      -- All functions returning String share the same node: (arbString::String)
      returnTypeId = case returnType of
        "String" -> "arbString"
        "Integer" -> "arbInteger"
        "Number" -> "arbNumber"
        "Boolean" -> "arbBoolean"
        "Object" -> "arbObject"
        "Array" -> "arbArray"
        _ -> "arb" <> T.unpack returnType  -- Fallback convention
      targetNode = createTypeNode (Just (T.pack returnTypeId)) returnType Nothing
      -- Create relationship pattern with FunctionType label
      relationshipSubject = Subject
        { identity = Symbol ""  -- Anonymous relationship
        , labels = Set.fromList ["FunctionType"]
        , properties = Map.empty
        }
  in patternWith relationshipSubject [sourceNode, targetNode]

-- | Validate a type signature Pattern element.
--
-- Checks that the Pattern element represents a valid type signature.
validateTypeSignature :: Pattern Subject -> Either Text ()
validateTypeSignature patternElem = do
  -- Basic validation - ensure it's a path notation element
  -- Full validation will check path structure, node types, etc.
  Right ()  -- Placeholder - will be expanded
