{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Core language definitions for Pattern Agent.
--
-- This module provides the portable language specification - what gram notation
-- means for agent workflows. All types are Pattern Subject (serializable, portable).
--
-- This is the reference implementation that other languages (Python, JavaScript)
-- should implement to support the same gram notation format.
module PatternAgent.Language.Core
  ( -- * Language Types (Pattern Subject)
    Agent
  , Tool
  , Model(..)
  , Provider(..)
    -- * Agent Lenses
  , agentName
  , agentDescription
  , agentModel
  , agentInstruction
  , agentTools
    -- * Tool Lenses
  , toolName
  , toolDescription
  , toolTypeSignature
  , toolSchema
    -- * Agent Creation
  , createAgent
  , validateAgent
    -- * Tool Creation
  , createTool
  , validateTool
    -- * Model Creation
  , createModel
  ) where

import Control.Lens (Lens', lens, view, set)
import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Pattern (Pattern(..))
import Pattern.Core (value, elements, patternWith)
import Subject.Core (Subject(..), Symbol(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Subject.Value (Value(..))
import qualified Gram
import PatternAgent.Language.TypeSignature (typeSignatureToJSONSchema, extractTypeSignatureFromPattern, validateTypeSignature)

-- | Model provider enumeration.
data Provider
  = OpenAI
  | Anthropic
  | Google
  deriving (Eq, Show)

-- | Model type representing an LLM model identifier and provider.
-- Stored as simple string in Pattern: "OpenAI/gpt-3.5-turbo"
data Model = Model
  { modelId :: Text
  , modelProvider :: Provider
  }
  deriving (Eq, Show)

-- | Agent type - Pattern Subject representing an agent workflow specification.
-- This is the canonical, portable representation.
type Agent = Pattern Subject

-- | Tool type - Pattern Subject representing a tool specification.
-- This is the canonical, portable representation.
type Tool = Pattern Subject

-- TODO: Implement lenses using TemplateHaskell or manual lens definitions
-- For now, these are placeholders that will be implemented when Pattern Subject
-- structure is fully defined. The lenses will access Pattern Subject fields
-- using gram-hs operations.

-- | Lens for agent name (pattern identifier).
agentName :: Lens' Agent Text
agentName = lens getter setter
  where
    getter p = case identity (value p) of
      Symbol s -> T.pack s
    setter p n = p { value = (value p) { identity = Symbol (T.unpack n) } }

-- | Lens for agent description (property).
agentDescription :: Lens' Agent (Maybe Text)
agentDescription = lens getter setter
  where
    getter p = case Map.lookup "description" (properties (value p)) of
      Just (VString s) -> Just (T.pack s)
      _ -> Nothing
    setter p (Just desc) = p
      { value = (value p)
          { properties = Map.insert "description" (VString (T.unpack desc)) (properties (value p))
          }
      }
    setter p Nothing = p
      { value = (value p)
          { properties = Map.delete "description" (properties (value p))
          }
      }

-- | Lens for agent model (property, stored as "provider/model-name" string).
agentModel :: Lens' Agent Model
agentModel = lens getter setter
  where
    getter p = case Map.lookup "model" (properties (value p)) of
      Just (VString modelStr) -> parseModel (T.pack modelStr)
      _ -> Model { modelId = "gpt-4o-mini", modelProvider = OpenAI }  -- Default
    setter p model = p
      { value = (value p)
          { properties = Map.insert "model" (VString (T.unpack (modelToString model))) (properties (value p))
          }
      }
    parseModel :: Text -> Model
    parseModel s = case T.splitOn "/" s of
      [providerStr, modelIdStr] -> Model
        { modelId = modelIdStr
        , modelProvider = case providerStr of
            "OpenAI" -> OpenAI
            "Anthropic" -> Anthropic
            "Google" -> Google
            _ -> OpenAI  -- Default
        }
      _ -> Model { modelId = s, modelProvider = OpenAI }  -- Fallback

-- | Lens for agent instruction (property).
agentInstruction :: Lens' Agent Text
agentInstruction = lens getter setter
  where
    getter p = case Map.lookup "instruction" (properties (value p)) of
      Just (VString s) -> T.pack s
      _ -> T.empty
    setter p inst = p
      { value = (value p)
          { properties = Map.insert "instruction" (VString (T.unpack inst)) (properties (value p))
          }
      }

-- | Lens for agent tools (nested pattern elements).
agentTools :: Lens' Agent [Tool]
agentTools = lens getter setter
  where
    getter p = elements p
    setter p tools = p { elements = tools }

-- | Lens for tool name (pattern identifier).
toolName :: Lens' Tool Text
toolName = lens getter setter
  where
    getter p = case identity (value p) of
      Symbol s -> T.pack s
    setter p n = p { value = (value p) { identity = Symbol (T.unpack n) } }

-- | Lens for tool description (property).
toolDescription :: Lens' Tool Text
toolDescription = lens getter setter
  where
    getter p = case Map.lookup "description" (properties (value p)) of
      Just (VString s) -> T.pack s
      _ -> T.empty
    setter p desc = p
      { value = (value p)
          { properties = Map.insert "description" (VString (T.unpack desc)) (properties (value p))
          }
      }

-- | Lens for tool type signature (gram path notation in pattern elements).
-- Extracts the type signature path from pattern elements and serializes to text.
toolTypeSignature :: Lens' Tool Text
toolTypeSignature = lens getter setter
  where
    getter p = case elements p of
      [] -> T.empty
      [typeSigElem] -> T.pack $ Gram.toGram typeSigElem
      _ -> T.empty  -- Multiple elements not expected for type signature
    setter p sig = p
      { elements = case Gram.fromGram (T.unpack sig) of
          Right parsed -> [parsed]
          Left _ -> elements p  -- Keep existing if parse fails
      }

-- | Lens for tool JSON schema (generated from type signature, not stored).
-- Extracts type signature from pattern elements and converts to JSON schema.
toolSchema :: Lens' Tool (Data.Aeson.Value)
toolSchema = lens getter setter
  where
    getter p = case elements p of
      [] -> object ["type" .= ("object" :: Text), "properties" .= object []]
      [typeSigElem] -> case extractTypeSignatureFromPattern typeSigElem of
        Right typeSig -> typeSignatureToJSONSchema typeSig
        Left err -> do
          -- Debug: Log the error (but we can't use IO in a pure lens, so we'll handle this differently)
          -- For now, return empty schema - the error will be logged elsewhere
          object ["type" .= ("object" :: Text), "properties" .= object []]
      _ -> object ["type" .= ("object" :: Text), "properties" .= object []]  -- Multiple elements not expected
    setter p _ = p  -- Schema is computed, cannot be set directly

-- | Create an agent from components.
--
-- This constructs a Pattern Subject representing an agent workflow.
-- The agent name becomes the pattern identifier, other fields are properties.
createAgent
  :: Text                    -- ^ name: Agent identifier (becomes pattern identifier)
  -> Maybe Text              -- ^ description: Optional description (property)
  -> Model                   -- ^ model: LLM model (property, stored as string)
  -> Text                    -- ^ instruction: Agent instructions (property)
  -> [Tool]                  -- ^ tools: List of tool specifications (nested patterns)
  -> Either Text Agent       -- ^ Returns Right Agent or Left error message
createAgent name description model instruction tools
  | T.null name = Left "Agent name cannot be empty"
  | T.null instruction = Left "Agent instruction cannot be empty"
  | otherwise = do
      -- Validate unique tool names
      let toolNames = map (view toolName) tools
      let uniqueNames = List.nub toolNames
      unless (length toolNames == length uniqueNames) $
        Left "Tool names must be unique within agent's tool list"
      
      -- Construct properties map
      let baseProps = Map.fromList
            [ ("model", VString (T.unpack (modelToString model)))
            , ("instruction", VString (T.unpack instruction))
            ]
      let props = case description of
            Just desc -> Map.insert "description" (VString (T.unpack desc)) baseProps
            Nothing -> baseProps
      
      -- Construct Pattern Subject with Agent label, properties, and tools as elements
      let subject = Subject
            { identity = Symbol (T.unpack name)
            , labels = Set.fromList ["Agent"]
            , properties = props
            }
      Right $ patternWith subject tools

-- | Validate an agent pattern structure.
--
-- Checks that the agent has required fields and valid structure.
validateAgent :: Agent -> Either Text ()
validateAgent agent = do
  -- Check Agent label
  unless ("Agent" `Set.member` labels (value agent)) $
    Left "Agent must have 'Agent' label"
  
  -- Check non-empty name (pattern identifier)
  let ident = identity (value agent)
  case ident of
    Symbol s | T.null (T.pack s) -> Left "Agent must have non-empty name (pattern identifier)"
    Symbol _ -> return ()
  
  -- Check required properties: instruction and model
  let props = properties (value agent)
  case Map.lookup "instruction" props of
    Just (VString inst) | T.null (T.pack inst) -> Left "Agent instruction cannot be empty"
    Just (VString _) -> return ()
    _ -> Left "Agent must have 'instruction' property"
  
  case Map.lookup "model" props of
    Just (VString _) -> return ()  -- Model is stored as string, format validation happens elsewhere
    _ -> Left "Agent must have 'model' property"
  
  -- Validate nested tool patterns (if any)
  let tools = elements agent
  mapM_ validateTool tools

-- | Create a tool from components (programmatic, no parsing).
--
-- This constructs a Pattern Subject representing a tool specification.
-- The tool name becomes the pattern identifier, description is a property,
-- and type signature is stored in pattern elements.
--
-- For parsing from gram notation, use PatternAgent.Language.Serialization.parseTool.
createTool
  :: Text                    -- ^ name: Tool identifier (becomes pattern identifier)
  -> Text                    -- ^ description: Tool description (property)
  -> Pattern Subject         -- ^ typeSignature: Type signature as Pattern Subject element (no parsing)
  -> Either Text Tool        -- ^ Returns Right Tool or Left error message
createTool name description typeSigPattern
  | T.null name = Left "Tool name cannot be empty"
  | T.null description = Left "Tool description cannot be empty"
  | otherwise = do
      -- Validate the pattern represents a valid type signature
      _ <- validateTypeSignature typeSigPattern
      
      -- Construct Pattern Subject with Tool label, description property, and type signature as element
      let subject = Subject
            { identity = Symbol (T.unpack name)
            , labels = Set.fromList ["Tool"]
            , properties = Map.fromList [("description", VString (T.unpack description))]
            }
      Right $ patternWith subject [typeSigPattern]

-- | Validate a tool pattern structure.
--
-- Checks that the tool has required fields and valid type signature.
validateTool :: Tool -> Either Text ()
validateTool tool = do
  -- Check Tool label
  unless ("Tool" `Set.member` labels (value tool)) $
    Left "Tool must have 'Tool' label"
  
  -- Check non-empty name (pattern identifier)
  let ident = identity (value tool)
  case ident of
    Symbol s | T.null (T.pack s) -> Left "Tool must have non-empty name (pattern identifier)"
    Symbol _ -> return ()
  
  -- Check description property exists and is non-empty
  let props = properties (value tool)
  case Map.lookup "description" props of
    Just (VString desc) | T.null (T.pack desc) -> Left "Tool description cannot be empty"
    Just (VString _) -> return ()
    _ -> Left "Tool must have 'description' property"
  
  -- Check type signature element exists
  case elements tool of
    [] -> Left "Tool must have type signature in elements"
    [typeSigElem] -> validateTypeSignature typeSigElem
    _ -> Left "Tool should have exactly one type signature element"

-- | Helper function to convert Model to string representation.
modelToString :: Model -> Text
modelToString m = T.pack (show (modelProvider m)) <> "/" <> modelId m

-- | Create a model identifier for a specific provider.
--
-- Returns a string representation "provider/model-name" for storage in Pattern.
createModel :: Text -> Provider -> Model
createModel modelId provider = Model { modelId = modelId, modelProvider = provider }
