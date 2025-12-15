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
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Pattern (Pattern(..))
import Pattern.Core (value, elements)
import Subject.Core (Subject(..), Symbol(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Subject.Value (Value(..))
import qualified Gram
import PatternAgent.Language.TypeSignature (typeSignatureToJSONSchema, extractTypeSignatureFromPattern)

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
agentName = undefined -- TODO: Implement using Pattern Subject identifier access

-- | Lens for agent description (property).
agentDescription :: Lens' Agent (Maybe Text)
agentDescription = undefined -- TODO: Implement using Pattern Subject property access

-- | Lens for agent model (property, stored as "provider/model-name" string).
agentModel :: Lens' Agent Model
agentModel = undefined -- TODO: Implement using Pattern Subject property access

-- | Lens for agent instruction (property).
agentInstruction :: Lens' Agent Text
agentInstruction = undefined -- TODO: Implement using Pattern Subject property access

-- | Lens for agent tools (nested pattern elements).
agentTools :: Lens' Agent [Tool]
agentTools = undefined -- TODO: Implement using Pattern Subject elements access

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
toolSchema :: Lens' Tool Value
toolSchema = lens getter setter
  where
    getter p = case elements p of
      [] -> object ["type" .= ("object" :: Text), "properties" .= object []]
      [typeSigElem] -> case extractTypeSignatureFromPattern typeSigElem of
        Right typeSig -> typeSignatureToJSONSchema typeSig
        Left _ -> object ["type" .= ("object" :: Text), "properties" .= object []]  -- Default empty schema on error
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
  | otherwise = undefined -- TODO: Construct Pattern Subject with identifier=name, properties={description, model, instruction}, elements=tools

-- | Validate an agent pattern structure.
--
-- Checks that the agent has required fields and valid structure.
validateAgent :: Agent -> Either Text ()
validateAgent agent = undefined -- TODO: Validate Pattern Subject structure

-- | Create a tool from components.
--
-- This constructs a Pattern Subject representing a tool specification.
-- The tool name becomes the pattern identifier, description is a property,
-- and type signature is stored in pattern elements.
createTool
  :: Text                    -- ^ name: Tool identifier (becomes pattern identifier)
  -> Text                    -- ^ description: Tool description (property)
  -> Text                    -- ^ typeSignature: Gram path notation type signature (elements)
  -> Either Text Tool        -- ^ Returns Right Tool or Left error message
createTool name description typeSignature
  | T.null name = Left "Tool name cannot be empty"
  | T.null description = Left "Tool description cannot be empty"
  | T.null typeSignature = Left "Tool type signature cannot be empty"
  | otherwise = undefined -- TODO: Construct Pattern Subject with identifier=name, properties={description}, elements=typeSignature

-- | Validate a tool pattern structure.
--
-- Checks that the tool has required fields and valid type signature.
validateTool :: Tool -> Either Text ()
validateTool tool = undefined -- TODO: Validate Pattern Subject structure and type signature

-- | Create a model identifier for a specific provider.
--
-- Returns a string representation "provider/model-name" for storage in Pattern.
createModel :: Text -> Provider -> Model
createModel modelId provider = Model { modelId = modelId, modelProvider = provider }
