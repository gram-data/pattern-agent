{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Hello World example agent and sayHello tool.
--
-- This module provides a concrete example of an agent that uses the sayHello tool
-- to respond to user greetings in friendly conversations.
--
-- Status: Phase 6 implementation complete.
-- See specs/003-hello-world-agent/ for design details.
--
-- This module provides:
-- - sayHello: Tool (Pattern) for the sayHello tool specification
-- - sayHelloImpl: ToolImpl implementation for the sayHello tool
-- - helloWorldToolLibrary: ToolLibrary with sayHello tool registered
-- - helloWorldAgent: Agent that uses the sayHello tool

module HelloWorldExample
  ( -- * Tool Specification
    sayHello
    -- * Tool Implementation
  , sayHelloImpl
    -- * Tool Library
  , helloWorldToolLibrary
    -- * Agent
  , helloWorldAgent
  ) where

import PatternAgent.Language.Core (Agent, Tool, createAgent, createTool, createModel, Provider(..), toolSchema)
import PatternAgent.Language.TypeSignature (createFunctionTypePattern, TypeSignature(..), Parameter(..), typeSignatureToJSONSchema)
import PatternAgent.Runtime.ToolLibrary (ToolImpl, ToolLibrary, createToolImpl, emptyToolLibrary, registerTool)
import Data.Aeson (Value(..), object, (.=), toJSON, (.:?))
import Data.Aeson.Types (parseMaybe, withObject)
import Control.Lens (view)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Subject.Value as SubjectValue

-- | sayHello Tool (Pattern) - tool specification.
--
-- Type signature: (personName::String {default:"world"})==>(::String)
-- This is the declarative, serializable tool specification.
-- Uses capitalized JSON Schema type names (String) following Gram label convention.
sayHello :: Tool
sayHello = case createTool
  "sayHello"
  "Returns a friendly greeting message for the given name"
  (createFunctionTypePattern
    (Just "personName")
    "String"
    (Just (SubjectValue.VString "world"))
    "String")
  of
  Right tool -> tool
  Left err -> error $ "Failed to create sayHello tool: " ++ T.unpack err

-- | sayHelloImpl ToolImpl - tool implementation.
--
-- This is the executable implementation that extracts the personName parameter
-- from JSON arguments and returns a friendly greeting.
sayHelloImpl :: ToolImpl
sayHelloImpl = case createToolImpl
  "sayHello"
  "Returns a friendly greeting message for the given name"
  (getSayHelloSchema)
  (\args -> do
    -- Extract personName from args, default to "world" if not provided
    let name = case parseMaybe (withObject "args" $ \obj -> obj .:? "personName") args of
          Just (Just (String n)) -> T.unpack n
          _ -> "world"
    return $ String $ "Hello, " <> T.pack name <> "! Nice to meet you."
  )
  of
  Right impl -> impl
  Left err -> error $ "Failed to create sayHelloImpl: " ++ T.unpack err
  where
    -- Generate schema from type signature directly
    -- Type signature: (personName::String {default:"world"})==>(::String)
    -- Uses capitalized JSON Schema type names (String) following Gram label convention
    getSayHelloSchema = typeSignatureToJSONSchema $ TypeSignature
      { typeParams = [Parameter (Just "personName") "String" (Just (toJSON ("world" :: T.Text)))]
      , typeReturn = Parameter Nothing "String" Nothing
      }

-- | helloWorldToolLibrary - ToolLibrary with sayHello tool registered.
--
-- This is the runtime tool library that maps tool names to implementations.
-- Used during agent execution to bind tool specifications to implementations.
helloWorldToolLibrary :: ToolLibrary
helloWorldToolLibrary = registerTool "sayHello" sayHelloImpl emptyToolLibrary

-- | helloWorldAgent - Agent that uses the sayHello tool.
--
-- This agent is configured to:
-- - Use OpenAI gpt-3.5-turbo model
-- - Have friendly conversations with users
-- - Use the sayHello tool when responding to greetings
helloWorldAgent :: Agent
helloWorldAgent = case createAgent
  "hello_world_agent"
  (Just "A friendly agent that uses the sayHello tool to greet users")
  (createModel "gpt-3.5-turbo" OpenAI)
  "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
  [sayHello]
  of
  Right agent -> agent
  Left err -> error $ "Failed to create helloWorldAgent: " ++ T.unpack err

