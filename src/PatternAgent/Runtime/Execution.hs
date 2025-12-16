{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Agent execution engine (Runtime).
--
-- This module provides the core execution infrastructure for LLM agents,
-- including error handling and agent execution logic that uses the
-- standalone LLM client module.
--
-- This is part of the runtime implementation (Haskell-specific).
module PatternAgent.Runtime.Execution
  ( -- * Error Types
    AgentError(..)
    -- * Agent Execution
  , AgentResponse(..)
  , ToolInvocation(..)
  , executeAgent
  , executeAgentWithLibrary
    -- * Tool Binding
  , bindAgentTools
    -- * Context Conversion
  , contextToLLMMessages
  ) where

import PatternAgent.Language.Core (Agent, Tool, agentModel, agentInstruction, agentTools, toolName, toolDescription, toolSchema)
import PatternAgent.Runtime.LLM (LLMClient, LLMMessage(..), LLMResponse(..), FunctionCall(..), callLLM, createClientForModel, ApiKeyError(..))
import PatternAgent.Runtime.Context
  ( ConversationContext
  , Message(..)
  , MessageRole(..)
  , addMessage
  )
import PatternAgent.Runtime.ToolLibrary (ToolLibrary, ToolImpl, bindTool, lookupTool, validateToolArgs, toolImplInvoke, toolImplName, toolImplSchema)
import Data.Aeson (Value(..), object, (.=), decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Control.Lens (view)
import Control.Monad (mapM, when)
import Control.Exception (try, SomeException)
import qualified Data.Map.Strict as Map

-- | Error types for agent execution.
data AgentError
  = LLMAPIError Text      -- ^ LLM API call failed
  | ToolError Text        -- ^ Tool execution failed
  | ValidationError Text  -- ^ Input validation failed
  | ConfigurationError Text  -- ^ Agent configuration invalid (e.g., missing API key)
  deriving (Eq, Show, Generic)

-- | Agent response type.
data AgentResponse = AgentResponse
  { responseContent :: Text
  , responseToolsUsed :: [ToolInvocation]
  }
  deriving (Eq, Show, Generic)

-- | Tool invocation record.
data ToolInvocation = ToolInvocation
  { invocationToolName :: Text
  , invocationArgs :: Value
  , invocationResult :: Either Text Value
  }
  deriving (Eq, Show, Generic)

-- | Convert Context.Message to LLM.LLMMessage format.
contextToLLMMessage :: Message -> LLMMessage
contextToLLMMessage (Message role content) = LLMMessage
  { llmMessageRole = case role of
      UserRole -> "user"
      AssistantRole -> "assistant"
      FunctionRole toolName -> "function"
  , llmMessageContent = content
  , llmMessageName = case role of
      FunctionRole name -> Just name
      _ -> Nothing
  }

-- | Convert conversation context to LLM message list.
contextToLLMMessages :: ConversationContext -> [LLMMessage]
contextToLLMMessages = map contextToLLMMessage

-- | Execute an agent with user input and return the agent's response.
executeAgent
  :: Agent                -- ^ agent: Agent to execute (Pattern)
  -> Text                 -- ^ userInput: User's input message
  -> ConversationContext  -- ^ context: Previous conversation context
  -> IO (Either AgentError AgentResponse)
executeAgent agent userInput context
  | T.null userInput = return $ Left $ ValidationError "Empty user input"
  | otherwise = do
      -- TODO: Access agent fields using lenses
      -- For now, this is a placeholder that will be implemented when
      -- Pattern Subject structure and lenses are fully defined
      undefined

-- | Convert Tools to OpenAI functions format.
toolsToFunctions :: [Tool] -> [Value]
toolsToFunctions tools = map toolToFunction tools
  where
    toolToFunction tool = object
      [ "name" .= view toolName tool
      , "description" .= view toolDescription tool
      , "parameters" .= view toolSchema tool
      ]

-- | Execute an agent with tool library support.
--
-- Binds tools from agent to implementations in library, then executes.
-- Implements iterative execution loop: detect tool call → validate → invoke → send result to LLM → get final response.
executeAgentWithLibrary
  :: Agent                -- ^ agent: Agent with Tools (Pattern)
  -> Text                 -- ^ userInput: User's input message
  -> ConversationContext  -- ^ context: Previous conversation context
  -> ToolLibrary          -- ^ library: Tool library for binding
  -> IO (Either AgentError AgentResponse)
executeAgentWithLibrary agent userInput context library = do
  -- Validate input
  if T.null userInput
    then return $ Left $ ValidationError "Empty user input"
    else do
      -- Bind tools
      boundToolsResult <- return $ bindAgentTools agent library
      case boundToolsResult of
        Left err -> return $ Left $ ToolError err
        Right boundTools -> do
          -- Create LLM client
          let model = view agentModel agent
          clientResult <- createClientForModel model
          case clientResult of
            Left apiKeyErr -> return $ Left $ ConfigurationError $ case apiKeyErr of
              ApiKeyNotFound msg -> msg
              ApiKeyInvalid msg -> msg
            Right client -> do
              -- T108: Add user message to context (first message in conversation)
              let userMessageResult = addMessage UserRole userInput context
              case userMessageResult of
                Left err -> return $ Left $ ValidationError err
                Right updatedContext -> do
                  -- Execute iterative loop
                  executeIteration client agent boundTools updatedContext [] 0
  where
    -- Maximum iteration limit to prevent infinite loops
    maxIterations = 10
    
    -- Execute one iteration of the tool execution loop
    executeIteration
      :: LLMClient
      -> Agent
      -> [ToolImpl]
      -> ConversationContext
      -> [ToolInvocation]  -- Accumulated tool invocations
      -> Int               -- Current iteration count
      -> IO (Either AgentError AgentResponse)
    executeIteration client agent boundTools context toolInvocations iteration
      | iteration >= maxIterations = return $ Left $ ToolError "Maximum iteration limit reached"
      | otherwise = do
          -- Build LLM request
          -- T106: Verify conversation context is properly passed through iterative execution loop
          -- T107: Verify LLM API requests include full conversation history with tool invocations
          let model = view agentModel agent
          let instruction = view agentInstruction agent
          let tools = view agentTools agent
          let functions = if null tools then Nothing else Just (toolsToFunctions tools)
          let messages = contextToLLMMessages context  -- Full conversation history including tool invocations
          
          -- Call LLM
          llmResult <- callLLM client model instruction messages Nothing Nothing functions
          case llmResult of
            Left err -> return $ Left $ LLMAPIError err
            Right llmResponse -> do
              -- Check if function call is present
              case responseFunctionCall llmResponse of
                Just functionCall -> do
                  -- Tool call detected - invoke tool
                  toolInvocationResult <- invokeToolFromFunctionCall functionCall boundTools
                  case toolInvocationResult of
                    Left err -> return $ Left err
                    Right invocation -> do
                      -- T108: Verify context updates include user message, assistant message with tool call, function message with tool result, and final assistant response
                      -- Add assistant message with tool call to context
                      let assistantContent = if T.null (responseText llmResponse)
                            then "Calling " <> functionCallName functionCall
                            else responseText llmResponse
                      let assistantMsgResult = addMessage AssistantRole assistantContent context
                      case assistantMsgResult of
                        Left err -> return $ Left $ ValidationError err
                        Right contextWithAssistant -> do
                          -- T105: Verify conversation context includes FunctionRole messages for tool results
                          -- Add function message with tool result to context
                          let functionContent = case invocationResult invocation of
                                Right val -> T.pack $ show val  -- Simplified - would use proper JSON encoding
                                Left err -> "Error: " <> err
                          let functionMsgResult = addMessage (FunctionRole (invocationToolName invocation)) functionContent contextWithAssistant
                          case functionMsgResult of
                            Left err -> return $ Left $ ValidationError err
                            Right contextWithFunction -> do
                              -- T106: Continue iteration with updated context (context is passed through loop)
                              executeIteration client agent boundTools contextWithFunction (invocation : toolInvocations) (iteration + 1)
                
                Nothing -> do
                  -- No function call - final text response
                  -- T108: Add final assistant response to context
                  let finalContent = responseText llmResponse
                  if T.null finalContent
                    then return $ Left $ LLMAPIError "LLM returned empty response"
                    else do
                      -- Add final assistant message to context (for conversation history)
                      let finalMsgResult = addMessage AssistantRole finalContent context
                      case finalMsgResult of
                        Left err -> return $ Left $ ValidationError err
                        Right _ -> do
                          -- Return final response
                          return $ Right $ AgentResponse
                            { responseContent = finalContent
                            , responseToolsUsed = reverse toolInvocations  -- Reverse to get chronological order
                            }
    
    -- Invoke tool from function call
    invokeToolFromFunctionCall
      :: FunctionCall
      -> [ToolImpl]
      -> IO (Either AgentError ToolInvocation)
    invokeToolFromFunctionCall functionCall boundTools = do
      -- Find tool implementation
      let toolName = functionCallName functionCall
      let toolImpl = findToolImpl toolName boundTools
      case toolImpl of
        Nothing -> return $ Left $ ToolError $ "Tool '" <> toolName <> "' not found in bound tools"
        Just impl -> do
          -- Parse arguments JSON
          let argsJson = functionCallArguments functionCall
          let argsValue = case decode (BL.fromStrict $ TE.encodeUtf8 argsJson) of
                Just val -> val
                Nothing -> object []  -- Default to empty object if parsing fails
          
          -- Validate arguments
          let schema = toolImplSchema impl
          case validateToolArgs schema argsValue of
            Left err -> return $ Right $ ToolInvocation
              { invocationToolName = toolName
              , invocationArgs = argsValue
              , invocationResult = Left err
              }
            Right validatedArgs -> do
              -- Invoke tool
              result <- tryInvokeTool impl validatedArgs
              return $ Right $ ToolInvocation
                { invocationToolName = toolName
                , invocationArgs = validatedArgs
                , invocationResult = result
                }
      where
        findToolImpl :: Text -> [ToolImpl] -> Maybe ToolImpl
        findToolImpl name tools = foldr (\tool acc -> if toolImplName tool == name then Just tool else acc) Nothing tools
        
        tryInvokeTool :: ToolImpl -> Value -> IO (Either Text Value)
        tryInvokeTool impl args = do
          result <- try (toolImplInvoke impl args) :: IO (Either SomeException Value)
          case result of
            Left ex -> return $ Left $ T.pack $ show ex
            Right val -> return $ Right val

-- | Bind all agent tools to implementations from library.
bindAgentTools
  :: Agent                -- ^ agent: Agent with Tools (Pattern)
  -> ToolLibrary          -- ^ library: Tool library
  -> Either Text [ToolImpl]   -- ^ Bound tool implementations or error
bindAgentTools agent library = do
  let tools = view agentTools agent
  -- Bind each tool to its implementation
  boundTools <- mapM (\tool -> 
    case bindTool tool library of
      Just toolImpl -> Right toolImpl
      Nothing -> Left $ "Tool '" <> view toolName tool <> "' not found in library or validation failed"
    ) tools
  return boundTools
