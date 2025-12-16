{-# LANGUAGE OverloadedStrings #-}
-- | Test execution helpers using Mock LLM.
--
-- This module provides test-specific execution functions that use MockLLM
-- instead of real LLM API calls, enabling fast scenario tests.
module TestExecution
  ( -- * Test Execution
    executeAgentWithMockLLM
    -- * Re-exported
  , AgentError(..)
  , AgentResponse(..)
  , ToolInvocation(..)
  ) where

import PatternAgent.Language.Core (Agent, agentModel, agentInstruction, agentTools, toolName, toolDescription, toolSchema)
import PatternAgent.Runtime.LLM (LLMMessage(..), FunctionCall(..), LLMResponse(..))
import PatternAgent.Runtime.Context
  ( ConversationContext
  , Message(..)
  , MessageRole(..)
  , addMessage
  )
import PatternAgent.Runtime.ToolLibrary (ToolLibrary, ToolImpl, bindTool, lookupTool, validateToolArgs, toolImplInvoke, toolImplName, toolImplSchema)
import PatternAgent.Runtime.Execution (AgentError(..), AgentResponse(..), ToolInvocation(..), contextToLLMMessages, toolsToFunctions)
import MockLLM (MockLLMClient(..), createMockClient, mockCallLLM)
import Data.Aeson (Value(..), object, (.=), decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Control.Lens (view)
import Control.Monad (mapM, when)
import Control.Exception (try, SomeException)

-- | Execute an agent with mock LLM (for testing).
--
-- Same interface as executeAgentWithLibrary but uses MockLLM instead of real API.
executeAgentWithMockLLM
  :: Agent                -- ^ agent: Agent with Tools (Pattern)
  -> Text                 -- ^ userInput: User's input message
  -> ConversationContext  -- ^ context: Previous conversation context
  -> ToolLibrary          -- ^ library: Tool library for binding
  -> IO (Either AgentError AgentResponse)
executeAgentWithMockLLM agent userInput context library = do
  -- Validate input
  if T.null userInput
    then return $ Left $ ValidationError "Empty user input"
    else do
      -- Bind tools
      boundToolsResult <- return $ bindAgentTools agent library
      case boundToolsResult of
        Left err -> return $ Left $ ToolError err
        Right boundTools -> do
          -- Create mock LLM client
          let model = view agentModel agent
          let mockClient = createMockClient model
          
          -- Add user message to context
          let userMessageResult = addMessage UserRole userInput context
          case userMessageResult of
            Left err -> return $ Left $ ValidationError err
            Right updatedContext -> do
              -- Execute iterative loop with mock LLM
              executeIteration mockClient agent boundTools updatedContext [] 0
  where
    -- Maximum iteration limit to prevent infinite loops
    maxIterations = 10
    
    -- Execute one iteration of the tool execution loop (using mock LLM)
    executeIteration
      :: MockLLMClient
      -> Agent
      -> [ToolImpl]
      -> ConversationContext
      -> [ToolInvocation]  -- Accumulated tool invocations
      -> Int               -- Current iteration count
      -> IO (Either AgentError AgentResponse)
    executeIteration mockClient agent boundTools context toolInvocations iteration
      | iteration >= maxIterations = return $ Left $ ToolError "Maximum iteration limit reached"
      | otherwise = do
          -- Build LLM request
          let model = view agentModel agent
          let instruction = view agentInstruction agent
          let tools = view agentTools agent
          let functions = if null tools then Nothing else Just (toolsToFunctions tools)
          let messages = contextToLLMMessages context  -- Full conversation history including tool invocations
          
          -- Call mock LLM
          llmResult <- mockCallLLM mockClient model instruction messages Nothing Nothing functions
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
                      -- Add assistant message with tool call to context
                      let assistantContent = if T.null (responseText llmResponse)
                            then "Calling " <> functionCallName functionCall
                            else responseText llmResponse
                      let assistantMsgResult = addMessage AssistantRole assistantContent context
                      case assistantMsgResult of
                        Left err -> return $ Left $ ValidationError err
                        Right contextWithAssistant -> do
                          -- Add function message with tool result to context
                          let functionContent = case invocationResult invocation of
                                Right val -> T.pack $ show val  -- Simplified - would use proper JSON encoding
                                Left err -> "Error: " <> err
                          let functionMsgResult = addMessage (FunctionRole (invocationToolName invocation)) functionContent contextWithAssistant
                          case functionMsgResult of
                            Left err -> return $ Left $ ValidationError err
                            Right contextWithFunction -> do
                              -- Continue iteration with updated context
                              executeIteration mockClient agent boundTools contextWithFunction (invocation : toolInvocations) (iteration + 1)
                
                Nothing -> do
                  -- No function call - final text response
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
          let argsValue = case decode (BL.fromStrict (TE.encodeUtf8 argsJson)) of
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

