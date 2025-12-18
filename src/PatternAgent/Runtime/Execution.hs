{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Agent execution engine (Runtime).
--
-- This module provides the core execution infrastructure for LLM agents,
-- including error handling and agent execution logic that uses the
-- standalone LLM client module.
--
-- == Edge Case Handling
--
-- This module handles the following edge cases:
--
-- * __Tool timeout scenarios__: Tools that take too long are timed out
-- * __Multiple simultaneous tool calls__: Not currently supported by OpenAI function calling format
-- * __Agent with no tools but LLM requests tool call__: Returns graceful error
-- * __Malformed tool call requests from LLM__: Validated and handled with error messages
-- * __Tool not found in library__: Clear error message returned
-- * __Tool parameter validation failures__: Detailed validation error returned
-- * __Tool execution exceptions__: Caught and converted to tool invocation errors
-- * __Maximum iteration limit__: Prevents infinite tool call loops
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
    -- * Tool Conversion
  , toolsToFunctions
    -- * Configuration
  , defaultToolTimeout
  , maxIterations
  ) where

import PatternAgent.Language.Core (Agent, Tool, Model(..), agentModel, agentInstruction, agentTools, toolName, toolDescription, toolSchema)
import PatternAgent.Runtime.LLM (LLMClient, LLMMessage(..), LLMResponse(..), FunctionCall(..), callLLM, createClientForModel, ApiKeyError(..))
import PatternAgent.Runtime.Context
  ( ConversationContext
  , Message(..)
  , MessageRole(..)
  , addMessage
  )
import PatternAgent.Runtime.ToolLibrary (ToolLibrary, ToolImpl, bindTool, validateToolArgs, toolImplInvoke, toolImplName, toolImplSchema)
import Data.Aeson (Value(..), object, (.=), decode, ToJSON(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import PatternAgent.Runtime.Logging (logDebug, logDebugJSON, loggerExecution)
import PatternAgent.Runtime.LLM (buildRequest)
import Control.Lens (view)
import Control.Monad (mapM)
import Control.Exception (try, SomeException)
import System.Timeout (timeout)

-- | Default tool timeout in microseconds (30 seconds).
--
-- Tools that take longer than this will be terminated and return a timeout error.
defaultToolTimeout :: Int
defaultToolTimeout = 30 * 1000000  -- 30 seconds in microseconds

-- | Maximum iteration limit to prevent infinite loops.
--
-- The execution loop will terminate after this many tool call iterations.
maxIterations :: Int
maxIterations = 10

-- | Error types for agent execution.
--
-- These errors cover various failure scenarios during agent execution:
--
-- * 'LLMAPIError' - Network or API errors when communicating with LLM
-- * 'ToolError' - Errors during tool execution (not found, validation, timeout)
-- * 'ValidationError' - Invalid input or state errors
-- * 'ConfigurationError' - Missing API keys or invalid agent configuration
-- * 'ToolTimeoutError' - Tool execution exceeded time limit
-- * 'UnexpectedToolCallError' - LLM requested tool but agent has no tools
data AgentError
  = LLMAPIError Text            -- ^ LLM API call failed
  | ToolError Text              -- ^ Tool execution failed
  | ValidationError Text        -- ^ Input validation failed
  | ConfigurationError Text     -- ^ Agent configuration invalid (e.g., missing API key)
  | ToolTimeoutError Text       -- ^ Tool execution timed out
  | UnexpectedToolCallError Text -- ^ LLM requested tool but agent has no tools configured
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

instance ToJSON ToolInvocation where
  toJSON inv = object
    [ "tool_name" .= invocationToolName inv
    , "args" .= invocationArgs inv
    , "result" .= case invocationResult inv of
        Left err -> object ["error" .= err]
        Right val -> object ["success" .= val]
    ]

instance ToJSON AgentResponse where
  toJSON resp = object
    [ "content" .= responseContent resp
    , "tools_used" .= responseToolsUsed resp
    ]

-- | Convert Context.Message to LLM.LLMMessage format.
contextToLLMMessage :: Message -> LLMMessage
contextToLLMMessage (Message role content) = LLMMessage
  { llmMessageRole = case role of
      UserRole -> "user"
      AssistantRole -> "assistant"
      FunctionRole _ -> "function"
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
  :: Bool                 -- ^ debug: Enable debug logging
  -> Agent                -- ^ agent: Agent with Tools (Pattern)
  -> Text                 -- ^ userInput: User's input message
  -> ConversationContext  -- ^ context: Previous conversation context
  -> ToolLibrary          -- ^ library: Tool library for binding
  -> IO (Either AgentError AgentResponse)
executeAgentWithLibrary debug agent userInput context library = do
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
              logDebug debug loggerExecution $ "Adding user message to context: " <> userInput
              let userMessageResult = addMessage UserRole userInput context
              case userMessageResult of
                Left err -> return $ Left $ ValidationError err
                Right updatedContext -> do
                  -- Execute iterative loop
                  executeIteration debug client agent boundTools updatedContext [] 0
  where
    -- Execute one iteration of the tool execution loop
    executeIteration
      :: Bool              -- ^ debug: Enable debug logging
      -> LLMClient
      -> Agent
      -> [ToolImpl]
      -> ConversationContext
      -> [ToolInvocation]  -- Accumulated tool invocations
      -> Int               -- Current iteration count
      -> IO (Either AgentError AgentResponse)
    executeIteration debug client agent boundTools context toolInvocations iteration
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
          
          -- Build LLM request
          let llmRequest = buildRequest model instruction messages Nothing Nothing functions
          
          -- DEBUG: Log conversation context and LLM request details
          logDebug debug loggerExecution "Conversation context before LLM call:"
          mapM_ (\msg -> do
            let msgType = case messageRole msg of
                  UserRole -> "user"
                  AssistantRole -> "assistant"
                  FunctionRole toolName -> "function:" <> toolName
            logDebug debug loggerExecution $ "  [" <> msgType <> "] " <> messageContent msg
            -- Log full message structure as JSON
            let llmMsg = contextToLLMMessage msg
            logDebugJSON debug loggerExecution ("Message: " <> msgType) (toJSON llmMsg)
            ) context
          logDebug debug loggerExecution $ "LLM call: model=" <> modelId model <> ", messages=" <> T.pack (show (length messages)) <> ", tools=" <> T.pack (show (maybe 0 length functions))
          logDebugJSON debug loggerExecution "LLM Request:" (toJSON llmRequest)
          
          -- Call LLM
          llmResult <- callLLM client model instruction messages Nothing Nothing functions
          case llmResult of
            Left err -> return $ Left $ LLMAPIError err
            Right llmResponse -> do
              logDebug debug loggerExecution $ "LLM response: text=" <> responseText llmResponse <> ", function_call=" <> T.pack (show (responseFunctionCall llmResponse))
              logDebugJSON debug loggerExecution "LLM Response:" (toJSON llmResponse)
              
              -- Check if function call is present
              case responseFunctionCall llmResponse of
                Just functionCall -> do
                  -- Edge case: LLM requested tool but agent has no tools
                  let agentToolList = view agentTools agent
                  if null agentToolList && null boundTools
                    then do
                      logDebug debug loggerExecution $ "LLM requested tool '" <> functionCallName functionCall <> "' but agent has no tools configured"
                      return $ Left $ UnexpectedToolCallError $ "LLM requested tool '" <> functionCallName functionCall <> "' but agent has no tools configured"
                    else do
                      -- Tool call detected - invoke tool
                      toolInvocationResult <- invokeToolFromFunctionCall debug functionCall boundTools
                      case toolInvocationResult of
                        Left err -> return $ Left err
                        Right invocation -> do
                          -- T108: Verify context updates include user message, assistant message with tool call, function message with tool result, and final assistant response
                          -- Add assistant message with tool call to context
                          let assistantContent = if T.null (responseText llmResponse)
                                then "Calling " <> functionCallName functionCall
                                else responseText llmResponse
                          logDebug debug loggerExecution $ "Adding assistant message to context: " <> assistantContent
                          logDebugJSON debug loggerExecution "Tool invocation:" (toJSON invocation)
                          let assistantMsgResult = addMessage AssistantRole assistantContent context
                          case assistantMsgResult of
                            Left err -> return $ Left $ ValidationError err
                            Right contextWithAssistant -> do
                              -- T105: Verify conversation context includes FunctionRole messages for tool results
                              -- Add function message with tool result to context
                              let functionContent = case invocationResult invocation of
                                    Right val -> T.pack $ show val  -- Simplified - would use proper JSON encoding
                                    Left err -> "Error: " <> err
                              logDebug debug loggerExecution $ "Adding function message to context: tool=" <> invocationToolName invocation <> ", content=" <> functionContent
                              let functionMsgResult = addMessage (FunctionRole (invocationToolName invocation)) functionContent contextWithAssistant
                              case functionMsgResult of
                                Left err -> return $ Left $ ValidationError err
                                Right contextWithFunction -> do
                                  -- T106: Continue iteration with updated context (context is passed through loop)
                                  executeIteration debug client agent boundTools contextWithFunction (invocation : toolInvocations) (iteration + 1)
                
                Nothing -> do
                  -- No function call - final text response
                  logDebug debug loggerExecution $ "Final assistant response (no function call): " <> responseText llmResponse
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
      :: Bool              -- ^ debug: Enable debug logging
      -> FunctionCall
      -> [ToolImpl]
      -> IO (Either AgentError ToolInvocation)
    invokeToolFromFunctionCall debug functionCall boundTools = do
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
          
          logDebug debug loggerExecution $ "Function call: tool=" <> toolName <> ", raw_args=" <> argsJson
          logDebugJSON debug loggerExecution "Function call details:" (toJSON functionCall)
          logDebugJSON debug loggerExecution "Parsed arguments:" argsValue
          
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
        
        -- | Invoke a tool with timeout protection.
        --
        -- Tools that exceed the timeout limit will be terminated and return a timeout error.
        tryInvokeTool :: ToolImpl -> Value -> IO (Either Text Value)
        tryInvokeTool impl args = do
          result <- timeout defaultToolTimeout $ try (toolImplInvoke impl args) :: IO (Maybe (Either SomeException Value))
          case result of
            Nothing -> return $ Left $ "Tool execution timed out after " <> T.pack (show (defaultToolTimeout `div` 1000000)) <> " seconds"
            Just (Left ex) -> return $ Left $ T.pack $ show ex
            Just (Right val) -> return $ Right val

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
