{-# LANGUAGE OverloadedStrings #-}
-- | Structured logging for Pattern Agent runtime.
--
-- Provides ADK-style logging format: timestamp - level - logger_name - message
-- Format: %(asctime)s - %(levelname)s - %(name)s - %(message)s
--
-- This is part of the runtime implementation (Haskell-specific).
module PatternAgent.Runtime.Logging
  ( -- * Log Levels
    LogLevel(..)
    -- * Logging Functions
  , logDebug
  , logInfo
  , logWarning
  , logError
  , logDebugJSON
  , logInfoJSON
    -- * Logger Names
  , loggerExecution
  , loggerBuiltinTools
  , loggerLLM
  , loggerCLI
  ) where

import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson (Value, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Monad (when)

-- | Log levels matching standard logging levels.
data LogLevel
  = DEBUG
  | INFO
  | WARNING
  | ERROR
  deriving (Eq, Show)

-- | Format timestamp as: YYYY-MM-DD HH:MM:SS,mmm
formatTimestamp :: IO String
formatTimestamp = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now ++ ",000"  -- Simplified: no milliseconds

-- | Format log message in ADK style: timestamp - level - logger_name - message
formatLogMessage :: LogLevel -> String -> Text -> IO String
formatLogMessage level loggerName message = do
  timestamp <- formatTimestamp
  let levelStr = show level
  return $ timestamp ++ " - " ++ levelStr ++ " - " ++ loggerName ++ " - " ++ T.unpack message

-- | Log a DEBUG message (only if debug flag is True).
logDebug :: Bool -> String -> Text -> IO ()
logDebug debug loggerName message = when debug $ do
  formatted <- formatLogMessage DEBUG loggerName message
  putStrLn formatted

-- | Log an INFO message.
logInfo :: String -> Text -> IO ()
logInfo loggerName message = do
  formatted <- formatLogMessage INFO loggerName message
  putStrLn formatted

-- | Log a WARNING message.
logWarning :: String -> Text -> IO ()
logWarning loggerName message = do
  formatted <- formatLogMessage WARNING loggerName message
  putStrLn formatted

-- | Log an ERROR message.
logError :: String -> Text -> IO ()
logError loggerName message = do
  formatted <- formatLogMessage ERROR loggerName message
  putStrLn formatted

-- | Log a DEBUG message with JSON data (newline-delimited, only if debug flag is True).
logDebugJSON :: Bool -> String -> Text -> Value -> IO ()
logDebugJSON debug loggerName message jsonValue = when debug $ do
  formatted <- formatLogMessage DEBUG loggerName message
  putStrLn formatted
  let jsonText = T.pack $ TL.unpack $ decodeUtf8 $ encodePretty jsonValue
  putStrLn $ T.unpack jsonText

-- | Log an INFO message with JSON data (newline-delimited).
logInfoJSON :: String -> Text -> Value -> IO ()
logInfoJSON loggerName message jsonValue = do
  formatted <- formatLogMessage INFO loggerName message
  putStrLn formatted
  let jsonText = T.pack $ TL.unpack $ decodeUtf8 $ encodePretty jsonValue
  putStrLn $ T.unpack jsonText

-- | Logger name for execution module.
loggerExecution :: String
loggerExecution = "pattern_agent.runtime.execution"

-- | Logger name for builtin tools module.
loggerBuiltinTools :: String
loggerBuiltinTools = "pattern_agent.runtime.builtin_tools"

-- | Logger name for LLM module.
loggerLLM :: String
loggerLLM = "pattern_agent.runtime.llm"

-- | Logger name for CLI.
loggerCLI :: String
loggerCLI = "pattern_agent.cli"

