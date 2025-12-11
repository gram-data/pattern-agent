{-# LANGUAGE OverloadedStrings #-}
-- | Environment variable loading from .env files.
--
-- This module provides functionality to load environment variables from
-- a .env file in the current directory. It supports standard .env file
-- format with KEY=value pairs, quoted values, and comments.
module PatternAgent.Env
  ( -- * Loading .env files
    loadEnvFile
  , loadEnvFileFrom
    -- * Parsing
  , parseEnvLine
  , EnvParseError(..)
  ) where

import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)

-- | Error type for .env parsing issues.
data EnvParseError
  = EnvFileNotFound FilePath
  | EnvParseError String
  deriving (Eq, Show)

-- | Parse a line from .env file.
--
-- Handles:
-- - KEY=value format
-- - Quoted values: KEY="value" or KEY='value'
-- - Comments: Lines starting with # are ignored
-- - Empty lines are ignored
--
-- Examples:
-- >>> parseEnvLine "OPENAI_API_KEY=sk-123"
-- Just ("OPENAI_API_KEY", "sk-123")
--
-- >>> parseEnvLine "KEY=\"quoted value\""
-- Just ("KEY", "quoted value")
--
-- >>> parseEnvLine "# This is a comment"
-- Nothing
parseEnvLine :: String -> Maybe (String, String)
parseEnvLine line = do
  -- Remove leading/trailing whitespace
  let trimmed = dropWhileEnd (== ' ') $ dropWhile (== ' ') line
  -- Skip empty lines and comments
  case trimmed of
    [] -> Nothing
    '#':_ -> Nothing
    _ -> do
      -- Split on first '='
      case break (== '=') trimmed of
        (key, '=':value) -> do
          let key' = dropWhileEnd (== ' ') key
          -- Validate that key is non-empty (POSIX env var names cannot be empty)
          if null key'
            then Nothing
            else do
              let value' = dropWhile (== ' ') value
              -- Remove quotes if present
              let value'' = case value' of
                    '"':rest -> if not (null rest) && last rest == '"' then init rest else value'
                    '\'':rest -> if not (null rest) && last rest == '\'' then init rest else value'
                    _ -> value'
              Just (key', value'')
        _ -> Nothing

-- | Load environment variables from .env file if it exists.
--
-- Looks for a .env file in the current directory and loads all
-- KEY=value pairs into the environment. Variables that are already
-- set in the environment are not overridden.
--
-- This is a no-op if the .env file doesn't exist.
loadEnvFile :: IO ()
loadEnvFile = loadEnvFileFrom ".env"

-- | Load environment variables from a specific .env file.
--
-- Loads all KEY=value pairs from the specified file into the environment.
-- Variables that are already set in the environment are not overridden.
--
-- This is a no-op if the file doesn't exist.
loadEnvFileFrom :: FilePath -> IO ()
loadEnvFileFrom filePath = do
  envExists <- doesFileExist filePath
  if envExists
    then do
      contents <- readFile filePath
      let lines' = lines contents
      let vars = mapMaybe parseEnvLine lines'
      -- Only set variables that aren't already in the environment
      mapM_ (\(key, value) -> do
        existing <- lookupEnv key
        case existing of
          Nothing -> setEnv key value
          Just _ -> return ()  -- Don't override existing env vars
        ) vars
    else return ()
