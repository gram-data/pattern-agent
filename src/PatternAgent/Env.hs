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
-- Returns Right with parsed key-value pair, or Left with error message
-- for malformed lines that should have been parsed.
--
-- Examples:
-- >>> parseEnvLine "OPENAI_API_KEY=sk-123"
-- Right (Just ("OPENAI_API_KEY", "sk-123"))
--
-- >>> parseEnvLine "KEY=\"quoted value\""
-- Right (Just ("KEY", "quoted value"))
--
-- >>> parseEnvLine "# This is a comment"
-- Right Nothing
--
-- >>> parseEnvLine "INVALID LINE"
-- Left (EnvParseError "Missing '=' separator in line: \"INVALID LINE\"")
parseEnvLine :: String -> Either EnvParseError (Maybe (String, String))
parseEnvLine line = do
  -- Remove leading/trailing whitespace
  let trimmed = dropWhileEnd (== ' ') $ dropWhile (== ' ') line
  -- Skip empty lines and comments (these are valid, not errors)
  case trimmed of
    [] -> Right Nothing
    '#':_ -> Right Nothing
    _ -> do
      -- Split on first '='
      case break (== '=') trimmed of
        (key, '=':value) -> do
          let key' = dropWhileEnd (== ' ') key
          -- Validate that key is non-empty (POSIX env var names cannot be empty)
          if null key'
            then Left $ EnvParseError "Empty key name (missing key before '=')"
            else do
              let value' = dropWhile (== ' ') value
              -- Remove quotes if present
              let value'' = case value' of
                    '"':rest -> if not (null rest) && last rest == '"' then init rest else value'
                    '\'':rest -> if not (null rest) && last rest == '\'' then init rest else value'
                    _ -> value'
              Right $ Just (key', value'')
        _ -> Left $ EnvParseError $ "Missing '=' separator in line: " ++ show trimmed

-- | Load environment variables from .env file if it exists.
--
-- Looks for a .env file in the current directory and loads all
-- KEY=value pairs into the environment. Variables that are already
-- set in the environment are not overridden.
--
-- Returns Right () on success, Left with error if parsing fails.
-- Returns Right () if the .env file doesn't exist (no error).
loadEnvFile :: IO (Either EnvParseError ())
loadEnvFile = loadEnvFileFrom ".env"

-- | Load environment variables from a specific .env file.
--
-- Loads all KEY=value pairs from the specified file into the environment.
-- Variables that are already set in the environment are not overridden.
--
-- Returns Right () on success, Left with error if parsing fails.
-- Returns Right () if the file doesn't exist (no error).
loadEnvFileFrom :: FilePath -> IO (Either EnvParseError ())
loadEnvFileFrom filePath = do
  envExists <- doesFileExist filePath
  if envExists
    then do
      contents <- readFile filePath
      let lines' = lines contents
      -- Parse all lines and collect errors
      let parseResults = zip [1..] $ map parseEnvLine lines'
      -- Find first error, if any
      case foldl findFirstError Nothing parseResults of
        Just err -> return $ Left err
        Nothing -> do
          -- Extract all successfully parsed key-value pairs
          let vars = concatMap (\(_, result) -> case result of
                Right (Just kv) -> [kv]
                _ -> []) parseResults
          -- Only set variables that aren't already in the environment
          mapM_ (\(key, value) -> do
            existing <- lookupEnv key
            case existing of
              Nothing -> setEnv key value
              Just _ -> return ()  -- Don't override existing env vars
            ) vars
          return $ Right ()
    else return $ Right ()
  where
    findFirstError :: Maybe EnvParseError -> (Int, Either EnvParseError (Maybe (String, String))) -> Maybe EnvParseError
    findFirstError acc (lineNum, result) = case acc of
      Just err -> Just err  -- Already found an error
      Nothing -> case result of
        Left err -> Just $ case err of
          EnvParseError msg -> EnvParseError $ "Line " ++ show lineNum ++ ": " ++ msg
          EnvFileNotFound _ -> EnvParseError $ "Line " ++ show lineNum ++ ": unexpected file error"  -- Shouldn't happen here
        Right _ -> Nothing
