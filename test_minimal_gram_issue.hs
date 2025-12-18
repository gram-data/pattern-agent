{-# LANGUAGE OverloadedStrings #-}
import qualified Gram
import qualified Data.Text as T

main = do
  -- Minimal multiline example that exhibits the issue
  -- The problem: newline after "} |" causes parse error
  let gramMultiline = T.unlines
        [ "[test:Agent {"
        , "  description: \"test\""
        , "} |"
        , "  [tool:Tool {description: \"test\"} | (param::Text)==>(::String)]"
        , "]"
        ]
  
  putStrLn "=== Minimal Multiline Example (FAILS) ==="
  putStrLn ""
  putStrLn (T.unpack gramMultiline)
  putStrLn ""
  putStrLn "Parse result:"
  case Gram.fromGram (T.unpack gramMultiline) of
    Right _ -> putStrLn "  ✓ Success"
    Left err -> putStrLn $ "  ✗ Error: " ++ show err
  
  putStrLn "\n"
  
  -- Single-line version that works
  let gramSingle = "[test:Agent {description: \"test\"} | [tool:Tool {description: \"test\"} | (param::Text)==>(::String)]]"
  
  putStrLn "=== Single-line Version (WORKS) ==="
  putStrLn ""
  putStrLn gramSingle
  putStrLn ""
  putStrLn "Parse result:"
  case Gram.fromGram gramSingle of
    Right _ -> putStrLn "  ✓ Success"
    Left err -> putStrLn $ "  ✗ Error: " ++ show err

