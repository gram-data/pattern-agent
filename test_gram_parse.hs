import qualified Gram
import qualified Data.Text as T

main = do
  -- Test multiline format (like the example file)
  let gramMultiline = T.unlines
        [ "[hello_world_agent:Agent {"
        , "  description: \"test\""
        , "} |"
        , "  [sayHello:Tool {description: \"test\"} | (personName::Text)==>(::String)]"
        , "]"
        ]
  
  putStrLn "Testing multiline format:"
  case Gram.fromGram (T.unpack gramMultiline) of
    Right _ -> putStrLn "  ✓ Success"
    Left err -> putStrLn $ "  ✗ Error: " ++ show err
  
  -- Test single-line format (what we're using now)
  let gramSingle = "[hello_world_agent:Agent {description: \"test\"} | [sayHello:Tool {description: \"test\"} | (personName::Text)==>(::String)]]"
  
  putStrLn "\nTesting single-line format:"
  case Gram.fromGram gramSingle of
    Right _ -> putStrLn "  ✓ Success"
    Left err -> putStrLn $ "  ✗ Error: " ++ show err

