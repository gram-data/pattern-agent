# Minimal Example: Multiline Gram Parsing Issue

## Issue Description

When parsing gram notation with multiline format, a newline immediately after `} |` causes a parse error: "unexpected newline".

## Minimal Failing Example

```gram
[test:Agent {
  description: "test"
} |
  [tool:Tool {description: "test"} | (param::Text)==>(::String)]
]
```

**Error**: `gram:3:4: unexpected newline expecting '"', ''', '(', '-', '@', '[', '`', '{', digit, space, or tab`

The error occurs at position 3:4, which is the newline character immediately after `} |`.

## Working Single-line Version

```gram
[test:Agent {description: "test"} | [tool:Tool {description: "test"} | (param::Text)==>(::String)]]
```

This single-line version parses successfully.

## Test Code

```haskell
import qualified Gram
import qualified Data.Text as T

main = do
  let gramMultiline = T.unlines
        [ "[test:Agent {"
        , "  description: \"test\""
        , "} |"
        , "  [tool:Tool {description: \"test\"} | (param::Text)==>(::String)]"
        , "]"
        ]
  
  case Gram.fromGram (T.unpack gramMultiline) of
    Right _ -> putStrLn "Success"
    Left err -> putStrLn $ "Error: " ++ show err
```

## Expected Behavior

The multiline format should parse successfully, as it's valid gram syntax according to `gram-lint`. The parser should handle newlines after `} |` when followed by pattern elements on subsequent lines.

## Context

This issue was discovered when writing tests for `pattern-agent` that parse agents with nested tools from gram notation. The example file `specs/003-hello-world-agent/examples/helloAgent.gram` uses the same multiline format and passes `gram-lint`, but fails when parsed programmatically with `Gram.fromGram`.

