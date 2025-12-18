# API Contract: Interactive CLI Mode

**Feature**: Agent Execution with Scenario Tests, Interactive CLI, and Observability  
**Module**: `app/Main.hs`  
**Date**: 2025-01-27

## Overview

Interactive CLI mode enables real-time conversations with agents through a terminal interface. Users can send messages and receive responses immediately, maintaining conversation context across multiple exchanges.

## Command Line Interface

### Activation

**Flags**: `--interactive` or `-i`

**Usage**:
```bash
pattern-agent --interactive <gram-file>
pattern-agent -i <gram-file>
```

**Behavior**:
- Activates interactive mode
- Loads agent from specified gram file
- Enters input loop for real-time conversation
- Maintains conversation context across messages

### Exit Commands

**Supported Exit Commands**:
- `"exit"` - Exit interactive mode gracefully
- `"quit"` - Exit interactive mode gracefully
- `Ctrl+D` (EOF) - Exit interactive mode gracefully

**Behavior**:
- All exit commands terminate the session gracefully
- No error message displayed on exit
- Conversation context is discarded (not persisted)

## Functions

### parseArgs

**Signature**:
```haskell
parseArgs :: [String] -> Either String (CLIMode, Bool)
```

**Extended Behavior**:
- Recognizes `--interactive` or `-i` flag
- Returns `InteractiveMode gramFile` when flag detected
- Maintains backward compatibility with existing modes

**Example**:
```haskell
parseArgs ["--interactive", "agent.gram"]
-- Right (InteractiveMode "agent.gram", False)

parseArgs ["-i", "agent.gram"]
-- Right (InteractiveMode "agent.gram", False)
```

### handleInteractiveMode

**Signature**:
```haskell
handleInteractiveMode :: FilePath -> Bool -> IO ()
```

**Parameters**:
- `gramFile`: Path to gram file containing agent definition
- `debug`: Enable debug logging

**Behavior**:
1. Load agent from gram file
2. Create tool library from agent's tools
3. Initialize empty conversation context
4. Enter input loop:
   - Read user input (`getLine`)
   - Check exit commands
   - Execute agent with current message and context
   - Display agent response
   - Update conversation context
   - Continue loop

**Error Handling**:
- Gram file not found: Display error, exit with failure
- Invalid gram file: Display error, exit with failure
- Tool library creation failure: Display error, exit with failure
- Agent execution failure: Display error, continue loop (allow retry)

**Output**:
- User messages: Display with `💬` icon (normal mode) or log (debug mode)
- Agent responses: Display with `🤖` icon (normal mode) or log (debug mode)
- Tool calls: Display with `🛠️` icon (normal mode) or log (debug mode)

## Data Types

### CLIMode Extension

**Extended Definition**:
```haskell
data CLIMode
  = StandardMode (Maybe String)      -- Existing
  | AgentMode String [String]        -- Existing
  | InteractiveMode String            -- NEW
```

## Usage Examples

### Basic Interactive Session

```bash
$ pattern-agent --interactive examples/helloAgent.gram
💬 Hello!
🤖 Hello! How can I help you today?
💬 What's the weather like?
🤖 I don't have access to weather information, but I can help with other tasks!
💬 exit
```

### Interactive Session with Tools

```bash
$ pattern-agent -i examples/multiToolAgent.gram
💬 What time is it?
🛠️  getCurrentTime()
🤖 The current time is 2025-01-27T10:30:00Z
💬 Calculate 15 * 7
🛠️  calculate({"expression": "15 * 7"})
🤖 15 * 7 = 105
💬 quit
```

## Error Cases

### Invalid Gram File

```bash
$ pattern-agent --interactive nonexistent.gram
Error: File not found: nonexistent.gram
```

### Invalid Agent Definition

```bash
$ pattern-agent -i invalid.gram
Error: Failed to parse agent from gram file: [error details]
```

### Tool Library Creation Failure

```bash
$ pattern-agent --interactive agent.gram
Error: Error creating tool library: Unsupported tool: unknownTool
```

## Implementation Notes

- Uses standard Haskell I/O (`getLine`, `putStrLn`)
- Terminal handles text wrapping automatically
- Empty input is skipped (no error)
- Ctrl+C interrupts program (acceptable for initial implementation)
- Conversation context maintained in memory (not persisted)

