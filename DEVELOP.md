# Developer Guide

This guide will help you get started developing the Pattern Agent project.

## Prerequisites

Before you begin, ensure you have the following installed:

### Required

- **GHC (Glasgow Haskell Compiler)**: Version 9.10.3 or compatible
  - Check your version: `ghc --version`
  - Install: [GHCup](https://www.haskell.org/ghcup/) (recommended) or your system package manager

- **Cabal**: Version 3.0 or later
  - Check your version: `cabal --version`
  - Install: Usually comes with GHCup, or install separately

- **Git**: For version control
  - Check: `git --version`

### Optional but Recommended

- **Haskell Language Server (HLS)**: For IDE support
  - Install via GHCup: `ghcup install hls`
  - Or via your editor's extension marketplace

- **cabal-install**: Usually included with Cabal
  - Verify: `cabal --version`

## Project Structure

```
pattern-agent/
├── app/                    # Executable source
│   └── Main.hs            # CLI application entry point
├── src/                    # Library source code
│   └── PatternAgent/      # Main library modules
│       ├── Agent.hs       # Agent type and operations
│       ├── LLM.hs         # LLM API client
│       ├── Execution.hs   # Agent execution logic
│       ├── Context.hs     # Conversation context management
│       ├── Env.hs         # .env file parsing
│       └── ...
├── tests/                  # Test suites
│   ├── unit/              # Unit tests
│   └── scenario/          # Scenario/integration tests
├── specs/                  # Feature specifications
├── pattern-agent.cabal    # Cabal package configuration
└── cabal.project          # Project-level configuration
```

## Initialization

### 1. Clone the Repository

```bash
git clone <repository-url>
cd pattern-agent
```

### 2. Install Dependencies

The project uses a local dependency on `gram-hs`. Ensure the `gram-hs` project is available at the expected path (typically `../gram-hs` relative to this project).

```bash
# Update Cabal package index
cabal update

# Build dependencies (this will download and build all required packages)
cabal build --only-dependencies
```

### 3. Configure Environment Variables

Create a `.env` file in the project root for API keys:

```bash
# .env file
OPENAI_API_KEY=sk-your-api-key-here
```

Alternatively, you can set environment variables directly:

```bash
export OPENAI_API_KEY=sk-your-api-key-here
```

**Note**: The `.env` file is already in `.gitignore` and won't be committed.

## Building

### Build the Library

```bash
# Build the library
cabal build lib:pattern-agent

# Or build everything
cabal build
```

### Build the Executable

```bash
# Build the CLI executable
cabal build exe:pattern-agent

# Or build everything
cabal build
```

### Clean Build Artifacts

```bash
# Clean build artifacts
cabal clean
```

## Running the CLI Application

### Basic Usage

```bash
# Run with a message
cabal run pattern-agent "What is the capital of France?"

# Or after building, run directly
cabal exec pattern-agent "What is the capital of France?"
```

### Debug Mode

Use the `--debug` flag to see raw request/response JSON:

```bash
cabal run pattern-agent -- --debug "What is the capital of France?"
```

**Note**: The `--` separates cabal's options from the program's arguments.

This will output:
- Raw request JSON sent to the API
- Raw response JSON received from the API
- Normal formatted response

### Command Line Options

```
Usage: pattern-agent [--debug] <message>

Options:
  --debug    Show raw request/response JSON transcript

Examples:
  cabal run pattern-agent -- "What is the capital of France?"
  cabal run pattern-agent -- --debug "What is the capital of France?"
```

## Testing

### Run All Tests

```bash
# Run the test suite
cabal test

# Or with verbose output
cabal test --test-show-details=direct
```

### Run Specific Test Suites

The project uses `tasty` as the test framework. Tests are organized into:

- **Unit tests**: Component-level tests in `tests/unit/`
- **Scenario tests**: End-to-end integration tests in `tests/scenario/`

### Writing Tests

Tests follow the dual testing strategy:
- **Unit tests**: Test individual functions and components
- **Scenario tests**: Test complete user workflows and goals

Example test structure:

```haskell
-- tests/unit/AgentTest.hs
module AgentTest where

import Test.Tasty
import Test.Tasty.HUnit
import PatternAgent.Agent

testAgentCreation :: TestTree
testAgentCreation = testCase "Create agent" $ do
  let model = createModel "gpt-4" OpenAI
  let result = createAgent "test" model "instruction" Nothing
  -- ... assertions
```

## Development Workflow

### 1. Make Changes

Edit source files in `src/PatternAgent/` or `app/`.

### 2. Build and Test

```bash
# Quick build check
cabal build

# Run tests
cabal test

# Run the CLI to test manually
cabal run pattern-agent "test message"
```

### 3. Check for Errors

```bash
# Build with all warnings
cabal build --ghc-options=-Wall

# Check for linting issues (if configured)
# Most editors with HLS will show these automatically
```

### 4. Format Code (Optional)

While the project doesn't enforce a specific formatter, consider using:
- `ormolu` or `brittany` for code formatting
- `hlint` for style suggestions

## Environment Configuration

### .env File Format

The project supports loading environment variables from a `.env` file:

```bash
# .env
OPENAI_API_KEY=sk-your-key-here
ANTHROPIC_API_KEY=your-anthropic-key  # For future Anthropic support
GOOGLE_API_KEY=your-google-key         # For future Google support
```

**Features:**
- Supports quoted values: `KEY="value"` or `KEY='value'`
- Comments: Lines starting with `#` are ignored
- Empty lines are ignored
- Environment variables take precedence over `.env` file values

### API Key Priority

1. **Environment variables** (highest priority)
2. **`.env` file** (if variable not in environment)
3. **Error** (if not found in either)

## Debugging

### Using Debug Mode

The `--debug` flag provides detailed API interaction logs:

```bash
cabal run pattern-agent --debug "your message"
```

This shows:
- Complete request JSON sent to the LLM API
- Complete response JSON received from the LLM API

### Common Issues

#### Build Errors

**Dependency conflicts:**
```bash
# Update package index
cabal update

# Clean and rebuild
cabal clean
cabal build
```

**Missing dependencies:**
```bash
# Install missing dependencies
cabal install --dependencies-only
```

#### Runtime Errors

**Missing API key:**
```
❌ Authentication Error:
   OPENAI_API_KEY environment variable not set
```

**Solution:** Set `OPENAI_API_KEY` in environment or `.env` file.

**Network errors:**
```
❌ Error: Network error: ...
```

**Solution:** Check internet connectivity and API endpoint availability.

**API errors:**
```
❌ Error: API error: 401 Unauthorized
```

**Solution:** Verify your API key is valid and has sufficient credits.

### IDE Support

#### VS Code / Cursor

1. Install the "Haskell" extension
2. Install HLS: `ghcup install hls`
3. Open the project - HLS should start automatically

#### Emacs

Use `lsp-haskell` or `haskell-mode` with HLS.

#### Vim / Neovim

Use `haskell-tools.nvim` or `coc-haskell` with HLS.

## Project Dependencies

### Core Dependencies

- `base ^>=4.20.2.0` - Haskell base library
- `pattern` - Local dependency from `gram-hs`
- `aeson >=2.1` - JSON serialization
- `http-client ^>=0.7` - HTTP client
- `http-client-tls ^>=0.3` - TLS support
- `text >=2.0` - Text handling
- `bytestring ^>=0.11` - Byte string handling

### Test Dependencies

- `tasty ^>=1.4` - Test framework
- `tasty-hunit ^>=0.10` - Unit test support
- `tasty-quickcheck ^>=0.10` - Property-based testing

### Development Dependencies

- `aeson-pretty ^>=0.8` - Pretty JSON printing (for debug output)

## Code Organization

### Module Structure

- **`PatternAgent.Core`**: Core type definitions
- **`PatternAgent.Types`**: Type aliases and supporting types
- **`PatternAgent.Agent`**: Agent type and creation
- **`PatternAgent.LLM`**: Standalone LLM API client
- **`PatternAgent.Execution`**: Agent execution logic
- **`PatternAgent.Context`**: Conversation context management
- **`PatternAgent.Env`**: Environment variable loading from .env files
- **`PatternAgent.Tool`**: Tool system (in development)

### Adding New Features

1. Create a feature specification in `specs/XXX-feature-name/`
2. Follow the specification → plan → tasks workflow
3. Implement in appropriate module or create new module
4. Add tests (both unit and scenario)
5. Update documentation

## Getting Help

- Check the `README.md` for project overview
- Review `specs/` for feature documentation
- Check `TODO.md` for planned features
- Review test files for usage examples

## Next Steps

1. **Explore the codebase**: Start with `src/PatternAgent/LLM.hs` to understand the LLM client
2. **Run the CLI**: Test with `cabal run pattern-agent "hello"`
3. **Read the specs**: Check `specs/002-llm-agent/` for detailed feature documentation
4. **Write tests**: Add tests for new features or edge cases
5. **Contribute**: Follow the project's development principles and conventions

## Development Principles

This project follows these principles (from `constitution.md`):

1. **Design-Driven Development**: Design before implementation
2. **Why Before How**: Understand the problem before solving it
3. **Dual Testing Strategy**: Both unit and scenario tests
4. **Expressiveness and Correctness**: Type safety and clear code
5. **Progressive Iteration**: Start simple, add complexity when needed

Happy coding! 🚀
