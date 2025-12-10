# Research: Basic LLM Agent

**Feature**: Basic LLM Agent  
**Date**: 2025-01-27  
**Purpose**: Resolve technical unknowns identified in Phase 0 planning

## Research Questions

### 1. LLM API Client Library for Haskell

**Question**: What Haskell libraries are available for calling LLM APIs (OpenAI, Anthropic, Google Gemini, etc.)?

**Research Approach**: 
- Evaluate Haskell HTTP client libraries
- Research existing LLM API bindings for Haskell
- Consider direct HTTP client approach vs specialized libraries
- Evaluate JSON serialization options

**Findings**:
- **Decision**: Use `http-client` and `http-client-tls` for HTTP requests, `aeson` for JSON serialization, and `bytestring` for request/response handling
- **Rationale**: 
  - `http-client` is the standard HTTP client library for Haskell
  - `aeson` is the standard JSON library for Haskell
  - Direct HTTP approach provides flexibility to support multiple LLM providers
  - No need for provider-specific bindings initially (can be added later if needed)
  - Simple, composable approach aligns with Principle 5 (Progressive Iteration)
- **Alternatives Considered**:
  - Provider-specific Haskell libraries (e.g., `openai-haskell`) - rejected (adds dependencies, may not support all providers)
  - `wreq` (higher-level HTTP client) - rejected (http-client is more standard and flexible)
  - `servant-client` - rejected (overkill for simple API calls, adds complexity)
- **Dependencies**: 
  - `http-client ^>=0.7` (HTTP client)
  - `http-client-tls ^>=0.3` (TLS support)
  - `aeson ^>=2.0` (JSON serialization)
  - `bytestring ^>=0.11` (byte string handling)
  - `text ^>=2.0` (text handling)
  - `mtl ^>=2.3` (monad transformers for error handling)

### 2. LLM Provider Selection

**Question**: Which LLM provider should be supported initially, and how should the design support multiple providers?

**Research Approach**:
- Evaluate LLM provider APIs (OpenAI, Anthropic, Google Gemini)
- Consider API consistency and ease of integration
- Design for extensibility to support multiple providers

**Findings**:
- **Decision**: Start with OpenAI API (GPT models) as initial provider, design with provider abstraction for extensibility
- **Rationale**:
  - OpenAI API is well-documented and widely used
  - API structure is straightforward (POST requests with JSON)
  - Design abstraction layer allows adding other providers later
  - Google Gemini and Anthropic can be added as additional providers
  - Abstraction aligns with Principle 5 (start simple, add complexity when needed)
- **Provider Abstraction Design**:
  - Create `LLMProvider` typeclass for provider-specific implementations
  - Each provider implements: `callLLM :: LLMRequest -> IO LLMResponse`
  - Provider-specific configuration (API keys, endpoints) handled by provider implementation
  - Initial implementation: `OpenAIProvider` with `Model` type for model selection
- **Alternatives Considered**:
  - Support all providers from start - rejected (violates Principle 5, adds unnecessary complexity)
  - Google Gemini only - rejected (OpenAI more widely used, easier to test)
  - Provider-agnostic from start - rejected (needs concrete implementation first)

### 3. Tool Function Representation

**Question**: How should tool functions be represented in Haskell's type system to allow safe invocation?

**Research Approach**:
- Evaluate Haskell approaches for dynamic function invocation
- Consider type safety vs flexibility tradeoffs
- Research how Google ADK handles tool functions in Python

**Findings**:
- **Decision**: Use a `Tool` type that wraps function metadata and provides type-safe invocation interface
- **Rationale**:
  - `Tool` type stores: name, description, parameter schema, and invocation function
  - Parameter schema uses `aeson` `Value` type for JSON representation
  - Invocation function: `ToolArgs -> IO ToolResult` where both are JSON-serializable
  - Type safety maintained through schema validation before invocation
  - Function tools automatically extract schema from Haskell function signatures (using Template Haskell or manual specification)
- **Tool Type Design**:
  ```haskell
  data Tool = Tool
    { toolName :: Text
    , toolDescription :: Text
    , toolSchema :: Value  -- JSON schema for parameters
    , toolInvoke :: Value -> IO Value  -- Invocation function
    }
  ```
- **Function Tool Creation**:
  - Developers provide Haskell functions with clear type signatures
  - Tool creation extracts parameter names and types
  - Schema generated from function signature (or manually specified)
  - Wrapper converts JSON arguments to Haskell types, invokes function, converts result to JSON
- **Alternatives Considered**:
  - Fully dynamic tool invocation (no type safety) - rejected (violates Principle 4 correctness)
  - Template Haskell for automatic schema generation - considered but deferred (manual specification simpler initially)
  - Type-level tool representation - rejected (too complex, violates Principle 5)

### 4. Conversation Context Management

**Question**: How should conversation context (message history) be managed and passed to LLM APIs?

**Research Approach**:
- Review LLM API requirements for conversation history
- Evaluate in-memory vs persistent storage approaches
- Consider context size limits and truncation strategies

**Findings**:
- **Decision**: In-memory conversation context with message list, support for context truncation
- **Rationale**:
  - Simple in-memory approach sufficient for initial implementation (Principle 5)
  - Message list: `[Message]` where `Message` has role (user/assistant) and content
  - Context passed to LLM API as array of messages
  - Truncation strategy: Keep most recent N messages or N tokens (deferred to future if needed)
  - Persistence can be added later if user goals require it
- **Message Type Design**:
  ```haskell
  data MessageRole = UserRole | AssistantRole
  data Message = Message
    { messageRole :: MessageRole
    , messageContent :: Text
    }
  type ConversationContext = [Message]
  ```
- **Alternatives Considered**:
  - Persistent storage from start - rejected (not required by user goals, violates Principle 5)
  - Token-based truncation from start - rejected (message-based simpler, can add token counting later)
  - Context compression - rejected (premature optimization)

### 5. Error Handling Strategy

**Question**: How should LLM API failures, tool execution errors, and other failures be handled?

**Research Approach**:
- Evaluate Haskell error handling patterns (Either, Exception, etc.)
- Consider user experience for error cases
- Review Google ADK error handling approach

**Findings**:
- **Decision**: Use `Either` monad for recoverable errors, `IO` exceptions for unrecoverable errors
- **Rationale**:
  - `Either String a` for LLM API errors (network, API errors) - can be retried or reported
  - `Either String a` for tool execution errors - can be reported to user
  - `IO` exceptions for programming errors (invalid tool schema, etc.)
  - Clear error messages for developers
  - Error handling aligns with Principle 4 (correctness under all conditions)
- **Error Types**:
  ```haskell
  data AgentError
    = LLMAPIError Text      -- LLM API call failed
    | ToolError Text        -- Tool execution failed
    | ValidationError Text  -- Input validation failed
    | ConfigurationError Text  -- Agent configuration invalid
  ```
- **Alternatives Considered**:
  - Exception-only approach - rejected (Either provides better type safety)
  - Custom monad transformer - rejected (adds complexity, violates Principle 5)

## Resolved Technical Context

After research, the following clarifications are resolved:

- **HTTP Client**: `http-client` and `http-client-tls` for LLM API calls
- **JSON Serialization**: `aeson` for request/response handling
- **LLM Provider**: OpenAI API initially, with abstraction for extensibility
- **Tool Representation**: `Tool` type with schema and invocation function
- **Conversation Context**: In-memory `[Message]` list
- **Error Handling**: `Either` for recoverable errors, exceptions for programming errors

## Resolved Technical Decisions

1. **LLM Integration**:
   - Use `http-client` for HTTP requests
   - Use `aeson` for JSON serialization
   - Create `LLMProvider` typeclass for provider abstraction
   - Initial provider: OpenAI API

2. **Tool System**:
   - `Tool` type with name, description, schema, and invocation function
   - Function tools extract schema from Haskell function signatures
   - Type-safe invocation with JSON parameter conversion

3. **Conversation Management**:
   - In-memory message list for conversation context
   - Simple message-based history (token counting deferred)

4. **Error Handling**:
   - `Either AgentError a` for recoverable errors
   - Clear error messages for debugging

## Notes

- Research aligns with Principle 5 (Progressive Iteration) by starting simple
- Design supports extensibility for future features (multi-provider, persistence, etc.)
- Type safety prioritized (Principle 4: Correctness)
- API design focuses on expressiveness (Principle 4: Expressiveness)
