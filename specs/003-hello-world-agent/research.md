# Research: Hello World Agent with Tool Execution

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Resolve technical unknowns identified in Phase 0 planning

## Research Questions

### 1. OpenAI Function Calling Format

**Question**: How does OpenAI's API format function/tool calls in requests and responses?

**Research Approach**: 
- Review OpenAI API documentation for function calling
- Understand how tool definitions are sent in requests
- Understand how tool calls are returned in responses
- Determine parsing requirements for tool call detection

**Findings**:
- **Decision**: Use OpenAI's function calling format with `functions` array in requests and `function_call` in response messages
- **Rationale**: 
  - OpenAI API supports function calling via `functions` parameter in chat completion requests
  - Functions are defined with `name`, `description`, and `parameters` (JSON schema)
  - LLM responses include `function_call` object when tool should be invoked
  - `function_call` contains `name` (tool name) and `arguments` (JSON string of parameters)
  - After tool execution, tool result is sent back as a message with `role: "function"` and `name` (tool name)
  - This format is standard and well-documented
- **Request Format**:
  ```json
  {
    "model": "gpt-3.5-turbo",
    "messages": [...],
    "functions": [
      {
        "name": "sayHello",
        "description": "Returns a friendly greeting message",
        "parameters": {
          "type": "object",
          "properties": {
            "name": {
              "type": "string",
              "description": "The name of the person to greet"
            }
          },
          "required": ["name"]
        }
      }
    ]
  }
  ```
- **Response Format** (tool call):
  ```json
  {
    "choices": [{
      "message": {
        "role": "assistant",
        "content": null,
        "function_call": {
          "name": "sayHello",
          "arguments": "{\"name\": \"Alice\"}"
        }
      }
    }]
  }
  ```
- **Tool Result Message Format**:
  ```json
  {
    "role": "function",
    "name": "sayHello",
    "content": "Hello, Alice! Nice to meet you."
  }
  ```
- **Alternatives Considered**:
  - Anthropic tool use format - rejected (OpenAI format already implemented, can add Anthropic later)
  - Custom tool calling format - rejected (standard format better for compatibility)

### 2. Tool Call Detection and Parsing

**Question**: How should tool calls be detected and parsed from LLM API responses?

**Research Approach**:
- Evaluate parsing strategies for OpenAI function calling format
- Consider error handling for malformed tool calls
- Determine how to handle multiple tool calls in a single response

**Findings**:
- **Decision**: Parse `function_call` object from OpenAI response message, extract tool name and arguments JSON, validate against tool schema before invocation
- **Rationale**:
  - OpenAI responses include `function_call` in message when tool should be invoked
  - `function_call.name` identifies the tool to invoke
  - `function_call.arguments` is a JSON string that must be parsed
  - Arguments must be validated against tool's JSON schema before invocation
  - If `function_call` is present, response content may be null (tool call instead of text response)
  - After tool execution, send tool result back to LLM for final response generation
- **Parsing Strategy**:
  - Check if `message.function_call` exists in response
  - Extract `name` and `arguments` from `function_call`
  - Parse `arguments` JSON string to `Value`
  - Validate parsed arguments against tool's schema
  - If validation passes, invoke tool with arguments
  - If validation fails, return error to LLM
- **Multiple Tool Calls**:
  - OpenAI typically returns one function call per response
  - If multiple calls needed, LLM may make multiple requests
  - Initial implementation: handle single tool call per response (can extend later)
- **Error Handling**:
  - Malformed JSON in arguments: return validation error
  - Missing required parameters: return validation error
  - Wrong parameter types: return validation error
  - Tool not found: return error message to LLM
  - Tool execution failure: catch exception and return error to LLM
- **Alternatives Considered**:
  - Parse tool calls from response text - rejected (unreliable, OpenAI provides structured format)
  - Support multiple simultaneous tool calls - deferred (single call sufficient for hello world example)

### 3. Tool Schema Validation

**Question**: How should tool parameter schemas be validated against LLM-provided arguments?

**Research Approach**:
- Evaluate JSON schema validation libraries for Haskell
- Consider validation approach (library vs manual validation)
- Determine error reporting strategy

**Findings**:
- **Decision**: Use manual JSON schema validation for initial implementation (validate required fields, types, structure)
- **Rationale**:
  - Full JSON schema validation libraries (e.g., `aeson-schema`) add dependencies
  - For initial implementation, manual validation of common cases sufficient
  - Validate: required fields present, field types match schema, nested objects/arrays
  - Manual validation simpler and aligns with Principle 5 (Progressive Iteration)
  - Can add full schema validation library later if needed
- **Validation Strategy**:
  - Check all required fields are present in arguments
  - Validate field types match schema (string, number, boolean, object, array)
  - For nested objects, recursively validate structure
  - Return clear error messages for validation failures
- **Validation Function**:
  ```haskell
  validateToolArgs :: Value -> Value -> Either Text Value
  -- Validates arguments Value against schema Value
  -- Returns Right validated args or Left error message
  ```
- **Alternatives Considered**:
  - Full JSON schema validation library (aeson-schema) - deferred (manual validation simpler initially)
  - No validation - rejected (violates Principle 4 correctness, unsafe)

### 4. Tool Invocation Flow

**Question**: What is the complete flow for tool invocation during agent execution?

**Research Approach**:
- Design the execution loop that handles tool calls
- Determine how tool results are fed back to the LLM
- Consider conversation context with tool invocations

**Findings**:
- **Decision**: Implement iterative execution loop: detect tool call → validate → invoke → send result to LLM → get final response
- **Rationale**:
  - OpenAI function calling requires iterative conversation:
    1. Send request with tool definitions
    2. LLM responds with tool call request
    3. Execute tool and send result back to LLM
    4. LLM generates final response incorporating tool result
  - Conversation context must include tool invocation messages
  - Tool result messages have `role: "function"` and `name: <tool_name>`
  - Loop continues until LLM returns text response (no more tool calls)
  - Maximum iterations to prevent infinite loops (e.g., 10 iterations)
- **Execution Flow**:
  1. Build LLM request with agent instructions, conversation context, and tool definitions
  2. Send request to LLM API
  3. Parse response:
     - If `function_call` present: validate and invoke tool, add tool result to context, loop back to step 2
     - If text response: return final response to user
  4. Handle errors at each step (validation, invocation, API errors)
- **Context Management**:
  - Add assistant message with tool call to context
  - Add function message with tool result to context
  - Final text response also added to context
- **Iteration Limits**:
  - Maximum tool call iterations: 10 (prevents infinite loops)
  - If limit reached, return error or final response
- **Alternatives Considered**:
  - Single-pass execution (no tool result feedback) - rejected (OpenAI requires iterative flow)
  - Async tool execution - deferred (synchronous simpler, can add async later)

### 5. Tool Description vs Implementation Separation

**Question**: How should tool descriptions be separated from tool implementations to enable serialization, late binding, and A/B testing?

**Research Approach**:
- Consider gram notation serialization requirements (tool implementations not serializable)
- Evaluate late binding architecture (bind descriptions to implementations at execution time)
- Design for A/B testing (same agent specification with different tool implementations)
- Review TODO.md requirements for tool description/implementation separation

**Findings**:
- **Decision**: Separate ToolSpecification (serializable, declarative) from Tool (executable, bound at runtime) with ToolLibrary for late binding
- **Rationale**:
  - Gram notation requires serializable agent specifications (tool implementations contain function closures, not serializable)
  - Late binding enables A/B testing: same agent specification with different tool implementations
  - ToolLibrary pattern maps tool names to implementations, enabling runtime binding
  - Separation aligns with TODO.md requirement: "tool descriptions in gram, implementations bound at execution time"
- **Architecture**:
  - **ToolSpecification**: Contains name, description, schema (serializable, used in Agent)
  - **Tool**: Contains name, description, schema, invoke function (executable, registered in ToolLibrary)
  - **ToolLibrary**: Registry mapping tool names to Tool implementations
  - **Binding**: At execution time, ToolSpecifications bound to Tools from ToolLibrary
- **Flow**:
  1. Gram notation → Agent with ToolSpecifications (serializable)
  2. Deserialization → Agent with ToolSpecifications
  3. Execution environment → ToolLibrary with Tool implementations
  4. Tool binding → Match ToolSpecifications to Tools from ToolLibrary
  5. Execution → Use bound Tools to invoke
- **A/B Testing**:
  - Same Agent (with ToolSpecifications) can be executed with different ToolLibrary instances
  - Different ToolLibrary instances provide different Tool implementations for same tool name
  - Enables comparing tool implementation effectiveness
- **Alternatives Considered**:
  - Tool with implementation in Agent - rejected (not serializable, no A/B testing support)
  - Tool binding at agent creation - rejected (too early, prevents A/B testing)
  - Tool binding at deserialization - rejected (still too early, implementations not available)

### 6. Gram Type Signature Design

**Question**: How should tool type signatures be represented in gram notation using Hindley-Milner style syntax?

**Research Approach**:
- Study gram notation capabilities for representing types
- Design grammar for function type signatures (e.g., `(a)-->(b)-->(c)`)
- Design mapping from gram type signatures to JSON schemas
- Evaluate expressiveness vs simplicity tradeoffs
- Consider how gram notation can represent parameter names, types, and optional parameters

**Findings**:
- **Decision**: Use gram notation with Hindley-Milner style for type signatures (e.g., `(name: Text) --> IO Text`)
- **Rationale**:
  - Gram notation supports type representation (e.g., `(a)-->(b)-->(c)` is valid gram)
  - Hindley-Milner style is familiar to Haskell developers
  - More concise than manual JSON schemas
  - Self-documenting (parameter names in signature)
  - Serializable in gram notation
  - Can be parsed and converted to JSON schema automatically
- **Type Signature Grammar**:
  - Simple function: `(Text) --> IO Text`
  - Named parameters: `(name: Text) --> IO Text`
  - Multiple parameters: `(name: Text, age: Int) --> IO Text`
  - Optional parameters: `(name: Text, age: Maybe Int) --> IO Text`
  - Record parameters: `(params: {name: Text, age: Int}) --> IO Text`
  - Nested types: `(user: {name: Text, address: {city: Text}}) --> IO Text`
- **Schema Generation**:
  - Parse gram type signature to extract parameter names, types, and optionality
  - Convert gram types to JSON schema types (Text → string, Int → number, etc.)
  - Handle optional parameters (Maybe types become optional in JSON schema)
  - Generate required fields list from non-optional parameters
  - Handle nested records as nested JSON objects
- **Type Mapping**:
  - `Text` → JSON `"type": "string"`
  - `Int` → JSON `"type": "integer"`
  - `Double` → JSON `"type": "number"`
  - `Bool` → JSON `"type": "boolean"`
  - `Maybe T` → Optional parameter (not in required list)
  - `{field1: T1, field2: T2}` → JSON object with properties
  - `[T]` → JSON `"type": "array"`
- **Parser Design**:
  - Parse type signature string to structured representation
  - Extract parameter list (with names and types)
  - Extract return type
  - Validate grammar syntax
- **Schema Generator Design**:
  - Convert parsed type signature to JSON schema
  - Generate properties from parameter list
  - Generate required list from non-optional parameters
  - Recursively handle nested types
- **Alternatives Considered**:
  - Manual JSON schemas - rejected (too verbose, error-prone, not self-documenting)
  - Haskell type inference - rejected (not serializable in gram, requires function implementation)
  - Template Haskell - rejected (too complex, not needed with gram notation)
  - Record types with auto-derivation - considered but gram type signatures more flexible for serialization

### 7. sayHello Tool Implementation

**Question**: What should the `sayHello` tool do and how should it be implemented?

**Research Approach**:
- Design simple greeting tool for hello world example
- Determine tool parameters and return value
- Consider tool implementation pattern

**Findings**:
- **Decision**: Implement `sayHello` tool that accepts a `name` parameter (string) and returns a friendly greeting message (string)
- **Rationale**:
  - Simple tool demonstrates complete tool execution flow
  - Single parameter keeps implementation straightforward
  - Friendly greeting aligns with agent instructions ("have friendly conversations")
  - Tool serves as concrete example and test case
- **Tool Specification** (in gram notation):
  - Name: `sayHello`
  - Description: "Returns a friendly greeting message for the given name"
  - Type Signature: `(name: Text) --> IO Text`
  - JSON Schema: Auto-generated from type signature
- **Gram Notation Example**:
  ```gram
  toolSpecification: sayHello
    name: "sayHello"
    description: "Returns a friendly greeting message for the given name"
    typeSignature: "(name: Text) --> IO Text"
  ```
- **Implementation**:
  ```haskell
  sayHelloTool :: Tool
  sayHelloTool = createTool
    "sayHello"
    "Returns a friendly greeting message for the given name"
    (typeSignatureToJSONSchema "(name: Text) --> IO Text")  -- Auto-generated schema
    (\args -> do
      -- Extract name from args JSON
      -- Return greeting message
    )
  ```
- **Hello World Agent**:
  - Agent name: "hello_world_agent"
  - Instructions: "You are a friendly assistant. Have friendly conversations with the user. When the user greets you or says hello, use the `sayHello` tool to respond with a personalized greeting."
  - ToolSpecifications: [sayHelloSpec] (serializable)
  - Model: Any OpenAI model (e.g., gpt-3.5-turbo)
- **ToolLibrary**:
  - sayHello Tool registered in ToolLibrary
  - Tool binding happens at execution time
- **Alternatives Considered**:
  - More complex tool (multiple parameters) - rejected (simpler tool better for hello world example)
  - Tool with side effects (e.g., logging) - rejected (pure greeting sufficient)

## Resolved Technical Context

After research, the following clarifications are resolved:

- **OpenAI Function Calling**: Use `functions` array in requests, `function_call` in responses, `function` role for tool results
- **Tool Call Parsing**: Extract `function_call.name` and `function_call.arguments` from OpenAI responses, parse arguments JSON
- **Schema Validation**: Manual JSON schema validation for required fields and types (full schema library deferred)
- **Tool Invocation Flow**: Iterative execution loop: detect tool call → validate → invoke → send result to LLM → get final response
- **sayHello Tool**: Simple greeting tool with `name` parameter, returns friendly greeting message
- **Tool Specification vs Implementation**: Separate ToolSpecification (serializable) from Tool (executable), bind at execution time via ToolLibrary
- **Late Binding Architecture**: Tool descriptions in Agent, tool implementations in ToolLibrary, binding happens at execution time for A/B testing support
- **Gram Type Signatures**: Tool specifications use gram notation with Hindley-Milner style type signatures (e.g., `(name: Text) --> IO Text`), JSON schemas auto-generated from type signatures

## Resolved Technical Decisions

1. **OpenAI Function Calling Integration**:
   - Add `functions` parameter to LLM requests when agent has tools
   - Parse `function_call` from LLM responses
   - Send tool results as `function` role messages
   - Iterative execution loop until final text response

2. **Tool System Implementation**:
   - **ToolSpecification**: Serializable tool specification (name, description, gram type signature) - used in Agent
   - **Type Signature**: Gram notation with Hindley-Milner style (e.g., `(name: Text) --> IO Text`)
   - **Schema Generation**: JSON schemas auto-generated from gram type signatures
   - **Tool**: Executable tool implementation (name, description, schema, invoke function) - registered in ToolLibrary
   - **ToolLibrary**: Registry mapping tool names to implementations - enables late binding
   - **Late Binding**: Tool specifications bound to implementations at execution time via ToolLibrary
   - Parameter validation before invocation
   - Tool execution with error handling

3. **Architecture for A/B Testing**:
   - Agent contains ToolSpecifications (serializable, declarative)
   - ToolLibrary contains Tool implementations (executable, bound at runtime)
   - Same Agent specification can be executed with different ToolLibrary instances
   - Enables A/B testing: same agent, different tool implementations

4. **Hello World Example**:
   - sayHello ToolSpecification with gram type signature `(name: Text) --> IO Text` (serializable)
   - JSON schema auto-generated from type signature
   - sayHello Tool implementation (executable)
   - sayHello Tool registered in ToolLibrary
   - Hello world agent with ToolSpecification
   - Execution binds ToolSpecification to Tool from ToolLibrary
   - Scenario test demonstrating complete flow

## Notes

- Research aligns with Principle 5 (Progressive Iteration) by starting with simple manual validation and single tool calls
- Design supports extensibility for future features (multiple tool calls, full schema validation, async execution)
- Type safety prioritized (Principle 4: Correctness) through parameter validation
- API design focuses on expressiveness (Principle 4: Expressiveness) with clear tool creation and execution interfaces

