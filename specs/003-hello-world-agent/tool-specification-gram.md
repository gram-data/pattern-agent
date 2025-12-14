# Tool Specification Gram Schema

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the gram notation schema for tool specifications

## Overview

Tool specifications are represented in gram notation with the following structure:
- Tool name (unique identifier)
- Tool description (natural language)
- Type signature (gram path notation, curried form with property records)

## Gram Schema Structure

```gram
[<toolName>:ToolSpecification {
  description: "<natural language description>"
} |
  <curried function signature with property records for parameter names>
]
```

**Key Points**:
- Pattern identifier (`<toolName>`) is the unique tool name
- Tool name must be globally unique (required for LLM tool calling)
- Label `:ToolSpecification` indicates the type
- Description stored in property record

## Example: sayHello Tool

```gram
[sayHello:ToolSpecification {
  description: "Returns a friendly greeting message for the given name"
} |
  (::Text {paramName:"name"})==>(::String)
]
```

## Field Definitions

### Tool Name (Pattern Identifier)

- **Type**: Pattern identifier (symbol)
- **Required**: Yes
- **Description**: Unique identifier for the tool (stored as pattern identifier, not property)
- **Constraints**: Must be globally unique (required for LLM tool calling), must be valid gram identifier
- **Example**: `sayHello:ToolSpecification` - tool name is `sayHello`

### `description`

- **Type**: Text
- **Required**: Yes
- **Description**: Natural language description of what the tool does
- **Constraints**: Must be non-empty
- **Purpose**: Helps LLM understand when and how to use the tool

### Type Signature (Curried Form)

- **Type**: Gram path notation (curried form with property records)
- **Required**: Yes
- **Description**: Function signature in gram path notation using curried form with property records for parameter names
- **Format**: Curried form with `==>` arrows, parameter names in `{paramName:"..."}` properties
- **Examples**:
  - `(::Text)==>(::String)` - Simple function (no parameter name needed)
  - `(::Text {paramName:"name"})==>(::String)` - Named parameter
  - `(::Text {paramName:"name"})==>(::Int {paramName:"age"})==>(::String)` - Multiple parameters (curried)
  - `(::Text {paramName:"name"})==>(::Int {paramName:"age", optional:true})==>(::String)` - Optional parameter
  - `(::Object {paramName:"params", fields:[{name:"name", type:"Text"}, {name:"age", type:"Int"}]})==>(::String)` - Record parameter

## Schema Generation

The curried function signature (gram path notation) is used to automatically generate JSON schema for LLM API compatibility. The schema generation process:

1. Extract parameter nodes from curried chain (all nodes before final return type)
2. Extract `paramName` from each node's properties
3. Extract type labels from each parameter node
4. Convert gram types to JSON schema types (Text → string, Int → integer, etc.)
5. Group parameters into object structure with properties and required fields
6. Handle optional parameters (marked with `optional:true` in properties)

See `type-signature-grammar.md` for detailed grammar and schema generation rules.

## Serialization

Tool specifications are fully serializable in gram notation. The type signature is stored as gram path notation (curried form) in the pattern elements, and JSON schema is generated during deserialization or when needed for LLM API calls.

## Notes

- Tool specifications are declarative (no implementation)
- Type signatures use gram notation (serializable)
- JSON schemas are derived, not stored
- Tool specifications can be shared across multiple agents

