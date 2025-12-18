# Tool Gram Schema

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the gram notation schema for tools

## Overview

Tools are represented in gram notation with the following structure:
- Tool name (unique identifier)
- Tool description (natural language)
- Type signature (gram path notation, curried form with parameter names as identifiers)

## Gram Schema Structure

```gram
[<toolName>:Tool {
  description: "<natural language description>"
} |
  <curried function signature with parameter names as identifiers>
]
```

**Key Points**:
- Pattern identifier (`<toolName>`) is the unique tool name
- Tool name must be globally unique (required for LLM tool calling)
- Label `:Tool` indicates the type
- Description stored in property record

## Example: sayHello Tool

```gram
[sayHello:Tool {
  description: "Returns a friendly greeting message for the given name"
} |
  (personName::Text {default:"world"})==>(::String)
]
```

## Field Definitions

### Tool Name (Pattern Identifier)

- **Type**: Pattern identifier (symbol)
- **Required**: Yes
- **Description**: Unique identifier for the tool (stored as pattern identifier, not property)
- **Constraints**: Must be globally unique (required for LLM tool calling), must be valid gram identifier
- **Example**: `sayHello:Tool` - tool name is `sayHello`

### `description`

- **Type**: Text
- **Required**: Yes
- **Description**: Natural language description of what the tool does
- **Constraints**: Must be non-empty
- **Purpose**: Helps LLM understand when and how to use the tool

### Type Signature (Curried Form)

- **Type**: Gram path notation (curried form with parameter names as identifiers)
- **Required**: Yes
- **Description**: Function signature in gram path notation using curried form with parameter names as node identifiers
- **Format**: Curried form with `==>` arrows, parameter names as identifiers (e.g., `personName::Text`)
- **Examples**:
  - `()==>(::String)` - Simple function with no parameters
  - `(personName::Text)==>(::String)` - Single named parameter
  - `(personName::Text)==>(age::Int)==>(::String)` - Multiple parameters (curried)
  - `(personName::Text)==>(age::Int {default:18})==>(::String)` - Optional parameter with default value
  - `(userParams::Object {fields:[{name:"name", type:"Text"}, {name:"age", type:"Int"}]})==>(::String)` - Record parameter

## Schema Generation

The curried function signature (gram path notation) is used to automatically generate JSON schema for LLM API compatibility. The schema generation process:

1. Extract parameter nodes from curried chain (all nodes before final return type)
2. Extract parameter name from each node's identifier
3. Extract type labels from each parameter node
4. Convert gram types to JSON schema types (Text → string, Int → integer, etc.)
5. Group parameters into object structure with properties and required fields
6. Handle optional parameters (marked with `default:value` in properties, include default in schema)

**Note**: Parameter names are identifiers and must be globally unique, encouraging a consistent vocabulary across tool specifications.

See `type-signature-grammar.md` for detailed grammar and schema generation rules.

## Serialization

Tool specifications are fully serializable in gram notation. The type signature is stored as gram path notation (curried form) in the pattern elements, and JSON schema is generated during deserialization or when needed for LLM API calls.

## Notes

- Tools are declarative (no implementation)
- Type signatures use gram notation (serializable)
- JSON schemas are derived, not stored
- Tools can be shared across multiple agents

