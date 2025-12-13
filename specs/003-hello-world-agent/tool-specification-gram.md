# Tool Specification Gram Schema

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the gram notation schema for tool specifications

## Overview

Tool specifications are represented in gram notation with the following structure:
- Tool name (unique identifier)
- Tool description (natural language)
- Type signature (gram notation, Hindley-Milner style)

## Gram Schema Structure

```gram
toolSpecification: <toolName>
  name: "<toolName>"
  description: "<natural language description>"
  typeSignature: "<gram type signature>"
```

## Example: sayHello Tool

```gram
toolSpecification: sayHello
  name: "sayHello"
  description: "Returns a friendly greeting message for the given name"
  typeSignature: "(name: Text) --> IO Text"
```

## Field Definitions

### `name`

- **Type**: Text
- **Required**: Yes
- **Description**: Unique identifier for the tool
- **Constraints**: Must be non-empty, must be unique within an agent's tool list

### `description`

- **Type**: Text
- **Required**: Yes
- **Description**: Natural language description of what the tool does
- **Constraints**: Must be non-empty
- **Purpose**: Helps LLM understand when and how to use the tool

### `typeSignature`

- **Type**: Text (gram notation type signature)
- **Required**: Yes
- **Description**: Type signature in gram notation using Hindley-Milner style
- **Format**: See `type-signature-grammar.md` for complete grammar definition
- **Examples**:
  - `(Text) --> IO Text` - Simple function
  - `(name: Text) --> IO Text` - Named parameter
  - `(name: Text, age: Int) --> IO Text` - Multiple parameters
  - `(name: Text, age: Maybe Int) --> IO Text` - Optional parameter
  - `(params: {name: Text, age: Int}) --> IO Text` - Record parameter

## Schema Generation

The `typeSignature` field is used to automatically generate JSON schema for LLM API compatibility. The schema generation process:

1. Parse the gram type signature
2. Extract parameter names, types, and optionality
3. Convert gram types to JSON schema types
4. Generate properties and required fields
5. Handle nested types recursively

See `type-signature-grammar.md` for detailed grammar and schema generation rules.

## Serialization

Tool specifications are fully serializable in gram notation. The type signature is stored as text in gram notation, and JSON schema is generated during deserialization or when needed for LLM API calls.

## Notes

- Tool specifications are declarative (no implementation)
- Type signatures use gram notation (serializable)
- JSON schemas are derived, not stored
- Tool specifications can be shared across multiple agents

