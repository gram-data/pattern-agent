# Type Signature Grammar

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the grammar for tool type signatures in gram notation (Hindley-Milner style)

## Grammar Overview

Tool type signatures use gram notation with Hindley-Milner style syntax, similar to Haskell type signatures but adapted for gram notation serialization.

## Basic Syntax

### Function Type

```
<paramList> --> <returnType>
```

Where:
- `-->` is the function arrow (gram notation supports `(a)-->(b)-->(c)`)
- `<paramList>` is a comma-separated list of parameters
- `<returnType>` is the return type (typically `IO Text` for tools)

### Parameter List

```
( <param> [, <param>]* )
```

Where `<param>` can be:
- Type only: `Text`
- Named parameter: `name: Text`
- Optional parameter: `name: Maybe Text`
- Record parameter: `params: {field1: Type1, field2: Type2}`

## Type System

### Basic Types

- `Text` - Text string
- `Int` - Integer
- `Double` - Floating point number
- `Bool` - Boolean
- `Maybe T` - Optional type (nullable)
- `[T]` - Array/list type
- `{field1: T1, field2: T2}` - Record/object type

### Return Types

Tools typically return `IO Text` (IO action producing text), but can also return:
- `IO Int`
- `IO Bool`
- `IO [Text]`
- `IO {field: Type}`

## Examples

### Simple Function

```
() --> IO Text
```

No parameters, returns text.

### Named Parameter

```
(name: Text) --> IO Text
```

Single named parameter.

### Multiple Parameters

```
(name: Text, age: Int) --> IO Text
```

Multiple parameters.

### Optional Parameter

```
(name: Text, age: Maybe Int) --> IO Text
```

Optional `age` parameter (not in required list).

### Record Parameter

```
(user: {name: Text, email: Text}) --> IO Text
```

Record/object parameter.

### Nested Types

```
(user: {name: Text, address: {city: Text, country: Text}}) --> IO Text
```

Nested record types.

### Array Parameter

```
(items: [Text]) --> IO Int
```

Array parameter.

### Complex Example

```
(searchParams: {query: Text, filters: {category: Maybe Text, priceRange: Maybe {min: Double, max: Double}}, limit: Maybe Int}) --> IO [Text]
```

Complex nested structure with optional fields.

## JSON Schema Generation

### Type Mapping

| Gram Type | JSON Schema Type | Notes |
|-----------|------------------|-------|
| `Text` | `"type": "string"` | |
| `Int` | `"type": "integer"` | |
| `Double` | `"type": "number"` | |
| `Bool` | `"type": "boolean"` | |
| `Maybe T` | Same as `T` | Not in required list |
| `[T]` | `"type": "array", "items": {schema for T}` | |
| `{field1: T1, ...}` | `"type": "object", "properties": {...}` | |

### Schema Generation Rules

1. **Parameter Names**: Extract from `name: Type` format, use as JSON property names
2. **Required Fields**: All non-`Maybe` parameters are required
3. **Optional Fields**: `Maybe T` parameters are not in required list
4. **Nested Types**: Recursively generate schemas for nested records
5. **Arrays**: Generate `items` schema from element type

### Example: Schema Generation

**Type Signature**:
```
(name: Text, age: Maybe Int) --> IO Text
```

**Generated JSON Schema**:
```json
{
  "type": "object",
  "properties": {
    "name": {
      "type": "string"
    },
    "age": {
      "type": "integer"
    }
  },
  "required": ["name"]
}
```

Note: `age` is not in `required` because it's `Maybe Int`.

## Parser Design

### Parsing Steps

1. **Tokenize**: Split type signature into tokens
2. **Parse Parameter List**: Extract parameters from `(...)` section
3. **Parse Parameters**: For each parameter, extract name (if present) and type
4. **Parse Return Type**: Extract return type after `-->`
5. **Validate**: Check syntax and type validity

### Parse Tree Structure

```haskell
data TypeSignature = TypeSignature
  { params :: [Param]
  , returnType :: ReturnType
  }

data Param = Param
  { paramName :: Maybe Text  -- Optional name
  , paramType :: Type
  }

data Type = TextType
          | IntType
          | DoubleType
          | BoolType
          | MaybeType Type
          | ArrayType Type
          | RecordType [(Text, Type)]
```

## Validation Rules

- Type signature must be valid gram notation
- Parameter names must be valid identifiers (if present)
- Types must be recognized (Text, Int, Double, Bool, Maybe, arrays, records)
- Return type must be valid
- Nested types must be well-formed

## Error Handling

Parser should return clear error messages for:
- Invalid syntax
- Unknown types
- Malformed parameter lists
- Invalid record structures
- Missing return type

## Notes

- Grammar is designed to be familiar to Haskell developers
- Supports common tool patterns (single param, multiple params, optional params, records)
- Extensible for future type additions
- Fully serializable in gram notation

