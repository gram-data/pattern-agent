# Type Signature Grammar

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the grammar for tool type signatures in gram path notation (curried form with parameter names as identifiers)

## Grammar Overview

Tool type signatures use gram path notation in curried form with parameter names as node identifiers. This approach:
- Creates graph structures enabling function composition and pattern matching
- Encourages consistent vocabulary by requiring globally unique parameter names
- Represents only JSON Schema types (not Haskell implementation details like `IO`)
- Uses `==>` arrows for function type relationships (convention for clarity; gram treats all arrow types as semantically equivalent)

## Basic Syntax

### Curried Function Type

```
<param1>==><param2>==>...==><returnType>
```

Where:
- `==>` is the function arrow (used by convention for clarity; gram treats `==>`, `-->`, `~~>`, etc. as semantically equivalent - arrow types are decorative)
- Each `<param>` is a type node with a parameter name as its identifier
- `<returnType>` is the final type node (JSON Schema type, not Haskell `IO` types)
- Parameters are chained in curried form: `Type1 ==> Type2 ==> Type3`
- **Note**: While we use `==>` for clarity, other arrow types (`-->`, `~~>`, etc.) would also work - gram does not enforce semantic differences between arrow types

### Parameter Nodes

```
(paramName::TypeLabel {default: value})
```

Where:
- `paramName` is the parameter name as a node identifier (must be globally unique)
- `::TypeLabel` is a type label (e.g., `::Text`, `::Int`, `::String`)
- `default: value` specifies the default value for optional parameters (not in required list)
- Parameter name is the node identifier, encouraging consistent vocabulary across tool specifications
- Default value must match the parameter type

### Type Labels

Type labels use JSON Schema types:
- `:Text` - String type
- `:Int` - Integer type
- `:Double` - Number type
- `:Bool` - Boolean type
- `:String` - String type (return type)
- `:Object` - Object type
- `:Array` - Array type

**Note**: Return types use JSON Schema types (e.g., `::String`), not Haskell types (e.g., `IO Text`). The `IO` and Haskell-specific details are implementation concerns, not part of the gram representation.

## Type System

### Basic Types

- `::Text` - Text string (JSON Schema: `string`)
- `::Int` - Integer (JSON Schema: `integer`)
- `::Double` - Number (JSON Schema: `number`)
- `::Bool` - Boolean (JSON Schema: `boolean`)
- `::String` - String (JSON Schema: `string`, typically used for return types)
- `::Object` - Object type (JSON Schema: `object`)
- `::Array` - Array type (JSON Schema: `array`)

### Optional Parameters

Optional parameters are marked with `default: value` in the property record:
```gram
(age::Int {default:18})==>(::String)
```

If `age` is not provided, it defaults to `18`.

### Record Parameters

Record parameters use `:Object` with field definitions in properties:
```gram
(userParams::Object {fields:[{name:"name", type:"Text"}, {name:"age", type:"Int"}]})==>(::String)
```

### Array Parameters

Array parameters use `:Array` with element type:
```gram
(items::Array {elementType:"Text"})==>(::Int)
```

## Examples

### Simple Function (No Parameters)

```gram
()==>(::String)
```

No parameters, returns string.

### Single Named Parameter

```gram
(personName::Text)==>(::String)
```

Single named parameter `personName` of type `Text`, returns `String`.

### Multiple Parameters (Curried Form)

```gram
(personName::Text)==>(age::Int)==>(::String)
```

Multiple parameters in curried form: `personName: Text` then `age: Int`, returns `String`.

**JSON Schema Mapping**: Parameters are grouped into an object:
```json
{
  "type": "object",
  "properties": {
    "name": {"type": "string"},
    "age": {"type": "integer"}
  },
  "required": ["name", "age"]
}
```

### Optional Parameter

```gram
(personName::Text)==>(age::Int {default:18})==>(::String)
```

Optional `age` parameter with default value `18` (not in required list).

### Record Parameter

```gram
(userParams::Object {fields:[{name:"name", type:"Text"}, {name:"email", type:"Text"}]})==>(::String)
```

Record/object parameter with nested fields.

### Nested Record Types

```gram
(userParams::Object {
  fields:[
    {name:"name", type:"Text"},
    {name:"address", type:"Object", fields:[{name:"city", type:"Text"}, {name:"country", type:"Text"}]}
  ]
})==>(::String)
```

Nested record types with recursive field definitions.

### Array Parameter

```gram
(items::Array {elementType:"Text"})==>(::Int)
```

Array parameter with element type.

### Complex Example

```gram
(searchParams::Object {
  fields:[
    {name:"query", type:"Text"},
    {name:"filters", type:"Object", fields:[
      {name:"category", type:"Text", default:""},
      {name:"priceRange", type:"Object", default:{min:0.0, max:1000.0}, fields:[
        {name:"min", type:"Double"},
        {name:"max", type:"Double"}
      ]}
    ]},
    {name:"limit", type:"Int", default:10}
  ]
})==>(results::Array {elementType:"Text"})
```

Complex nested structure with optional fields that have default values.

## JSON Schema Generation

### Type Mapping

| Gram Type Label | JSON Schema Type | Notes |
|----------------|------------------|-------|
| `::Text` | `"type": "string"` | |
| `::Int` | `"type": "integer"` | |
| `::Double` | `"type": "number"` | |
| `::Bool` | `"type": "boolean"` | |
| `::String` | `"type": "string"` | Return type |
| `::Object` | `"type": "object"` | With properties from fields |
| `::Array` | `"type": "array"` | With items from elementType |

### Schema Generation Rules

1. **Extract Parameter Chain**: Traverse curried chain, collect all nodes before final return type
2. **Extract Parameter Names**: Extract parameter name from each parameter node's identifier
3. **Extract Types**: Extract type labels from each parameter node
4. **Convert Types**: Map gram type labels to JSON Schema types
5. **Group Parameters**: Group all parameters into object structure with properties
6. **Required Fields**: Include all parameters without `default` in required list
7. **Optional Fields**: Exclude parameters with `default` from required list, include default value in schema
8. **Nested Types**: Recursively generate schemas for `:Object` and `:Array` types

### Example: Schema Generation

**Type Signature** (Curried Form):
```gram
(personName::Text)==>(age::Int {default:18})==>(::String)
```

**Generated JSON Schema**:
```json
{
  "type": "object",
  "properties": {
    "personName": {
      "type": "string"
    },
    "age": {
      "type": "integer",
      "default": 18
    }
  },
  "required": ["personName"]
}
```

**Note**: `age` is not in `required` because it has `default: 18` in its properties. The default value is included in the JSON schema.

## Parser Design

### Parsing Steps

1. **Parse Gram Path**: Parse curried chain as gram path notation
2. **Extract Parameter Nodes**: Collect all nodes before final return type node
3. **Extract Identifiers**: For each parameter node, extract parameter name from node identifier
4. **Extract Properties**: Extract `default` value from properties (if present, parameter is optional)
5. **Extract Types**: Extract type labels from each node
6. **Build Parameter List**: Construct parameter list with names, types, and default values
7. **Extract Return Type**: Extract final node as return type
8. **Validate**: Check syntax and type validity (default values must match parameter types)

### Parse Tree Structure

```haskell
data TypeSignature = TypeSignature
  { params :: [Param]
  , returnType :: ReturnType
  }

data Param = Param
  { paramName :: Text           -- From node identifier
  , paramType :: Type
  , paramDefault :: Maybe Value -- From {default:value} property (Nothing if required)
  }

data Type = TextType
          | IntType
          | DoubleType
          | BoolType
          | StringType
          | ObjectType [Field]  -- For record parameters
          | ArrayType Type      -- For array parameters

data Field = Field
  { fieldName :: Text
  , fieldType :: Type
  , optional :: Bool
  }
```

## Validation Rules

- Type signature must be valid gram path notation
- Curried chain must use relationship arrows (`==>`, `-->`, `~~>`, etc. - all are semantically equivalent in gram; we use `==>` by convention for clarity)
- Parameter names must be node identifiers (globally unique)
- Type labels must be recognized JSON Schema types (`::Text`, `::Int`, `::Double`, `::Bool`, `::String`, `::Object`, `::Array`)
- Return type must be a valid JSON Schema type (not Haskell `IO` types)
- Nested types must be well-formed
- Property records must be valid gram syntax

## Error Handling

Parser should return clear error messages for:
- Invalid gram path syntax
- Unknown type labels
- Missing parameter name identifier
- Duplicate parameter name identifiers (global uniqueness violation)
- Default value type mismatch (default value must match parameter type)
- Malformed property records
- Invalid curried chain structure
- Missing return type
- Use of Haskell types (e.g., `IO Text`) instead of JSON Schema types

**Note on Arrow Types**: Gram treats all arrow types (`==>`, `-->`, `~~>`, `<--`, etc.) as semantically equivalent. We use `==>` by convention for clarity in function type signatures, but parsers should accept any valid gram relationship arrow.

## Global Identifier Constraints

**Design Decision**: Parameter names are node identifiers and must be globally unique. This constraint:
- ✅ Encourages consistent vocabulary across tool specifications
- ✅ Makes parameter names more prominent and readable
- ✅ Aligns with gram notation's first-class identifier concept
- ✅ Simplifies parsing (extract identifier directly)
- ✅ Promotes descriptive names (e.g., `personName` instead of generic `name`)

**Example**:
```gram
// Function 1
[sayHello:Tool |
  (personName::Text)==>(::String)
]

// Function 2 - Uses descriptive name to avoid conflict
[greet:Tool |
  (userName::Text)==>(::String)
]
```

Both functions use descriptive parameter names that are globally unique, encouraging a consistent vocabulary.

## Notes

- Grammar uses curried form for graph structure benefits (composition, pattern matching)
- Parameter names are node identifiers, requiring global uniqueness and encouraging consistent vocabulary
- Represents only JSON Schema types (not Haskell implementation details)
- Fully serializable in gram path notation
- Supports common tool patterns (single param, multiple params, optional params, records)
- Extensible for future type additions
- Graph structure enables function composition and type querying
