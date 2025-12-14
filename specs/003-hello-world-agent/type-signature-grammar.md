# Type Signature Grammar

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define the grammar for tool type signatures in gram path notation (curried form with property records)

## Grammar Overview

Tool type signatures use gram path notation in curried form with property records for parameter names. This approach:
- Creates graph structures enabling function composition and pattern matching
- Avoids global identifier conflicts by storing parameter names in property records
- Represents only JSON Schema types (not Haskell implementation details like `IO`)
- Uses `==>` arrows for function type relationships (convention for clarity; gram treats all arrow types as semantically equivalent)

## Basic Syntax

### Curried Function Type

```
<param1>==><param2>==>...==><returnType>
```

Where:
- `==>` is the function arrow (used by convention for clarity; gram treats `==>`, `-->`, `~~>`, etc. as semantically equivalent - arrow types are decorative)
- Each `<param>` is a type node with optional `{paramName:"..."}` property
- `<returnType>` is the final type node (JSON Schema type, not Haskell `IO` types)
- Parameters are chained in curried form: `Type1 ==> Type2 ==> Type3`
- **Note**: While we use `==>` for clarity, other arrow types (`-->`, `~~>`, etc.) would also work - gram does not enforce semantic differences between arrow types

### Parameter Nodes

```
(::TypeLabel {paramName: "name", optional: true|false})
```

Where:
- `::TypeLabel` is a type label (e.g., `::Text`, `::Int`, `::String`)
- `{paramName: "name"}` is a property record storing the parameter name (avoids global identifier conflicts)
- `optional: true` marks optional parameters (not in required list)
- Parameter name is stored in properties, not as an identifier

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

Optional parameters are marked with `optional: true` in the property record:
```gram
(::Int {paramName:"age", optional:true})==>(::String)
```

### Record Parameters

Record parameters use `:Object` with field definitions in properties:
```gram
(::Object {paramName:"user", fields:[{name:"name", type:"Text"}, {name:"age", type:"Int"}]})==>(::String)
```

### Array Parameters

Array parameters use `:Array` with element type:
```gram
(::Array {paramName:"items", elementType:"Text"})==>(::Int)
```

## Examples

### Simple Function (No Parameters)

```gram
()==>(::String)
```

No parameters, returns string.

### Single Named Parameter

```gram
(::Text {paramName:"name"})==>(::String)
```

Single named parameter `name` of type `Text`, returns `String`.

### Multiple Parameters (Curried Form)

```gram
(::Text {paramName:"name"})==>(::Int {paramName:"age"})==>(::String)
```

Multiple parameters in curried form: `name: Text` then `age: Int`, returns `String`.

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
(::Text {paramName:"name"})==>(::Int {paramName:"age", optional:true})==>(::String)
```

Optional `age` parameter (not in required list).

### Record Parameter

```gram
(::Object {paramName:"user", fields:[{name:"name", type:"Text"}, {name:"email", type:"Text"}]})==>(::String)
```

Record/object parameter with nested fields.

### Nested Record Types

```gram
(::Object {
  paramName:"user",
  fields:[
    {name:"name", type:"Text"},
    {name:"address", type:"Object", fields:[{name:"city", type:"Text"}, {name:"country", type:"Text"}]}
  ]
})==>(::String)
```

Nested record types with recursive field definitions.

### Array Parameter

```gram
(::Array {paramName:"items", elementType:"Text"})==>(::Int)
```

Array parameter with element type.

### Complex Example

```gram
(::Object {
  paramName:"searchParams",
  fields:[
    {name:"query", type:"Text"},
    {name:"filters", type:"Object", fields:[
      {name:"category", type:"Text", optional:true},
      {name:"priceRange", type:"Object", optional:true, fields:[
        {name:"min", type:"Double"},
        {name:"max", type:"Double"}
      ]}
    ]},
    {name:"limit", type:"Int", optional:true}
  ]
})==>(::Array {elementType:"Text"})
```

Complex nested structure with optional fields.

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
2. **Extract Parameter Names**: Extract `paramName` from each parameter node's properties
3. **Extract Types**: Extract type labels from each parameter node
4. **Convert Types**: Map gram type labels to JSON Schema types
5. **Group Parameters**: Group all parameters into object structure with properties
6. **Required Fields**: Include all parameters without `optional: true` in required list
7. **Optional Fields**: Exclude parameters with `optional: true` from required list
8. **Nested Types**: Recursively generate schemas for `:Object` and `:Array` types

### Example: Schema Generation

**Type Signature** (Curried Form):
```gram
(::Text {paramName:"name"})==>(::Int {paramName:"age", optional:true})==>(::String)
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

**Note**: `age` is not in `required` because it has `optional: true` in its properties.

## Parser Design

### Parsing Steps

1. **Parse Gram Path**: Parse curried chain as gram path notation
2. **Extract Parameter Nodes**: Collect all nodes before final return type node
3. **Extract Properties**: For each parameter node, extract `paramName` and `optional` from properties
4. **Extract Types**: Extract type labels from each node
5. **Build Parameter List**: Construct parameter list with names and types
6. **Extract Return Type**: Extract final node as return type
7. **Validate**: Check syntax and type validity

### Parse Tree Structure

```haskell
data TypeSignature = TypeSignature
  { params :: [Param]
  , returnType :: ReturnType
  }

data Param = Param
  { paramName :: Text           -- From {paramName:"..."} property
  , paramType :: Type
  , optional :: Bool            -- From {optional:true} property (default: false)
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
- Parameter names must be stored in `{paramName:"..."}` properties (not as identifiers)
- Type labels must be recognized JSON Schema types (`::Text`, `::Int`, `::Double`, `::Bool`, `::String`, `::Object`, `::Array`)
- Return type must be a valid JSON Schema type (not Haskell `IO` types)
- Nested types must be well-formed
- Property records must be valid gram syntax

## Error Handling

Parser should return clear error messages for:
- Invalid gram path syntax
- Unknown type labels
- Missing `paramName` in parameter properties
- Malformed property records
- Invalid curried chain structure
- Missing return type
- Use of Haskell types (e.g., `IO Text`) instead of JSON Schema types

**Note on Arrow Types**: Gram treats all arrow types (`==>`, `-->`, `~~>`, `<--`, etc.) as semantically equivalent. We use `==>` by convention for clarity in function type signatures, but parsers should accept any valid gram relationship arrow.

## Global Identifier Constraints

**Critical Constraint**: Gram notation does **not** scope identifiers. All identifiers must be globally unique, even when nested inside patterns.

**Solution**: Use property records for parameter names:
- ✅ Parameter names in `{paramName:"name"}` properties (not identifiers)
- ✅ No global uniqueness constraint
- ✅ Names available for JSON Schema generation
- ✅ Maintains graph structure benefits

**Example**:
```gram
// Function 1
[:Function {name:"sayHello"} |
  (::Text {paramName:"name"})==>(::String)
]

// Function 2 - No conflict!
[:Function {name:"greet"} |
  (::Text {paramName:"name"})==>(::String)
]
```

Both functions can use `paramName:"name"` without conflicts because parameter names are in properties, not identifiers.

## Notes

- Grammar uses curried form for graph structure benefits (composition, pattern matching)
- Parameter names stored in property records to avoid global identifier conflicts
- Represents only JSON Schema types (not Haskell implementation details)
- Fully serializable in gram path notation
- Supports common tool patterns (single param, multiple params, optional params, records)
- Extensible for future type additions
- Graph structure enables function composition and type querying
