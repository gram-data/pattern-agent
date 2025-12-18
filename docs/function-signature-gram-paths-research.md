# Research: Representing Function Signatures as Gram Paths

**Purpose**: Explore how gram notation's path syntax could represent function signatures for LLM tool calling as graph structures.

**Date**: 2025-01-27  
**Context**: Function signatures in pattern-agent represent tools as presented to LLMs. LLM tool calling uses JSON Schema with simple JSON types (string, number, integer, boolean, object, array). This research explores using gram's path notation to represent these JSON-schema-compatible function signatures as structured graph patterns.

**Key Constraint**: Gram representations must be expressible in JSON Schema. Haskell implementation details (IO, type constructors, etc.) are not part of the gram representation - they are implementation concerns.

**Status**: All examples in this document have been verified using `gram-lint` CLI tool.

**Note**: For general gram syntax reference (identifiers, relationships, etc.), see `gram-notation-reference.md`.

## Three Required Mappings

1. **Gram → Haskell**: Parse gram signature, bind to Haskell implementation (which may have IO, type constructors, etc.)
2. **Gram → JSON Schema**: Convert gram signature to JSON Schema for LLM tool calling
3. **Haskell → Gram** (optional): Convert Haskell function signature to gram, but only if expressible in JSON Schema

## JSON Schema Type System

Function signatures in gram represent JSON Schema types, not full Haskell types:

| JSON Schema Type | Gram Representation | Haskell Equivalent |
|-----------------|---------------------|-------------------|
| `string` | `Text` | `Text`, `String` |
| `integer` | `Int` | `Int`, `Integer` |
| `number` | `Double` | `Double`, `Float` |
| `boolean` | `Bool` | `Bool` |
| `object` | `{field1: T1, ...}` | Record types |
| `array` | `[T]` | `[T]`, list types |
| optional | `Maybe T` | `Maybe T` (not in required list) |

**Note**: Return types like `IO Text` in gram signatures are documentation only - JSON Schema only defines parameter types. The `IO` and return type are Haskell implementation details.

## Quick Reference: Function Signature Syntax

**Simple Function Type** (JSON Schema types only):
- `(Text)==>(String)` - Double arrow (used by convention for clarity)
- `(Text)-[func]->(String)` - With relationship identifier
- `(Text)-[:FunctionType]->(String)` - With relationship label

**Note**: Gram treats all arrow types (`==>`, `-->`, `~~>`, etc.) as semantically equivalent - they are decorative. We use `==>` by convention for clarity in function type signatures, but any valid gram relationship arrow would work.

**Parameter Types** (JSON Schema compatible):
- `(name: Text)` - String parameter
- `(age: Int)` - Integer parameter
- `(price: Double)` - Number parameter
- `(active: Bool)` - Boolean parameter
- `(items: [Text])` - Array parameter
- `(user: {name: Text, age: Int})` - Object parameter

**Patterns Containing Paths**:
- `[funcType:FunctionType | (Text)==>(String)]` - Pattern with path element

## Current Approach: Text-Based Type Signatures

Currently, function signatures are stored as text strings in gram property records:

```gram
[toolSpec:ToolSpecification {
  name: "sayHello",
  typeSignature: "(name: Text) --> IO Text"  // Text string
}]
```

**Note**: The `--> IO Text` return type is documentation only. JSON Schema (for LLM) only defines parameter types. The `IO` and return type are Haskell implementation details.

**Limitations**:
- Type signatures are opaque strings (not structured)
- Cannot query or manipulate type structure in gram
- Type information is lost (must be parsed from text)
- No way to represent type relationships in gram

## Proposal: Function Signatures as Gram Paths

### Core Idea

Use gram's path notation to represent function signatures as graph structures, where:
- **Type nodes** represent JSON Schema types (Text, Int, Double, Bool, objects, arrays)
- **Function arrows** represent parameter-to-return mappings (`-->` for function arrow)
- **Parameter nodes** represent function parameters (with names and types)
- **Return nodes** represent return types (documentation only - not in JSON Schema)

**Key Constraint**: Only JSON Schema types are represented. Haskell implementation details (IO, type constructors, etc.) are not part of the gram representation.

### Basic Function Type

**JSON Schema**: Parameter `string`, Return `string` (documentation)

**Gram Path Representation** (Verified):
```gram
(Text:ParameterType)==>(String:ReturnType)
```

**With Relationship Identifier** (Verified):
```gram
(Text:ParameterType)-[func]->(String:ReturnType)
```

**With Relationship Label** (Verified):
```gram
(Text:ParameterType)-[:FunctionType]->(String:ReturnType)
```

**Note**: Return types are documentation only. JSON Schema only defines parameter types. The actual Haskell implementation may be `Text -> IO Text`, but gram represents only the JSON Schema interface.

### Function with Parameters

**Haskell**: `(name: Text) -> IO Text`

**Gram Path Representation** (Verified):
```gram
// Parameter node with relationship
(name:Parameter {type: "Text"})-[is_input_of]->(func:FunctionType)

// Function arrow
(func)-[maps_to]->(IO)
```

**Using Double Arrow** (Verified):
```gram
(name:Parameter {type: "Text"})==>(IO)
```

**Note**: For documentation purposes, you could use backtick-delimited identifiers for Haskell return types, but gram path representation should use JSON Schema types:
```gram
// Documentation (Haskell return type)
(name:Parameter {type: "Text"})==>(`IO Text`:HaskellReturnType)

// JSON Schema representation (preferred)
(name:Parameter {type: "Text"})==>(String:ReturnType)
```

### Multiple Parameters

**JSON Schema**: Parameters `{name: string, age: integer}`, Return `string` (documentation)

**Gram Path Representation** (Verified):
```gram
// Multiple parameter nodes
(name:Parameter {type: "Text"})-[is_input_of]->(func:FunctionType)
(age:Parameter {type: "Int"})-[is_input_of]->(func)

// Function arrow to return type
(func)-[maps_to]->(String:ReturnType)
```

**Using Parameter List Pattern** (Verified):
```gram
[params:ParameterList |
  (name:Parameter {type: "Text"}),
  (age:Parameter {type: "Int"})
]
(params)==>(String:ReturnType)
```

### Curried Form for Multiple Parameters (Graph Structure)

**Key Advantage**: Curried form creates a graph structure that enables function composition, decomposition, and pattern matching.

**JSON Schema**: Parameters `{repetitions: integer, name: string}`, Return `string` (documentation)

**Gram Path Representation** (Curried Form):
```gram
(repetitions:Integer)==>(name:String)==>(:String)
```

**Graph Structure Benefits**:

1. **Function Composition**: Intermediate types create graph nodes that can be matched
   ```gram
   // Function 1: Integer -> String
   (:Integer)==>(:String)
   
   // Function 2: String -> String
   (:String)==>(:String)
   
   // Composition: Integer -> String -> String
   (:Integer)==>(:String)==>(:String)
   ```

2. **Pattern Matching**: Query for compatible functions
   ```gram
   // Find functions that can be composed
   (?func1)-[:returns]->(?intermediate:Type)
   (?func2)-[:takes]->(?intermediate)
   ```

3. **Type Sharing**: Multiple functions share intermediate type nodes
   ```gram
   // Multiple functions sharing String type
   (:Integer)==>(:String)
   (:String)==>(:Boolean)
   (:Integer)==>(:String)==>(:Boolean)  // Composed
   ```

4. **Decomposition**: Break functions into parts
   ```gram
   // Original: (Integer)==>(String)==>(String)
   // Decompose into:
   (:Integer)==>(:String)  // First part
   (:String)==>(:String)   // Second part
   ```

**⚠️ Critical Constraint: Global Identifier Uniqueness**

Gram notation does **not** scope identifiers - all identifiers must be globally unique, even when nested inside containers.

**Problem**: Parameter names as identifiers must be globally unique:
```gram
// Function 1
(repetitions:Integer)==>(name:String)==>(:String)

// Function 2 - CONFLICT!
(repetitions:Integer)==>(count:Integer)==>(:String)
// ERROR: 'repetitions' already defined globally
```

**Solutions**:

**Option A: Use Anonymous Parameters with Type Labels**
```gram
// No parameter names, just types
(:Integer)==>(:String)==>(:String)
```
- ✅ No identifier conflicts
- ❌ Loses parameter names (needed for JSON Schema property names)

**Option B: Use Scoped Identifiers via Pattern Containment**
```gram
[:Function {name:"repeatHello"} |
  // Parameters scoped within function pattern
  (repetitions:Integer)==>(name:String)==>(:String)
]
```
- ⚠️ **Note**: Even inside patterns, identifiers are still global in gram
- Parameter names must still be unique across all functions

**Option C: Use Property Records for Parameter Names** (Recommended)
```gram
[:Function {name:"repeatHello"} |
  (:Integer {paramName:"repetitions"})==>(:String {paramName:"name"})==>(:String)
]
```
- ✅ Parameter names in properties (not identifiers)
- ✅ No global uniqueness constraint
- ✅ Names available for JSON Schema generation

**Option D: Use Qualified Identifiers**
```gram
[:Function {name:"repeatHello"} |
  (repeatHello_repetitions:Integer)==>(repeatHello_name:String)==>(:String)
]
```
- ✅ Globally unique via prefixing
- ❌ Verbose
- ❌ Requires naming convention

**Recommended: Option C (Property Records)**

Use property records to store parameter names, avoiding global identifier conflicts:

```gram
[:Function {name:"repeatHello", description:"Produce repeated greeting"} |
  (:Integer {paramName:"repetitions"})==>(:String {paramName:"name"})==>(:String)
]
```

**Mapping to JSON Schema**:
1. Extract parameter nodes from curried chain (all nodes before final return type)
2. Extract `paramName` from each node's properties
3. Group into object structure:
   ```json
   {
     "type": "object",
     "properties": {
       "repetitions": {"type": "integer"},
       "name": {"type": "string"}
     },
     "required": ["repetitions", "name"]
   }
   ```

**Alternative: Anonymous Types with Metadata Pattern**
```gram
[:Function {name:"repeatHello"} |
  [signature:CurriedSignature |
    (:Integer)==>(:String)==>(:String)
  ],
  [params:ParameterMapping |
    (param1:Mapping {position: 1, name: "repetitions", type: "Integer"}),
    (param2:Mapping {position: 2, name: "name", type: "String"})
  ]
]
```

This separates the graph structure (curried form) from parameter naming (mapping pattern).

### Curried Functions

**Note**: For multiple parameters, curried form creates a graph structure enabling composition and pattern matching. See "Curried Form for Multiple Parameters" section above for details and the global identifier uniqueness constraint.

**Haskell**: `Text -> Int -> IO Text`

**Gram Path Representation** (Verified):
```gram
(Text:ParameterType)-[func1]->(Int:IntermediateType)-[func2]->(String:ReturnType)
```

**Using Double Arrows** (Verified):
```gram
(Text)==>(Int)==>(String)
```

**Note**: In practice, curried functions would be represented as a single parameter object in JSON Schema: `{text: string, int: integer}`. The return type `String` represents the JSON Schema type - the Haskell implementation may be `IO Text`, but gram represents only the JSON Schema interface.

### Higher-Order Functions

**Haskell**: `(Text -> Int) -> IO Text`

**Gram Path Representation** (Verified):
```gram
// Inner function type as pattern
[innerFunc:FunctionType |
  (Text)-[maps_to]->(Int)
]

// Outer function
(innerFunc)==>(IO)
```

**Alternative: Nested Path** (Verified):
```gram
[outerFunc:FunctionType |
  (Text)-[inner]->(Int)
]
(outerFunc)==>(IO)
```

### JSON Schema Types

**JSON Schema**: `string`, `integer`, `number`, `boolean`, `object`, `array`

**Gram Path Representation** (Verified):

**Primitive Types**:
```gram
(Text:JSONType {schemaType: "string"})
(Int:JSONType {schemaType: "integer"})
(Double:JSONType {schemaType: "number"})
(Bool:JSONType {schemaType: "boolean"})
```

**Array Types** (Verified):
```gram
[Array:JSONType {schemaType: "array"} | Text]
```

**Object Types** (Verified):
```gram
[Object:JSONType {schemaType: "object"} |
  (name:Field {type: "Text"}),
  (age:Field {type: "Int"})
]
```

**Note**: `IO`, `Maybe` are Haskell implementation details, not part of JSON Schema. Gram represents only JSON Schema types. `Maybe T` in gram means optional (not in required list), not a type constructor.

### Object Types (Records)

**JSON Schema**: `{name: string, age: integer}`

**Gram Path Representation** (Verified):
```gram
[record:ObjectType {schemaType: "object"} |
  (name:Field {type: "Text"}),
  (age:Field {type: "Int"})
]
```

### Complex Example

**JSON Schema**: Parameter `{query: string, filters: {category?: string}}`, Return `string[]` (documentation)

**Gram Path Representation** (Verified):
```gram
// Nested object structure
[searchParams:Parameter |
  [filters:ObjectType {schemaType: "object"} |
    (category:Field {type: "Text", optional: true})  // Optional field
  ],
  (query:Field {type: "Text"})
]

// Function arrow to return type
(searchParams)==>(Array:ReturnType)-[:contains]->(Text:ElementType)
```

**Note**: The return type `IO [Text]` in Haskell becomes just `[Text]` (array of strings) in gram - `IO` is a Haskell implementation detail.

## Comparison: Text vs. Path Representation

### Text-Based (Current)
```gram
[toolSpec:ToolSpecification {
  typeSignature: "(name: Text) --> IO Text"
}]
```

**Pros**:
- Simple, compact
- Familiar to Haskell developers
- Easy to parse with existing parsers

**Cons**:
- Opaque (cannot query structure)
- Type information lost
- Cannot represent type relationships
- Hard to manipulate in gram

### Path-Based (Proposed, Verified)
```gram
[toolSpec:ToolSpecification |
  [func:FunctionType |
    (name:Parameter {type: "Text"})==>(IO)
  ]
]
```

Or using relationship identifier:
```gram
[toolSpec:ToolSpecification |
  [func:FunctionType |
    (name:Parameter {type: "Text"})-[maps_to]->(IO)
  ]
]
```

**Pros**:
- Structured (can query/manipulate)
- Type relationships explicit
- Can traverse type graph
- Integrates with gram's graph model

**Cons**:
- More verbose
- Requires type node definitions
- More complex parsing
- May be overkill for simple types

## Hybrid Approach: Structured Type Patterns

### Concept

Use gram patterns to represent type signatures, combining:
- **Pattern notation** for type structure (hierarchical)
- **Path notation** for function arrows (directional)
- **Property records** for type metadata

### Example: Function Type Pattern (Verified)

```gram
[funcType:FunctionType {
  signature: "(name: Text) --> IO Text"  // Text for compatibility
} |
  // Structured representation (JSON Schema types only)
  (name:Parameter {type: "Text"})-[maps_to]->(String:ReturnType)
]
```

**Note**: The `IO Text` return type in the text signature is documentation. The gram path represents only JSON Schema types. The actual Haskell implementation may be `Text -> IO Text`, but gram represents the JSON Schema interface.

**Benefits**:
- Text signature for compatibility/display
- Structured representation for querying
- Best of both worlds

## Path Notation Advantages for Type Signatures

### 1. Type Graph Traversal

**Query**: "Find all functions that return string types"
```gram
// Hypothetical query syntax (using verified syntax)
(?func:FunctionType)==>(?return:String:ReturnType)
```

### 2. Type Relationship Analysis

**Query**: "Find functions with Text (string) parameters"
```gram
(?func:FunctionType)<-[is_input_of]-(?param:Parameter {type: "Text"})
```

### 3. JSON Schema Type Composition

**Query**: "Find functions with object parameters"
```gram
(?func:FunctionType)<-[is_input_of]-(?param:Parameter)-[:has_type]->(?obj:ObjectType)
```

### 4. Type Unification

**Query**: "Find functions with matching parameter types"
```gram
(?func1:FunctionType)<-[is_input_of]-(?param:Parameter {type: ?type})
(?func2:FunctionType)<-[is_input_of]-(?param2:Parameter {type: ?type})
```

## Global Identifier Constraints in Gram

**Critical Constraint**: Gram notation does **not** scope identifiers. All identifiers must be globally unique, even when nested inside patterns or containers.

### Impact on Function Signatures

When using curried form with named parameters:
```gram
(repetitions:Integer)==>(name:String)==>(:String)
```

The identifiers `repetitions` and `name` are **global** - they must be unique across the entire gram document, not just within the function.

### Solutions

1. **Property Records** (Recommended): Store parameter names in properties, not as identifiers
   ```gram
   (:Integer {paramName:"repetitions"})==>(:String {paramName:"name"})==>(:String)
   ```
   - ✅ No global uniqueness constraint
   - ✅ Parameter names available for JSON Schema
   - ✅ Maintains graph structure benefits

2. **Qualified Identifiers**: Prefix parameter names with function name
   ```gram
   (repeatHello_repetitions:Integer)==>(repeatHello_name:String)==>(:String)
   ```
   - ✅ Globally unique via prefixing
   - ❌ Verbose
   - ❌ Requires naming convention

3. **Anonymous Parameters**: Use only type labels, store names separately
   ```gram
   (:Integer)==>(:String)==>(:String)
   // Parameter names in separate mapping pattern
   ```
   - ✅ No identifier conflicts
   - ❌ Separates graph structure from naming

4. **Pattern Metadata**: Store parameter mapping in pattern properties
   ```gram
   [:Function {
     name:"repeatHello",
     params: [
       {position: 1, name: "repetitions", type: "Integer"},
       {position: 2, name: "name", type: "String"}
     ]
   } | (:Integer)==>(:String)==>(:String)]
   ```
   - ✅ Names in properties (not identifiers)
   - ✅ Maintains graph structure
   - ⚠️ Requires parsing properties for JSON Schema generation

### Recommendation

Use **property records** for parameter names to avoid global uniqueness constraints while maintaining the graph structure benefits of curried form:

```gram
[:Function {name:"repeatHello", description:"Produce repeated greeting"} |
  (:Integer {paramName:"repetitions"})==>(:String {paramName:"name"})==>(:String)
]
```

This approach:
- ✅ Avoids global identifier conflicts
- ✅ Maintains graph structure (enables composition, pattern matching)
- ✅ Provides parameter names for JSON Schema generation
- ✅ Keeps syntax relatively clean

## The Three Mappings

### 1. Gram → Haskell (Parsing and Binding)

**Purpose**: Parse gram signature, bind to Haskell implementation

**Process**:
1. Parse gram path representation to extract parameter types and return type
2. Map JSON Schema types to Haskell types:
   - `Text` → `Text` or `String`
   - `Int` → `Int` or `Integer`
   - `Double` → `Double` or `Float`
   - `Bool` → `Bool`
   - `{...}` → Record type
   - `[T]` → `[T]`
3. Bind to Haskell function with appropriate signature (may include IO, type constructors, etc.)

**Example**:
```gram
(name:Parameter {type: "Text"})==>(String:ReturnType)
```
→ Binds to: `sayHello :: Text -> IO Text`

**Note**: Haskell implementation may have `IO`, type constructors, etc. - these are implementation details, not in gram representation.

### 2. Gram → JSON Schema (LLM Tool Calling)

**Purpose**: Convert gram signature to JSON Schema for LLM

**Process**:
1. Extract parameter types from gram path
2. Convert gram types to JSON Schema types:
   - `Text` → `"type": "string"`
   - `Int` → `"type": "integer"`
   - `Double` → `"type": "number"`
   - `Bool` → `"type": "boolean"`
   - `{field1: T1, ...}` → `"type": "object", "properties": {...}`
   - `[T]` → `"type": "array", "items": {...}`
3. Generate required fields (non-optional parameters)
4. Return type is ignored (not in JSON Schema)

**Example**:
```gram
(name:Parameter {type: "Text"})==>(String:ReturnType)
```
→ JSON Schema:
```json
{
  "type": "object",
  "properties": {
    "name": {"type": "string"}
  },
  "required": ["name"]
}
```

### 3. Haskell → Gram (Optional, Constrained)

**Purpose**: Convert Haskell function signature to gram (if expressible in JSON Schema)

**Process**:
1. Extract parameter types from Haskell signature
2. Check if types are JSON Schema expressible:
   - ✅ `Text`, `String` → `Text`
   - ✅ `Int`, `Integer` → `Int`
   - ✅ `Double`, `Float` → `Double`
   - ✅ `Bool` → `Bool`
   - ✅ Record types → `{...}`
   - ✅ List types → `[T]`
   - ❌ `IO T` → Extract `T`, ignore `IO`
   - ❌ `Maybe T` → `T` (mark as optional)
   - ❌ Type constructors → Extract inner type
3. Generate gram path representation
4. Return type is documentation only

**Example**:
```haskell
sayHello :: Text -> IO Text
```
→ Gram:
```gram
(Text:ParameterType)==>(Text:ReturnType)
```

**Constraint**: Only functions expressible in JSON Schema can be converted. Functions with unsupported types (e.g., functions as parameters) cannot be represented.

## Implementation Considerations

### JSON Schema Type Node Definitions

Need to define standard JSON Schema type nodes:
```gram
(Text:JSONType {schemaType: "string"})
(Int:JSONType {schemaType: "integer"})
(Double:JSONType {schemaType: "number"})
(Bool:JSONType {schemaType: "boolean"})
(Object:JSONType {schemaType: "object"})
(Array:JSONType {schemaType: "array"})
```

### Function Arrow Relationship

Standard relationship type for function arrows:
```gram
==>  // Function type arrow (double arrow, used by convention for clarity)
-[maps_to]->  // Explicit mapping relationship
-[:FunctionType]->  // Function type relationship
-[:has_type]->  // Field/parameter has type
-[:is_input_of]->  // Parameter is input of function
```

**Note**: Gram treats all arrow types (`==>`, `-->`, `~~>`, etc.) as semantically equivalent - they are decorative. We use `==>` by convention for clarity in function type signatures, but any valid gram relationship arrow would work.

### Parsing Strategy

**From Text to Path**:
1. Parse text signature (existing parser)
2. Convert to structured representation
3. Generate gram path pattern

**From Path to Text**:
1. Traverse path pattern
2. Extract type information
3. Generate text signature

### Serialization Format

**Option A: Path-Only**
```gram
[toolSpec:ToolSpecification |
  (name:Parameter {type: "Text"})-[:->]->(IO Text)
]
```

**Option B: Hybrid (Recommended)**
```gram
[toolSpec:ToolSpecification {
  typeSignature: "(name: Text) --> IO Text"  // Text for compatibility
} |
  // Structured representation (optional, JSON Schema types only)
  (name:Parameter {type: "Text"})==>(String:ReturnType)
]
```

**Note**: The text signature may include `IO Text` for documentation, but the gram path representation uses only JSON Schema types (`String`). The `IO` is a Haskell implementation detail.

## JSON Schema Constraints

**Note**: JSON Schema doesn't support type variables, type classes, or polymorphism. Gram representations are constrained to concrete JSON Schema types.

### Optional Parameters

**JSON Schema**: `{name: string, age?: integer}` (age is optional)

**Gram Path Representation** (Verified):
```gram
[params:ParameterList |
  (name:Parameter {type: "Text", required: true}),
  (age:Parameter {type: "Int", required: false})  // Optional
]
```

Or using `Maybe` notation (Haskell convention):
```gram
[params:ParameterList |
  (name:Parameter {type: "Text"}),
  (age:Parameter {type: "Maybe Int"})  // Maybe indicates optional
]
```

**Note**: `Maybe T` in gram means optional (not in required list), not a type constructor. The actual JSON Schema type is just `T`.

## Use Cases

### 1. Type Querying

Find all tools that accept Text parameters:
```gram
(?tool:ToolSpecification)<-[:has_type]-(?param:Parameter {type: "Text"})
```

### 2. Type Compatibility

Check if function types are compatible:
```gram
(?func1:FunctionType)-[:->]->(?return1:Type)
(?func2:FunctionType)<-[:is_input_of]-(?param:Parameter {type: ?return1})
```

### 3. Type Inference

Infer return types from parameter types:
```gram
(?func:FunctionType)<-[:is_input_of]-(?param:Parameter {type: ?inputType})
(?func)-[:->]->(?returnType:Type)
// Infer: returnType based on inputType
```

### 4. Type Documentation

Generate type documentation from gram patterns:
```gram
[funcType:FunctionType {
  description: "Maps Text to String (returns Text in Haskell)"
} |
  (Text:ParameterType)==>(String:ReturnType)
]
```

**Note**: Documentation can mention Haskell return types, but gram representation uses JSON Schema types.

## Challenges and Limitations

### 1. Verbosity

Path notation is more verbose than text signatures:
- Text: `"(name: Text) --> IO Text"` (1 line)
- Path: Multiple lines with nodes and relationships

### 2. Type Node Definitions

Need to define JSON Schema type nodes:
- Basic types (Text, Int, Double, Bool) - map to JSON Schema primitives
- Object types - map to JSON Schema objects
- Array types - map to JSON Schema arrays
- Optional parameters - marked with `required: false` or `Maybe` notation

**Note**: No need for type constructors (IO, Maybe as constructors), type variables, or type classes - these are Haskell implementation details, not JSON Schema concepts.

### 3. Parsing Complexity

Converting between text and path representations requires:
- Text parser (existing)
- Path generator (new)
- Path parser (new)
- Round-trip validation

### 4. Compatibility

Existing code expects text signatures:
- Need backward compatibility
- Migration path for existing data
- Dual representation (text + path)

## Recommendations

### Short Term: Keep Text, Add Optional Path

**Hybrid Approach**:
```gram
[toolSpec:ToolSpecification {
  typeSignature: "(name: Text) --> IO Text"  // Required: text for compatibility
} |
  // Optional: structured representation (JSON Schema types only)
  (name:Parameter {type: "Text"})==>(String:ReturnType)
]
```

**Benefits**:
- Backward compatible
- Optional enhancement
- Can query when available
- Falls back to text parsing

### Long Term: Full Path Representation

If path representation proves valuable:
1. Make path representation primary
2. Generate text from path (for display)
3. Support both for transition period
4. Eventually deprecate text-only

### Use Cases for Path Representation

**When to use path representation**:
- Type querying and analysis
- Type compatibility checking
- Type inference systems
- Complex type relationships
- Type documentation generation

**When text is sufficient**:
- Simple type signatures
- Display/formatting
- Human readability
- Quick type checking
- Basic validation

## Example: Complete Tool Specification with Path Type

```gram
[toolSpec:ToolSpecification {
  name: "sayHello",
  description: "Returns a friendly greeting",
  typeSignature: "(name: Text) --> IO Text"  // Text for compatibility (IO Text is Haskell doc)
} |
  // Structured type representation (verified, JSON Schema types only)
  [funcType:FunctionType |
    (name:Parameter {type: "Text"})==>(String:ReturnType)
  ]
]
```

Or with explicit relationship:
```gram
[toolSpec:ToolSpecification {
  name: "sayHello",
  description: "Returns a friendly greeting",
  typeSignature: "(name: Text) --> IO Text"  // Text signature (IO Text is Haskell doc)
} |
  (name:Parameter {type: "Text"})-[maps_to]->(String:ReturnType)
]
```

**Note**: The text signature `"(name: Text) --> IO Text"` includes `IO Text` for documentation (Haskell return type). The gram path representation uses only JSON Schema types (`String`). The actual Haskell implementation is `Text -> IO Text`, but gram represents the JSON Schema interface.

## Verified Syntax Patterns

Based on testing with `gram-lint`, the following syntax patterns are valid for function signatures:

### Function Type Syntax
- `(A)==>(B)` - Double arrow (used by convention for clarity; gram treats all arrow types as equivalent)
- `(A)-[func]->(B)` - Relationship with identifier
- `(A)-[:FunctionType]->(B)` - Anonymous relationship with label
- `(A)-[func:FunctionType]->(B)` - Relationship with identifier and label

**Note**: Arrow types (`==>`, `-->`, `~~>`, etc.) are decorative in gram - they have no enforced semantics. We use `==>` by convention for clarity.

### JSON Schema Type Syntax
- `[Object:JSONType {schemaType: "object"} | fields]` - Pattern notation for objects
- `[Array:JSONType {schemaType: "array"} | elementType]` - Pattern notation for arrays
- `(Text:JSONType {schemaType: "string"})` - Primitive types
- `` (`IO Text`:Type) `` - Backtick-delimited identifier (for documentation, not JSON Schema)

**Note**: Type constructors like `IO`, `Maybe` are Haskell implementation details, not JSON Schema types. Gram represents only JSON Schema types (string, integer, number, boolean, object, array).

### Invalid Syntax for Function Types
- `[:->]` - Cannot use colon-prefixed arrow as relationship label

## Conclusion

**Path notation for function signatures offers**:
- Structured type representation (JSON Schema types only)
- Queryable type graphs
- Type relationship analysis
- Integration with gram's graph model
- Clear separation: JSON Schema interface vs. Haskell implementation

**But requires**:
- More verbose syntax
- JSON Schema type node definitions
- Parsing infrastructure for three mappings
- Backward compatibility strategy

**Key Constraint**: Gram representations must be expressible in JSON Schema. Haskell implementation details (IO, type constructors, etc.) are not part of the gram representation.

**The Three Mappings**:
1. **Gram → Haskell**: Parse gram, bind to Haskell implementation (may have IO, etc.)
2. **Gram → JSON Schema**: Convert gram to JSON Schema for LLM tool calling
3. **Haskell → Gram** (optional): Convert Haskell to gram, but only if JSON Schema expressible

**Verified Syntax**:
- Double arrow `==>` works for simple function types (used by convention; gram treats all arrow types as equivalent)
- Relationship identifiers work: `(A)-[func]->(B)`
- Pattern notation works for objects/arrays: `[Object:JSONType | fields]`
- Pattern notation can contain paths: `[funcType | (Text)==>(String)]`

**Note on Arrow Types**: Gram does not distinguish between `==>`, `-->`, `~~>`, etc. - they are decorative and semantically equivalent. We use `==>` by convention for clarity in function type signatures.

**Recommendation**: Start with hybrid approach (text + optional path), evaluate benefits, then decide on full migration. Focus on JSON Schema types, not Haskell implementation details.

