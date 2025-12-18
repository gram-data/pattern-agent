# Gram Notation Reference

**Purpose**: Brief reference guide to gram notation syntax and semantics for use in pattern-agent project.

**Source**: Based on gram-hs repository (`../gram-hs`) and gram notation specification.

## Overview

Gram notation is a graph-oriented serialization format that represents graph structures using patterns. It supports two complementary syntaxes:

- **Pattern notation**: Declarative nested structures using `[...]`
- **Path notation**: Sequential graph traversals using `(nodes)` and relationships

## Core Concepts

### Patterns

Patterns are the fundamental building blocks. A pattern has:
- **Identity**: Optional identifier (symbol, quoted string, or number)
- **Labels**: Zero or more type classifications (prefixed with `:`)
- **Properties**: Key-value record (in curly braces `{...}`)
- **Elements**: Zero or more nested patterns

### Subject

A Subject is a self-descriptive object containing:
- **Identity**: Symbol identifier (required in Haskell, optional in gram)
- **Labels**: Set of strings (can be empty or contain multiple labels)
- **Properties**: Map of key-value pairs (PropertyRecord)

## Identifiers

### Plain Identifiers
- Must start with a letter or underscore
- Can contain letters, numbers, underscores, and some special characters
- **Cannot contain spaces** (use backticks for spaces)

**Examples**:
```
a, myId, _private, node123
```

### Backtick-Delimited Identifiers
- Use backticks to include spaces and special characters
- Syntax: `` `identifier with spaces` ``
- Works in all contexts (node identifiers, relationship identifiers, pattern identifiers)

**Examples**:
```
(`IO Text`:Type)                    // Node with spaces in identifier
(`this is ok`:Example)              // Any identifier with spaces
[`IO Text`:TypeConstructor | Text]  // Pattern with backtick identifier
```

## Syntax

### Pattern Notation

```
[identifier:Label1:Label2 {key1: value1, key2: value2} | element1, element2]
```

**Components**:
- `identifier` - Optional identifier (symbol, quoted string, or number)
- `:Label1:Label2` - Zero or more labels (prefixed with `:`)
- `{key1: value1}` - Optional property record
- `| element1, element2` - Optional list of pattern elements

**Examples**:
```
[a]                                    // Atomic pattern with identity 'a'
[a:Person]                             // Pattern with label
[a:Person {name: "Alice"}]             // Pattern with label and properties
[b | a]                                // Pattern 'b' containing element 'a'
[b | [a], [c]]                         // Pattern 'b' with two anonymous elements
```

### Path Notation

```
(node)-[relationship]->(node)
```

**Components**:
- `(node)` - Node with optional identity, labels, and properties
- `-[relationship]->` - Relationship with optional identity, labels, and properties
- Direction: `-->` (forward), `<--` (reverse), `--` (undirected)

**Relationship Syntax**:
- `(A)==>(B)` - Double arrow (simplest)
- `(A)-[rel]->(B)` - Relationship with identifier
- `(A)-[:Label]->(B)` - Anonymous relationship with label
- `(A)-[rel:Label]->(B)` - Relationship with identifier and label
- `(A)<-[rel]-(B)` - Reverse direction
- `(A)-[rel]-(B)` - Undirected

**Examples**:
```
(a:Person {name: "Alice"})             // Node
(a)-[:knows]->(b)                       // Anonymous relationship
(a)-[r:knows {since: 2024}]->(b)       // Named relationship with properties
(Text)==>(IO)                           // Double arrow
(Text)-[func]->(IO)                     // Relationship with identifier
(Text)-[:FunctionType]->(IO)            // Relationship with label
```

### Definition Rules

**Key Rule**: Brackets create definitions, bare identifiers create references

```
[a]                     // Defines pattern 'a'
[a {k:"v"}]            // Defines 'a' with properties
[b | a]                // Defines 'b', references 'a'
[b | [a]]              // Defines both 'b' and 'a'
```

**Constraints**:
- Each identified pattern can only be defined once
- Patterns are immutable once defined
- Forward references are allowed
- No direct self-reference (indirect is OK)

## Values

Gram supports rich value types:

### Standard Types
- **Integers**: `42`, `-10`
- **Decimals**: `3.14`, `-0.5`
- **Booleans**: `true`, `false`
- **Strings**: `"hello"`, `'world'`
- **Symbols**: Unquoted identifiers or backticked strings

### Extended Types
- **Tagged strings**: ``url`https://example.com` ``
- **Arrays**: `[1, 2, 3]`
- **Maps**: `{key1: "value1", key2: 42}`
- **Ranges**: `1..10`, `1...`, `...10`
- **Measurements**: `100km`, `5.5kg`

## Labels

Labels are type classifications:
- Prefixed with `:` or `::`
- Multiple labels per pattern: `:Person:Employee`
- Treated as a set (no duplicates, order doesn't matter)
- Can be empty (no labels)

**Examples**:
```
(a:Person)                             // Single label
(a:Person:Employee)                    // Multiple labels
(a)                                    // No labels
```

## Property Records

Property records store structured data:
- Syntax: `{key1: value1, key2: value2}`
- Keys are identifiers (strings)
- Values can be any gram value type
- Can be nested (maps contain maps/arrays)
- Can be empty (no properties)

**Examples**:
```
{name: "Alice", age: 30}               // Simple properties
{metadata: {created: 2024, tags: ["a", "b"]}}  // Nested structure
{}                                     // Empty properties
```

## Pattern vs Path Notation

### When to Use Pattern Notation
- Hierarchical/nested structures
- Declarative definitions
- Complex relationships
- Reusable pattern definitions

### When to Use Path Notation
- Sequential graph traversals
- Linear relationships
- Natural graph-like syntax
- Cypher-like familiarity

### Mixing Notations
Both can be mixed with consistency rules:
```
[team | alice, bob]                   // Pattern notation
(team)-[:works_on]->(project)          // Path notation using pattern as node
```

## Relationship Syntax Details

### Arrow Types
- `-->` - Right arrow (forward)
- `<--` - Left arrow (reverse)
- `--` - Undirected arrow
- `==>` - Double right arrow
- `<==` - Double left arrow
- `<==>` - Bidirectional arrow

### Relationship Components
- **Identifier**: Optional name for the relationship (e.g., `[r:knows]`)
- **Labels**: Optional type classification (e.g., `[:knows]` or `[r:knows]`)
- **Properties**: Optional key-value record (e.g., `[r:knows {since: 2024}]`)

### Chained Relationships
Relationships can be chained:
```
(a)-[r1]->(b)-[r2]->(c)  // Valid chain
```

### Patterns Containing Paths
Pattern notation can contain path notation as elements:
```
[funcType:FunctionType | (Text)==>(IO)]  // Pattern with path element
```

## Limitations

### Property Records
- Cannot contain predicates (e.g., `{n > 1}` is invalid)
- Keys must be identifiers (strings or backtick-delimited)
- Values must be valid gram value types

### Pattern Structure
- Anonymous patterns are always unique (even if structurally identical)
- Identified patterns must be defined exactly once
- Patterns cannot directly contain themselves

### Identifiers
- Plain identifiers cannot contain spaces
- Use backtick-delimited identifiers for spaces: `` `IO Text` ``
- Relationship labels cannot use colon-prefixed arrows like `[:->]`

## Integration with Haskell

In gram-hs, patterns are represented as:

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration (typically Subject)
  , elements :: [Pattern v]    -- Nested patterns
  }

data Subject = Subject
  { identity   :: Symbol       -- Required identifier
  , labels     :: Set String   -- Set of labels
  , properties :: PropertyRecord  -- Map String Value
  }
```

**Key Mapping**:
- Pattern identity → Subject identity
- Pattern labels → Subject labels (Set)
- Pattern properties → Subject properties (Map)
- Pattern elements → Pattern elements list

## References

- gram-hs repository: `../gram-hs`
- Design documentation: `../gram-hs/design/DESIGN.md`
- Semantics: `../gram-hs/design/SEMANTICS.md`
- Extended semantics: `../gram-hs/design/EXTENDED-SEMANTICS.md`
- Syntax notes: `../gram-hs/libs/gram/SYNTAX_NOTES.md`

