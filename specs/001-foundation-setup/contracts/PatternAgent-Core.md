# Module Contract: PatternAgent.Core

**Module**: `PatternAgent.Core`  
**Purpose**: Core PatternAgent type definitions and basic operations  
**Feature**: Foundation & Setup

## Exports

### Types

```haskell
-- Core type alias for Pattern<Agent>
type PatternAgent = Pattern Agent
```

### Type Accessors

```haskell
-- Access the agent value (V component)
value :: PatternAgent -> Agent

-- Access the sub-agents (Elements component)
elements :: PatternAgent -> [PatternAgent]
```

### Construction Functions

```haskell
-- Construct an atomic PatternAgent (no sub-agents)
atomicPattern :: Agent -> PatternAgent

-- Construct a compound PatternAgent with sub-agents
compoundPattern :: Agent -> [PatternAgent] -> PatternAgent

-- Construct from a list of sub-patterns
fromPatternList :: [PatternAgent] -> PatternAgent
```

### Query Functions

```haskell
-- Check if pattern is atomic (no sub-agents)
isAtomic :: PatternAgent -> Bool

-- Check if pattern is compound (has sub-agents)
isCompound :: PatternAgent -> Bool

-- Get the number of direct sub-agents
patternLength :: PatternAgent -> Int

-- Get total size including all nested patterns
patternSize :: PatternAgent -> Int

-- Get maximum nesting depth
patternDepth :: PatternAgent -> Int
```

### Validation Functions

```haskell
-- Validate that pattern structure is well-formed
validatePattern :: PatternAgent -> Bool

-- Check for circular references (DAG validation)
isAcyclic :: PatternAgent -> Bool
```

## Implementation Notes

- All functions delegate to underlying `Pattern` operations from gram-hs
- Type alias enables direct use of Pattern typeclass instances (Functor, Applicative, etc.)
- Validation functions ensure DAG structure integrity
- Construction functions provide convenient API over Pattern constructors

## Dependencies

- `Pattern` module from `pattern` package (gram-hs)
- `Agent` type (minimal definition for foundation)

## Testing Requirements

- Unit tests for all construction functions
- Unit tests for all query functions
- Unit tests for validation functions
- Scenario tests demonstrating atomic and compound pattern creation
- Scenario tests demonstrating DAG structure access

