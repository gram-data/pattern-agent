# Data Model: Foundation & Setup

**Feature**: Foundation & Setup  
**Date**: 2025-01-27

## Entities

### PatternAgent

**Type**: `type PatternAgent = Pattern Agent`

**Description**: The core type representing an agent system. Can be atomic (single agent) or compound (multi-agent system).

**Structure**:
- **Value (V)**: `Agent` type containing all information defining agent capability (prompt, tools, constraints)
- **Elements**: `[PatternAgent]` - sub-agents in a decomposition (empty for atomic patterns)
- **DAG Structure**: Implicit in the recursive `Pattern` structure - elements form the DAG relationships

**Properties**:
- Atomic pattern: `elements == []` (no sub-agents)
- Compound pattern: `elements /= []` (contains sub-agents)
- Recursive: Elements are themselves `PatternAgent` instances, enabling arbitrary nesting

**Validation Rules**:
- All elements must be valid `PatternAgent` instances
- DAG structure must be acyclic (no circular references in element chains)
- Value type `Agent` must be defined (deferred to future feature)

**State Transitions**: N/A (immutable data structure)

### Agent (Value Type)

**Type**: To be defined in future feature (execution feature)

**Description**: The value type that decorates a Pattern, containing agent capability information.

**Expected Structure** (conceptual, to be implemented later):
- **prompt**: System prompt for the agent
- **tools**: Available tools/functions the agent can use
- **constraints**: Behavioral constraints or parameters

**Note**: This entity is referenced but not implemented in this foundation feature. The foundation establishes the `PatternAgent` type that will hold `Agent` values once defined.

### Pattern (from gram-hs)

**Type**: `Pattern v` (from `Pattern` module)

**Description**: Base Pattern type from gram-hs that provides the recursive structure.

**Structure**:
- `value :: Pattern v -> v` - accessor for decoration value
- `elements :: Pattern v -> [Pattern v]` - accessor for sub-patterns

**Operations Available**:
- Construction: `pattern`, `patternWith`, `fromList`
- Query: `length`, `size`, `depth`, `values`
- Typeclass instances: Functor, Applicative, Comonad, Foldable, Traversable, etc.

**Relationship to PatternAgent**:
- `PatternAgent` is a type alias: `type PatternAgent = Pattern Agent`
- All Pattern operations work directly on PatternAgent
- Pattern typeclass instances are available for PatternAgent

## Relationships

- **PatternAgent** uses **Pattern** (type alias relationship)
- **PatternAgent** contains **Agent** (value decoration)
- **PatternAgent** contains **PatternAgent** (recursive, elements relationship)
- **PatternAgent** elements form DAG structure (implicit in recursive structure)

## Type Definitions

```haskell
-- Core type alias
type PatternAgent = Pattern Agent

-- Agent type (placeholder, to be defined in execution feature)
-- For now, we establish the type structure without full Agent definition
data Agent = Agent
  { prompt :: String  -- Placeholder, actual structure TBD
  , tools :: [String]  -- Placeholder, actual structure TBD
  , constraints :: [String]  -- Placeholder, actual structure TBD
  }
  deriving (Eq, Show)
```

**Note**: The `Agent` type definition above is a placeholder to enable type checking. The actual Agent structure will be defined in the execution feature. For foundation, we establish that PatternAgent = Pattern Agent and provide a minimal Agent definition.

## Module Structure

- `PatternAgent.Core` - Core type definitions and basic operations
- `PatternAgent.Types` - Type aliases and supporting types

## Notes

- PatternAgent leverages Pattern's existing DAG representation through recursive structure
- Atomic patterns (empty elements) represent single agents
- Compound patterns (non-empty elements) represent multi-agent systems
- DAG relationships are implicit in the element list ordering and nesting
- Future features will add operations for DAG traversal, cycle detection, etc.

