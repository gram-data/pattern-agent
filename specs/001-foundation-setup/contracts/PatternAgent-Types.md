# Module Contract: PatternAgent.Types

**Module**: `PatternAgent.Types`  
**Purpose**: Type aliases and supporting type definitions  
**Feature**: Foundation & Setup

## Exports

### Type Aliases

```haskell
-- Main type alias
type PatternAgent = Pattern Agent
```

### Supporting Types

```haskell
-- Agent value type (minimal definition for foundation)
-- Full definition deferred to execution feature
data Agent = Agent
  { prompt :: String
  , tools :: [String]
  , constraints :: [String]
  }
  deriving (Eq, Show, Ord, Hashable)
```

## Implementation Notes

- Agent type is a placeholder to enable type checking
- Actual Agent structure will be defined in execution feature
- PatternAgent is the single, canonical type alias for Pattern Agent
- Agent derives standard typeclasses for use in Pattern operations

## Dependencies

- `Pattern` module from `pattern` package (gram-hs)
- Standard Haskell typeclasses

## Testing Requirements

- Unit tests for Agent construction
- Unit tests for Agent typeclass instances (Eq, Show, Ord, Hashable)
- Scenario tests demonstrating Agent usage in PatternAgent

