# Research: Foundation & Setup

**Feature**: Foundation & Setup  
**Date**: 2025-01-27  
**Purpose**: Resolve technical unknowns identified in Phase 0 planning

## Research Questions

### 1. gram-hs Pattern Abstraction API

**Question**: What are the exact module names and API surface of gram-hs Pattern abstraction?

**Research Approach**: 
- Examine gram-hs source code structure
- Identify Pattern type definition and operations
- Document module exports and type signatures

**Findings**:
- **Decision**: Use `Pattern` module from `pattern` package (gram-hs/libs/pattern)
- **Module**: `Pattern` (re-exports from `Pattern.Core` and `Pattern.Graph`)
- **Type**: `Pattern v` where `v` is the value type parameter
- **Structure**: 
  - `value :: Pattern v -> v` - accessor for decoration value (V in our terminology)
  - `elements :: Pattern v -> [Pattern v]` - accessor for sub-patterns (Elements in our terminology)
- **Key Operations**:
  - `pattern :: v -> Pattern v` - construct atomic pattern
  - `patternWith :: v -> [Pattern v] -> Pattern v` - construct pattern with elements
  - `fromList :: [Pattern v] -> Pattern v` - construct from list
  - Query functions: `length`, `size`, `depth`, `values`
  - Typeclass instances: Functor, Applicative, Comonad, Foldable, Traversable, etc.
- **Rationale**: 
  - Pattern type matches our needs: value (V) + elements (sub-patterns)
  - Recursive structure supports DAG representation
  - Rich typeclass instances enable transformations
- **Dependency**: `pattern` package from `../gram-hs/libs/pattern` (local path dependency)
- **Package Name**: `pattern` (not `gram-hs`, it's a sub-library)

### 2. Haskell Testing Framework

**Question**: Which Haskell testing framework should be used (HUnit, QuickCheck, Tasty, etc.)?

**Research Approach**:
- Evaluate testing framework options for Haskell
- Consider requirements: unit tests, scenario tests, property-based testing
- Check industry standards and best practices

**Findings**:
- **Decision**: Use Tasty test framework with HUnit for unit tests and QuickCheck for property-based tests
- **Rationale**: 
  - Tasty provides unified test runner and organization
  - HUnit is standard for unit testing in Haskell
  - QuickCheck enables property-based testing for correctness verification
  - Tasty integrates both frameworks seamlessly
  - Industry standard for Haskell projects
- **Alternatives Considered**:
  - HUnit alone - rejected (lacks test organization features)
  - QuickCheck alone - rejected (better for properties than unit tests)
  - Hedgehog - rejected (less common, Tasty more standard)
- **Dependencies**: `tasty`, `tasty-hunit`, `tasty-quickcheck`

### 3. Pattern<Agent> Type Implementation

**Question**: Should Pattern<Agent> be a type alias or a newtype wrapper around Pattern from gram-hs?

**Research Approach**:
- Consider type safety vs flexibility tradeoffs
- Evaluate pattern matching and API clarity
- Consider future extensibility needs

**Findings**:
- **Decision**: Start with type alias, evolve to newtype if needed
- **Rationale**:
  - Type alias provides maximum flexibility and direct access to Pattern operations
  - Can be upgraded to newtype later if additional invariants or operations needed
  - Simpler initial implementation aligns with incremental development approach
  - Type alias allows Pattern operations to work directly
- **Alternatives Considered**:
  - Newtype from start - rejected (premature abstraction, adds complexity)
  - Completely new type - rejected (defeats purpose of using gram-hs Pattern)
- **Implementation**: `type PatternAgent = Pattern Agent` where `Agent` is the agent value type
- **Module Structure**: `PatternAgent.Core` module exporting `PatternAgent` type alias

## Resolved Technical Context

After research, the following clarifications are resolved:

- **Testing Framework**: Tasty with HUnit and QuickCheck
- **Pattern<Agent> Implementation**: Type alias (initially), can evolve to newtype
- **gram-hs Integration**: Requires examination of actual gram-hs codebase (action item)

## Resolved Technical Decisions

1. **gram-hs Integration**: 
   - Package: `pattern` from `../gram-hs/libs/pattern`
   - Module: `Pattern` (re-exports core functionality)
   - Type: `Pattern v` where `v` is value type
   - For Pattern<Agent>: `type PatternAgent = Pattern Agent`

2. **Compatibility**: 
   - Pattern library requires `base >=4.17.0.0 && <5` (compatible with base 4.20.2.0)
   - Uses Haskell2010 (compatible with GHC2024)
   - Cabal build system supported

## Notes

- Research assumes gram-hs follows standard Haskell module conventions
- Testing framework decision aligns with Principle 3 (Dual Testing Strategy)
- Type alias approach supports Principle 4 (Expressiveness) by keeping API simple

