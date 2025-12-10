# Quickstart: Foundation & Setup

**Feature**: Foundation & Setup  
**Purpose**: Verify foundation setup is complete and working

## Prerequisites

- Haskell development environment (GHC 2024, Cabal)
- Access to gram-hs repository at `../gram-hs`

## Setup Steps

### 1. Verify Project Structure

```bash
# Check that project structure exists
ls -la src/PatternAgent/
ls -la test/unit/
ls -la test/scenario/
ls -la docs/
```

### 2. Verify Dependencies

```bash
# Check that pattern dependency is configured
grep -A 5 "build-depends" pattern-agent.cabal | grep pattern

# Verify pattern package is accessible
cd ../gram-hs/libs/pattern && cabal build
```

### 3. Build the Project

```bash
# Build the library
cabal build

# Verify build succeeds without errors
echo $?  # Should be 0
```

### 4. Verify Pattern Integration

```haskell
-- In GHCi
:set -XNoImplicitPrelude
import Pattern
import PatternAgent.Core
import PatternAgent.Types

-- Create a simple atomic pattern
let agent = Agent "test prompt" [] []
let atomic = atomicPattern agent

-- Verify value access
value atomic  -- Should return the Agent

-- Verify elements access
elements atomic  -- Should return []
```

### 5. Verify Type System

```haskell
-- In GHCi
:set -XNoImplicitPrelude
import PatternAgent.Core
import PatternAgent.Types

-- Verify PatternAgent type
:t atomicPattern  -- Should show: Agent -> PatternAgent

-- Verify compound pattern
let agent1 = Agent "agent1" [] []
let agent2 = Agent "agent2" [] []
let compound = compoundPattern agent1 [atomicPattern agent2]

-- Verify structure
isAtomic compound  -- Should return False
isCompound compound  -- Should return True
patternLength compound  -- Should return 1
```

### 6. Run Tests

```bash
# Run unit tests
cabal test --test-show-details=direct

# Verify all tests pass
echo $?  # Should be 0
```

### 7. Verify Documentation

```bash
# Check that core concepts documentation exists
cat docs/core-concepts.md

# Verify it covers V, Elements, and DAG concepts
grep -i "V\|Elements\|DAG" docs/core-concepts.md
```

## Success Criteria

All of the following must pass:

- [ ] Project builds successfully (`cabal build` exits with code 0)
- [ ] All dependencies resolve (`cabal build` completes without dependency errors)
- [ ] Pattern module can be imported (`import Pattern` works in GHCi)
- [ ] PatternAgent type is defined and accessible
- [ ] Atomic patterns can be created and accessed
- [ ] Compound patterns can be created and accessed
- [ ] All unit tests pass
- [ ] All scenario tests pass
- [ ] Documentation exists and covers core concepts

## Troubleshooting

### Build Fails

- Check that `pattern` dependency path is correct in `pattern-agent.cabal`
- Verify `../gram-hs/libs/pattern` exists and builds
- Check GHC and Cabal versions are compatible

### Import Errors

- Verify `Pattern` module is exported from `pattern` package
- Check that `PatternAgent.Core` and `PatternAgent.Types` modules exist
- Verify module exports match contracts

### Type Errors

- Verify `Agent` type is defined in `PatternAgent.Types`
- Check that `PatternAgent` type alias is correct
- Verify Pattern type from gram-hs is compatible

## Next Steps

After foundation setup is complete:

1. Proceed to execution feature (Feature 2) to define full Agent type
2. Implement execution environment for running agents
3. Add decomposition and composition features

