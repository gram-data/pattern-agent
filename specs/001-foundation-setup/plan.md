# Implementation Plan: Foundation & Setup

**Branch**: `001-foundation-setup` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-foundation-setup/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Establish foundational structure for the pattern-agent framework by setting up project structure with proper dependencies, integrating gram-hs Pattern abstraction, defining the core `Pattern<Agent>` type system, and documenting core concepts. This foundation enables all future feature development (execution, decomposition, composition) by providing the essential abstractions and infrastructure.

## Technical Context

**Language/Version**: Haskell / GHC 2024 (GHC2024 language standard)  
**Primary Dependencies**: 
- base ^>=4.20.2.0 (Haskell base library)
- pattern (local path dependency from ../gram-hs/libs/pattern) - provides Pattern type and operations
- tasty ^>=1.4 (test framework)
- tasty-hunit ^>=0.10 (unit test support)
- tasty-quickcheck ^>=0.10 (property-based testing)

**Storage**: N/A (foundational setup, no persistence required)  
**Testing**: Tasty with HUnit (unit tests) and QuickCheck (property-based tests)  
**Target Platform**: Cross-platform (Haskell compiles to native binaries)  
**Project Type**: Single Haskell library with executable and test suite  
**Performance Goals**: N/A (foundational setup phase, performance optimization deferred)  
**Constraints**: 
- Must follow Haskell best practices and Cabal conventions
- Must integrate cleanly with gram-hs Pattern abstraction
- Type system must support both atomic and compound patterns
- Documentation must be clear for framework users

**Scale/Scope**: 
- Initial scope: Core type definitions and basic operations
- Library module structure to support incremental expansion
- Documentation covering V, Elements, and DAG concepts

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Principle 1: Design-Driven Development
- [x] **User Goal Statement**: Clear user goal documented in spec.md - "Establish foundational structure, core abstractions, and integration points needed for the pattern-agent framework"
- [x] **Design Validation**: Proposed design validated against user goal:
  - ✅ Project structure (src/, test/, docs/) enables incremental development
  - ✅ Pattern integration provides core abstraction layer
  - ✅ PatternAgent type definition enables all future features (execution, decomposition, composition)
  - ✅ Documentation covers core concepts (V, Elements, DAG) for developer understanding
- [x] **Rationale**: Design decisions justified by user goal satisfaction:
  - Type alias approach (PatternAgent = Pattern Agent) provides flexibility while leveraging gram-hs
  - Module structure (Core, Types) supports incremental expansion
  - Dual testing strategy (unit + scenario) validates both correctness and user goal satisfaction
  - Documentation strategy ensures developers understand abstractions before using them

### Principle 2: Why Before How
- [x] **Why Documented**: Rationale documented in spec.md - "Foundation required before any execution, decomposition, or composition features can be built"
- [x] **Clarifying Questions**: Questions asked and answered - Spec indicates no clarifications needed
- [x] **Implementation Plan References Why**: This plan references the documented rationale in Summary section

### Principle 3: Dual Testing Strategy
- [x] **Unit Tests Planned**: Unit-level tests identified in spec.md for:
  - Build configuration validation
  - Pattern import and operations
  - Pattern<Agent> type definition and operations
  - Documentation completeness/accuracy
- [x] **Scenario Tests Planned**: Scenario tests identified in spec.md that simulate:
  - Successful project build and dependency resolution
  - Pattern abstraction integration and usage
  - Pattern<Agent> creation and manipulation
  - Developer understanding from documentation
- [x] **Test Strategy**: Both unit and scenario testing approaches defined:
  - Unit tests: Tasty with HUnit for component-level testing
  - Scenario tests: Tasty with scenario test structure for user goal validation
  - Test organization: `test/unit/` and `test/scenario/` directories
  - Coverage: All construction, query, and validation functions have unit tests
  - Scenario tests validate: build success, integration, type usage, documentation understanding

### Principle 4: Expressiveness and Correctness
- [x] **API Design**: APIs designed for intuitive use and clarity:
  - `atomicPattern` and `compoundPattern` provide clear construction API
  - `value` and `elements` accessors match spec terminology (V, Elements)
  - Query functions (`isAtomic`, `isCompound`, `patternLength`) are self-documenting
  - Type alias `PatternAgent` is clear and matches framework terminology
- [x] **Edge Cases**: Edge cases identified in spec.md:
  - gram-hs unavailable/incompatible
  - Invalid Pattern<Agent> constructions (circular DAGs)
  - Malformed pattern operations
  - Missing/incorrect dependency versions
- [x] **Documentation Plan**: Documentation strategy defined in spec.md - Markdown format covering V, Elements, DAG concepts

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
src/
├── PatternAgent/
│   ├── Core.hs              # Pattern<Agent> type definition and basic operations
│   └── Types.hs             # Type aliases and core type definitions
└── MyLib.hs                 # Temporary placeholder (to be replaced)

app/
└── Main.hs                  # Executable entry point

test/
├── unit/                    # Unit tests
│   ├── PatternAgent/
│   │   ├── CoreSpec.hs      # Unit tests for Pattern<Agent> operations
│   │   └── TypesSpec.hs     # Unit tests for type definitions
│   └── BuildSpec.hs         # Unit tests for build configuration
├── scenario/                # Scenario tests
│   ├── BuildScenario.hs     # Scenario: project builds successfully
│   ├── IntegrationScenario.hs  # Scenario: gram-hs integration works
│   └── UsageScenario.hs     # Scenario: developers can use Pattern<Agent>
└── Main.hs                  # Test suite entry point

docs/
└── core-concepts.md         # Documentation of V, Elements, DAG concepts
```

**Structure Decision**: Single Haskell project following standard Cabal structure. Source code organized in `src/PatternAgent/` module hierarchy. Tests split into `unit/` and `scenario/` directories per Principle 3 (Dual Testing Strategy). Documentation in `docs/` directory.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., 4th project] | [current need] | [why 3 projects insufficient] |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient] |
