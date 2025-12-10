# Tasks: Foundation & Setup

**Input**: Design documents from `/specs/001-foundation-setup/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Per Principle 3 (Dual Testing Strategy), all features MUST include both unit tests and scenario tests. Scenario tests simulate user goal satisfaction end-to-end.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `test/` at repository root
- Paths shown below follow Haskell Cabal structure

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [ ] T001 Create project directory structure (src/PatternAgent/, test/unit/, test/scenario/, docs/)
- [ ] T002 Update pattern-agent.cabal with pattern dependency (local path to ../gram-hs/libs/pattern)
- [ ] T003 [P] Add testing dependencies to pattern-agent.cabal (tasty, tasty-hunit, tasty-quickcheck)
- [ ] T004 [P] Configure test suite in pattern-agent.cabal for unit and scenario tests

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T005 Create minimal Agent type placeholder in src/PatternAgent/Types.hs to enable type checking
- [ ] T006 Verify pattern package builds and is accessible from pattern-agent project

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Project Structure and Dependencies (Priority: P1) 🎯 MVP

**Goal**: Establish well-organized project structure with proper dependency management for framework development

**Independent Test**: Verify project builds successfully, dependencies resolve correctly, and directory structure follows Haskell best practices

### Tests for User Story 1 (Principle 3: Dual Testing Strategy) ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

**Scenario Tests** (simulate user goal satisfaction):
- [ ] T007 [P] [US1] Scenario test: project builds successfully in test/scenario/BuildScenario.hs
- [ ] T008 [P] [US1] Scenario test: dependencies resolve correctly in test/scenario/BuildScenario.hs

**Unit Tests** (component correctness):
- [ ] T009 [P] [US1] Unit test: verify cabal file syntax in test/unit/BuildSpec.hs
- [ ] T010 [P] [US1] Unit test: verify project structure directories exist in test/unit/BuildSpec.hs
- [ ] T011 [P] [US1] Unit test: verify dependency declarations are valid in test/unit/BuildSpec.hs

### Implementation for User Story 1 (Principle 4: Expressiveness and Correctness)

- [ ] T012 [US1] Ensure src/PatternAgent/ directory exists with proper structure
- [ ] T013 [US1] Ensure test/unit/ and test/scenario/ directories exist
- [ ] T014 [US1] Ensure docs/ directory exists
- [ ] T015 [US1] Update pattern-agent.cabal library section with exposed-modules: PatternAgent.Core, PatternAgent.Types
- [ ] T016 [US1] Configure pattern dependency in pattern-agent.cabal (path: ../gram-hs/libs/pattern)
- [ ] T017 [US1] Add tasty dependencies to pattern-agent.cabal test-suite section
- [ ] T018 [US1] Verify project builds with `cabal build` command

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently - project structure is complete and build succeeds

---

## Phase 4: User Story 2 - Pattern Abstraction Integration (Priority: P1)

**Goal**: Integrate framework with gram-hs Pattern abstraction to leverage the core formalism

**Independent Test**: Verify Pattern abstraction is accessible, types are properly imported, and basic Pattern operations work

### Tests for User Story 2 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T019 [P] [US2] Scenario test: Pattern types can be imported in test/scenario/IntegrationScenario.hs
- [ ] T020 [P] [US2] Scenario test: Pattern operations work correctly in test/scenario/IntegrationScenario.hs

**Unit Tests**:
- [ ] T021 [P] [US2] Unit test: verify Pattern import succeeds in test/unit/PatternAgent/IntegrationSpec.hs
- [ ] T022 [P] [US2] Unit test: verify Pattern construction operations in test/unit/PatternAgent/IntegrationSpec.hs
- [ ] T023 [P] [US2] Unit test: verify Pattern type compatibility in test/unit/PatternAgent/IntegrationSpec.hs

### Implementation for User Story 2 (Principle 4: Expressiveness and Correctness)

- [ ] T024 [US2] Import Pattern module in src/PatternAgent/Core.hs
- [ ] T025 [US2] Verify Pattern module exports are accessible
- [ ] T026 [US2] Test basic Pattern operations (pattern, patternWith, value, elements) in GHCi
- [ ] T027 [US2] Document Pattern integration in module comments

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently - Pattern abstraction is integrated and accessible

---

## Phase 5: User Story 3 - Pattern<Agent> Type Definition (Priority: P1)

**Goal**: Define well-defined Pattern<Agent> type with basic operations to represent agent systems

**Independent Test**: Verify developers can create Pattern<Agent> instances, access their components (V, Elements), and perform basic operations

### Tests for User Story 3 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T028 [P] [US3] Scenario test: create atomic PatternAgent in test/scenario/UsageScenario.hs
- [ ] T029 [P] [US3] Scenario test: create compound PatternAgent in test/scenario/UsageScenario.hs
- [ ] T030 [P] [US3] Scenario test: access V and Elements components in test/scenario/UsageScenario.hs

**Unit Tests**:
- [ ] T031 [P] [US3] Unit test: PatternAgent type definition in test/unit/PatternAgent/CoreSpec.hs
- [ ] T032 [P] [US3] Unit test: atomic pattern creation in test/unit/PatternAgent/CoreSpec.hs
- [ ] T033 [P] [US3] Unit test: compound pattern creation in test/unit/PatternAgent/CoreSpec.hs
- [ ] T034 [P] [US3] Unit test: value accessor in test/unit/PatternAgent/CoreSpec.hs
- [ ] T035 [P] [US3] Unit test: elements accessor in test/unit/PatternAgent/CoreSpec.hs
- [ ] T036 [P] [US3] Unit test: query functions (isAtomic, isCompound, patternLength) in test/unit/PatternAgent/CoreSpec.hs
- [ ] T037 [P] [US3] Unit test: validation functions (validatePattern, isAcyclic) in test/unit/PatternAgent/CoreSpec.hs

### Implementation for User Story 3 (Principle 4: Expressiveness and Correctness)

- [ ] T038 [US3] Define PatternAgent type alias in src/PatternAgent/Types.hs: `type PatternAgent = Pattern Agent`
- [ ] T039 [US3] Export PatternAgent from src/PatternAgent/Core.hs
- [ ] T040 [US3] Implement atomicPattern function in src/PatternAgent/Core.hs (delegates to Pattern.pattern)
- [ ] T041 [US3] Implement compoundPattern function in src/PatternAgent/Core.hs (delegates to Pattern.patternWith)
- [ ] T042 [US3] Implement fromPatternList function in src/PatternAgent/Core.hs (delegates to Pattern.fromList)
- [ ] T043 [US3] Re-export value and elements accessors from Pattern in src/PatternAgent/Core.hs
- [ ] T044 [US3] Implement isAtomic function in src/PatternAgent/Core.hs (checks if elements is empty)
- [ ] T045 [US3] Implement isCompound function in src/PatternAgent/Core.hs (checks if elements is non-empty)
- [ ] T046 [US3] Implement patternLength function in src/PatternAgent/Core.hs (delegates to Pattern.length)
- [ ] T047 [US3] Implement patternSize function in src/PatternAgent/Core.hs (delegates to Pattern.size)
- [ ] T048 [US3] Implement patternDepth function in src/PatternAgent/Core.hs (delegates to Pattern.depth)
- [ ] T049 [US3] Implement validatePattern function in src/PatternAgent/Core.hs (validates structure is well-formed)
- [ ] T050 [US3] Implement isAcyclic function in src/PatternAgent/Core.hs (checks for circular DAG references)
- [ ] T051 [US3] Add module documentation to src/PatternAgent/Core.hs explaining PatternAgent API

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently - PatternAgent type is defined with all basic operations

---

## Phase 6: User Story 4 - Documentation of Core Concepts (Priority: P2)

**Goal**: Provide clear documentation of Pattern<Agent> representation concepts (V, Elements, DAG structure) for developer understanding

**Independent Test**: Verify a new developer can read the documentation and correctly understand V, Elements, and DAG concepts

### Tests for User Story 4 (Principle 3: Dual Testing Strategy) ⚠️

**Scenario Tests**:
- [ ] T052 [P] [US4] Scenario test: documentation exists and is readable in test/scenario/DocumentationScenario.hs

**Unit Tests**:
- [ ] T053 [P] [US4] Unit test: verify documentation completeness (V, Elements, DAG covered) in test/unit/DocumentationSpec.hs
- [ ] T054 [P] [US4] Unit test: verify documentation examples compile in test/unit/DocumentationSpec.hs
- [ ] T055 [P] [US4] Unit test: verify documentation clarity in test/unit/DocumentationSpec.hs

### Implementation for User Story 4 (Principle 4: Expressiveness and Correctness)

- [ ] T056 [US4] Create docs/core-concepts.md with V (Agent value) explanation
- [ ] T057 [US4] Add Elements concept explanation to docs/core-concepts.md
- [ ] T058 [US4] Add DAG structure explanation to docs/core-concepts.md
- [ ] T059 [US4] Include code examples in docs/core-concepts.md demonstrating V, Elements, DAG usage
- [ ] T060 [US4] Verify all examples in docs/core-concepts.md compile and run correctly
- [ ] T061 [US4] Add cross-references from README.md to docs/core-concepts.md

**Checkpoint**: At this point, all user stories should be independently functional - documentation enables developer understanding

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T062 [P] Update README.md with foundation setup completion status
- [ ] T063 [P] Code cleanup and refactoring across all modules
- [ ] T064 [P] Additional unit tests in test/unit/ for edge cases (Principle 3)
- [ ] T065 [P] Additional scenario tests in test/scenario/ for integration scenarios (Principle 3)
- [ ] T066 Run quickstart.md validation checklist
- [ ] T067 Verify all tests pass: `cabal test`
- [ ] T068 Verify project builds cleanly: `cabal build`
- [ ] T069 Verify documentation is accessible and accurate

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2)
- **Polish (Final Phase)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - Depends on US1 for project structure
- **User Story 3 (P1)**: Can start after Foundational (Phase 2) - Depends on US2 for Pattern integration
- **User Story 4 (P2)**: Can start after Foundational (Phase 2) - Depends on US3 for PatternAgent type

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Types before functions
- Core functions before validation
- Implementation before documentation
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel (T003, T004)
- All Foundational tasks can run in parallel (T005, T006)
- Once Foundational phase completes, user stories can start sequentially (US1 → US2 → US3 → US4)
- All tests for a user story marked [P] can run in parallel
- Different test files within a story marked [P] can run in parallel
- Documentation tasks in US4 marked [P] can run in parallel

---

## Parallel Example: User Story 3

```bash
# Launch all tests for User Story 3 together (Principle 3):
Task: "Scenario test: create atomic PatternAgent in test/scenario/UsageScenario.hs"
Task: "Scenario test: create compound PatternAgent in test/scenario/UsageScenario.hs"
Task: "Unit test: PatternAgent type definition in test/unit/PatternAgent/CoreSpec.hs"
Task: "Unit test: atomic pattern creation in test/unit/PatternAgent/CoreSpec.hs"

# Launch query function implementations together:
Task: "Implement isAtomic function in src/PatternAgent/Core.hs"
Task: "Implement isCompound function in src/PatternAgent/Core.hs"
Task: "Implement patternLength function in src/PatternAgent/Core.hs"
```

---

## Implementation Strategy

### MVP First (User Stories 1-3 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Project Structure)
4. Complete Phase 4: User Story 2 (Pattern Integration)
5. Complete Phase 5: User Story 3 (PatternAgent Type)
6. **STOP and VALIDATE**: Test all three stories independently
7. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Validate (MVP foundation!)
3. Add User Story 2 → Test independently → Validate
4. Add User Story 3 → Test independently → Validate
5. Add User Story 4 → Test independently → Validate
6. Each story adds value without breaking previous stories

### Sequential Strategy (Recommended for Single Developer)

With a single developer:

1. Complete Setup + Foundational together
2. Complete User Story 1 (Project Structure) → Validate build
3. Complete User Story 2 (Pattern Integration) → Validate integration
4. Complete User Story 3 (PatternAgent Type) → Validate type system
5. Complete User Story 4 (Documentation) → Validate understanding
6. Polish and finalize

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- All file paths use absolute or relative paths from repository root
- Haskell module names use dot notation (PatternAgent.Core maps to src/PatternAgent/Core.hs)

