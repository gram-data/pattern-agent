<!--
Sync Impact Report:
Version: 1.0.0 → 1.2.0
Change Type: MINOR (new principle added + development practices guidance)
Modified Principles: N/A
Added Sections: 
  - Principle 5: Progressive Iteration
  - Development Practices (version control and checkpoints)
Removed Sections: N/A
Templates Requiring Updates:
  - ⚠ pending: .specify/templates/plan-template.md (needs Constitution Check update for Principle 5)
  - ⚠ pending: .specify/templates/spec-template.md (may need updates if progressive iteration affects spec structure)
  - ⚠ pending: .specify/templates/tasks-template.md (may need updates to reflect progressive iteration in task planning)
  - ⚠ pending: .specify/templates/commands/*.md (needs review for any outdated references)
Follow-up TODOs: Update templates to include Principle 5 in constitution checks
-->

# Project Constitution

**Project:** pattern-agent  
**Version:** 1.2.0  
**Ratification Date:** 2025-01-27  
**Last Amended:** 2025-01-27

## Purpose

This constitution establishes the non-negotiable principles and governance rules for the pattern-agent project. All development work MUST align with these principles. Amendments require explicit review and versioning.

## Principles

### Principle 1: Design-Driven Development

**Rule:** All feature development MUST begin with design validation against a stated user goal. Design decisions MUST be justified by how they satisfy the user goal before implementation begins.

**Rationale:** Features exist to serve user needs. Starting with design validation ensures we build the right thing before investing in implementation. This prevents wasted effort on solutions that don't address actual user problems.

**Enforcement:** 
- Feature proposals MUST include a clear user goal statement
- Design reviews MUST validate that the proposed design satisfies the stated goal
- Implementation MUST NOT proceed until design validation is complete

### Principle 2: Why Before How

**Rule:** Before planning implementation, developers MUST establish WHY a feature is being created by asking clarifying questions. The rationale for a feature MUST be documented and validated before HOW it will be built is determined.

**Rationale:** Understanding the underlying motivation prevents premature optimization and ensures we solve the right problem. Clarifying questions surface assumptions and reveal whether a feature is truly necessary or if existing capabilities can be extended.

**Enforcement:**
- Feature requests MUST include or prompt for a "why" statement
- Design sessions MUST begin with rationale exploration
- Implementation plans MUST reference the documented "why"

### Principle 3: Dual Testing Strategy

**Rule:** All features MUST be tested at both unit-test level and scenario level. Scenario tests MUST simulate how a user goal can be satisfied end-to-end.

**Rationale:** Unit tests verify correctness of components in isolation, while scenario tests validate that the system actually satisfies user goals. Both are necessary: unit tests catch implementation bugs, scenario tests catch design gaps.

**Enforcement:**
- Every feature MUST have unit tests covering its components
- Every feature MUST have at least one scenario test demonstrating user goal satisfaction
- Scenario tests MUST be written from the user's perspective, not the implementation's

### Principle 4: Expressiveness and Correctness

**Rule:** All features MUST prioritize both expressiveness (clarity of intent and usage) and correctness (accurate behavior). Code MUST be written to clearly communicate its purpose, and MUST behave correctly under all specified conditions.

**Rationale:** Expressiveness enables maintainability and reduces cognitive load. Correctness ensures reliability and trust. Both are essential for a framework that others will build upon. Expressiveness without correctness is misleading; correctness without expressiveness is fragile.

**Enforcement:**
- Code reviews MUST evaluate both clarity and correctness
- APIs MUST be designed for intuitive use
- Documentation MUST accurately reflect behavior
- Edge cases MUST be handled explicitly

### Principle 5: Progressive Iteration

**Rule:** Implementation MUST start with the simplest solution that satisfies the user goal, then incrementally add capability only when user goals require it. Avoid premature abstraction or complexity until there is concrete evidence of need.

**Rationale:** Starting simple reduces risk, accelerates delivery, and prevents over-engineering. Complexity should emerge from real requirements, not speculation. This aligns with the incremental, example-driven development approach and prevents building features that aren't actually needed.

**Enforcement:**
- Initial implementations MUST use the simplest approach that meets the stated user goal
- Additional complexity (abstractions, composition, advanced features) MUST be justified by specific user goals
- When considering adding complexity, ask: "What user goal requires this?"
- If no user goal requires it, defer the complexity
- Document the rationale when choosing simple over complex approaches

## Governance

### Amendment Procedure

1. Proposed amendments MUST be documented with rationale
2. Amendments affecting principles require review and consensus
3. Version MUST be updated according to semantic versioning rules
4. Sync Impact Report MUST be updated in the constitution header
5. Dependent templates and documentation MUST be updated

### Versioning Policy

- **MAJOR:** Backward incompatible governance/principle removals or redefinitions
- **MINOR:** New principle/section added or materially expanded guidance
- **PATCH:** Clarifications, wording, typo fixes, non-semantic refinements

### Development Practices

**Version Control and Checkpoints:**
- Significant changes MUST be committed to git to provide rollback checkpoints
- Commit when:
  - A phase or user story is complete and independently testable
  - A working state is achieved (even if incomplete)
  - A refactoring or significant structural change is made
  - Tests pass after a meaningful increment
- Commit messages SHOULD reference the task ID and user story when applicable
- Avoid committing broken or non-functional states unless explicitly marking as WIP
- Use feature branches for work-in-progress, merge to main when feature is complete

**Rationale:** Good rollback checkpoints enable safe experimentation, support progressive iteration (Principle 5), and allow recovery from mistakes. They also provide a clear history of how the codebase evolved.

### Compliance Review

- All feature work MUST reference relevant principles
- Code reviews MUST verify principle compliance
- Design reviews MUST validate against Principle 1, Principle 2, and Principle 5 (progressive iteration)
- Test reviews MUST verify Principle 3 compliance
- Code quality reviews MUST verify Principle 4 compliance
- Implementation reviews MUST verify Principle 5 compliance (simplicity before complexity)
- Version control practices MUST follow Development Practices guidelines

## Notes

This constitution is a living document. As the project evolves, principles may be refined based on practical experience, but changes require explicit amendment and versioning.
