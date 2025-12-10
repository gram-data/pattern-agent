<!--
Sync Impact Report:
Version: 0.0.0 → 1.0.0
Change Type: MAJOR (initial constitution creation)
Modified Principles: N/A (new)
Added Sections: All sections (initial creation)
Removed Sections: N/A
Templates Requiring Updates:
  - ✅ updated: .specify/templates/plan-template.md (added Constitution Check with all 4 principles)
  - ✅ updated: .specify/templates/spec-template.md (added User Goal & Rationale section, enhanced testing sections)
  - ✅ updated: .specify/templates/tasks-template.md (updated to emphasize dual testing strategy, expressiveness/correctness)
  - ⚠ pending: .specify/templates/commands/*.md (needs review for any outdated references)
Follow-up TODOs: None
-->

# Project Constitution

**Project:** pattern-agent  
**Version:** 1.0.0  
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

### Compliance Review

- All feature work MUST reference relevant principles
- Code reviews MUST verify principle compliance
- Design reviews MUST validate against Principle 1 and Principle 2
- Test reviews MUST verify Principle 3 compliance
- Code quality reviews MUST verify Principle 4 compliance

## Notes

This constitution is a living document. As the project evolves, principles may be refined based on practical experience, but changes require explicit amendment and versioning.
