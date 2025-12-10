# TODO

## 1. Foundation & Setup

- [ ] Set up basic project structure and dependencies
- [ ] Integrate with gram-hs Pattern abstraction
- [ ] Define core `Pattern<Agent>` type and basic operations (Principle 4: expressiveness and correctness)
- [ ] Document basic Pattern<Agent> representation (V, Elements, DAG structure)

## 2. Execution

- [ ] **Principle 2: Why Before How** - Document user goal and rationale for execution environment
- [ ] **Principle 1: Design-Driven Development** - Validate execution environment design against user goal
- [ ] Create basic module for calling an LLM API (Principle 4: expressive API design)
- [ ] Design execution environment that "runs the agent" (validate against user goal)
- [ ] Implement execution environment for single atomic agents (Principle 4: correctness for all conditions)
- [ ] Support for fully specifying an agent (system prompt, LLM selection) - expressive API
- [ ] Create early test case: simple conversational agent with no tools
  - [ ] **Principle 2** - Document user goal for conversational agent
  - [ ] **Principle 1** - Validate agent design against user goal
  - [ ] Define single agent with system prompt and LLM (Principle 4: clear, expressive specification)
  - [ ] **Principle 3: Dual Testing Strategy**
    - [ ] Unit tests for agent definition and LLM API module
    - [ ] Scenario test: user goal satisfaction (e.g., "user can have a conversation with agent")
  - [ ] Execute agent in conversation loop
  - [ ] Validate basic conversational behavior
- [ ] Design execution semantics for Pattern<Agent> (validate against user goals)
- [ ] Implement basic pattern execution engine (Principle 4: expressiveness and correctness)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests and scenario tests for execution engine
- [ ] Support for execution visualization
- [ ] Document execution semantics and DAG interpretation

## 3. Decomposition (Factorization)

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for decomposition feature
- [ ] **Principle 1: Design-Driven Development** - Validate decomposition API design against user goals
- [ ] Design decomposition API for breaking agents into sub-agents (Principle 4: expressive, intuitive API)
- [ ] Implement basic factorization strategy (e.g., task-based decomposition) (Principle 4: correctness)
- [ ] Create first concrete decomposition example:
  - [ ] **Principle 2** - Document why this specific agent needs decomposition
  - [ ] **Principle 1** - Validate decomposition design against stated goal
  - [ ] Start with a complex agent (e.g., multi-step reasoning agent)
  - [ ] Factorize into specialized sub-agents
  - [ ] Document the decomposition process and rationale
  - [ ] **Principle 3: Dual Testing Strategy**
    - [ ] Unit tests for decomposition operations
    - [ ] Scenario test: decomposed agent satisfies same user goal as original
- [ ] Support for identifying "prime agents" (atomic patterns) (Principle 4: clear, expressive API)
- [ ] Implement multiple decomposition strategies:
  - [ ] Task-based decomposition
  - [ ] Tool-based decomposition
  - [ ] Domain-based decomposition
- [ ] Validate decomposed agents maintain original capability (Principle 4: correctness)
- [ ] **Principle 3: Dual Testing Strategy** - Unit and scenario tests for each decomposition strategy
- [ ] Decomposition toolkit with factorization strategies

## 4. Composition

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for composition feature
- [ ] **Principle 1: Design-Driven Development** - Validate composition API design against user goals
- [ ] Design composition API for assembling patterns (Principle 4: expressive, intuitive API)
- [ ] Implement basic pattern assembly (DAG construction) (Principle 4: correctness)
- [ ] Create first concrete composition example:
  - [ ] **Principle 2** - Document why composition is needed for this use case
  - [ ] **Principle 1** - Validate composition design against stated goal
  - [ ] Start with library of simple pattern agents
  - [ ] Compose into a working multi-agent system
  - [ ] Document composition process and architecture
  - [ ] **Principle 3: Dual Testing Strategy**
    - [ ] Unit tests for composition operations
    - [ ] Scenario test: composed system satisfies user goal
- [ ] Support for workflow agents that coordinate sub-agents (Principle 4: expressive API)
- [ ] Pattern validation (checking DAG structure, dependencies) (Principle 4: correctness)
- [ ] Composition engine for pattern assembly and validation
- [ ] Support for parallelization opportunities in DAG structure
- [ ] Support for synchronization points in workflows
- [ ] **Principle 3: Dual Testing Strategy** - Unit and scenario tests for composition features

## 5. Pattern Library

- [ ] **Principle 2: Why Before How** - Document rationale for pattern library and catalog structure
- [ ] Create initial catalog of reusable agent patterns (Principle 4: expressive naming and organization)
- [ ] Document pattern behaviors and use cases (Principle 4: accurate documentation)
- [ ] Establish pattern naming conventions (Principle 4: expressiveness)
- [ ] Build library of reusable agent components organically
- [ ] Create pattern comparison utilities (Principle 4: expressive API, correct behavior)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests for library utilities, scenario tests for pattern usage

## 6. Equivalence & Comparison

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for equivalence feature
- [ ] **Principle 1: Design-Driven Development** - Validate equivalence design against user goals
- [ ] Design equivalence representation for patterns (Principle 4: expressive representation)
- [ ] Implement basic equivalence checking (Principle 4: correctness)
- [ ] Support for equivalence classes (different implementations, same goal)
- [ ] Pattern comparison utilities (Principle 4: expressive API, correct behavior)
- [ ] Document equivalence strategies (Principle 4: accurate documentation)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests for equivalence operations, scenario tests demonstrating equivalence validation

## 7. Tracing

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for tracing feature
- [ ] **Principle 1: Design-Driven Development** - Validate tracing design against user goals
- [ ] Design trace representation (potentially `Pattern<Trace>`) (Principle 4: expressive structure)
- [ ] Implement execution tracing (Principle 4: correctness - accurate trace capture)
- [ ] Integrate tracing with execution environment
- [ ] Support for trace analysis and inspection (Principle 4: expressive API)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests for tracing operations, scenario tests for trace capture and analysis

## 8. Evolution & Feedback

- [ ] **Principle 2: Why Before How** - Document user goals and rationale for evolution mechanisms
- [ ] **Principle 1: Design-Driven Development** - Validate evolution design against user goals
- [ ] Design feedback loop mechanisms (Principle 4: expressive, clear design)
- [ ] Explore RL-like mechanisms for pattern improvement
- [ ] Implement pattern evolution based on traces (Principle 4: correctness)
- [ ] Evolution mechanisms with tracing and feedback loops
- [ ] Automated pattern optimization (Principle 4: correctness - valid optimizations)
- [ ] **Principle 3: Dual Testing Strategy** - Unit tests for evolution mechanisms, scenario tests for pattern improvement workflows

## 9. Theoretical Foundations

- [ ] Explore category theory connections
  - [ ] Investigate if patterns form a category
  - [ ] Define morphisms (transformations between patterns)
  - [ ] Document theoretical foundations
- [ ] Formalize bidirectional transformation properties
- [ ] Document mathematical foundations

## 10. Documentation & Examples

- [ ] Create comprehensive examples for decomposition (Principle 4: expressive, clear examples)
- [ ] Create comprehensive examples for composition (Principle 4: expressive, clear examples)
- [ ] Document emerging patterns and strategies (Principle 4: accurate documentation)
- [ ] Create tutorial/guide for using the framework (Principle 4: expressiveness)
- [ ] Document best practices for agent design
- [ ] Document user goals and rationale for each major feature (Principle 2: Why Before How)
- [ ] Document design validation process and examples (Principle 1: Design-Driven Development)
- [ ] Document testing strategy with unit and scenario test examples (Principle 3: Dual Testing Strategy)

---

# Unplanned features to consider

Todo sometime:

- serialization using gram notation
