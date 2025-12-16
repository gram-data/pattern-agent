```markdown
# Pattern Agents

A framework for exploring and composing agentic architectures using the Pattern abstraction from [gram-hs](../gram-hs).

## Overview

Pattern Agents provides tools for reasoning about and experimenting with different agentic system architectures through a unified pattern-based formalism. The core idea is to represent agent systems as `Pattern<Agent>` structures, enabling both:

1. **Decomposition (factorization):** Breaking complex agents into specialized multi-agent systems
2. **Composition:** Assembling reusable agent patterns into novel architectures

## Core Concepts

### Pattern<Agent>

An agent system is represented as a Pattern where:
- **Atomic patterns:** Single agents that cannot be meaningfully factorized further (prime agents)
- **Compound patterns:** Multi-agent systems represented as DAGs of sub-agents
- **V (Agent value):** Contains all information defining agent capability (name, description, model, instruction)
- **Elements:** Sub-agents in a decomposition
- **Equivalence:** Different patterns that accomplish the same goal through different means

### Key Properties

- **Bidirectional transformation:** The same formalism handles both top-down factorization and bottom-up composition
- **Equivalence classes:** Multiple patterns can be equivalent while varying in implementation (e.g., different architectures for the same goal)
- **Execution semantics:** DAG structure reveals pipelines, parallelization opportunities, and synchronization points
- **Composability:** Workflow agents coordinate sub-agents, creating hierarchical architectures

## Goals

1. **Given an agent, decompose it** into a multi-agent system to:
   - Reduce individual agent responsibility
   - Use alternative LLMs for different sub-tasks
   - Focus and simplify prompts

2. **Given a library of pattern agents, compose them** into solutions:
   - Reuse tested components with known behaviors
   - Experiment with different architectural combinations
   - Maintain comparability across alternatives

## Development Approach

The project follows an **incremental, example-driven** design philosophy:
- Start with concrete agent decomposition examples
- Let practical constraints reveal the right abstractions
- Document patterns and strategies as they emerge
- Build library of reusable components organically

## Open Questions

Areas under active exploration:
- **Equivalence representation:** How to formally declare that two patterns are equivalent
- **Tracing:** How execution traces are captured and represented (potentially `Pattern<Trace>`)
- **Feedback loops:** How RL-like mechanisms can automate pattern improvement
- **Category theory:** Are we forming a category with patterns as objects and transformations as morphisms?

## Dependencies

- [gram-hs](../gram-hs): Provides the core Pattern abstraction and gram notation support

## Status

Early development - establishing foundations through practical examples.

## Running the CLI

The pattern-agent CLI can be used to execute agents from gram files or interact directly with LLMs.

### Building

```bash
cabal build
```

### Usage

**Important:** When using `cabal exec pattern-agent`, you must use `--` to separate cabal's flags from pattern-agent's flags:

```bash
# Standard mode (direct LLM interaction)
cabal exec pattern-agent -- "What is the capital of France?"

# With debug output
cabal exec pattern-agent -- --debug "What is the capital of France?"

# Agent mode (load agent from gram file with tool support)
cabal exec pattern-agent -- --agent examples/helloAgent.gram "Hello!"

# Agent mode with debug
cabal exec pattern-agent -- --agent examples/helloAgent.gram --debug "Hello!"
```

### Prerequisites

Set the `OPENAI_API_KEY` environment variable:

```bash
export OPENAI_API_KEY=your-api-key-here
```

### After Installation

If you install the executable:

```bash
cabal install --installdir=$HOME/.local/bin
```

You can then run it directly without `cabal exec`:

```bash
pattern-agent --agent examples/helloAgent.gram "Hello!"
```

## Future Directions

- Decomposition toolkit with factorization strategies
- Composition engine for pattern assembly and validation
- Evolution mechanisms with tracing and feedback loops
- Pattern library of reusable agent components
```
