# Module Architecture: Language vs Runtime Separation

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Define module separation between portable language specification and implementation-specific runtime

## Overview

The Pattern Agent framework should be structured with clear separation between:

1. **Language Module** - Portable specification (gram notation schema, validation, serialization)
2. **Runtime Module** - Implementation-specific execution engine (Haskell-specific features)

This separation enables:
- **Portability**: Other languages (Python, JavaScript) can implement their own runtime while using the same language specification
- **Clear Boundaries**: Language concerns (what gram notation means) vs runtime concerns (how to execute)
- **Reference Implementation**: The language module serves as the reference specification for other implementations
- **Maintainability**: Changes to language spec vs runtime implementation are clearly separated

## Module Structure

### Language Module (`PatternAgent.Language.*`)

**Purpose**: Define the portable language specification - what gram notation means for agent workflows.

**Responsibilities**:
- Pattern schema definitions (Agent, Tool as Pattern Subject structure)
- Gram notation validation (structure, types, constraints)
- Type signature parsing (gram path notation → parsed representation)
- Schema generation (gram type signature → JSON schema)
- Lens definitions for Pattern access (type-safe field accessors)
- Serialization/deserialization (gram ↔ Pattern Subject)
- Pattern construction helpers (`createAgent`, `createTool`)
- Validation rules (agent structure, tool structure, type signatures)

**Key Characteristics**:
- ✅ **Portable**: No Haskell-specific features (no IO, no function closures)
- ✅ **Pure**: All functions are pure (no side effects)
- ✅ **Serializable**: Everything can be serialized to/from gram notation
- ✅ **Reference**: This is what other languages implement

**Modules**:
```
PatternAgent.Language
  - Core language definitions (Agent, Tool as Pattern Subject)
  - Lenses for type-safe access
  - Pattern construction and validation

PatternAgent.Language.Schema
  - Gram notation schema definitions
  - Schema validation rules
  - Structure constraints

PatternAgent.Language.TypeSignature
  - Type signature parsing (gram path notation)
  - Type signature validation
  - JSON schema generation from type signatures

PatternAgent.Language.Serialization
  - Gram ↔ Pattern Subject conversion
  - Pattern Subject ↔ JSON conversion (for API compatibility)
```

**Example**:
```haskell
-- PatternAgent.Language
module PatternAgent.Language where

import Pattern (Pattern Subject)

-- Language types (Pattern Subject)
type Agent = Pattern Subject
type Tool = Pattern Subject

-- Language operations (pure, portable)
createAgent :: Text -> Text -> Model -> [Tool] -> Either Text Agent
validateAgent :: Agent -> Either Text ()
agentName :: Lens' Agent Text
agentTools :: Lens' Agent [Tool]
```

### Runtime Module (`PatternAgent.Runtime.*`)

**Purpose**: Implementation-specific execution engine - how to execute agent workflows.

**Responsibilities**:
- Execution engine (interpretation/compilation of Pattern)
- Tool binding (Tool Pattern → ToolImpl)
- LLM client integration (API calls, response parsing)
- Tool invocation (calling ToolImpl functions)
- Conversation context management
- ToolLibrary management (runtime registry)
- Error handling and recovery
- Iterative execution loop (tool calls → LLM → tool calls)

**Key Characteristics**:
- ⚙️ **Implementation-specific**: Uses Haskell features (IO, function closures, etc.)
- ⚙️ **Side effects**: LLM API calls, tool invocations, state management
- ⚙️ **Runtime state**: ToolLibrary, ConversationContext
- ⚙️ **Execution**: Actually runs agent workflows

**Modules**:
```
PatternAgent.Runtime
  - Execution engine (executePattern, executeAgentRuntime)
  - Tool binding (bindTool, bindAgentTools)
  - Execution loop (iterative tool calling)

PatternAgent.Runtime.ToolLibrary
  - ToolImpl type (executable tool implementation)
  - ToolLibrary type (runtime registry)
  - Tool registration and lookup

PatternAgent.Runtime.LLM
  - LLM client (API calls, response parsing)
  - Model configuration
  - Function calling format handling

PatternAgent.Runtime.Context
  - ConversationContext management
  - Message history
  - Tool result integration
```

**Example**:
```haskell
-- PatternAgent.Runtime
module PatternAgent.Runtime where

import PatternAgent.Language (Agent, Tool)
import PatternAgent.Runtime.ToolLibrary (ToolImpl, ToolLibrary)

-- Runtime execution (Haskell-specific)
executePattern 
  :: Agent              -- Language: Pattern specification
  -> ToolLibrary        -- Runtime: Tool implementations
  -> ConversationContext -- Runtime: Conversation state
  -> IO (Either AgentError AgentResponse)

bindTool 
  :: Tool              -- Language: Tool specification
  -> ToolLibrary       -- Runtime: Tool registry
  -> Maybe ToolImpl    -- Runtime: Bound implementation
```

## Benefits of This Separation

### 1. Portability

**Language Module** can be implemented in any language:
- Python: Parse gram notation, validate structure, generate JSON schemas
- JavaScript: Same - language spec is language-agnostic
- Other languages: Implement the same language specification

**Runtime Module** is implementation-specific:
- Haskell: Uses IO, function closures, type system
- Python: Uses async/await, callable objects, type hints
- JavaScript: Uses Promises, functions, TypeScript types

### 2. Clear Boundaries

**Language concerns** (what):
- What is a valid Agent pattern?
- What is a valid Tool pattern?
- What does a type signature mean?
- How to serialize/deserialize?

**Runtime concerns** (how):
- How to execute an agent?
- How to bind tools to implementations?
- How to call LLM APIs?
- How to manage conversation state?

### 3. Reference Implementation

The **Language Module** serves as the reference specification:
- Other implementations can verify against Haskell's language module
- Language spec changes are clearly documented
- Test cases can be shared (gram notation examples)

### 4. Maintainability

**Separation of concerns**:
- Language spec changes don't affect runtime
- Runtime optimizations don't affect language spec
- Clear ownership of changes

**Testing**:
- Language module: Pure functions, easy to test
- Runtime module: Integration tests, mock LLM APIs

## Migration Path

### Current Structure
```
PatternAgent/
  - Agent.hs          (mixed: language + runtime)
  - Tool.hs           (mixed: language + runtime)
  - Execution.hs      (runtime)
  - LLM.hs            (runtime)
  - Context.hs         (runtime)
```

### Proposed Structure
```
PatternAgent/
  Language/
    - Core.hs         (Agent, Tool as Pattern Subject)
    - Schema.hs        (gram schema definitions)
    - TypeSignature.hs (parsing, validation, schema generation)
    - Serialization.hs (gram ↔ Pattern conversion)
  Runtime/
    - Execution.hs    (execution engine)
    - ToolLibrary.hs   (ToolImpl, ToolLibrary)
    - LLM.hs          (LLM client)
    - Context.hs       (ConversationContext)
```

### Migration Steps

1. **Extract Language Module**:
   - Move Pattern Subject definitions to `Language.Core`
   - Move lens definitions to `Language.Core`
   - Move validation to `Language.Schema`
   - Move type signature parsing to `Language.TypeSignature`

2. **Refactor Runtime Module**:
   - Keep execution logic in `Runtime.Execution`
   - Move ToolImpl to `Runtime.ToolLibrary`
   - Keep LLM client in `Runtime.LLM`
   - Keep context in `Runtime.Context`

3. **Update Dependencies**:
   - Runtime depends on Language
   - Language has no dependencies on Runtime
   - Clear import boundaries

## Example: Multi-Language Support

### Haskell Implementation
```haskell
-- Language module (portable spec)
import PatternAgent.Language (Agent, Tool, createAgent, validateAgent)

-- Runtime module (Haskell-specific)
import PatternAgent.Runtime (executePattern, ToolLibrary)
```

### Python Implementation (Future)
```python
# Language module (same spec, Python implementation)
from pattern_agent.language import Agent, Tool, create_agent, validate_agent

# Runtime module (Python-specific)
from pattern_agent.runtime import execute_pattern, ToolLibrary
```

### JavaScript Implementation (Future)
```javascript
// Language module (same spec, JavaScript implementation)
import { Agent, Tool, createAgent, validateAgent } from '@pattern-agent/language';

// Runtime module (JavaScript-specific)
import { executePattern, ToolLibrary } from '@pattern-agent/runtime';
```

All three implementations share the same **language specification** (gram notation schema, validation rules, type signatures) but have different **runtime implementations** (execution engines, tool binding, LLM clients).

## Recommendations

1. **Start with separation**: Even if initially only Haskell is implemented, structure modules with this separation from the start
2. **Language module first**: Implement and stabilize the language module before optimizing runtime
3. **Clear documentation**: Document what belongs in Language vs Runtime
4. **Shared test cases**: Language module tests can be ported to other languages
5. **Reference implementation**: Haskell's language module becomes the reference spec

## Notes

- **Language Module** = "What gram notation means" (portable, pure, serializable)
- **Runtime Module** = "How to execute it" (implementation-specific, side effects, stateful)
- This separation aligns with the "Pattern as Source Code" model from `ARCHITECTURE.md`
- Language module is like a compiler frontend (parsing, validation, AST)
- Runtime module is like a compiler backend (execution, optimization, code generation)
