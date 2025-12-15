# Architecture: Pattern as Agent Workflow Language

**Feature**: Hello World Agent with Tool Execution  
**Date**: 2025-01-27  
**Purpose**: Reflect on the architectural model of Pattern as a programming language for agent workflows

## The Language Model

### Pattern as Source Code

Pattern Subject represents a **declarative programming language** for specifying agent workflows:

- **Pattern = Source Code**: Gram notation patterns are like source code - declarative, serializable, versionable
- **Agent Workflows = Programs**: Agent specifications with tools are programs in this language
- **Execution = Runtime**: The execution engine interprets/compiles and runs these programs

### Two Execution Models

#### 1. Interpretation Model (Direct Execution)

**Pattern → Execute Directly**

```haskell
-- Execution engine interprets Pattern directly
executePattern 
  :: Pattern Subject        -- Source: agent workflow program
  -> ToolLibrary           -- Runtime: tool implementations
  -> ConversationContext   -- Runtime: conversation state
  -> IO (Either AgentError AgentResponse)
```

**Characteristics**:
- No intermediate representation
- Pattern is canonical (source of truth)
- Execution engine reads Pattern structure directly
- Uses lenses to access Pattern fields
- More flexible, can use gram operations during execution

**Benefits**:
- Single source of truth (Pattern)
- No conversion overhead
- Can query/transform Pattern during execution
- Gram-native operations available

**Tradeoffs**:
- Pattern access may be slower than concrete types
- Less type safety at execution boundaries

#### 2. Compilation Model (Optimized Execution)

**Pattern → Concrete Types → Execute**

```haskell
-- Compile Pattern to optimized representation
compilePattern 
  :: Pattern Subject 
  -> Either Text Agent  -- Compiled representation

-- Execute compiled representation
executeAgent
  :: Agent              -- Compiled: optimized for execution
  -> ToolLibrary
  -> ConversationContext
  -> IO (Either AgentError AgentResponse)
```

**Characteristics**:
- Pattern compiled to concrete types
- Concrete types optimized for execution
- Type safety at compile boundaries
- Faster execution (no Pattern traversal)

**Benefits**:
- Better performance (direct field access)
- Type safety (concrete types)
- Clear separation (source vs runtime)

**Tradeoffs**:
- Conversion overhead
- Two representations to maintain
- Less flexible (can't use gram operations)

### Hybrid Model (Recommended)

**Support Both**: Interpretation as default, compilation as optimization

```haskell
-- Direct interpretation (default)
executePattern 
  :: Pattern Subject
  -> ToolLibrary
  -> ConversationContext
  -> IO (Either AgentError AgentResponse)

-- Optional compilation for performance
compilePattern :: Pattern Subject -> Either Text Agent
executeCompiled :: Agent -> ToolLibrary -> ConversationContext -> IO (Either AgentError AgentResponse)

-- Convenience: auto-compile if needed
executePatternOptimized 
  :: Pattern Subject
  -> ToolLibrary
  -> ConversationContext
  -> IO (Either AgentError AgentResponse)
executePatternOptimized p lib ctx = do
  case compilePattern p of
    Right agent -> executeCompiled agent lib ctx
    Left err -> executePattern p lib ctx  -- Fallback to interpretation
```

## Terminology Alternatives

### Current Terminology

- **Pattern Subject** = Specification/Declarative representation
- **Concrete Types** = Implementation/Execution representation
- **Conversion** = Pattern → Agent transformation

### Alternative Terminology (Language Model)

**Option 1: Source/Target**
- **Pattern** = Source code
- **Agent** = Compiled/target code
- **Compilation** = Pattern → Agent transformation
- **Execution Engine** = Runtime that runs compiled code

**Option 2: Grammar/Runtime**
- **Pattern** = Grammar (language definition)
- **Agent** = Runtime representation
- **Parsing/Compilation** = Pattern → Agent transformation
- **Interpreter/Executor** = Runtime that executes

**Option 3: Schema/Instance**
- **Pattern** = Schema (structure definition)
- **Agent** = Instance (concrete data)
- **Instantiation** = Pattern → Agent transformation
- **Execution** = Runtime that operates on instances

**Option 4: Declarative/Imperative**
- **Pattern** = Declarative specification
- **Agent** = Imperative representation
- **Materialization** = Pattern → Agent transformation
- **Execution** = Runtime that executes

### Recommended Terminology

**Pattern = Source Code / Workflow Language**
- Pattern Subject = Source code (canonical)
- Agent (concrete) = Compiled representation (optional optimization)
- Tool = Runtime implementation (not part of language)
- Execution Engine = Interpreter/compiler that runs workflows

**Key Terms**:
- **Pattern**: The source language (gram notation)
- **Workflow**: An agent specification (program in Pattern language)
- **Compilation**: Optional Pattern → Agent transformation (optimization)
- **Interpretation**: Direct Pattern → Execution (default)
- **Runtime**: Execution environment (ToolLibrary, ConversationContext)
- **Execution Engine**: System that interprets/compiles and runs workflows

## Canonical Form

**Pattern Subject is Canonical**

- Pattern is the source of truth (like source code)
- All serialization is Pattern (gram notation)
- Concrete types are derived/compiled from Pattern
- Execution can work directly with Pattern (interpretation) or compile first (optimization)

**Implications**:
- Storage: Always store as Pattern
- Versioning: Version Pattern files
- Sharing: Share Pattern files
- Execution: Can interpret directly or compile first

## Execution Engine Design

### If Interpreted (No Conversion Targets)

**Direct Pattern Execution**:
```haskell
-- Execution engine works directly with Pattern
executePattern 
  :: Pattern Subject        -- Source workflow
  -> ToolLibrary           -- Runtime registry
  -> ConversationContext   -- Runtime state
  -> IO (Either AgentError AgentResponse)

-- Uses lenses to access Pattern fields
executePattern agent lib ctx = do
  let name = view agentName agent
  let instruction = view agentInstruction agent
  let model = view agentModel agent
  let tools = view agentTools agent
  
  -- Bind tools to implementations
  boundTools <- bindTools tools lib
  
  -- Execute using Pattern structure directly
  ...
```

**No Conversion Needed**:
- Execution engine reads Pattern directly
- Uses lenses for type-safe access
- Pattern structure drives execution
- No intermediate representation

**Benefits**:
- Simpler architecture (one representation)
- Pattern is always canonical
- Can use gram operations during execution
- No conversion overhead

### If Compiled (With Conversion Targets)

**Pattern → AgentRuntime → Execute**:
```haskell
-- Compile Pattern to AgentRuntime
compileAgent :: Agent -> Either Text AgentRuntime

-- Execute compiled AgentRuntime
executeAgentRuntime :: AgentRuntime -> ToolLibrary -> ConversationContext -> IO (Either AgentError AgentResponse)

-- Full flow
executeCompiled agent lib ctx = do
  agentRuntime <- case compileAgent agent of
    Right a -> return a
    Left err -> throwError err
  executeAgentRuntime agentRuntime lib ctx
```

**Conversion Targets**:
- Agent (Pattern) → AgentRuntime (for execution optimization)
- Tool (Pattern) → ToolRuntime (for tool binding)
- Pattern → Model (for LLM client creation)

**Benefits**:
- Better performance (direct field access)
- Type safety (concrete types)
- Clear execution boundaries

## Recommendation: Hybrid Approach

**Support Both Models**:

1. **Default: Interpretation**
   - Pattern is canonical
   - Execution engine interprets Pattern directly
   - Uses lenses for access
   - Flexible, gram-native

2. **Optional: Compilation**
   - Compile Pattern → Agent for performance
   - Use when execution is hot path
   - Fallback to interpretation if compilation fails
   - Clear optimization boundary

3. **Terminology**:
   - **Pattern** = Source code / Workflow language
   - **Workflow** = Agent specification (program)
   - **Compilation** = Optional Pattern → Agent optimization
   - **Interpretation** = Direct Pattern execution (default)
   - **Runtime** = Execution environment (ToolLibrary, Context)
   - **Execution Engine** = System that runs workflows

## Implementation Strategy

### Phase 1: Interpretation (Default)
- Implement direct Pattern execution
- Use lenses for type-safe access
- Pattern is canonical, no conversion

### Phase 2: Optional Compilation
- Add Pattern → Agent compilation
- Use for performance optimization
- Keep interpretation as fallback

### Phase 3: Hybrid Execution
- Auto-detect when to compile vs interpret
- Profile and optimize hot paths
- Maintain Pattern as canonical source

## Key Insights

1. **Pattern is a Programming Language**: Gram notation patterns are like source code for agent workflows
2. **Canonical Form**: Pattern Subject is always canonical (source of truth)
3. **Execution Models**: Can interpret directly or compile for optimization
4. **No Required Conversion**: If interpreted, no conversion targets needed - just execution engine
5. **Hybrid Approach**: Support both interpretation (default) and compilation (optimization)
