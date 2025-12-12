# API Contract: LLM Agent

**Feature**: Basic LLM Agent  
**Date**: 2025-01-27  
**Purpose**: Define the API contract for creating and configuring LLM agents

## Agent Creation

### `createAgent`

Creates a new LLM agent with the specified configuration.

**Signature**:
```haskell
createAgent
  :: Text                    -- name: Unique agent identifier
  -> Model                   -- model: LLM model to use
  -> Text                    -- instruction: Agent behavior instructions
  -> Maybe Text              -- description: Optional agent description
  -> [Tool]                  -- tools: List of tools available to agent
  -> Maybe GenerateContentConfig  -- config: Optional LLM generation config
  -> Agent
```

**Preconditions**:
- `name` must be non-empty
- `name` must be unique within agent scope (runtime check)
- `model` must be valid for the configured provider
- `instruction` must be non-empty
- All tools in `tools` list must have unique names

**Postconditions**:
- Returns `Agent` with specified configuration
- Agent is ready for execution

**Errors**:
- `ValidationError "Agent name cannot be empty"`
- `ValidationError "Agent instruction cannot be empty"`
- `ValidationError "Duplicate tool name: {name}"`

**Example**:
```haskell
let agent = createAgent
      "capital_agent"
      (Model "gpt-4" OpenAI)
      "You are an agent that provides capital cities of countries."
      (Just "Answers questions about capital cities")
      []
      Nothing
```

## Agent Configuration Accessors

### `agentName`

Returns the agent's unique name.

**Signature**:
```haskell
agentName :: Agent -> Text
```

### `agentDescription`

Returns the agent's description (if provided).

**Signature**:
```haskell
agentDescription :: Agent -> Maybe Text
```

### `agentModel`

Returns the agent's configured model.

**Signature**:
```haskell
agentModel :: Agent -> Model
```

### `agentInstruction`

Returns the agent's instructions.

**Signature**:
```haskell
agentInstruction :: Agent -> Text
```

### `agentTools`

Returns the list of tools available to the agent.

**Signature**:
```haskell
agentTools :: Agent -> [Tool]
```

## Model Configuration

### `createModel`

Creates a model identifier for a specific provider.

**Signature**:
```haskell
createModel
  :: Text        -- modelId: Model identifier (e.g., "gpt-4", "gpt-3.5-turbo")
  -> LLMProvider -- provider: LLM provider
  -> Model
```

**Preconditions**:
- `modelId` must be non-empty
- `modelId` should be valid for the specified provider (runtime validation may occur during API calls)

**Example**:
```haskell
let model = createModel "gpt-4" OpenAI
```

## GenerateContentConfig

### `createGenerateContentConfig`

Creates LLM generation configuration.

**Signature**:
```haskell
createGenerateContentConfig
  -> Maybe Double  -- temperature: Sampling temperature (0.0-2.0)
  -> Maybe Int     -- maxTokens: Maximum response tokens
  -> Maybe Double  -- topP: Nucleus sampling (0.0-1.0)
  -> Maybe Int     -- topK: Top-K sampling
  -> GenerateContentConfig
```

**Preconditions**:
- `temperature` must be in range [0.0, 2.0] if present
- `maxTokens` must be positive if present
- `topP` must be in range [0.0, 1.0] if present
- `topK` must be positive if present

**Errors**:
- `ValidationError "Temperature must be between 0.0 and 2.0"`
- `ValidationError "maxTokens must be positive"`
- `ValidationError "topP must be between 0.0 and 1.0"`
- `ValidationError "topK must be positive"`

**Example**:
```haskell
let config = createGenerateContentConfig
      (Just 0.7)   -- temperature
      (Just 500)   -- maxTokens
      Nothing      -- topP
      Nothing      -- topK
```

## Notes

- All functions are pure (no side effects) except where noted
- Validation errors are returned as `Either AgentError a` or raised as exceptions depending on context
- Agent creation does not validate model availability (validation occurs during execution)
- Tool uniqueness is checked during agent creation, not tool creation
