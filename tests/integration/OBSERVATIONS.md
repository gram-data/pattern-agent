# Integration Test Observations

**Date**: 2025-01-27  
**Purpose**: Document observed behavior from real LLM API integration tests to inform mock LLM design

## Test Execution Status

✅ **All tests passing**  
✅ **Tool binding fixed** - `extractTypeSignatureFromPattern` implementation completed

## Observed LLM Behavior

### Test 1: Multi-Turn Conversation with Name Extraction ✅

**Scenario**:
1. Turn 1: User says "My name is Bob"
2. Turn 2: User says "the weather is nice today"
3. Turn 3: User says "oh, hello btw"

**Observed Behavior**:
- ✅ **Agent successfully extracted 'Bob' from conversation history**
- ✅ Tool `sayHello` was called with `personName="Bob"` (extracted from Turn 1)
- ✅ Tool result: `"Hello, Bob! Nice to meet you."`
- ✅ Final response: `"Hello! How can I help you today, Bob?"`

**Key Observations**:
1. **Conversation history works**: The LLM successfully extracted "Bob" from the first message even though it wasn't mentioned in the third message
2. **Tool call format**: The LLM called `sayHello` with the correct parameter name `personName`
3. **Parameter extraction**: The LLM was able to extract the name from natural language ("My name is Bob")
4. **Response integration**: The agent incorporated both the tool result and the extracted name into the final response

### Test 2: Simple Greeting with Tool Call ✅

**Scenario**: User says "Hello!"

**Observed Behavior**:
- ✅ Tool `sayHello` was called with `personName="NOT_FOUND"` (no name provided, defaulted)
- ✅ Tool result: `"Hello, world! Nice to meet you."` (used default value)
- ✅ Final response: `"Hello! Nice to meet you. How can I assist you today?"`

**Key Observations**:
1. **Tool call on direct greeting**: The LLM recognized "Hello!" as a greeting and called the tool
2. **Default parameter handling**: When no name was provided, the tool used the default value "world"
3. **Response generation**: The agent generated a friendly response incorporating the tool result

## Tool Call Patterns Observed

### Tool Call Format
- Tool name: `sayHello`
- Parameter format: JSON object with parameter name as key
  - Example: `{"personName": "Bob"}`
- Parameter extraction: LLM extracts values from conversation history

### Tool Call Decision Making
- LLM calls tools when:
  - User sends a greeting (even without explicit name)
  - Context suggests tool usage is appropriate
- LLM does NOT call tools when:
  - Message is general conversation (Turn 2: "the weather is nice today")
  - Tool usage isn't contextually appropriate

### Conversation History Usage
- LLM successfully extracts information from earlier messages
- Information persists across multiple turns
- LLM can reference information from any point in the conversation

## Schema Matching

**Fixed Issue**: Tool schema was empty because `extractTypeSignatureFromPattern` was not implemented.

**Solution**: Implemented `extractTypeSignatureFromPattern` to extract type signature from Pattern structure:
- Extracts FunctionType pattern with source and target nodes
- Extracts parameter info (name, type, default) from source node
- Extracts return type from target node
- Converts to TypeSignature and generates JSON schema

**Result**: Tool schema now matches Impl schema:
```json
{
  "type": "object",
  "properties": {
    "personName": {
      "type": "string",
      "default": "world"
    }
  },
  "required": []
}
```

## Mock LLM Design Implications

Based on these observations, a mock LLM should:

1. **Tool Call Detection**:
   - Detect greetings (keywords: "hello", "hi", "greetings")
   - Extract names from patterns like "My name is X" or "I'm X"
   - Remember extracted information across conversation turns

2. **Tool Call Format**:
   - Call tools with JSON object parameters
   - Use parameter names matching tool schema
   - Extract values from conversation history when available

3. **Response Generation**:
   - Incorporate tool results into responses
   - Reference extracted information (like names) in responses
   - Generate natural, conversational responses

4. **Conversation History**:
   - Maintain full conversation history
   - Extract information from any point in history
   - Use extracted information in tool calls and responses

## Next Steps

1. ✅ Create failing unit test for bindTool - **DONE**
2. ✅ Fix tool binding issue - **DONE** (implemented `extractTypeSignatureFromPattern`)
3. ✅ Run integration tests - **DONE**
4. ✅ Document observations - **DONE**
5. ⏭️ Create mock LLM based on observed behavior
6. ⏭️ Use mock LLM for faster unit tests
