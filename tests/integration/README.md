# Integration Tests

Integration tests use a **real LLM API** to observe actual behavior. These tests help us understand how the LLM behaves in practice, which informs the design of mock LLMs for faster unit testing.

## Running Integration Tests

Integration tests require a valid API key and are disabled by default to avoid accidental API usage.

### Prerequisites

1. Set your OpenAI API key:
   ```bash
   export OPENAI_API_KEY=your_key_here
   ```

2. Enable integration tests:
   ```bash
   export INTEGRATION_TESTS=1
   ```

3. Run tests:
   ```bash
   cabal test
   ```

   Or run only integration tests:
   ```bash
   cabal test --test-option="--pattern=Integration"
   ```

## Test Purpose

These tests observe real LLM behavior to:

1. **Understand tool call patterns**: How does the LLM decide when to call tools?
2. **Observe conversation history usage**: How does the LLM extract information from earlier messages?
3. **Document actual responses**: What do real responses look like?
4. **Inform mock design**: Use observed behavior to create accurate mocks

## Current Tests

- **Multi-turn conversation with name extraction**: Tests if the agent can remember a user's name from an earlier message and use it in a tool call
- **Simple greeting with tool call**: Tests basic tool call flow with a direct greeting

## Observations

The tests print observations to stdout to help document real LLM behavior. These observations will be used to design mock LLMs that accurately simulate the real API.

