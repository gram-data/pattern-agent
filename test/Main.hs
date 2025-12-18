module Main (main) where

import Test.Tasty
import System.Environment (lookupEnv)
import qualified AgentTest
import qualified AgentIdentityTest
import qualified ToolTest
import qualified ToolCreationTest
import qualified AgentToolAssociationTest
import qualified AgentCreationScenariosTest
import qualified ExecutionTest
import qualified ContextTest
import qualified MultiTurnToolConversationTest
import qualified MultiTurnConversationIntegrationTest
import qualified ZeroToolExecutionTest
import qualified OneToolExecutionTest

main :: IO ()
main = do
  -- Check if integration tests should be run
  -- Set INTEGRATION_TESTS=1 to enable integration tests (requires API key)
  integrationEnabled <- lookupEnv "INTEGRATION_TESTS"
  let runIntegration = integrationEnabled == Just "1"
  
  defaultMain $ testGroup "Pattern Agent Tests"
    [ testGroup "Unit Tests"
        [ AgentTest.tests
        , ToolTest.tests
        , ExecutionTest.tests
        , ContextTest.tests
        ]
    , testGroup "Scenario Tests"
        [ AgentIdentityTest.tests
        , ToolCreationTest.tests
        , AgentToolAssociationTest.tests
        , AgentCreationScenariosTest.tests
        , MultiTurnToolConversationTest.tests
        , ZeroToolExecutionTest.tests
        , OneToolExecutionTest.tests
        ]
    , if runIntegration
        then testGroup "Integration Tests (requires API key)"
            [ MultiTurnConversationIntegrationTest.tests
            ]
        else testGroup "Integration Tests (disabled - set INTEGRATION_TESTS=1)"
            []
    ]
