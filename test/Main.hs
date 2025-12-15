module Main (main) where

import Test.Tasty
import qualified AgentTest
import qualified AgentIdentityTest
import qualified ToolTest
import qualified ToolCreationTest
import qualified AgentToolAssociationTest
import qualified AgentCreationScenariosTest

main :: IO ()
main = defaultMain $ testGroup "Pattern Agent Tests"
  [ testGroup "Unit Tests"
      [ AgentTest.tests
      , ToolTest.tests
      ]
  , testGroup "Scenario Tests"
      [ AgentIdentityTest.tests
      , ToolCreationTest.tests
      , AgentToolAssociationTest.tests
      , AgentCreationScenariosTest.tests
      ]
  ]
