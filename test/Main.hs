module Main (main) where

import Test.Tasty
import qualified AgentTest
import qualified AgentIdentityTest

main :: IO ()
main = defaultMain $ testGroup "Pattern Agent Tests"
  [ testGroup "Unit Tests"
      [ AgentTest.tests
      ]
  , testGroup "Scenario Tests"
      [ AgentIdentityTest.tests
      ]
  ]
