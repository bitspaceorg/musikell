module Main where

import Test.Tasty

import qualified Test.Musikell.Functions.Base as FunctionTests
import qualified Test.Musikell.Nodes.Base as NodeTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "All Tests"
        [ FunctionTests.tests,
          NodeTests.tests
        ]
