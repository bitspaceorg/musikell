module Test.Musikell.Nodes.Base where

import Test.Tasty

import qualified Test.Musikell.Nodes.BaseNode as BaseTests
import qualified Test.Musikell.Nodes.OscilatorNode as OscilatorTests

tests :: TestTree
tests =
    testGroup
        "Node tests"
        [ BaseTests.tests,
          OscilatorTests.tests
        ]
