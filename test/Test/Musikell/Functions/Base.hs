module Test.Musikell.Functions.Base where

import Test.Tasty

import qualified Test.Musikell.Functions.Wave as WaveTests

tests :: TestTree
tests =
    testGroup
        "Function tests"
        [ WaveTests.tests
        ]
