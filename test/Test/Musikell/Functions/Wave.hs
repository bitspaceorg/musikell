module Test.Musikell.Functions.Wave where

import Test.Musikell.Functions.Oscillator.Sawtooth as SawtoothTests
import Test.Musikell.Functions.Oscillator.Sine as SineTests
import Test.Musikell.Functions.Oscillator.Square as SquareTests
import Test.Musikell.Functions.Oscillator.Triangle as TriangleTests
import Test.Tasty

tests :: TestTree
tests =
    testGroup
        "Waveform Tests"
        [ SineTests.tests,
          SquareTests.tests,
          TriangleTests.tests,
          SawtoothTests.tests
        ]
