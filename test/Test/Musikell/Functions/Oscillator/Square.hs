module Test.Musikell.Functions.Oscillator.Square where

import Data.List (genericLength)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Musikell.Functions.Oscillator.Square as Square
import Musikell.Functions.Wave (wavetable)

import qualified Musikell.Types.Base as Unit

tests :: TestTree
tests =
    testGroup
        "Square wave tests"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "square output at time 0 is ±1"
            $ let x = Square.square 440 0 0
              in  (x == 1 || x == -1) @? "square not ±1"
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ QC.testProperty "square output in [-1,1]"
            $ \f p t ->
                let freq = abs f + 1
                    t' = abs t
                    x = Square.square freq p t'
                in  x >= -1 && x <= 1,
          QC.testProperty "square wavetable length matches sample count"
            $ \f p ->
                let freq = abs f + 1
                    nSamples = floor (1 * Unit.defaultSampleRate) + 1
                in  genericLength (wavetable Square.square freq p) == nSamples
        ]
