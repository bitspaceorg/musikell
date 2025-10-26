module Test.Musikell.Functions.Oscillator.Triangle where

import Data.List (genericLength)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Musikell.Functions.Oscillator.Triangle as Triangle
import Musikell.Functions.Wave (wavetable)

import qualified Musikell.Types.Base as Unit

tests :: TestTree
tests =
    testGroup
        "Triangle wave tests"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "triangle output at time 0 is between -1 and 1"
            $ let x = Triangle.triangle 440 0 0
              in  x >= -1 && x <= 1 @? "triangle out of bounds"
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ QC.testProperty "triangle output in [-1,1]"
            $ \f p t ->
                let freq = abs f + 1
                    t' = abs t
                    x = Triangle.triangle freq p t'
                in  x >= -1 && x <= 1,
          QC.testProperty "triangle wavetable length matches sample count"
            $ \f p ->
                let freq = abs f + 1
                    nSamples = floor (1 * Unit.defaultSampleRate) + 1
                in  genericLength (wavetable Triangle.triangle freq p) == nSamples
        ]
