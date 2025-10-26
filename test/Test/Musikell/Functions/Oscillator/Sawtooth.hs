module Test.Musikell.Functions.Oscillator.Sawtooth where

import Data.List (genericLength)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Musikell.Functions.Oscillator.Sawtooth as Sawtooth
import Musikell.Functions.Wave (wavetable)

import qualified Musikell.Types.Base as Unit

tests :: TestTree
tests =
    testGroup
        "Sawtooth wave tests"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "sawtooth output at time 0 is between -1 and 1"
            $ let x = Sawtooth.sawtooth 440 0 0
              in  x >= -1 && x <= 1 @? "sawtooth out of bounds"
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ QC.testProperty "sawtooth output in [-1,1]"
            $ \f p t ->
                let freq = abs f + 1
                    t' = abs t
                    x = Sawtooth.sawtooth freq p t'
                in  x >= -1 && x <= 1,
          QC.testProperty "sawtooth wavetable length matches sample count"
            $ \f p ->
                let freq = abs f + 1
                    nSamples = floor (1 * Unit.defaultSampleRate) + 1
                in  genericLength (wavetable Sawtooth.sawtooth freq p) == nSamples
        ]
