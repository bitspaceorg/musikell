module Test.Musikell.Functions.Oscillator.Sine where

import Data.List (genericLength)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Musikell.Functions.Oscillator.Sine as Sine
import Musikell.Functions.Wave (wavetable)

import qualified Musikell.Types.Base as Unit

tests :: TestTree
tests =
    testGroup
        "Sine wave tests"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "sine wave at phase 0 and time 0 is 0"
            $ Sine.sine 440 0 0 @?= 0,
          testCase "sine wave at phase pi/2 and time 0 is 1"
            $ Sine.sine 440 (pi / 2) 0 @?= 1,
          testCase "sine wave is periodic with period 1/f"
            $ let f = 440
                  t = 0.001
                  p = 0
              in  abs (Sine.sine f p t - Sine.sine f p (t + 1 / f)) < 1e-6 @? "sine not periodic"
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ QC.testProperty "sine output in [-1,1]"
            $ \f p t ->
                let freq = abs f + 1
                    t' = abs t
                    x = Sine.sine freq p t'
                in  x >= -1 && x <= 1,
          QC.testProperty "sine wavetable length matches sample count"
            $ \f p ->
                let freq = abs f + 1
                    nSamples = floor (1 * Unit.defaultSampleRate) + 1
                in  genericLength (wavetable Sine.sine freq p) == nSamples
        ]
