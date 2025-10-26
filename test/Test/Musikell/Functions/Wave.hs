module Test.Musikell.Functions.Wave where

import Data.List (genericLength)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Musikell.Functions.Wave

import qualified Musikell.Types.Base as Unit

tests :: TestTree
tests =
    testGroup
        "Waveform Tests"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "wave at phase 0 and time 0 is 0"
            $ wave 440 0 0 @?= 0,
          testCase "wave at phase pi/2 and time 0 is 1"
            $ wave 440 (pi / 2) 0 @?= 1,
          testCase "wave is periodic with period 1/f"
            $ let f = 440
                  t = 0.001
                  p = 0
              in  abs (wave f p t - wave f p (t + 1 / f)) < 1e-6 @? "wave not periodic"
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ QC.testProperty "wave output always in [-1,1]"
            $ \f p t ->
                let f' = abs f + 1
                    x = wave f' p t
                in  x >= -1 && x <= 1,
          QC.testProperty "wavetable length matches number of time samples"
            $ \f p ->
                let f' = abs f + 1
                    duration = 1
                    nSamples = floor (duration * Unit.defaultSampleRate) + 1
                in  genericLength (wavetable f' p) == nSamples
        ]
