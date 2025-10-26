module Test.Musikell.Nodes.BaseNode where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Musikell.Nodes.Base.Base
import Musikell.Nodes.Base.In
import Musikell.Nodes.Base.Out
import Musikell.Nodes.Oscilator.Descriptor
import Musikell.Nodes.Oscilator.In

import qualified Musikell.Types.Base as Unit

tests :: TestTree
tests =
    testGroup
        "Base Node Tests"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "playNode with amplitude 0 gives wavetable of 0s"
            $ let node = OscilatorNode $ iOscilator Sine 440 0
                  nodeOut = playNode node 0
              in  all (== 0) (wavetable nodeOut) @? "wavetable not all 0s",
          testCase "playNode produces a sine wave that peaks at the correct time"
            $ let frequency = 1
                  amplitude = 1
                  node = OscilatorNode $ iOscilator Sine frequency 0
                  nodeOut = playNode node amplitude
                  sampleRate = Unit.defaultSampleRate
                  peakIndex = round (sampleRate / (4 * frequency))
                  peakValue = wavetable nodeOut !! peakIndex
              in  abs (peakValue - amplitude) < 1e-6 @? "sine wave does not peak at the correct value"
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ QC.testProperty "wavetable values are within [-amplitude, amplitude]"
            $ \f p amp ->
                let f' = abs f + 1
                    amp' = abs amp
                    node = OscilatorNode $ iOscilator Sine f' p
                    nodeOut = playNode node amp'
                in  all (\x -> x >= -amp' && x <= amp') (wavetable nodeOut)
        ]
