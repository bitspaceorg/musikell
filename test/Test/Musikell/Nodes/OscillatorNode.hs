module Test.Musikell.Nodes.OscillatorNode where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Musikell.Nodes.Base.Out
import Musikell.Nodes.Oscillator.Base
import Musikell.Nodes.Oscillator.Descriptor
import Musikell.Nodes.Oscillator.In
import Musikell.Nodes.Oscillator.Out

import qualified Musikell.Functions.Wave as Wave
import qualified Musikell.Types.Base as Unit

instance QC.Arbitrary OscillatorType where
    arbitrary = QC.elements [Sine, Square, SawTooth, Triangle]

tests :: TestTree
tests =
    testGroup
        "Oscillator Node Tests"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "playOscillator returns correct status"
            $ let osc = iOscillator Sine 440 0
                  nodeOut = playOscillator osc
                  status = case node nodeOut of
                    OscillatorOutNode (OscillatorOut s) -> s
              in  status @?= 1,
          testCase "iOscillatorOut creates correct OscillatorOut"
            $ let (OscillatorOut s1) = iOscillatorOut 5
                  (OscillatorOut s2) = OscillatorOut 5
              in  s1 @?= s2,
          testCase "getStatus extracts correct status"
            $ let nodeOutType = OscillatorOutNode (iOscillatorOut 10)
              in  getStatus nodeOutType @?= 10,
          testCase "iOscillator can construct Square"
            $ let (Oscillator (OscillatorIn t _ _)) = iOscillator Square 440 0
              in  t @?= Square,
          testCase "iOscillator can construct SawTooth"
            $ let (Oscillator (OscillatorIn t _ _)) = iOscillator SawTooth 440 0
              in  t @?= SawTooth,
          testCase "iOscillator can construct Triangle"
            $ let (Oscillator (OscillatorIn t _ _)) = iOscillator Triangle 440 0
              in  t @?= Triangle
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ QC.testProperty "playOscillator generates correct wavetable for all types"
            $ \f p ->
                let f' = abs f + 1
                    testType t =
                        let osc = iOscillator t f' p
                            nodeOut = playOscillator osc
                        in  wavetable nodeOut == oscillate t f' p
                in  all testType [Sine, Square, Triangle, SawTooth],
          QC.testProperty "iOscillator constructs with correct parameters"
            $ \t f p ->
                let (Oscillator (OscillatorIn t' f' p')) = iOscillator t f p
                in  t == t' && f == f' && p == p',
          QC.testProperty "iOscillatorOut constructs with correct status"
            $ \s ->
                let (OscillatorOut s1) = iOscillatorOut s
                    (OscillatorOut s2) = OscillatorOut s
                in  s1 == s2,
          QC.testProperty "getStatus matches iOscillatorOut"
            $ \s -> getStatus (OscillatorOutNode (iOscillatorOut s)) == s
        ]
