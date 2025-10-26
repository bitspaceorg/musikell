module Test.Musikell.Nodes.OscilatorNode where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Musikell.Nodes.Base.Out
import Musikell.Nodes.Oscilator.Base
import Musikell.Nodes.Oscilator.Descriptor
import Musikell.Nodes.Oscilator.In
import Musikell.Nodes.Oscilator.Out

import qualified Musikell.Functions.Wave as Wave
import qualified Musikell.Types.Base as Unit

instance QC.Arbitrary OscilatorType where
    arbitrary = QC.elements [Sine, Sqaure, SawTooth, Triangle]

tests :: TestTree
tests =
    testGroup
        "Oscilator Node Tests"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "playOscilator returns correct status"
            $ let osc = iOscilator Sine 440 0
                  nodeOut = playOscilator osc
                  status = case node nodeOut of
                    OscilatorOutNode (OscilatorOut s) -> s
              in  status @?= 1,
          testCase "iOscilatorOut creates correct OscilatorOut"
            $ let (OscilatorOut s1) = iOscilatorOut 5
                  (OscilatorOut s2) = OscilatorOut 5
              in  s1 @?= s2,
          testCase "getStatus extracts correct status"
            $ let nodeOutType = OscilatorOutNode (iOscilatorOut 10)
              in  getStatus nodeOutType @?= 10,
          testCase "iOscilator can construct Sqaure"
            $ let (Oscilator (OscilatorIn t _ _)) = iOscilator Sqaure 440 0
              in  t @?= Sqaure,
          testCase "iOscilator can construct SawTooth"
            $ let (Oscilator (OscilatorIn t _ _)) = iOscilator SawTooth 440 0
              in  t @?= SawTooth,
          testCase "iOscilator can construct Triangle"
            $ let (Oscilator (OscilatorIn t _ _)) = iOscilator Triangle 440 0
              in  t @?= Triangle
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ QC.testProperty "playOscilator generates correct wavetable"
            $ \f p ->
                let f' = abs f + 1
                    osc = iOscilator Sine f' p
                    nodeOut = playOscilator osc
                    expectedWavetable = Wave.wavetable f' p
                in  wavetable nodeOut == expectedWavetable,
          QC.testProperty "iOscilator constructs with correct parameters"
            $ \t f p ->
                let (Oscilator (OscilatorIn t' f' p')) = iOscilator t f p
                in  t == t' && f == f' && p == p',
          QC.testProperty "iOscilatorOut constructs with correct status"
            $ \s ->
                let (OscilatorOut s1) = iOscilatorOut s
                    (OscilatorOut s2) = OscilatorOut s
                in  s1 == s2,
          QC.testProperty "getStatus matches iOscilatorOut"
            $ \s -> getStatus (OscilatorOutNode (iOscilatorOut s)) == s
        ]
