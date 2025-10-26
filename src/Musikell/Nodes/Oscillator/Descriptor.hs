module Musikell.Nodes.Oscillator.Descriptor (
    Oscillator (..),
    iOscillator,
    iOscillatorOut,
    getStatus,
) where

import Musikell.Functions.Wave
import Musikell.Nodes.Base.Out
import Musikell.Nodes.Oscillator.In
import Musikell.Nodes.Oscillator.Out

import qualified Musikell.Types.Base as Unit

-- change it to data
newtype Oscillator = Oscillator OscillatorIn deriving (Show)

iOscillator :: OscillatorType -> Unit.Hz -> Unit.Radian -> Oscillator
iOscillator t f r = Oscillator (OscillatorIn t f r)

iOscillatorOut :: Int -> OscillatorOut
iOscillatorOut = OscillatorOut

getStatus :: NodeOutType -> Int
getStatus (OscillatorOutNode (OscillatorOut status)) = status
