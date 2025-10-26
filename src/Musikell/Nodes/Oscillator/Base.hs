module Musikell.Nodes.Oscillator.Base (
    playOscillator,
    Oscillatable (..),
) where

import Musikell.Functions.Oscillator.Base
import Musikell.Nodes.Base.Descriptor
import Musikell.Nodes.Base.Out hiding (wavetable)
import Musikell.Nodes.Oscillator.Descriptor
import Musikell.Nodes.Oscillator.In

import qualified Musikell.Types.Base as Unit

class Oscillatable a where
    oscillate :: a -> Unit.Hz -> Unit.Radian -> [Unit.Meter]

instance Oscillatable OscillatorType where
    oscillate Sine = sineWavetable
    oscillate Square = squareWavetable
    oscillate SawTooth = sawtoothWavetable
    oscillate Triangle = triangleWavetable

playOscillator :: Oscillator -> NodeOut
playOscillator (Oscillator (OscillatorIn _type frequency phase)) = iNodeOut table (OscillatorOutNode oscillatorOut)
    where
        table = oscillate _type frequency phase
        oscillatorOut = iOscillatorOut 1
