module Musikell.Nodes.Oscilator.Base (
    playOscilator,
) where

import Musikell.Functions.Wave
import Musikell.Nodes.Base.Descriptor
import Musikell.Nodes.Base.Out hiding (wavetable)
import Musikell.Nodes.Oscilator.Descriptor
import Musikell.Nodes.Oscilator.In

playOscilator :: Oscilator -> NodeOut
playOscilator (Oscilator (OscilatorIn _ frequency phase)) = iNodeOut table (OscilatorOutNode oscilatorOut)
    where
        table = wavetable frequency phase
        oscilatorOut = iOscilatorOut 1
