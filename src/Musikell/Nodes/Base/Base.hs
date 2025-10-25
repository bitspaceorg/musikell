module Musikell.Nodes.Base.Base (
    Playable (..),
    playNode,
) where

import Musikell.Nodes.Base.In
import Musikell.Nodes.Base.Out
import Musikell.Nodes.Oscilator.Base

import qualified Musikell.Types.Base as Unit

-- The Nodes don't play themselves they only generate the Out params.
-- Until we get a fitting name we will be sticking with `Playable` & `play`.
class Playable a where
    play :: a -> NodeOut

instance Playable NodeType where
    play (OscilatorNode oscilator) = playOscilator oscilator

-- Usage: playNode (OscilatorNode $ iOscilator Sine 440 0) 10
playNode :: NodeType -> Unit.Meter -> NodeOut
playNode node amplitude = nodeOut
    where
        nodeRes = play node
        maxWave = maximum (map abs (wavetable nodeRes))
        normalise x = if maxWave > 1.0 then x / maxWave else x
        wavetableAmp = map (normalise . (amplitude *)) (wavetable nodeRes)
        nodeOut = nodeRes {wavetable = wavetableAmp}

-- For reference - should clean later
-- playNode (OscilatorNode iOscilator ....) 10
-- -> NodeIn {
-- 		amplitude :: Unit.Meter
-- 		node :: OscilatorIn
-- }
-- or
-- -> NodeIn {
-- 		amplitude :: Unit.Meter
-- 		node :: OscilatorIn {
-- 			wavetable :: [ Unit.Meter ]
-- 	}
-- }
-- -> NodeOut {
-- 		wavetable :: [ Unit.Meter ]
-- 		node :: OscilatorOut
-- }
