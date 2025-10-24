module Musikell.Nodes.Oscilator.Descriptor (
    Oscilator (..),
    iOscilator,
    iOscilatorOut,
    getStatus,
) where

import Musikell.Functions.Wave
import Musikell.Nodes.Base.Out
import Musikell.Nodes.Oscilator.In
import Musikell.Nodes.Oscilator.Out

import qualified Musikell.Types.Base as Unit

-- change it to data
newtype Oscilator = Oscilator OscilatorIn deriving (Show)

iOscilator :: OscilatorType -> Unit.Hz -> Unit.Radian -> Oscilator
iOscilator t f r = Oscilator (OscilatorIn t f r)

iOscilatorOut :: Int -> OscilatorOut
iOscilatorOut = OscilatorOut

getStatus :: NodeOutType -> Int
getStatus (OscilatorOutNode (OscilatorOut status)) = status
