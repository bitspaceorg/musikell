module Musikell.Nodes.Base.Out (
    NodeOut (..),
    NodeOutType (..),
) where

import Musikell.Nodes.Oscillator.Out

import qualified Musikell.Types.Base as Unit

-- change it to data
newtype NodeOutType = OscillatorOutNode OscillatorOut deriving (Show)
data NodeOut = NodeOut
    { wavetable :: [Unit.Meter],
      node :: NodeOutType
    }
    deriving (Show)
