module Musikell.Nodes.Base.Out (
    NodeOut (..),
    NodeOutType (..),
) where

import Musikell.Nodes.Oscilator.Out

import qualified Musikell.Types.Base as Unit

-- change it to data
newtype NodeOutType = OscilatorOutNode OscilatorOut deriving (Show)
data NodeOut = NodeOut
    { wavetable :: [Unit.Meter],
      node :: NodeOutType
    }
    deriving (Show)
