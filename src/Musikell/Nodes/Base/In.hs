module Musikell.Nodes.Base.In (
    NodeIn (..),
    NodeType (..),
) where

import Musikell.Nodes.Oscillator.Descriptor

import qualified Musikell.Types.Base as Unit

-- change it to data
newtype NodeType = OscillatorNode Oscillator
data NodeIn = NodeIn
    { nodeIn :: NodeType,
      amplitude :: Unit.Meter
    }
