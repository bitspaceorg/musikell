module Musikell.Nodes.Base.In (
    NodeIn (..),
    NodeType (..),
) where

import Musikell.Nodes.Oscilator.Descriptor

import qualified Musikell.Types.Base as Unit

-- change it to data
newtype NodeType = OscilatorNode Oscilator
data NodeIn = NodeIn
    { nodeIn :: NodeType,
      amplitude :: Unit.Meter
    }
