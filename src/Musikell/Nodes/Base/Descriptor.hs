module Musikell.Nodes.Base.Descriptor (
    iNodeOut,
) where

import Musikell.Nodes.Base.In
import Musikell.Nodes.Base.Out

import qualified Musikell.Types.Base as Unit

iNodeOut :: [Unit.Meter] -> NodeOutType -> NodeOut
iNodeOut = NodeOut
