module Musikell.Nodes.Oscilator.In (
    OscilatorIn (..),
    OscilatorType (..),
) where

import qualified Musikell.Types.Base as Unit

data OscilatorType = Sine | Sqaure | SawTooth | Triangle
    deriving (Enum, Eq, Show)

data OscilatorIn = OscilatorIn
    { _type :: OscilatorType,
      frequency :: Unit.Hz,
      phase :: Unit.Radian
    }
    deriving (Show)
