module Musikell.Nodes.Oscillator.In (
    OscillatorIn (..),
    OscillatorType (..),
) where

import qualified Musikell.Types.Base as Unit

data OscillatorType = Sine | Square | SawTooth | Triangle
    deriving (Enum, Eq, Show)

data OscillatorIn = OscillatorIn
    { _type :: OscillatorType,
      frequency :: Unit.Hz,
      phase :: Unit.Radian
    }
    deriving (Show)
