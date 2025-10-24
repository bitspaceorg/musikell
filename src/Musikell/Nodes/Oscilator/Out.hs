module Musikell.Nodes.Oscilator.Out (
    OscilatorOut (..),
) where

-- change it to data
newtype OscilatorOut = OscilatorOut
    { status :: Int
    }
    deriving (Show)
