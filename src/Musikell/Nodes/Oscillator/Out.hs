module Musikell.Nodes.Oscillator.Out (
    OscillatorOut (..),
) where

-- change it to data
newtype OscillatorOut = OscillatorOut
    { status :: Int
    }
    deriving (Show)
