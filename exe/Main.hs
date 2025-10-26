module Main where

import Musikell.Nodes.Base.Base
import Musikell.Nodes.Base.In
import Musikell.Nodes.Base.Out
import Musikell.Nodes.Oscillator.Descriptor
import Musikell.Nodes.Oscillator.In

main :: IO ()
main = do
    putStrLn "Executing oscillator node A4 (440hz)"
    print $ wavetable $ playNode (OscillatorNode $ iOscillator Triangle 440 0) 1
