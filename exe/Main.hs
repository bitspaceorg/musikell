module Main where

import Musikell.Nodes.Base.Base
import Musikell.Nodes.Base.In
import Musikell.Nodes.Base.Out
import Musikell.Nodes.Oscilator.Descriptor
import Musikell.Nodes.Oscilator.In

main :: IO ()
main = do
    putStrLn "Executing oscillator node A4 (440hz)"
    print $ playNode (OscilatorNode $ iOscilator Sine 440 0) 10
