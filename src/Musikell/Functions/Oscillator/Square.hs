module Musikell.Functions.Oscillator.Square where

import Musikell.Functions.Wave
import Musikell.Types.Base as Unit

square :: Unit.Hz -> Unit.Radian -> Unit.Second -> Unit.Meter
square freq phase time = if sin (2 * pi * freq * time + phase) >= 0 then 1 else -1

squareWavetable :: Unit.Hz -> Unit.Radian -> [Unit.Meter]
squareWavetable = wavetable square
