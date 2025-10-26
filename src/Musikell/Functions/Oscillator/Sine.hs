module Musikell.Functions.Oscillator.Sine where

import Musikell.Functions.Wave
import Musikell.Types.Base as Unit

sine :: Unit.Hz -> Unit.Radian -> Unit.Second -> Unit.Meter
sine freq phase time = sin (2 * pi * freq * time + phase)

sineWavetable :: Unit.Hz -> Unit.Radian -> [Unit.Meter]
sineWavetable = wavetable sine
