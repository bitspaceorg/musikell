module Musikell.Functions.Oscillator.Triangle where

import Musikell.Functions.Wave
import Musikell.Types.Base as Unit

fractional :: Float -> Float
fractional x = x - fromIntegral (floor x)

triangle :: Unit.Hz -> Unit.Radian -> Unit.Second -> Unit.Meter
triangle freq phase time = 4 * abs (fractional (freq * time + phase / (2 * pi)) - 0.5) - 1

triangleWavetable :: Unit.Hz -> Unit.Radian -> [Unit.Meter]
triangleWavetable = wavetable triangle
