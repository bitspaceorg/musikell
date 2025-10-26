module Musikell.Functions.Oscillator.Sawtooth where

import Musikell.Functions.Wave
import Musikell.Types.Base as Unit

fractional :: Float -> Float
fractional x = x - fromIntegral (floor x)

sawtooth :: Unit.Hz -> Unit.Radian -> Unit.Second -> Unit.Meter
sawtooth freq phase time = 2 * fractional (freq * time + phase / (2 * pi)) - 1

sawtoothWavetable :: Unit.Hz -> Unit.Radian -> [Unit.Meter]
sawtoothWavetable = wavetable sawtooth
