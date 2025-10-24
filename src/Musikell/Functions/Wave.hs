module Musikell.Functions.Wave (
    wavetable,
) where

import Musikell.Functions.Base

import qualified Musikell.Types.Base as Unit

wave :: Unit.Hz -> Unit.Radian -> Unit.Second -> Unit.Meter
wave frequency phase time = sin (angularFrequency frequency * time + phase)

wavetable :: Unit.Hz -> Unit.Radian -> [Unit.Meter]
wavetable frequency phase = map waveform times
    where
        duration = 1
        times = [0, (1 / defaultSampleRate) .. duration]
        waveform = wave frequency phase
