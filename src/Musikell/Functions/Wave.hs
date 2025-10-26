module Musikell.Functions.Wave (
    wavetable,
) where

import Musikell.Types.Base as Unit

wavetable ::
    (Unit.Hz -> Unit.Radian -> Unit.Second -> Unit.Meter) ->
    Unit.Hz ->
    Unit.Radian ->
    [Unit.Meter]
wavetable waveform freq phase = map (waveform freq phase) times
    where
        duration = 1
        times = [0, (1 / Unit.defaultSampleRate) .. duration]
