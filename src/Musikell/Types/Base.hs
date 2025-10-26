module Musikell.Types.Base (
    Hz,
    Meter,
    Second,
    Radian,
    AngularFrequency,
    defaultSampleRate,
) where

type Hz = Float
type Meter = Float
type Second = Float
type Radian = Float

type AngularFrequency = Radian

defaultSampleRate :: Hz
defaultSampleRate = 44100
