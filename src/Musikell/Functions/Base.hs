module Musikell.Functions.Base (
    angularFrequency,
    defaultSampleRate,
) where

import qualified Musikell.Types.Base as Unit

defaultSampleRate :: Unit.Hz
defaultSampleRate = 44100

angularFrequency :: Unit.Hz -> Unit.AngularFrequency
angularFrequency = (*) ((*) 2 pi)
