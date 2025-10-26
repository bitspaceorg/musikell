module Musikell.Functions.Base (
    angularFrequency,
) where

import qualified Musikell.Types.Base as Unit

angularFrequency :: Unit.Hz -> Unit.AngularFrequency
angularFrequency = (*) ((*) 2 pi)
