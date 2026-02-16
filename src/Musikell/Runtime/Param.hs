-- File: src/Musikell/Runtime/Param.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/param.mdx
-- Module: Musikell.Runtime.Param
{-# LANGUAGE BangPatterns #-}

-- | Per-sample parameter smoothing via linear ramp interpolation.
--
-- When a parameter changes (e.g. gain knob moved), the ramp state
-- interpolates linearly from the current value to the target over a
-- configurable number of samples.  This eliminates clicks/pops.
module Musikell.Runtime.Param (
    -- * Ramp State
    RampState (..),
    initRamp,
    setTarget,
    advanceSample,
    advanceBlock,
    isRamping,
) where

import Data.IORef

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Musikell.Core.Types (BlockSize)

-- | Linear ramp interpolation state for one parameter.
data RampState = RampState
    { -- | Current interpolated value
      rsCurrent :: {-# UNPACK #-} !Float,
      -- | Target value
      rsTarget :: {-# UNPACK #-} !Float,
      -- | Per-sample increment
      rsStep :: {-# UNPACK #-} !Float,
      -- | Samples left in ramp
      rsRemaining :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Show)

-- | Create a static (non-ramping) state at the given value.
initRamp :: Float -> RampState
initRamp val = RampState val val 0.0 0

-- | Set a new target with ramp duration in samples.
-- If rampLen <= 0, the jump is instantaneous.
setTarget :: Float -> Int -> RampState -> RampState
setTarget target rampLen st
    | rampLen <= 0 = RampState target target 0.0 0
    | otherwise =
        let step = (target - rsCurrent st) / fromIntegral rampLen
        in  RampState (rsCurrent st) target step rampLen

-- | Advance one sample, returning the interpolated value and new state.
advanceSample :: RampState -> (Float, RampState)
advanceSample st
    | rsRemaining st <= 0 = (rsTarget st, st {rsCurrent = rsTarget st})
    | rsRemaining st == 1 =
        ( rsTarget st,
          st
            { rsCurrent = rsTarget st,
              rsStep = 0.0,
              rsRemaining = 0
            }
        )
    | otherwise =
        let !newVal = rsCurrent st + rsStep st
        in  (newVal, st {rsCurrent = newVal, rsRemaining = rsRemaining st - 1})
{-# INLINE advanceSample #-}

-- | Pre-compute the ramp for an entire block.
-- Returns a storable vector of interpolated values and the final state.
advanceBlock :: RampState -> BlockSize -> IO (V.Vector Float, RampState)
advanceBlock st blockSize
    | rsRemaining st <= 0 = do
        -- No ramping — return constant vector
        vec <- MV.replicate blockSize (rsTarget st)
        frozen <- V.unsafeFreeze vec
        pure (frozen, st {rsCurrent = rsTarget st})
    | otherwise = do
        vec <- MV.new blockSize
        finalState <- go vec st 0
        frozen <- V.unsafeFreeze vec
        pure (frozen, finalState)
    where
        go vec !s !i
            | i >= blockSize = pure s
            | otherwise = do
                let (!val, !s') = advanceSample s
                MV.unsafeWrite vec i val
                go vec s' (i + 1)

-- | Check if the ramp is still active.
isRamping :: RampState -> Bool
isRamping st = rsRemaining st > 0
