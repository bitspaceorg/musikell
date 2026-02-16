-- File: src/Musikell/Runtime/Kernel/Builtin/Crossfade.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/crossfade.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Crossfade
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Crossfade kernel for artifact-free route switching.
module Musikell.Runtime.Kernel.Builtin.Crossfade (
    mkCrossfade,
) where

import Data.IORef

import Musikell.Core.Types (BlockSize, KernelId (..))
import Musikell.Memory.Buffer (bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (forRange_)

-- | Crossfade state.
data CrossfadeState = CrossfadeState
    { cfPosition :: !Int,
      cfFadeLen :: !Int,
      cfActive :: !Bool
    }

-- | Create a crossfade kernel that blends between two inputs.
--   Input 0 = source A (fading out), Input 1 = source B (fading in).
--   When no crossfade is active, passes input 1 through unchanged.
mkCrossfade :: BlockSize -> IO KernelSpec
mkCrossfade fadeLen = do
    stateRef <- newIORef (CrossfadeState 0 fadeLen False)
    pure
        $ KernelSpec
            { kernelId = KernelId "crossfade",
              kernelInputs = 2,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (a : b : _, o : _) -> do
                    cfState <- readIORef stateRef
                    let n = bufferSize o
                    if not (cfActive cfState) then forRange_ n $ \idx -> do
                        s <- unsafeReadSample b idx
                        unsafeWriteSample o idx s
                    else do
                        let !startPos = cfPosition cfState
                            !fLen = cfFadeLen cfState
                        forRange_ n $ \idx -> do
                            let !pos = startPos + idx
                                !t =
                                    if pos >= fLen then
                                        1.0 :: Float
                                    else
                                        fromIntegral pos / fromIntegral fLen
                            sa <- unsafeReadSample a idx
                            sb <- unsafeReadSample b idx
                            unsafeWriteSample o idx (sa * (1.0 - t) + sb * t)
                        let newPos = startPos + n
                            done = newPos >= fLen
                        writeIORef stateRef
                            $! cfState
                                { cfPosition = if done then 0 else newPos,
                                  cfActive = not done
                                }
                _ -> pure ()
            }
