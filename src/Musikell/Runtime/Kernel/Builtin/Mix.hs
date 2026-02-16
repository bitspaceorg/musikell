-- File: src/Musikell/Runtime/Kernel/Builtin/Mix.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/mix.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Mix
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Mix and Clip kernels.
module Musikell.Runtime.Kernel.Builtin.Mix (
    mixKernel,
    clipKernel,
) where

import Musikell.Core.Types (KernelId (..))
import Musikell.Memory.Buffer (bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (forRange_)

-- | Sum all inputs sample-by-sample.
mixKernel :: KernelSpec
mixKernel =
    KernelSpec
        { kernelId = KernelId "mix",
          kernelInputs = 2,
          kernelOutputs = 1,
          kernelExecute = \ins outs -> case outs of
            (o : _) -> do
                let n = bufferSize o
                forRange_ n $ \idx -> unsafeWriteSample o idx 0.0
                mapM_
                    ( \i -> forRange_ n $ \idx -> do
                        s <- unsafeReadSample i idx
                        cur <- unsafeReadSample o idx
                        unsafeWriteSample o idx (cur + s)
                    )
                    ins
            _ -> pure ()
        }

-- | Hard clip to [lo, hi].
clipKernel :: Double -> Double -> KernelSpec
clipKernel lo hi =
    KernelSpec
        { kernelId = KernelId "clip",
          kernelInputs = 1,
          kernelOutputs = 1,
          kernelExecute = \ins outs -> case (ins, outs) of
            (i : _, o : _) -> do
                let !flo = realToFrac lo :: Float
                    !fhi = realToFrac hi :: Float
                    n = bufferSize i
                forRange_ n $ \idx -> do
                    s <- unsafeReadSample i idx
                    unsafeWriteSample o idx (max flo (min fhi s))
            _ -> pure ()
        }
