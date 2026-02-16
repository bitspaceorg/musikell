-- File: src/Musikell/Runtime/Kernel/Builtin/Gain.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/gain.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Gain
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Gain kernel — multiply every sample by a constant.
module Musikell.Runtime.Kernel.Builtin.Gain (
    gainKernel,
) where

import Musikell.Core.Types (KernelId (..))
import Musikell.Memory.Buffer (bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (forRange_)

-- | Multiply every sample by a constant.
gainKernel :: Double -> KernelSpec
gainKernel level =
    KernelSpec
        { kernelId = KernelId "gain",
          kernelInputs = 1,
          kernelOutputs = 1,
          kernelExecute = \ins outs -> case (ins, outs) of
            (i : _, o : _) -> do
                let !g = realToFrac level :: Float
                    n = bufferSize i
                forRange_ n $ \idx -> do
                    s <- unsafeReadSample i idx
                    unsafeWriteSample o idx (s * g)
            _ -> pure ()
        }
