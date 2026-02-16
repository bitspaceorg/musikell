-- File: src/Musikell/Runtime/Kernel/Builtin/Channel.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/channel.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Channel
{-# LANGUAGE OverloadedStrings #-}

-- | Channel utility kernels: split, merge, mono-to-stereo.
module Musikell.Runtime.Kernel.Builtin.Channel (
    channelSplitKernel,
    channelMergeKernel,
    monoToStereoKernel,
) where

import Musikell.Core.Types (KernelId (..))
import Musikell.Memory.Buffer (bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (forRange_)

-- | Extract a single channel from interleaved data.
--   channel 0 = left, channel 1 = right.
channelSplitKernel :: Int -> Int -> KernelSpec
channelSplitKernel channel totalChannels =
    KernelSpec
        { kernelId = KernelId "channel.split",
          kernelInputs = 1,
          kernelOutputs = 1,
          kernelExecute = \ins outs -> case (ins, outs) of
            (i : _, o : _) -> do
                let n = bufferSize o
                forRange_ n $ \idx -> do
                    s <- unsafeReadSample i (idx * totalChannels + channel)
                    unsafeWriteSample o idx s
            _ -> pure ()
        }

-- | Merge two mono buffers into interleaved stereo.
channelMergeKernel :: KernelSpec
channelMergeKernel =
    KernelSpec
        { kernelId = KernelId "channel.merge",
          kernelInputs = 2,
          kernelOutputs = 1,
          kernelExecute = \ins outs -> case (ins, outs) of
            (l : r : _, o : _) -> do
                let n = bufferSize l
                forRange_ n $ \idx -> do
                    sl <- unsafeReadSample l idx
                    sr <- unsafeReadSample r idx
                    unsafeWriteSample o (idx * 2) sl
                    unsafeWriteSample o (idx * 2 + 1) sr
            _ -> pure ()
        }

-- | Duplicate a mono signal to interleaved stereo.
monoToStereoKernel :: KernelSpec
monoToStereoKernel =
    KernelSpec
        { kernelId = KernelId "mono_to_stereo",
          kernelInputs = 1,
          kernelOutputs = 1,
          kernelExecute = \ins outs -> case (ins, outs) of
            (i : _, o : _) -> do
                let n = bufferSize i
                forRange_ n $ \idx -> do
                    s <- unsafeReadSample i idx
                    unsafeWriteSample o (idx * 2) s
                    unsafeWriteSample o (idx * 2 + 1) s
            _ -> pure ()
        }
