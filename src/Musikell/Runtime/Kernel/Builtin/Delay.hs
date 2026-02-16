-- File: src/Musikell/Runtime/Kernel/Builtin/Delay.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/delay.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Delay
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Delay kernel with linear interpolation for fractional delay.
--
-- Supports both integer and fractional delay times. Fractional delay
-- enables chorus, flanger, and pitch-shifting effects.
module Musikell.Runtime.Kernel.Builtin.Delay (
    mkDelay,
    mkDelayInterpolated,
) where

import Data.IORef

import qualified Data.Vector.Storable.Mutable as MV

import Musikell.Core.Types (BlockSize, KernelId (..))
import Musikell.Memory.Buffer (bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (forRange_)

-- | Integer-sample delay using a circular buffer.
mkDelay :: Int -> BlockSize -> IO KernelSpec
mkDelay delaySamples _blockSize = do
    delayBuf <- MV.replicate delaySamples (0.0 :: Float)
    posRef <- newIORef (0 :: Int)
    pure
        $ KernelSpec
            { kernelId = KernelId "delay",
              kernelInputs = 1,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (i : _, o : _) -> do
                    let n = bufferSize i
                    pos <- readIORef posRef
                    forRange_ n $ \idx -> do
                        let !wp = (pos + idx) `mod` delaySamples
                        delayed <- MV.unsafeRead delayBuf wp
                        s <- unsafeReadSample i idx
                        MV.unsafeWrite delayBuf wp s
                        unsafeWriteSample o idx delayed
                    writeIORef posRef $! (pos + n) `mod` delaySamples
                _ -> pure ()
            }

-- | Fractional-sample delay with linear interpolation.
--   Reads between two integer positions for sub-sample accuracy.
mkDelayInterpolated :: Double -> Int -> IO KernelSpec
mkDelayInterpolated delayTime maxDelaySamples = do
    delayBuf <- MV.replicate maxDelaySamples (0.0 :: Float)
    writePos <- newIORef (0 :: Int)
    pure
        $ KernelSpec
            { kernelId = KernelId "delay.interp",
              kernelInputs = 1,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (i : _, o : _) -> do
                    let n = bufferSize i
                        !delaySmp = delayTime
                    wp <- readIORef writePos
                    forRange_ n $ \idx -> do
                        let !wIdx = (wp + idx) `mod` maxDelaySamples
                        -- Write current input
                        s <- unsafeReadSample i idx
                        MV.unsafeWrite delayBuf wIdx s
                        -- Read with interpolation
                        let !readPos = fromIntegral (wp + idx) - delaySmp
                            !readIdx = floor readPos :: Int
                            !frac = realToFrac (readPos - fromIntegral readIdx) :: Float
                            !r0 = ((readIdx `mod` maxDelaySamples) + maxDelaySamples) `mod` maxDelaySamples
                            !r1 = (r0 + 1) `mod` maxDelaySamples
                        s0 <- MV.unsafeRead delayBuf r0
                        s1 <- MV.unsafeRead delayBuf r1
                        unsafeWriteSample o idx (s0 * (1.0 - frac) + s1 * frac)
                    writeIORef writePos $! (wp + n) `mod` maxDelaySamples
                _ -> pure ()
            }
