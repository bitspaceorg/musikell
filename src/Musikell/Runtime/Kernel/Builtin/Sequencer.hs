-- File: src/Musikell/Runtime/Kernel/Builtin/Sequencer.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/sequencer.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Sequencer
{-# LANGUAGE OverloadedStrings #-}

-- | Beat sequencer kernel — gates signal based on a rhythmic pattern.
module Musikell.Runtime.Kernel.Builtin.Sequencer (
    mkBeatSequencer,
) where

import Data.IORef

import Musikell.Core.Types (BlockSize, KernelId (..), SampleRate)
import Musikell.Memory.Buffer (bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (forRange_)

-- | Create a beat sequencer that gates input based on a pattern string.
--   'x'/'X' = full volume, 'o'/'O' = soft (0.5), '_' = silence.
--   Pattern cycles at the given BPM and subdivision.
mkBeatSequencer :: String -> Double -> Int -> BlockSize -> SampleRate -> IO KernelSpec
mkBeatSequencer pat bpm division _blockSize sr = do
    posRef <- newIORef (0 :: Int)
    let patLen = length pat
        samplesPerStep = round (fromIntegral sr * 60.0 / bpm / fromIntegral division :: Double)
        gains = map stepGain pat
        stepGain 'x' = 1.0 :: Float
        stepGain 'X' = 1.0
        stepGain 'o' = 0.5
        stepGain 'O' = 0.5
        stepGain _ = 0.0
    pure
        $ KernelSpec
            { kernelId = KernelId "beat",
              kernelInputs = 1,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (i : _, o : _) -> do
                    let n = bufferSize i
                    samplePos <- readIORef posRef
                    forRange_ n $ \idx -> do
                        let globalSample = samplePos + idx
                            stepIdx = (globalSample `div` samplesPerStep) `mod` patLen
                            g = gains !! stepIdx
                        s <- unsafeReadSample i idx
                        unsafeWriteSample o idx (s * g)
                    writeIORef posRef $! samplePos + n
                _ -> pure ()
            }
