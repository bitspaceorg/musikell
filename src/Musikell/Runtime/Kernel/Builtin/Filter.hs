-- File: src/Musikell/Runtime/Kernel/Builtin/Filter.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/filter.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Filter
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | State-Variable Filter (SVF) kernels.
--
-- The SVF topology computes lowpass, highpass, bandpass, and notch
-- simultaneously from the same state. Numerically stable and efficient.
--
-- Chamberlin form:
--   hp = input - lp - Q * bp
--   bp = bp + f * hp
--   lp = lp + f * bp
--
-- where f = 2 * sin(pi * cutoff / sampleRate)
--       Q = 1 / resonance
module Musikell.Runtime.Kernel.Builtin.Filter (
    FilterMode (..),
    mkSVFilter,
) where

import Data.IORef

import Musikell.Core.Types (KernelId (..), SampleRate)
import Musikell.Memory.Buffer (Buffer, bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (flushDenormals)

-- | Which output of the SVF to tap.
data FilterMode
    = Lowpass
    | Highpass
    | Bandpass
    | Notch
    deriving (Eq, Show)

-- | Create a state-variable filter kernel.
mkSVFilter :: FilterMode -> Double -> Double -> SampleRate -> IO KernelSpec
mkSVFilter mode cutoff resonance sr = do
    bpRef <- newIORef (0.0 :: Float)
    lpRef <- newIORef (0.0 :: Float)
    let !f = realToFrac (2.0 * sin (pi * cutoff / fromIntegral sr)) :: Float
        !q = realToFrac (1.0 / max 0.5 resonance) :: Float
        kid = case mode of
            Lowpass -> "filter.lowpass"
            Highpass -> "filter.highpass"
            Bandpass -> "filter.bandpass"
            Notch -> "filter.notch"
    pure
        $ KernelSpec
            { kernelId = KernelId kid,
              kernelInputs = 1,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (i : _, o : _) -> do
                    let n = bufferSize i
                    bp0 <- readIORef bpRef
                    lp0 <- readIORef lpRef
                    (bpFinal, lpFinal) <- svfLoop mode f q i o n bp0 lp0 0
                    writeIORef bpRef $! bpFinal
                    writeIORef lpRef $! lpFinal
                _ -> pure ()
            }

svfLoop ::
    FilterMode ->
    Float ->
    Float ->
    Buffer ->
    Buffer ->
    Int ->
    Float ->
    Float ->
    Int ->
    IO (Float, Float)
svfLoop !mode !f !q !i !o !n !bp !lp !idx
    | idx >= n = pure (bp, lp)
    | otherwise = do
        s <- unsafeReadSample i idx
        let !hp = s - lp - q * bp
            !bp' = flushDenormals (bp + f * hp)
            !lp' = flushDenormals (lp + f * bp')
            !out = case mode of
                Lowpass -> lp'
                Highpass -> hp
                Bandpass -> bp'
                Notch -> hp + lp'
        unsafeWriteSample o idx out
        svfLoop mode f q i o n bp' lp' (idx + 1)
