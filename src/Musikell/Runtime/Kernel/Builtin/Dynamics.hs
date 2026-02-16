-- File: src/Musikell/Runtime/Kernel/Builtin/Dynamics.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/dynamics.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Dynamics
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dynamics processing kernels: compressor, limiter, noise gate.
--
-- All use envelope-following with configurable attack/release times.
-- The compressor implements a feed-forward topology with soft-knee.
module Musikell.Runtime.Kernel.Builtin.Dynamics (
    mkCompressor,
    mkLimiter,
    mkGate,
) where

import Data.IORef

import Musikell.Core.Types (KernelId (..), SampleRate)
import Musikell.Memory.Buffer (Buffer, bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (flushDenormals)

-- | Compute attack/release coefficients from time in seconds.
timeToCoeff :: Double -> SampleRate -> Float
timeToCoeff t sr = realToFrac (exp ((-1.0) / (t * fromIntegral sr)))
{-# INLINE timeToCoeff #-}

-- | Convert linear amplitude to dB.
linToDb :: Float -> Float
linToDb !x = 20.0 * logBase 10.0 (max x 1.0e-10)
{-# INLINE linToDb #-}

-- | Convert dB to linear amplitude.
dbToLin :: Float -> Float
dbToLin !x = 10.0 ** (x / 20.0)
{-# INLINE dbToLin #-}

-- | Feed-forward compressor with soft knee.
--
--   @mkCompressor threshold ratio attack release knee sampleRate@
--
--   - threshold: dB level above which compression begins
--   - ratio: compression ratio (e.g. 4.0 = 4:1)
--   - attack: attack time in seconds
--   - release: release time in seconds
--   - knee: soft knee width in dB (0 = hard knee)
mkCompressor :: Double -> Double -> Double -> Double -> Double -> SampleRate -> IO KernelSpec
mkCompressor threshold ratio attack release knee sr = do
    envRef <- newIORef (0.0 :: Float)
    let !thresh = realToFrac threshold :: Float
        !rat = realToFrac ratio :: Float
        !kneeW = realToFrac knee :: Float
        !attCoeff = timeToCoeff attack sr
        !relCoeff = timeToCoeff release sr
    pure
        $ KernelSpec
            { kernelId = KernelId "compressor",
              kernelInputs = 1,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (i : _, o : _) -> do
                    env0 <- readIORef envRef
                    envFinal <- compLoop thresh rat kneeW attCoeff relCoeff i o (bufferSize i) env0 0
                    writeIORef envRef $! envFinal
                _ -> pure ()
            }

compLoop ::
    Float ->
    Float ->
    Float ->
    Float ->
    Float ->
    Buffer ->
    Buffer ->
    Int ->
    Float ->
    Int ->
    IO Float
compLoop !thresh !rat !kneeW !att !rel !i !o !n !env !idx
    | idx >= n = pure env
    | otherwise = do
        s <- unsafeReadSample i idx
        let !absS = abs s
            !dbIn = linToDb absS
            -- Envelope follower
            !env' =
                flushDenormals
                    $ if absS > env then
                        att * env + (1.0 - att) * absS
                    else
                        rel * env + (1.0 - rel) * absS
            -- Gain computation with soft knee
            !overDb = dbIn - thresh
            !gainDb
                | overDb <= (-kneeW) / 2 = 0.0 -- below threshold
                | overDb >= kneeW / 2 = overDb * (1.0 / rat - 1.0) -- full compression
                | otherwise -- soft knee region
                    =
                    let !x = overDb + kneeW / 2
                    in  (1.0 / rat - 1.0) * x * x / (2.0 * kneeW)
            !gain = dbToLin gainDb
        unsafeWriteSample o idx (s * gain)
        compLoop thresh rat kneeW att rel i o n env' (idx + 1)

-- | Brickwall limiter — compressor with infinite ratio and fast attack.
mkLimiter :: Double -> Double -> SampleRate -> IO KernelSpec
mkLimiter threshold release sr = do
    envRef <- newIORef (0.0 :: Float)
    let !thresh = realToFrac (dbToLin (realToFrac threshold)) :: Float
        !relCoeff = timeToCoeff release sr
    pure
        $ KernelSpec
            { kernelId = KernelId "limiter",
              kernelInputs = 1,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (i : _, o : _) -> do
                    env0 <- readIORef envRef
                    envFinal <- limLoop thresh relCoeff i o (bufferSize i) env0 0
                    writeIORef envRef $! envFinal
                _ -> pure ()
            }

limLoop :: Float -> Float -> Buffer -> Buffer -> Int -> Float -> Int -> IO Float
limLoop !thresh !rel !i !o !n !env !idx
    | idx >= n = pure env
    | otherwise = do
        s <- unsafeReadSample i idx
        let !absS = abs s
            !env' = flushDenormals $ max absS (rel * env)
            !gain = if env' > thresh then thresh / env' else 1.0
        unsafeWriteSample o idx (s * gain)
        limLoop thresh rel i o n env' (idx + 1)

-- | Noise gate — silence signal below threshold.
mkGate :: Double -> Double -> Double -> SampleRate -> IO KernelSpec
mkGate threshold attack release sr = do
    envRef <- newIORef (0.0 :: Float)
    let !thresh = realToFrac (dbToLin (realToFrac threshold)) :: Float
        !attCoeff = timeToCoeff attack sr
        !relCoeff = timeToCoeff release sr
    pure
        $ KernelSpec
            { kernelId = KernelId "gate",
              kernelInputs = 1,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (i : _, o : _) -> do
                    env0 <- readIORef envRef
                    envFinal <- gateLoop thresh attCoeff relCoeff i o (bufferSize i) env0 0
                    writeIORef envRef $! envFinal
                _ -> pure ()
            }

gateLoop :: Float -> Float -> Float -> Buffer -> Buffer -> Int -> Float -> Int -> IO Float
gateLoop !thresh !att !rel !i !o !n !env !idx
    | idx >= n = pure env
    | otherwise = do
        s <- unsafeReadSample i idx
        let !absS = abs s
            !env' =
                flushDenormals
                    $ if absS > env then
                        att * env + (1.0 - att) * absS
                    else
                        rel * env + (1.0 - rel) * absS
            !gate = if env' > thresh then 1.0 else 0.0
        unsafeWriteSample o idx (s * gate)
        gateLoop thresh att rel i o n env' (idx + 1)
