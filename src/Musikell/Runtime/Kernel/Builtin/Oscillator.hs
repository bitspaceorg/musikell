-- File: src/Musikell/Runtime/Kernel/Builtin/Oscillator.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/oscillator.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Oscillator
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Oscillator kernels with PolyBLEP anti-aliasing.
--
-- PolyBLEP (Polynomial Band-Limited Step) corrects the discontinuities
-- in naive waveforms by applying a polynomial correction near transitions.
-- This eliminates audible aliasing artefacts without requiring wavetables.
module Musikell.Runtime.Kernel.Builtin.Oscillator (
    mkOscillatorSine,
    mkOscillatorSquare,
    mkOscillatorSawtooth,
    mkOscillatorTriangle,
) where

import Data.IORef

import Musikell.Core.Types (KernelId (..), SampleRate)
import Musikell.Memory.Buffer (bufferSize, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (forRange_)

-- | PolyBLEP correction for discontinuities.
--   @t@ is the normalised phase [0,1), @dt@ is the phase increment per sample.
polyBLEP :: Double -> Double -> Double
polyBLEP !t !dt
    | t < dt = let t' = t / dt in t' + t' - t' * t' - 1.0
    | t > 1 - dt = let t' = (t - 1.0) / dt in t' * t' + t' + t' + 1.0
    | otherwise = 0.0
{-# INLINE polyBLEP #-}

-- | Sine oscillator (no aliasing by nature — pure sinusoid).
mkOscillatorSine :: Double -> SampleRate -> IO KernelSpec
mkOscillatorSine freq sr = do
    phaseRef <- newIORef (0.0 :: Double)
    let !inc = freq / fromIntegral sr
    pure
        $ KernelSpec
            { kernelId = KernelId "oscillator.sine",
              kernelInputs = 0,
              kernelOutputs = 1,
              kernelExecute = \_ outs -> case outs of
                (o : _) -> do
                    let n = bufferSize o
                    phase <- readIORef phaseRef
                    forRange_ n $ \idx -> do
                        let !p = phase + inc * fromIntegral idx
                        unsafeWriteSample o idx (realToFrac (sin (2.0 * pi * p)))
                    let !final = phase + inc * fromIntegral n
                    writeIORef phaseRef $! final - fromIntegral (floor final :: Int)
                _ -> pure ()
            }

-- | Square oscillator with PolyBLEP anti-aliasing.
mkOscillatorSquare :: Double -> SampleRate -> IO KernelSpec
mkOscillatorSquare freq sr = do
    phaseRef <- newIORef (0.0 :: Double)
    let !inc = freq / fromIntegral sr
    pure
        $ KernelSpec
            { kernelId = KernelId "oscillator.square",
              kernelInputs = 0,
              kernelOutputs = 1,
              kernelExecute = \_ outs -> case outs of
                (o : _) -> do
                    let n = bufferSize o
                    phase <- readIORef phaseRef
                    forRange_ n $ \idx -> do
                        let !p0 = phase + inc * fromIntegral idx
                            !frac = p0 - fromIntegral (floor p0 :: Int)
                            -- Naive square: +1 for first half, -1 for second
                            !naive = if frac < 0.5 then 1.0 else -1.0
                            -- PolyBLEP corrections at both transitions
                            !blep =
                                naive
                                    + polyBLEP frac inc
                                    - polyBLEP
                                        ( let f = frac - 0.5
                                          in  if f < 0 then f + 1.0 else f
                                        )
                                        inc
                        unsafeWriteSample o idx (realToFrac blep)
                    let !final = phase + inc * fromIntegral n
                    writeIORef phaseRef $! final - fromIntegral (floor final :: Int)
                _ -> pure ()
            }

-- | Sawtooth oscillator with PolyBLEP anti-aliasing.
mkOscillatorSawtooth :: Double -> SampleRate -> IO KernelSpec
mkOscillatorSawtooth freq sr = do
    phaseRef <- newIORef (0.0 :: Double)
    let !inc = freq / fromIntegral sr
    pure
        $ KernelSpec
            { kernelId = KernelId "oscillator.sawtooth",
              kernelInputs = 0,
              kernelOutputs = 1,
              kernelExecute = \_ outs -> case outs of
                (o : _) -> do
                    let n = bufferSize o
                    phase <- readIORef phaseRef
                    forRange_ n $ \idx -> do
                        let !p0 = phase + inc * fromIntegral idx
                            !frac = p0 - fromIntegral (floor p0 :: Int)
                            -- Naive sawtooth: linear ramp from -1 to +1
                            !naive = 2.0 * frac - 1.0
                            -- PolyBLEP correction at the discontinuity (wrap from +1 to -1)
                            !blep = naive - polyBLEP frac inc
                        unsafeWriteSample o idx (realToFrac blep)
                    let !final = phase + inc * fromIntegral n
                    writeIORef phaseRef $! final - fromIntegral (floor final :: Int)
                _ -> pure ()
            }

-- | Triangle oscillator (integrated PolyBLEP square, then leaky integrator).
--   Triangle is the integral of square, so we integrate the band-limited
--   square wave for a band-limited triangle.
mkOscillatorTriangle :: Double -> SampleRate -> IO KernelSpec
mkOscillatorTriangle freq sr = do
    phaseRef <- newIORef (0.0 :: Double)
    intRef <- newIORef (0.0 :: Double)
    let !inc = freq / fromIntegral sr
        -- Scale factor: 4 * freq / sampleRate for correct amplitude
        !scale = 4.0 * inc
    pure
        $ KernelSpec
            { kernelId = KernelId "oscillator.triangle",
              kernelInputs = 0,
              kernelOutputs = 1,
              kernelExecute = \_ outs -> case outs of
                (o : _) -> do
                    let n = bufferSize o
                    phase <- readIORef phaseRef
                    integ <- readIORef intRef
                    go o n phase integ 0
                    where
                        go o n phase integ idx
                            | idx >= n = do
                                let !final = phase + inc * fromIntegral n
                                writeIORef phaseRef $! final - fromIntegral (floor final :: Int)
                                writeIORef intRef $! integ
                            | otherwise = do
                                let !p0 = phase + inc * fromIntegral idx
                                    !frac = p0 - fromIntegral (floor p0 :: Int)
                                    -- Band-limited square
                                    !naive = if frac < 0.5 then 1.0 else -1.0
                                    !sq =
                                        naive
                                            + polyBLEP frac inc
                                            - polyBLEP
                                                ( let f = frac - 0.5
                                                  in  if f < 0 then f + 1.0 else f
                                                )
                                                inc
                                    -- Leaky integration with DC correction
                                    !integ' = integ + scale * sq
                                    -- Soft leak to prevent DC drift
                                    !integ'' = integ' * 0.999
                                unsafeWriteSample o idx (realToFrac integ'')
                                go o n phase integ'' (idx + 1)
                _ -> pure ()
            }
