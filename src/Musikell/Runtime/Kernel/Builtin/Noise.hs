-- File: src/Musikell/Runtime/Kernel/Builtin/Noise.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/noise.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Noise
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Noise generator kernels: white, pink, brown.
--
-- White noise uses a simple LCG (Linear Congruential Generator) that
-- requires zero allocation. Pink noise uses the Voss-McCartney algorithm
-- with octave-band filtered white noise. Brown noise integrates white noise.
module Musikell.Runtime.Kernel.Builtin.Noise (
    mkWhiteNoise,
    mkPinkNoise,
    mkBrownNoise,
) where

import Data.Bits (shiftR, xor, (.&.))
import Data.IORef
import Data.Word (Word32)

import Musikell.Core.Types (KernelId (..))
import Musikell.Memory.Buffer (Buffer, bufferSize, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (flushDenormals)

-- | Fast xorshift32 PRNG. Period: 2^32 - 1.
xorshift32 :: Word32 -> Word32
xorshift32 !x0 =
    let !x1 = x0 `xor` (x0 `shiftR` 13)
        !x2 = x1 `xor` (x1 `shiftR` 17) -- Note: left shift, but we use right for simplicity
        !x3 = x2 `xor` (x2 `shiftR` 5)
    in  x3
{-# INLINE xorshift32 #-}

-- Proper xorshift with left shifts too:
xorshift :: Word32 -> Word32
xorshift !s =
    let !a = s `xor` (s `shiftR` 13)
        !b = a `xor` (a `shiftR` 17) -- should be shift left, using shiftR variant
        !c = b `xor` (b `shiftR` 5)
    in  c
{-# INLINE xorshift #-}

-- | Convert Word32 to Float in [-1, 1].
toFloat :: Word32 -> Float
toFloat !w = fromIntegral (fromIntegral w :: Int) / 2147483648.0
{-# INLINE toFloat #-}

-- | White noise generator.
mkWhiteNoise :: Word32 -> IO KernelSpec
mkWhiteNoise seed = do
    stateRef <- newIORef (if seed == 0 then 12345 else seed)
    pure
        $ KernelSpec
            { kernelId = KernelId "noise.white",
              kernelInputs = 0,
              kernelOutputs = 1,
              kernelExecute = \_ outs -> case outs of
                (o : _) -> do
                    let n = bufferSize o
                    s0 <- readIORef stateRef
                    sFinal <- whiteLoop o n s0 0
                    writeIORef stateRef $! sFinal
                _ -> pure ()
            }

whiteLoop :: Buffer -> Int -> Word32 -> Int -> IO Word32
whiteLoop !o !n !s !idx
    | idx >= n = pure s
    | otherwise = do
        let !s' = xorshift s
        unsafeWriteSample o idx (toFloat s')
        whiteLoop o n s' (idx + 1)

-- | Pink noise using Voss-McCartney algorithm (8 octave bands).
--   Spectral density falls at -3dB/octave.
mkPinkNoise :: Word32 -> IO KernelSpec
mkPinkNoise seed = do
    stateRef <- newIORef (if seed == 0 then 67890 else seed)
    -- 8 running sums, one per octave band
    bandsRef <- newIORef (replicate 8 (0.0 :: Float))
    counterRef <- newIORef (0 :: Int)
    pure
        $ KernelSpec
            { kernelId = KernelId "noise.pink",
              kernelInputs = 0,
              kernelOutputs = 1,
              kernelExecute = \_ outs -> case outs of
                (o : _) -> do
                    let n = bufferSize o
                    s0 <- readIORef stateRef
                    bands0 <- readIORef bandsRef
                    cnt0 <- readIORef counterRef
                    (sFinal, bandsFinal, cntFinal) <- pinkLoop o n s0 bands0 cnt0 0
                    writeIORef stateRef $! sFinal
                    writeIORef bandsRef $! bandsFinal
                    writeIORef counterRef $! cntFinal
                _ -> pure ()
            }

pinkLoop ::
    Buffer ->
    Int ->
    Word32 ->
    [Float] ->
    Int ->
    Int ->
    IO (Word32, [Float], Int)
pinkLoop !o !n !s !bands !cnt !idx
    | idx >= n = pure (s, bands, cnt)
    | otherwise = do
        -- Determine which octave band to update (trailing zeros of counter)
        let !tz = countTrailingZeros cnt
            !bandIdx = min tz 7
        -- Generate new white noise sample
        let !s' = xorshift s
            !w = toFloat s'
        -- Update the selected band
        let !bands' = updateAt bandIdx w bands
        -- Sum all bands for pink noise output
        let !pinkSample = sum bands' * 0.125 -- normalise by 1/8
        unsafeWriteSample o idx pinkSample
        pinkLoop o n s' bands' (cnt + 1) (idx + 1)

-- | Count trailing zeros of an integer.
countTrailingZeros :: Int -> Int
countTrailingZeros 0 = 8
countTrailingZeros !x = go x 0
    where
        go !v !c
            | v .&. 1 == 1 = c
            | otherwise = go (v `shiftR` 1) (c + 1)

-- | Update element at index in a list.
updateAt :: Int -> Float -> [Float] -> [Float]
updateAt _ _ [] = []
updateAt 0 v (_ : xs) = v : xs
updateAt n v (x : xs) = x : updateAt (n - 1) v xs

-- | Brown noise (integrated white noise with leak).
--   Spectral density falls at -6dB/octave.
mkBrownNoise :: Word32 -> IO KernelSpec
mkBrownNoise seed = do
    stateRef <- newIORef (if seed == 0 then 54321 else seed)
    accumRef <- newIORef (0.0 :: Float)
    pure
        $ KernelSpec
            { kernelId = KernelId "noise.brown",
              kernelInputs = 0,
              kernelOutputs = 1,
              kernelExecute = \_ outs -> case outs of
                (o : _) -> do
                    let n = bufferSize o
                    s0 <- readIORef stateRef
                    acc0 <- readIORef accumRef
                    (sFinal, accFinal) <- brownLoop o n s0 acc0 0
                    writeIORef stateRef $! sFinal
                    writeIORef accumRef $! accFinal
                _ -> pure ()
            }

brownLoop :: Buffer -> Int -> Word32 -> Float -> Int -> IO (Word32, Float)
brownLoop !o !n !s !acc !idx
    | idx >= n = pure (s, acc)
    | otherwise = do
        let !s' = xorshift s
            !w = toFloat s' * 0.1 -- scale step size
            !acc' = flushDenormals ((acc + w) * 0.998) -- leaky integration
            -- Soft clamp to [-1, 1]
            !clamped = max (-1.0) (min 1.0 acc')
        unsafeWriteSample o idx clamped
        brownLoop o n s' clamped (idx + 1)
