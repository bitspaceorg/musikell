-- File: src/Musikell/Runtime/Kernel/Builtin/Util.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/util.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Util
{-# LANGUAGE BangPatterns #-}

-- | Shared utilities for built-in kernels.
module Musikell.Runtime.Kernel.Builtin.Util (
    forRange_,
    flushDenormals,
) where

-- | Strict loop over [0..n-1].
forRange_ :: Int -> (Int -> IO ()) -> IO ()
forRange_ !n f = go 0
    where
        go !i
            | i >= n = pure ()
            | otherwise = f i >> go (i + 1)
{-# INLINE forRange_ #-}

-- | Flush subnormal floats to zero to prevent 10-100x CPU slowdown
--   in IIR filters and feedback loops.
flushDenormals :: Float -> Float
flushDenormals !x
    | abs x < 1.0e-15 = 0.0
    | otherwise = x
{-# INLINE flushDenormals #-}
