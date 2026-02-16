-- File: src/Musikell/Runtime/Accelerate/Backend.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/accelerate/backend.mdx
-- Module: Musikell.Runtime.Accelerate.Backend
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Accelerate GPU backend (stub).
--
-- This module provides the Accelerate backend type and its Backend
-- typeclass instance.  The actual GPU dispatch is not yet implemented;
-- kernels currently fall through to CPU execution via the shared
-- KernelRegistry.
--
-- When Accelerate support lands, this module will marshal buffers to
-- Accelerate arrays, run GPU computations, and marshal back.
module Musikell.Runtime.Accelerate.Backend (
    -- * Accelerate Backend
    AccelerateBackend (..),
    defaultAccelerateBackend,
) where

-- | The Accelerate backend configuration.
newtype AccelerateBackend = AccelerateBackend
    { -- | Device ID (0 = CPU interpreter, >0 = GPU)
      accelerateDeviceId :: Int
    }
    deriving (Eq, Show)

-- | Default: CPU interpreter.
defaultAccelerateBackend :: AccelerateBackend
defaultAccelerateBackend = AccelerateBackend {accelerateDeviceId = 0}
