-- File: src/Musikell/Runtime/Kernel.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel.mdx
-- Module: Musikell.Runtime.Kernel

-- | Kernel specification and registry.
--
-- A kernel is a pure-ish function that reads from input buffers and writes
-- into pre-allocated output buffers. Kernels MUST NOT allocate memory.
-- All output buffers are owned by the caller (BufferPool) and pre-allocated
-- before the first block is ever executed.
--
-- This module is backend-agnostic; both CPU and Accelerate backends share
-- these types.
module Musikell.Runtime.Kernel (
    -- * Kernel specification
    KernelSpec (..),

    -- * Registry
    KernelRegistry,
    emptyRegistry,
    registerKernel,
    lookupKernel,
    registryKernels,
    registrySize,
) where

import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

import Musikell.Core.Types (KernelId (..))
import Musikell.Memory.Buffer (Buffer)

-- | A kernel implementation.
--
-- @kernelExecute inputs outputs@ reads from @inputs@ and writes results
-- into @outputs@.  Both lists are guaranteed to have the correct length
-- by the scheduler; the kernel MUST NOT allocate or free buffers.
data KernelSpec = KernelSpec
    { kernelId :: !KernelId,
      kernelInputs :: !Int,
      kernelOutputs :: !Int,
      kernelExecute :: [Buffer] -> [Buffer] -> IO ()
    }

instance Show KernelSpec where
    show s = "KernelSpec<" ++ show (kernelId s) ++ ">"

-- | Map from 'KernelId' to its implementation.
newtype KernelRegistry = KernelRegistry (Map KernelId KernelSpec)

instance Show KernelRegistry where
    show (KernelRegistry m) = "KernelRegistry[" ++ show (Map.size m) ++ "]"

emptyRegistry :: KernelRegistry
emptyRegistry = KernelRegistry Map.empty

registerKernel :: KernelSpec -> KernelRegistry -> KernelRegistry
registerKernel spec (KernelRegistry m) =
    KernelRegistry $ Map.insert (kernelId spec) spec m

lookupKernel :: KernelId -> KernelRegistry -> Maybe KernelSpec
lookupKernel kid (KernelRegistry m) = Map.lookup kid m

registryKernels :: KernelRegistry -> [KernelSpec]
registryKernels (KernelRegistry m) = Map.elems m

registrySize :: KernelRegistry -> Int
registrySize (KernelRegistry m) = Map.size m
