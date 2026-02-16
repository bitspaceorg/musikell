-- File: src/Musikell/Runtime/Accelerate/Kernel.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/accelerate/kernel.mdx
-- Module: Musikell.Runtime.Accelerate.Kernel

-- | Re-exports kernel types from the shared Runtime.Kernel module.
--
-- The Accelerate backend uses the same KernelSpec / KernelRegistry
-- types as every other backend.  This module exists for backward
-- compatibility.
module Musikell.Runtime.Accelerate.Kernel (
    module Musikell.Runtime.Kernel,
) where

import Musikell.Runtime.Kernel
