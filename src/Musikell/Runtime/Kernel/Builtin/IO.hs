-- File: src/Musikell/Runtime/Kernel/Builtin/IO.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/io.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.IO
{-# LANGUAGE OverloadedStrings #-}

-- | Input / Output / Passthrough / Null kernels.
--
-- These are stream boundary markers. The scheduler fills the input
-- node's buffer from stdin and reads the output node's buffer to stdout.
module Musikell.Runtime.Kernel.Builtin.IO (
    inputKernel,
    outputKernel,
    passthruKernel,
    nullKernel,
) where

import Musikell.Core.Types (KernelId (..))
import Musikell.Memory.Buffer (copyBuffer)
import Musikell.Runtime.Kernel (KernelSpec (..))

-- | Input node: scheduler fills its output buffer from stdin.
--   The kernel itself is a no-op.
inputKernel :: KernelSpec
inputKernel =
    KernelSpec
        { kernelId = KernelId "input",
          kernelInputs = 0,
          kernelOutputs = 1,
          kernelExecute = \_ _ -> pure ()
        }

-- | Output node: copies input -> output so the scheduler can extract it.
outputKernel :: KernelSpec
outputKernel =
    KernelSpec
        { kernelId = KernelId "output",
          kernelInputs = 1,
          kernelOutputs = 1,
          kernelExecute = \ins outs -> case (ins, outs) of
            (i : _, o : _) -> copyBuffer i o
            _ -> pure ()
        }

-- | Copy input to output unchanged.
passthruKernel :: KernelSpec
passthruKernel =
    KernelSpec
        { kernelId = KernelId "passthru",
          kernelInputs = 1,
          kernelOutputs = 1,
          kernelExecute = \ins outs -> case (ins, outs) of
            (i : _, o : _) -> copyBuffer i o
            _ -> pure ()
        }

-- | Consume input, produce nothing.
nullKernel :: KernelSpec
nullKernel =
    KernelSpec
        { kernelId = KernelId "null",
          kernelInputs = 1,
          kernelOutputs = 0,
          kernelExecute = \_ _ -> pure ()
        }
