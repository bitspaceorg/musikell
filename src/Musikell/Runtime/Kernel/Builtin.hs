-- File: src/Musikell/Runtime/Kernel/Builtin.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/builtin.mdx
-- Module: Musikell.Runtime.Kernel.Builtin

-- | Built-in kernel registry.
--
-- This module re-exports all built-in kernels from their individual
-- modules and provides the default registry.
module Musikell.Runtime.Kernel.Builtin (
    -- * Registry
    builtinRegistry,

    -- * Re-exports from submodules
    module Musikell.Runtime.Kernel.Builtin.IO,
    module Musikell.Runtime.Kernel.Builtin.Gain,
    module Musikell.Runtime.Kernel.Builtin.Mix,
    module Musikell.Runtime.Kernel.Builtin.Oscillator,
    module Musikell.Runtime.Kernel.Builtin.Delay,
    module Musikell.Runtime.Kernel.Builtin.Filter,
    module Musikell.Runtime.Kernel.Builtin.Dynamics,
    module Musikell.Runtime.Kernel.Builtin.Reverb,
    module Musikell.Runtime.Kernel.Builtin.Noise,
    module Musikell.Runtime.Kernel.Builtin.Envelope,
    module Musikell.Runtime.Kernel.Builtin.Channel,
    module Musikell.Runtime.Kernel.Builtin.Sequencer,
    module Musikell.Runtime.Kernel.Builtin.Crossfade,
) where

import Musikell.Runtime.Kernel (KernelRegistry, emptyRegistry, registerKernel)
import Musikell.Runtime.Kernel.Builtin.Channel
import Musikell.Runtime.Kernel.Builtin.Crossfade
import Musikell.Runtime.Kernel.Builtin.Delay
import Musikell.Runtime.Kernel.Builtin.Dynamics
import Musikell.Runtime.Kernel.Builtin.Envelope
import Musikell.Runtime.Kernel.Builtin.Filter
import Musikell.Runtime.Kernel.Builtin.Gain
import Musikell.Runtime.Kernel.Builtin.IO
import Musikell.Runtime.Kernel.Builtin.Mix
import Musikell.Runtime.Kernel.Builtin.Noise
import Musikell.Runtime.Kernel.Builtin.Oscillator
import Musikell.Runtime.Kernel.Builtin.Reverb
import Musikell.Runtime.Kernel.Builtin.Sequencer

-- | Registry pre-loaded with all stateless built-in kernels.
--   Stateful kernels (oscillators, delay, filter, etc.) must be
--   created via their @mk*@ constructors and registered separately
--   during lowering.
builtinRegistry :: KernelRegistry
builtinRegistry =
    foldr
        registerKernel
        emptyRegistry
        [ inputKernel,
          outputKernel,
          passthruKernel,
          nullKernel,
          gainKernel 1.0,
          mixKernel,
          clipKernel (-1.0) 1.0,
          channelMergeKernel,
          monoToStereoKernel
        ]
