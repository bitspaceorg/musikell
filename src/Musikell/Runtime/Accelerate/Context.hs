-- File: src/Musikell/Runtime/Accelerate/Context.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/accelerate/context.mdx
-- Module: Musikell.Runtime.Accelerate.Context

-- | Runtime context for the Accelerate backend.
--
-- A 'Context' bundles backend state, configuration, and the kernel
-- registry for a single runtime session.
module Musikell.Runtime.Accelerate.Context (
    -- * Context type
    Context (..),
    RuntimeConfig (..),

    -- * Defaults
    defaultRuntimeConfig,

    -- * Lifecycle
    createContext,
    destroyContext,
    withContext,
) where

import Control.Exception (bracket)

import Musikell.Core.Types (BlockSize, SampleRate, defaultBlockSize, defaultSampleRate)
import Musikell.Runtime.Kernel (KernelRegistry, emptyRegistry)

-- | Runtime configuration.
data RuntimeConfig = RuntimeConfig
    { configBlockSize :: !BlockSize,
      configSampleRate :: !SampleRate,
      configDebug :: !Bool
    }
    deriving (Eq, Show)

defaultRuntimeConfig :: RuntimeConfig
defaultRuntimeConfig =
    RuntimeConfig
        { configBlockSize = defaultBlockSize,
          configSampleRate = defaultSampleRate,
          configDebug = False
        }

-- | Compute context for a backend.
data Context b = Context
    { contextBackend :: !b,
      contextConfig :: !RuntimeConfig,
      contextRegistry :: !KernelRegistry
    }

instance (Show b) => Show (Context b) where
    show ctx =
        "Context { backend = "
            ++ show (contextBackend ctx)
            ++ ", config = "
            ++ show (contextConfig ctx)
            ++ " }"

createContext :: b -> RuntimeConfig -> IO (Context b)
createContext backend config =
    pure
        Context
            { contextBackend = backend,
              contextConfig = config,
              contextRegistry = emptyRegistry
            }

destroyContext :: Context b -> IO ()
destroyContext _ = pure ()

withContext :: b -> RuntimeConfig -> (Context b -> IO a) -> IO a
withContext backend config = bracket (createContext backend config) destroyContext
