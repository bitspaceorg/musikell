-- File: src/Musikell/Core/Scheduler.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/core/scheduler.mdx
-- Module: Musikell.Core.Scheduler

-- | Block-based deterministic scheduler.
--
-- Two execution modes:
--
--   1. 'executeBlock' — legacy path using ExecutionPlan + per-block lookups
--   2. 'executeResolvedBlock' (from ResolvedPlan) — zero-alloc hot path
--
-- Prefer 'executeResolvedBlock' for production use.
module Musikell.Core.Scheduler (
    -- * Scheduler configuration
    SchedulerConfig (..),
    defaultSchedulerConfig,

    -- * Scheduler state
    SchedulerState (..),
    initScheduler,

    -- * Execution (legacy — per-block lookups)
    executeBlock,

    -- * Execution (resolved — zero allocation)
    executeResolvedBlockWithState,
) where

import Data.Text (pack)

import Musikell.Core.ExecutionPlan (ExecutionPlan, planNodes)
import Musikell.Core.Graph (Edge (..), Graph, getIncomingEdges, getNode)
import Musikell.Core.Node (nodeKernel, nodeOutputs, portId)
import Musikell.Core.ResolvedPlan (ResolvedPlan, executeResolvedBlock)
import Musikell.Core.Types (
    BlockIndex,
    BlockSize,
    KernelId (..),
    NodeId,
    PortId (..),
    SchedulerError (..),
    defaultBlockSize,
 )
import Musikell.Memory.Pool (BufferPool, getPoolBuffer, swapFeedbackBuffers)
import Musikell.Runtime.Kernel (KernelRegistry, kernelExecute, lookupKernel)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

newtype SchedulerConfig = SchedulerConfig
    { configBlockSize :: BlockSize
    }
    deriving (Eq, Show)

defaultSchedulerConfig :: SchedulerConfig
defaultSchedulerConfig =
    SchedulerConfig
        { configBlockSize = defaultBlockSize
        }

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

data SchedulerState = SchedulerState
    { stateCurrentBlock :: !BlockIndex,
      statePool :: !BufferPool,
      stateConfig :: !SchedulerConfig
    }
    deriving (Show)

initScheduler :: SchedulerConfig -> BufferPool -> SchedulerState
initScheduler config pool =
    SchedulerState
        { stateCurrentBlock = 0,
          statePool = pool,
          stateConfig = config
        }

-- ---------------------------------------------------------------------------
-- Execution (resolved — zero allocation)
-- ---------------------------------------------------------------------------

-- | Execute one block using the pre-resolved plan.
--
-- This is the preferred hot path: walks a flat vector of pre-built
-- closures. Zero lookups, zero allocation per block.
executeResolvedBlockWithState ::
    ResolvedPlan ->
    SchedulerState ->
    IO SchedulerState
executeResolvedBlockWithState rplan state = do
    executeResolvedBlock rplan
    swapFeedbackBuffers (statePool state)
    pure $! state {stateCurrentBlock = stateCurrentBlock state + 1}

-- ---------------------------------------------------------------------------
-- Execution (legacy — per-block lookups, kept for tests)
-- ---------------------------------------------------------------------------

-- | Execute one block through the entire graph (legacy path).
--
-- This performs Map/IntMap lookups per node per block. Use
-- 'executeResolvedBlockWithState' for production.
executeBlock ::
    ExecutionPlan ->
    Graph ->
    KernelRegistry ->
    SchedulerState ->
    IO (Either SchedulerError SchedulerState)
executeBlock plan graph registry state = go (planNodes plan)
    where
        pool = statePool state

        go [] = do
            swapFeedbackBuffers pool
            pure $ Right state {stateCurrentBlock = stateCurrentBlock state + 1}
        go (nid : rest) = do
            result <- executeNode nid
            case result of
                Left err -> pure (Left err)
                Right () -> go rest

        executeNode :: NodeId -> IO (Either SchedulerError ())
        executeNode nid = do
            case getNode nid graph of
                Nothing ->
                    pure $ Left $ KernelExecutionFailed (KernelId (pack "?")) (pack "node not in graph")
                Just node -> do
                    let kid = nodeKernel node
                    case lookupKernel kid registry of
                        Nothing ->
                            pure $ Left $ KernelExecutionFailed kid (pack "kernel not registered")
                        Just spec -> do
                            let inEdges = getIncomingEdges nid graph
                            let mInputs =
                                    mapM
                                        ( \e ->
                                            let (srcNid, srcPid) = edgeSource e
                                            in  getPoolBuffer srcNid srcPid pool
                                        )
                                        inEdges

                            let outPorts = nodeOutputs node
                            let mOutputs = mapM (\p -> getPoolBuffer nid (portId p) pool) outPorts

                            case (mInputs, mOutputs) of
                                (Just inputs, Just outputs) -> do
                                    kernelExecute spec inputs outputs
                                    pure (Right ())
                                (Nothing, _) ->
                                    pure $ Left $ MissingBuffer nid (PortId 0)
                                (_, Nothing) ->
                                    pure $ Left $ MissingBuffer nid (PortId 0)
