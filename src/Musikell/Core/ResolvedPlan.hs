-- File: src/Musikell/Core/ResolvedPlan.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/core/resolved-plan.mdx
-- Module: Musikell.Core.ResolvedPlan
{-# LANGUAGE BangPatterns #-}

-- | Pre-resolved execution plan — zero allocation in the hot path.
--
-- The 'ResolvedPlan' captures all buffer pointers and kernel function
-- references at build time. During execution, the scheduler walks a
-- flat vector calling each pre-built IO () closure — no Map lookups,
-- no IntMap lookups, no list construction, no allocation.
--
-- This is the critical optimisation that makes Musikell's hot path
-- competitive with C-based audio engines.
module Musikell.Core.ResolvedPlan (
    -- * Types
    ResolvedNode (..),
    ResolvedPlan (..),

    -- * Construction
    buildResolvedPlan,

    -- * Execution
    executeResolvedBlock,

    -- * Queries
    resolvedPlanLength,
) where

import Data.Text (pack)

import qualified Data.Vector as V

import Musikell.Core.ExecutionPlan (ExecutionPlan, planNodes)
import Musikell.Core.Graph (Edge (..), Graph, getIncomingEdges, getNode)
import Musikell.Core.Node (NodeSpec, nodeKernel, nodeOutputs, portId)
import Musikell.Core.Types (
    KernelId (..),
    NodeId,
    PortId (..),
    SchedulerError (..),
 )
import Musikell.Memory.Buffer (Buffer)
import Musikell.Memory.Pool (BufferPool, getPoolBuffer)
import Musikell.Runtime.Kernel (
    KernelRegistry,
    KernelSpec (..),
    kernelExecute,
    lookupKernel,
 )

-- | A pre-resolved node: kernel function + buffer references captured
--   at build time. The 'rnExecute' closure contains everything needed
--   to process one block — no lookups required.
data ResolvedNode = ResolvedNode
    { rnNodeId :: !NodeId,
      rnKernel :: !KernelId,
      -- | Pre-resolved input buffers
      rnInputs :: ![Buffer],
      -- | Pre-resolved output buffers
      rnOutputs :: ![Buffer],
      -- | Pre-built closure: kernelExecute inputs outputs
      rnExecute :: IO ()
    }

-- | Flat vector of pre-resolved nodes in topological order.
newtype ResolvedPlan = ResolvedPlan
    { rpNodes :: V.Vector ResolvedNode
    }

instance Show ResolvedPlan where
    show rp = "ResolvedPlan[" ++ show (V.length (rpNodes rp)) ++ " nodes]"

-- | Build a resolved plan from an execution plan, graph, registry, and pool.
--
-- This is called ONCE at initialisation time. It resolves all kernel
-- references and buffer pointers so the hot path does zero lookups.
buildResolvedPlan ::
    ExecutionPlan ->
    Graph ->
    KernelRegistry ->
    BufferPool ->
    Either SchedulerError ResolvedPlan
buildResolvedPlan plan graph registry pool =
    case mapM resolveNode (planNodes plan) of
        Left err -> Left err
        Right nodes -> Right $ ResolvedPlan (V.fromList nodes)
    where
        resolveNode :: NodeId -> Either SchedulerError ResolvedNode
        resolveNode nid = do
            node <- case getNode nid graph of
                Nothing ->
                    Left
                        $ KernelExecutionFailed
                            (KernelId (pack "?"))
                            (pack "node not in graph")
                Just n -> Right n

            let kid = nodeKernel node
            spec <- case lookupKernel kid registry of
                Nothing -> Left $ KernelExecutionFailed kid (pack "kernel not registered")
                Just s -> Right s

            -- Resolve input buffers
            let inEdges = getIncomingEdges nid graph
            inputs <- case mapM resolveInputEdge inEdges of
                Nothing -> Left $ MissingBuffer nid (PortId 0)
                Just bs -> Right bs

            -- Resolve output buffers
            let outPorts = nodeOutputs node
            outputs <- case mapM (\p -> getPoolBuffer nid (portId p) pool) outPorts of
                Nothing -> Left $ MissingBuffer nid (PortId 0)
                Just bs -> Right bs

            -- Build the closure — this captures spec, inputs, outputs
            let !execute = kernelExecute spec inputs outputs

            Right
                $ ResolvedNode
                    { rnNodeId = nid,
                      rnKernel = kid,
                      rnInputs = inputs,
                      rnOutputs = outputs,
                      rnExecute = execute
                    }

        resolveInputEdge :: Edge -> Maybe Buffer
        resolveInputEdge e =
            let (srcNid, srcPid) = edgeSource e
            in  getPoolBuffer srcNid srcPid pool

-- | Execute one block through the resolved plan.
--
-- Walks the flat vector calling each pre-built closure.
-- Zero lookups, zero allocation — just function calls.
executeResolvedBlock :: ResolvedPlan -> IO ()
executeResolvedBlock (ResolvedPlan nodes) = V.mapM_ rnExecute nodes
{-# INLINE executeResolvedBlock #-}

-- | Number of nodes in the resolved plan.
resolvedPlanLength :: ResolvedPlan -> Int
resolvedPlanLength = V.length . rpNodes
