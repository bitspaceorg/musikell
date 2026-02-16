-- File: src/Musikell/Memory/Pool.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/memory/pool.mdx
-- Module: Musikell.Memory.Pool

-- | Pre-allocated buffer pool for real-time safe scheduling.
--
-- Every output port in the graph gets a buffer allocated exactly once
-- at initialisation.  During execution the scheduler only performs
-- IntMap lookups — zero allocation in the hot path.
module Musikell.Memory.Pool (
    -- * Pool type
    BufferPool (..),

    -- * Construction
    allocatePool,
    allocatePoolWithFeedback,

    -- * Access
    getPoolBuffer,
    getFeedbackBuffer,
    poolSize,

    -- * Maintenance
    resetPool,
    swapFeedbackBuffers,
) where

import Data.IORef
import Data.IntMap.Strict (IntMap)

import qualified Data.IntMap.Strict as IntMap

import Musikell.Core.Graph (Edge (..), EdgeKind (..), Graph, getEdges, getNodes)
import Musikell.Core.Node (NodeSpec, nodeChannels, nodeId, nodeOutputs, portId)
import Musikell.Core.Types (BlockSize, NodeId (..), PortId (..))
import Musikell.Memory.Buffer (Buffer, allocateBuffer, fillBuffer)

-- ---------------------------------------------------------------------------
-- Key encoding
-- ---------------------------------------------------------------------------

-- | Encode (NodeId, PortId) into a single Int for IntMap lookup.
--   Supports up to 1 000 ports per node which is far beyond any
--   realistic audio graph.
bufferKey :: NodeId -> PortId -> Int
bufferKey (NodeId nid) (PortId pid) = nid * 1000 + pid

-- ---------------------------------------------------------------------------
-- Pool type
-- ---------------------------------------------------------------------------

-- | A feedback buffer pair: current (written this block) and previous (read this block).
data FeedbackPair = FeedbackPair
    { fbCurrent :: !(IORef Buffer),
      fbPrevious :: !(IORef Buffer)
    }

data BufferPool = BufferPool
    { poolBuffers :: !(IntMap Buffer),
      poolBlockSize :: !BlockSize,
      -- | Feedback buffer pairs keyed by (NodeId, PortId)
      poolFeedback :: !(IntMap FeedbackPair)
    }

instance Show BufferPool where
    show p =
        "BufferPool["
            ++ show (IntMap.size (poolBuffers p))
            ++ " bufs, "
            ++ show (IntMap.size (poolFeedback p))
            ++ " feedback, block="
            ++ show (poolBlockSize p)
            ++ "]"

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Allocate one zero-initialised buffer per output port in the graph.
allocatePool :: BlockSize -> Graph -> IO BufferPool
allocatePool bs graph = do
    bufs <- mapM (allocNodeBufs bs) (getNodes graph)
    pure $ BufferPool (IntMap.unions bufs) bs IntMap.empty

-- | Allocate pool with feedback buffer pairs for feedback edges.
allocatePoolWithFeedback :: BlockSize -> Graph -> IO BufferPool
allocatePoolWithFeedback bs graph = do
    bufs <- mapM (allocNodeBufs bs) (getNodes graph)
    fbPairs <- allocFeedbackPairs bs graph
    pure $ BufferPool (IntMap.unions bufs) bs fbPairs

-- | Allocate feedback buffer pairs for all feedback edges in the graph.
allocFeedbackPairs :: BlockSize -> Graph -> IO (IntMap FeedbackPair)
allocFeedbackPairs bs graph = do
    let feedbackEdges = filter ((== FeedbackEdge) . edgeKind) (getEdges graph)
    pairs <- mapM mkFbPair feedbackEdges
    pure $ IntMap.fromList pairs
    where
        mkFbPair edge = do
            let (srcNid, srcPid) = edgeSource edge
            cur <- allocateBuffer bs >>= newIORef
            prev <- allocateBuffer bs >>= newIORef
            pure (bufferKey srcNid srcPid, FeedbackPair cur prev)

allocNodeBufs :: BlockSize -> NodeSpec -> IO (IntMap Buffer)
allocNodeBufs bs node = do
    pairs <- mapM mkBuf (nodeOutputs node)
    pure $ IntMap.fromList pairs
    where
        nid = nodeId node
        channels = nodeChannels node
        mkBuf port = do
            buf <- allocateBuffer (bs * channels)
            pure (bufferKey nid (portId port), buf)

-- ---------------------------------------------------------------------------
-- Access
-- ---------------------------------------------------------------------------

-- | O(log n) buffer lookup by (NodeId, PortId).
getPoolBuffer :: NodeId -> PortId -> BufferPool -> Maybe Buffer
getPoolBuffer nid pid pool =
    IntMap.lookup (bufferKey nid pid) (poolBuffers pool)

-- | Get the previous-block (feedback) buffer for a feedback edge source.
-- Returns Nothing if no feedback pair exists for this (NodeId, PortId).
getFeedbackBuffer :: NodeId -> PortId -> BufferPool -> IO (Maybe Buffer)
getFeedbackBuffer nid pid pool =
    case IntMap.lookup (bufferKey nid pid) (poolFeedback pool) of
        Nothing -> pure Nothing
        Just fb -> Just <$> readIORef (fbPrevious fb)

-- | Number of buffers in the pool.
poolSize :: BufferPool -> Int
poolSize = IntMap.size . poolBuffers

-- ---------------------------------------------------------------------------
-- Maintenance
-- ---------------------------------------------------------------------------

-- | Zero every buffer in the pool.  Call between blocks only if your
--   kernels rely on zero-initialised output buffers.
resetPool :: BufferPool -> IO ()
resetPool pool = mapM_ (`fillBuffer` 0.0) (IntMap.elems (poolBuffers pool))

-- | Swap current ↔ previous pointers for all feedback buffer pairs.
-- This is a pointer swap, not a copy — zero allocation.
swapFeedbackBuffers :: BufferPool -> IO ()
swapFeedbackBuffers pool = mapM_ swapPair (IntMap.elems (poolFeedback pool))
    where
        swapPair (FeedbackPair curRef prevRef) = do
            cur <- readIORef curRef
            prev <- readIORef prevRef
            writeIORef curRef prev
            writeIORef prevRef cur
