-- File: src/Musikell/Core/Types.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/modules/core/types.mdx
-- Module: Musikell.Core.Types
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Musikell.Core.Types (
    -- * Size Types
    BlockSize,
    SampleRate,
    BlockIndex,
    ChannelCount,

    -- * Identifier Types
    NodeId (..),
    PortId (..),
    KernelId (..),

    -- * Accessors
    unNodeId,
    unPortId,

    -- * Defaults
    defaultBlockSize,
    defaultSampleRate,
    defaultChannelCount,

    -- * Error Types
    GraphError (..),
    PlanError (..),
    SchedulerError (..),
) where

import Data.Text (Text)

-- | Number of samples per block
type BlockSize = Int

-- | Samples per second
type SampleRate = Int

-- | Block counter for scheduling
type BlockIndex = Int

-- | Number of audio channels (1 = mono, 2 = stereo)
type ChannelCount = Int

-- | Unique identifier for a node in the graph
newtype NodeId = NodeId {getNodeId :: Int}
    deriving (Enum, Eq, Num, Ord, Show)

-- | Unique identifier for a port on a node
newtype PortId = PortId {getPortId :: Int}
    deriving (Enum, Eq, Num, Ord, Show)

-- | Identifier for a kernel implementation
newtype KernelId = KernelId {getKernelId :: Text}
    deriving (Eq, Ord, Show)

-- | Default block size: 256 frames
defaultBlockSize :: BlockSize
defaultBlockSize = 256

-- | Default sample rate: 44100 Hz
defaultSampleRate :: SampleRate
defaultSampleRate = 44100

-- | Default channel count: 1 (mono, backwards compatible)
defaultChannelCount :: ChannelCount
defaultChannelCount = 1

-- | Errors that can occur during graph validation
data GraphError
    = CycleDetected [NodeId]
    | DanglingEdge NodeId PortId
    | DuplicateNodeId NodeId
    | InvalidPort NodeId PortId
    deriving (Eq, Show)

-- | Errors that can occur during execution plan construction
data PlanError
    = GraphValidationFailed GraphError
    | EmptyGraph
    deriving (Eq, Show)

-- | Errors that can occur during scheduling
data SchedulerError
    = MissingBuffer NodeId PortId
    | KernelExecutionFailed KernelId Text
    | InvalidBlockSize BlockSize
    deriving (Eq, Show)

-- | Unwrap NodeId to raw Int
unNodeId :: NodeId -> Int
unNodeId = getNodeId

-- | Unwrap PortId to raw Int
unPortId :: PortId -> Int
unPortId = getPortId
