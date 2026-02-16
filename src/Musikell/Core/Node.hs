-- File: src/Musikell/Core/Node.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/core/node.mdx
-- Module: Musikell.Core.Node

module Musikell.Core.Node (
    -- * Port Types
    Port (..),
    mkPort,

    -- * Node Specification
    NodeSpec (..),
    mkNodeSpec,
    mkNodeSpecWithChannels,

    -- * Node Queries
    nodeInputCount,
    nodeOutputCount,
    hasInput,
    hasOutput,
) where

import Data.Text (Text)

import Musikell.Core.Types (ChannelCount, KernelId, NodeId, PortId, defaultChannelCount)

-- | A port is a connection point on a node
data Port = Port
    { portId :: !PortId,
      portName :: !Text
    }
    deriving (Eq, Show)

-- | Create a port with the given ID and name
mkPort :: PortId -> Text -> Port
mkPort = Port

-- | Specification of a node in the graph
data NodeSpec = NodeSpec
    { nodeId :: !NodeId,
      nodeInputs :: ![Port],
      nodeOutputs :: ![Port],
      nodeKernel :: !KernelId,
      nodeChannels :: !ChannelCount
    }
    deriving (Eq, Show)

-- | Create a node specification (defaults to mono)
mkNodeSpec :: NodeId -> [Port] -> [Port] -> KernelId -> NodeSpec
mkNodeSpec nid ins outs kid = NodeSpec nid ins outs kid defaultChannelCount

-- | Create a node specification with explicit channel count
mkNodeSpecWithChannels :: NodeId -> [Port] -> [Port] -> KernelId -> ChannelCount -> NodeSpec
mkNodeSpecWithChannels = NodeSpec

-- | Get the number of input ports
nodeInputCount :: NodeSpec -> Int
nodeInputCount = length . nodeInputs

-- | Get the number of output ports
nodeOutputCount :: NodeSpec -> Int
nodeOutputCount = length . nodeOutputs

-- | Check if a node has an input port with the given ID
hasInput :: PortId -> NodeSpec -> Bool
hasInput pid node = any ((== pid) . portId) (nodeInputs node)

-- | Check if a node has an output port with the given ID
hasOutput :: PortId -> NodeSpec -> Bool
hasOutput pid node = any ((== pid) . portId) (nodeOutputs node)
