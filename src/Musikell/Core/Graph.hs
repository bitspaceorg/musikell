-- File: src/Musikell/Core/Graph.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/modules/core/graph.mdx
-- Module: Musikell.Core.Graph

module Musikell.Core.Graph (
    -- * Edge Types
    EdgeKind (..),
    Edge (..),
    mkEdge,
    mkFeedbackEdge,

    -- * Graph Type
    Graph (..),
    emptyGraph,

    -- * Graph Construction
    addNode,
    addEdge,

    -- * Graph Queries
    nodeCount,
    edgeCount,
    getNode,
    getNodes,
    getEdges,
    getIncomingEdges,
    getOutgoingEdges,

    -- * Graph Validation
    validateGraph,
    isDAG,
) where

import Control.Monad (unless)
import Data.IntMap.Strict (IntMap)
import Data.List (foldl')

import qualified Data.IntMap.Strict as IntMap

import Musikell.Core.Node (NodeSpec, hasInput, hasOutput, nodeId)
import Musikell.Core.Types (GraphError (..), NodeId (..), PortId)

-- | The kind of edge in the graph
data EdgeKind
    = -- | Normal forward data flow
      ForwardEdge
    | -- | One-block-delay feedback loop
      FeedbackEdge
    deriving (Eq, Show)

-- | An edge connects an output port of one node to an input port of another
data Edge = Edge
    { -- | Source node and output port
      edgeSource :: !(NodeId, PortId),
      -- | Destination node and input port
      edgeSink :: !(NodeId, PortId),
      -- | Forward or feedback
      edgeKind :: !EdgeKind
    }
    deriving (Eq, Show)

-- | Create a forward edge from source to sink
mkEdge :: NodeId -> PortId -> NodeId -> PortId -> Edge
mkEdge srcNode srcPort dstNode dstPort = Edge (srcNode, srcPort) (dstNode, dstPort) ForwardEdge

-- | Create a feedback edge from source to sink
mkFeedbackEdge :: NodeId -> PortId -> NodeId -> PortId -> Edge
mkFeedbackEdge srcNode srcPort dstNode dstPort = Edge (srcNode, srcPort) (dstNode, dstPort) FeedbackEdge

-- | The audio processing graph (must be a DAG when feedback edges are excluded)
data Graph = Graph
    { graphNodes :: !(IntMap NodeSpec),
      graphEdges :: ![Edge]
    }
    deriving (Eq, Show)

-- | An empty graph with no nodes or edges
emptyGraph :: Graph
emptyGraph = Graph IntMap.empty []

-- | Add a node to the graph
addNode :: NodeSpec -> Graph -> Either GraphError Graph
addNode node graph
    | IntMap.member nid (graphNodes graph) = Left (DuplicateNodeId (NodeId nid))
    | otherwise = Right $ graph {graphNodes = IntMap.insert nid node (graphNodes graph)}
    where
        nid = getNodeId (nodeId node)

-- | Add an edge to the graph (validates ports exist for forward edges;
-- skips port validation for feedback edges since the target may not
-- exist yet in the topological sense)
addEdge :: Edge -> Graph -> Either GraphError Graph
addEdge edge graph = case edgeKind edge of
    FeedbackEdge -> Right $ graph {graphEdges = edge : graphEdges graph}
    ForwardEdge -> do
        -- Validate source node and port
        srcNode <-
            maybe (Left $ DanglingEdge srcNodeId srcPortId) Right
                $ getNode srcNodeId graph
        unless (hasOutput srcPortId srcNode)
            $ Left
            $ InvalidPort srcNodeId srcPortId

        -- Validate destination node and port
        dstNode <-
            maybe (Left $ DanglingEdge dstNodeId dstPortId) Right
                $ getNode dstNodeId graph
        unless (hasInput dstPortId dstNode)
            $ Left
            $ InvalidPort dstNodeId dstPortId

        Right $ graph {graphEdges = edge : graphEdges graph}
    where
        (srcNodeId, srcPortId) = edgeSource edge
        (dstNodeId, dstPortId) = edgeSink edge

-- | Get the number of nodes in the graph
nodeCount :: Graph -> Int
nodeCount = IntMap.size . graphNodes

-- | Get the number of edges in the graph
edgeCount :: Graph -> Int
edgeCount = length . graphEdges

-- | Get a node by ID
getNode :: NodeId -> Graph -> Maybe NodeSpec
getNode (NodeId nid) graph = IntMap.lookup nid (graphNodes graph)

-- | Get all nodes in the graph
getNodes :: Graph -> [NodeSpec]
getNodes = IntMap.elems . graphNodes

-- | Get all edges in the graph
getEdges :: Graph -> [Edge]
getEdges = graphEdges

-- | Get edges where the given node is the destination
getIncomingEdges :: NodeId -> Graph -> [Edge]
getIncomingEdges nid = filter ((== nid) . fst . edgeSink) . graphEdges

-- | Get edges where the given node is the source
getOutgoingEdges :: NodeId -> Graph -> [Edge]
getOutgoingEdges nid = filter ((== nid) . fst . edgeSource) . graphEdges

-- | Validate that the graph is a valid DAG (excluding feedback edges)
validateGraph :: Graph -> Either GraphError ()
validateGraph graph
    | not (isDAG graph) = Left $ CycleDetected []
    | otherwise = Right ()

-- | Check if the graph is a DAG using Kahn's algorithm.
-- Feedback edges are excluded from the check — only forward edges
-- must form a DAG.
isDAG :: Graph -> Bool
isDAG graph = go initialQueue initialInDegree 0
    where
        nodes = IntMap.keys (graphNodes graph)
        -- Only consider forward edges for DAG check
        edges = filter ((== ForwardEdge) . edgeKind) (graphEdges graph)

        -- Compute in-degree for each node
        initialInDegree :: IntMap Int
        initialInDegree = foldl' countIncoming (IntMap.fromList [(n, 0) | n <- nodes]) edges

        countIncoming :: IntMap Int -> Edge -> IntMap Int
        countIncoming deg edge =
            IntMap.adjust (+ 1) (getNodeId . fst $ edgeSink edge) deg

        -- Nodes with in-degree 0
        initialQueue :: [Int]
        initialQueue = [n | n <- nodes, IntMap.findWithDefault 0 n initialInDegree == 0]

        -- Build adjacency list
        adjacency :: IntMap [Int]
        adjacency = foldl' addAdj (IntMap.fromList [(n, []) | n <- nodes]) edges

        addAdj :: IntMap [Int] -> Edge -> IntMap [Int]
        addAdj adj edge =
            IntMap.adjust ((getNodeId . fst $ edgeSink edge) :) (getNodeId . fst $ edgeSource edge) adj

        -- Process nodes
        go :: [Int] -> IntMap Int -> Int -> Bool
        go [] _ processed = processed == length nodes
        go (n : queue) inDeg processed =
            let neighbors = IntMap.findWithDefault [] n adjacency
                (newQueue, newDeg) = foldl' processNeighbor (queue, inDeg) neighbors
            in  go newQueue newDeg (processed + 1)

        processNeighbor :: ([Int], IntMap Int) -> Int -> ([Int], IntMap Int)
        processNeighbor (q, deg) neighbor =
            let newDeg = IntMap.adjust (subtract 1) neighbor deg
                newInDeg = IntMap.findWithDefault 0 neighbor newDeg
            in  if newInDeg == 0 then
                    (neighbor : q, newDeg)
                else
                    (q, newDeg)
