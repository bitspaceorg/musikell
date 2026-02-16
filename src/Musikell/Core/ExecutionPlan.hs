-- File: src/Musikell/Core/ExecutionPlan.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/core/execution-plan.mdx
-- Module: Musikell.Core.ExecutionPlan

module Musikell.Core.ExecutionPlan (
    -- * Execution Plan
    ExecutionPlan (..),

    -- * Plan Construction
    buildPlan,

    -- * Plan Queries
    planLength,
    planNodes,
) where

import Control.Monad (when)
import Data.IntMap.Strict (IntMap)
import Data.List (foldl')

import qualified Data.IntMap.Strict as IntMap

import Musikell.Core.Graph (Edge (..), EdgeKind (..), Graph, graphEdges, graphNodes, validateGraph)
import Musikell.Core.Types (GraphError (..), NodeId (..), PlanError (..))

-- | Immutable execution plan containing topologically sorted node IDs
newtype ExecutionPlan = ExecutionPlan
    { planOrder :: [NodeId]
    }
    deriving (Eq, Show)

-- | Build an execution plan from a validated graph
buildPlan :: Graph -> Either PlanError ExecutionPlan
buildPlan graph = do
    -- First validate the graph
    case validateGraph graph of
        Left err -> Left (GraphValidationFailed err)
        Right () -> pure ()

    -- Check for empty graph
    when (IntMap.null (graphNodes graph))
        $ Left EmptyGraph

    -- Perform topological sort
    let sorted = topologicalSort graph
    Right $ ExecutionPlan sorted

-- | Get the number of nodes in the plan
planLength :: ExecutionPlan -> Int
planLength = length . planOrder

-- | Get the ordered list of nodes
planNodes :: ExecutionPlan -> [NodeId]
planNodes = planOrder

-- | Topological sort using Kahn's algorithm
topologicalSort :: Graph -> [NodeId]
topologicalSort graph = go initialQueue initialInDegree []
    where
        nodes = IntMap.keys (graphNodes graph)
        -- Only consider forward edges for topological ordering
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

        -- Process nodes in topological order
        go :: [Int] -> IntMap Int -> [NodeId] -> [NodeId]
        go [] _ result = reverse result
        go (n : queue) inDeg result =
            let neighbors = IntMap.findWithDefault [] n adjacency
                (newQueue, newDeg) = foldl' processNeighbor (queue, inDeg) neighbors
            in  go newQueue newDeg (NodeId n : result)

        processNeighbor :: ([Int], IntMap Int) -> Int -> ([Int], IntMap Int)
        processNeighbor (q, deg) neighbor =
            let newDeg = IntMap.adjust (subtract 1) neighbor deg
                newInDeg = IntMap.findWithDefault 0 neighbor newDeg
            in  if newInDeg == 0 then
                    (neighbor : q, newDeg)
                else
                    (q, newDeg)
