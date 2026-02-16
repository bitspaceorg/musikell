{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.Musikell.Core.ExecutionPlan
-- Description : Tests for Core.ExecutionPlan module
-- License     : MIT
module Test.Musikell.Core.ExecutionPlan (tests) where

import Data.List (elemIndex)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Musikell.Core.ExecutionPlan
import Musikell.Core.Graph
import Musikell.Core.Node
import Musikell.Core.Types

tests :: TestTree
tests =
    testGroup
        "Core.ExecutionPlan"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "buildPlan fails on empty graph"
            $ case buildPlan emptyGraph of
                Left EmptyGraph -> pure ()
                Left err -> assertFailure $ "Wrong error: " ++ show err
                Right _ -> assertFailure "Should fail on empty graph",
          testCase "buildPlan succeeds on single node" $ do
            let node = mkNodeSpec (NodeId 0) [] [] (KernelId "test")
            case addNode node emptyGraph of
                Left err -> assertFailure $ "addNode failed: " ++ show err
                Right g -> case buildPlan g of
                    Left err -> assertFailure $ "buildPlan failed: " ++ show err
                    Right plan -> planLength plan @?= 1,
          testCase "buildPlan produces topological order" $ do
            -- Build: 0 -> 1 -> 2
            let node0 = mkNodeSpec (NodeId 0) [] [mkPort (PortId 0) "out"] (KernelId "a")
            let node1 = mkNodeSpec (NodeId 1) [mkPort (PortId 0) "in"] [mkPort (PortId 0) "out"] (KernelId "b")
            let node2 = mkNodeSpec (NodeId 2) [mkPort (PortId 0) "in"] [] (KernelId "c")
            let edge01 = mkEdge (NodeId 0) (PortId 0) (NodeId 1) (PortId 0)
            let edge12 = mkEdge (NodeId 1) (PortId 0) (NodeId 2) (PortId 0)

            case addNode node0 emptyGraph
                >>= addNode node1
                >>= addNode node2
                >>= addEdge edge01
                >>= addEdge edge12 of
                Left err -> assertFailure $ "Graph construction failed: " ++ show err
                Right g -> case buildPlan g of
                    Left err -> assertFailure $ "buildPlan failed: " ++ show err
                    Right plan -> do
                        let order = planNodes plan
                        planLength plan @?= 3
                        -- Node 0 must come before Node 1, Node 1 before Node 2
                        let indexOf nid = case elemIndex nid order of
                                Nothing -> error "Node not in plan"
                                Just i -> i
                        (indexOf (NodeId 0) < indexOf (NodeId 1)) @?= True
                        (indexOf (NodeId 1) < indexOf (NodeId 2)) @?= True,
          testCase "buildPlan handles diamond graph" $ do
            -- Build:   0
            --         / \
            --        1   2
            --         \ /
            --          3
            let node0 =
                    mkNodeSpec
                        (NodeId 0)
                        []
                        [mkPort (PortId 0) "out1", mkPort (PortId 1) "out2"]
                        (KernelId "a")
            let node1 =
                    mkNodeSpec
                        (NodeId 1)
                        [mkPort (PortId 0) "in"]
                        [mkPort (PortId 0) "out"]
                        (KernelId "b")
            let node2 =
                    mkNodeSpec
                        (NodeId 2)
                        [mkPort (PortId 0) "in"]
                        [mkPort (PortId 0) "out"]
                        (KernelId "c")
            let node3 =
                    mkNodeSpec
                        (NodeId 3)
                        [mkPort (PortId 0) "in1", mkPort (PortId 1) "in2"]
                        []
                        (KernelId "d")
            let edge01 = mkEdge (NodeId 0) (PortId 0) (NodeId 1) (PortId 0)
            let edge02 = mkEdge (NodeId 0) (PortId 1) (NodeId 2) (PortId 0)
            let edge13 = mkEdge (NodeId 1) (PortId 0) (NodeId 3) (PortId 0)
            let edge23 = mkEdge (NodeId 2) (PortId 0) (NodeId 3) (PortId 1)

            case addNode node0 emptyGraph
                >>= addNode node1
                >>= addNode node2
                >>= addNode node3
                >>= addEdge edge01
                >>= addEdge edge02
                >>= addEdge edge13
                >>= addEdge edge23 of
                Left err -> assertFailure $ "Graph construction failed: " ++ show err
                Right g -> case buildPlan g of
                    Left err -> assertFailure $ "buildPlan failed: " ++ show err
                    Right plan -> do
                        planLength plan @?= 4
                        -- Node 0 must be first, Node 3 must be last
                        let order = planNodes plan
                        head order @?= NodeId 0
                        last order @?= NodeId 3
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ testProperty "plan length equals node count"
            $ \(Positive n) ->
                let nodes = [mkNodeSpec (NodeId i) [] [] (KernelId "test") | i <- [0 .. n - 1]]
                    graphResult = foldl (\g node -> g >>= addNode node) (Right emptyGraph) nodes
                in  case graphResult of
                        Left _ -> False
                        Right g -> case buildPlan g of
                            Left _ -> False
                            Right plan -> planLength plan == n,
          testProperty "all nodes appear in plan"
            $ \(Positive n) ->
                let nodeIds = [NodeId i | i <- [0 .. n - 1]]
                    nodes = [mkNodeSpec nid [] [] (KernelId "test") | nid <- nodeIds]
                    graphResult = foldl (\g node -> g >>= addNode node) (Right emptyGraph) nodes
                in  case graphResult of
                        Left _ -> False
                        Right g -> case buildPlan g of
                            Left _ -> False
                            Right plan -> all (`elem` planNodes plan) nodeIds
        ]
