{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.Musikell.Core.Graph
-- Description : Tests for Core.Graph module
-- License     : MIT
module Test.Musikell.Core.Graph (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Musikell.Core.Graph
import Musikell.Core.Node
import Musikell.Core.Types

tests :: TestTree
tests =
    testGroup
        "Core.Graph"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "emptyGraph has no nodes"
            $ nodeCount emptyGraph @?= 0,
          testCase "emptyGraph has no edges"
            $ edgeCount emptyGraph @?= 0,
          testCase "addNode increases node count" $ do
            let node = mkNodeSpec (NodeId 0) [] [] (KernelId "test")
            case addNode node emptyGraph of
                Left err -> assertFailure $ "addNode failed: " ++ show err
                Right g -> nodeCount g @?= 1,
          testCase "addNode rejects duplicate IDs" $ do
            let node = mkNodeSpec (NodeId 0) [] [] (KernelId "test")
            case addNode node emptyGraph of
                Left _ -> assertFailure "First addNode should succeed"
                Right g1 -> case addNode node g1 of
                    Left (DuplicateNodeId _) -> pure ()
                    Left err -> assertFailure $ "Wrong error type: " ++ show err
                    Right _ -> assertFailure "Should reject duplicate ID",
          testCase "addEdge validates source port" $ do
            let node1 = mkNodeSpec (NodeId 0) [] [mkPort (PortId 0) "out"] (KernelId "a")
            let node2 = mkNodeSpec (NodeId 1) [mkPort (PortId 0) "in"] [] (KernelId "b")
            let edge = mkEdge (NodeId 0) (PortId 0) (NodeId 1) (PortId 0)
            case addNode node1 emptyGraph >>= addNode node2 >>= addEdge edge of
                Left err -> assertFailure $ "addEdge failed: " ++ show err
                Right g -> edgeCount g @?= 1,
          testCase "validateGraph accepts DAG" $ do
            let node1 = mkNodeSpec (NodeId 0) [] [mkPort (PortId 0) "out"] (KernelId "a")
            let node2 = mkNodeSpec (NodeId 1) [mkPort (PortId 0) "in"] [] (KernelId "b")
            let edge = mkEdge (NodeId 0) (PortId 0) (NodeId 1) (PortId 0)
            case addNode node1 emptyGraph >>= addNode node2 >>= addEdge edge of
                Left err -> assertFailure $ "Graph construction failed: " ++ show err
                Right g -> case validateGraph g of
                    Left err -> assertFailure $ "validateGraph failed: " ++ show err
                    Right () -> pure (),
          testCase "isDAG returns True for linear graph" $ do
            let node1 = mkNodeSpec (NodeId 0) [] [mkPort (PortId 0) "out"] (KernelId "a")
            let node2 = mkNodeSpec (NodeId 1) [mkPort (PortId 0) "in"] [mkPort (PortId 0) "out"] (KernelId "b")
            let node3 = mkNodeSpec (NodeId 2) [mkPort (PortId 0) "in"] [] (KernelId "c")
            let edge1 = mkEdge (NodeId 0) (PortId 0) (NodeId 1) (PortId 0)
            let edge2 = mkEdge (NodeId 1) (PortId 0) (NodeId 2) (PortId 0)
            case addNode node1 emptyGraph
                >>= addNode node2
                >>= addNode node3
                >>= addEdge edge1
                >>= addEdge edge2 of
                Left err -> assertFailure $ "Graph construction failed: " ++ show err
                Right g -> isDAG g @?= True,
          testCase "mkEdge creates ForwardEdge" $ do
            let edge = mkEdge (NodeId 0) (PortId 0) (NodeId 1) (PortId 0)
            edgeKind edge @?= ForwardEdge,
          testCase "mkFeedbackEdge creates FeedbackEdge" $ do
            let edge = mkFeedbackEdge (NodeId 0) (PortId 0) (NodeId 1) (PortId 0)
            edgeKind edge @?= FeedbackEdge,
          testCase "feedback edge skips port validation" $ do
            let node1 = mkNodeSpec (NodeId 0) [] [mkPort (PortId 0) "out"] (KernelId "a")
            let fbEdge = mkFeedbackEdge (NodeId 0) (PortId 0) (NodeId 99) (PortId 0)
            -- Node 99 doesn't exist, but feedback edge should still succeed
            case addNode node1 emptyGraph >>= addEdge fbEdge of
                Left err -> assertFailure $ "feedback edge should skip validation: " ++ show err
                Right g -> edgeCount g @?= 1,
          testCase "isDAG ignores feedback edges" $ do
            -- A -> B with forward, B -> A with feedback should still be a DAG
            let node1 = mkNodeSpec (NodeId 0) [mkPort (PortId 0) "in"] [mkPort (PortId 0) "out"] (KernelId "a")
            let node2 = mkNodeSpec (NodeId 1) [mkPort (PortId 0) "in"] [mkPort (PortId 0) "out"] (KernelId "b")
            let fwdEdge = mkEdge (NodeId 0) (PortId 0) (NodeId 1) (PortId 0)
            let fbEdge = mkFeedbackEdge (NodeId 1) (PortId 0) (NodeId 0) (PortId 0)
            case addNode node1 emptyGraph >>= addNode node2 >>= addEdge fwdEdge >>= addEdge fbEdge of
                Left err -> assertFailure $ "Graph construction failed: " ++ show err
                Right g -> do
                    isDAG g @?= True
                    edgeCount g @?= 2
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ testProperty "node count increases monotonically"
            $ \(Positive n) ->
                let nodes = [mkNodeSpec (NodeId i) [] [] (KernelId "test") | i <- [0 .. n - 1]]
                    result = foldl (\g node -> g >>= addNode node) (Right emptyGraph) nodes
                in  case result of
                        Left _ -> False
                        Right g -> nodeCount g == n,
          testProperty "getNode retrieves added node"
            $ \(NonNegative nid) ->
                let node = mkNodeSpec (NodeId nid) [] [] (KernelId "test")
                in  case addNode node emptyGraph of
                        Left _ -> False
                        Right g -> case getNode (NodeId nid) g of
                            Nothing -> False
                            Just n -> nodeId n == NodeId nid
        ]
