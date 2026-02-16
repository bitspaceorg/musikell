{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the CLI argument parser.
module Test.Musikell.CLI.Parse (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Musikell.CLI.Command
import Musikell.CLI.Parse (parseArgs)
import Musikell.Core.Types (defaultBlockSize)

tests :: TestTree
tests =
    testGroup
        "Parse"
        [ testGroup
            "run"
            [ testCase "basic run"
                $ parseArgs ["run", "graph.mkl"]
                    @?= Right (CmdRun defaultRunOptions "graph.mkl"),
              testCase "run with --block before file"
                $ parseArgs ["run", "--block", "512", "graph.mkl"]
                    @?= Right (CmdRun (defaultRunOptions {runBlockSize = 512}) "graph.mkl"),
              testCase "run with --block after file"
                $ parseArgs ["run", "graph.mkl", "--block", "512"]
                    @?= Right (CmdRun (defaultRunOptions {runBlockSize = 512}) "graph.mkl"),
              testCase "run with --backend before file"
                $ parseArgs ["run", "--backend", "github:User/repo", "graph.mkl"]
                    @?= Right (CmdRun (defaultRunOptions {runBackend = BackendExplicit "github:User/repo"}) "graph.mkl"),
              testCase "run with --backend after file"
                $ parseArgs ["run", "graph.mkl", "--backend", "github:User/repo"]
                    @?= Right (CmdRun (defaultRunOptions {runBackend = BackendExplicit "github:User/repo"}) "graph.mkl"),
              testCase "run with --no-backend"
                $ parseArgs ["run", "--no-backend", "graph.mkl"]
                    @?= Right (CmdRun (defaultRunOptions {runBackend = BackendNone}) "graph.mkl"),
              testCase "order independence: --block --backend file"
                $ parseArgs ["run", "--block", "512", "--backend", "x", "f.mkl"]
                    @?= Right (CmdRun RunOptions {runBackend = BackendExplicit "x", runBlockSize = 512} "f.mkl"),
              testCase "order independence: file --backend --block"
                $ parseArgs ["run", "f.mkl", "--backend", "x", "--block", "512"]
                    @?= Right (CmdRun RunOptions {runBackend = BackendExplicit "x", runBlockSize = 512} "f.mkl"),
              testCase "order independence: --backend file --block"
                $ parseArgs ["run", "--backend", "x", "f.mkl", "--block", "512"]
                    @?= Right (CmdRun RunOptions {runBackend = BackendExplicit "x", runBlockSize = 512} "f.mkl"),
              testCase "run missing file"
                $ parseArgs ["run"]
                    @?= Left (MissingArgument "run requires a file argument"),
              testCase "run invalid --block value"
                $ parseArgs ["run", "--block", "abc", "f.mkl"]
                    @?= Left (InvalidArgument "--block" "abc"),
              testCase "conflicting --backend and --no-backend"
                $ case parseArgs ["run", "--backend", "x", "--no-backend", "f.mkl"] of
                    Left (AmbiguousFlag _) -> pure ()
                    other -> assertFailure $ "expected AmbiguousFlag, got: " ++ show other,
              testCase "conflicting --no-backend and --backend"
                $ case parseArgs ["run", "--no-backend", "--backend", "x", "f.mkl"] of
                    Left (AmbiguousFlag _) -> pure ()
                    other -> assertFailure $ "expected AmbiguousFlag, got: " ++ show other,
              testCase "unknown flag rejected"
                $ case parseArgs ["run", "--verbose", "f.mkl"] of
                    Left (InvalidArgument _ _) -> pure ()
                    other -> assertFailure $ "expected InvalidArgument, got: " ++ show other
            ],
          testGroup
            "config"
            [ testCase "config set"
                $ parseArgs ["config", "set", "key=value"]
                    @?= Right (CmdConfigSet "key" "value"),
              testCase "config get"
                $ parseArgs ["config", "get", "mykey"]
                    @?= Right (CmdConfigGet "mykey"),
              testCase "config set malformed"
                $ parseArgs ["config", "set", "noequals"]
                    @?= Left (MalformedKeyValue "noequals"),
              testCase "config set missing arg"
                $ parseArgs ["config", "set"]
                    @?= Left (MissingArgument "config set requires key=value"),
              testCase "config get missing arg"
                $ parseArgs ["config", "get"]
                    @?= Left (MissingArgument "config get requires a key")
            ],
          testGroup
            "top-level"
            [ testCase "no args -> help"
                $ parseArgs [] @?= Right CmdHelp,
              testCase "help"
                $ parseArgs ["help"] @?= Right CmdHelp,
              testCase "-h"
                $ parseArgs ["-h"] @?= Right CmdHelp,
              testCase "--help"
                $ parseArgs ["--help"] @?= Right CmdHelp,
              testCase "version"
                $ parseArgs ["version"] @?= Right CmdVersion,
              testCase "unknown command"
                $ parseArgs ["foobar"]
                    @?= Left (UnknownCommand "foobar")
            ]
        ]
