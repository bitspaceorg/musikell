-- File: src/Musikell/CLI/Dispatch.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/cli/dispatch.mdx
-- Module: Musikell.CLI.Dispatch
{-# LANGUAGE OverloadedStrings #-}

-- | CLI command dispatch for musikell.
--
-- Maps parsed 'Command' values to IO actions.
module Musikell.CLI.Dispatch (
    dispatch,
) where

import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Musikell.Backend.Resolver (resolveBackend)
import Musikell.CLI.Command
import Musikell.Config.Types (getConfigKey, setConfigKey)
import Musikell.Core.Types (defaultChannelCount)
import Musikell.IO.ConduitPipeline (runPipeline)
import Musikell.Language.Lowering (LoweringResult (..), lowerProgram)
import Musikell.Language.Parser (parseFile)
import Musikell.Runtime.Kernel (KernelRegistry, registerKernel)
import Musikell.Runtime.Kernel.Builtin (builtinRegistry)
import Musikell.Runtime.Kernel.Foreign (loadBackend)

-- | Execute a parsed CLI command.
dispatch :: Command -> IO ()
dispatch (CmdRun opts file) = runGraph opts file
dispatch (CmdConfigSet key val) = configSet key val
dispatch (CmdConfigGet key) = configGet key
dispatch CmdVersion = printVersion
dispatch CmdHelp = printHelp

-- | Set a config key=value.
configSet :: Text -> Text -> IO ()
configSet key val = do
    setConfigKey key val
    hPutStrLn stderr $ "[musikell] set " ++ T.unpack key ++ "=" ++ T.unpack val

-- | Get a config value.
configGet :: Text -> IO ()
configGet key = do
    mval <- getConfigKey key
    case mval of
        Nothing -> hPutStrLn stderr $ "[musikell] " ++ T.unpack key ++ " is not set"
        Just val -> TIO.putStrLn val

-- | Resolve a 'BackendOption' to a 'Maybe Text' for the registry builder.
resolveBackendOption :: BackendOption -> IO (Maybe Text)
resolveBackendOption BackendDefault = getConfigKey "default_backend"
resolveBackendOption (BackendExplicit ref) = pure (Just ref)
resolveBackendOption BackendNone = pure Nothing

-- | Build the kernel registry, optionally loading an external backend.
buildRegistry :: Maybe Text -> IO KernelRegistry
buildRegistry Nothing = pure builtinRegistry
buildRegistry (Just flakeRef) = do
    hPutStrLn stderr $ "[musikell] resolving backend: " ++ T.unpack flakeRef
    eLibPath <- resolveBackend flakeRef
    case eLibPath of
        Left err -> do
            hPutStrLn stderr $ "[musikell] backend resolve error: " ++ show err
            exitFailure
        Right libPath -> do
            hPutStrLn stderr $ "[musikell] loading: " ++ libPath
            eSpecs <- loadBackend libPath
            case eSpecs of
                Left err -> do
                    hPutStrLn stderr $ "[musikell] backend load error: " ++ show err
                    exitFailure
                Right specs -> do
                    hPutStrLn stderr $ "[musikell] loaded " ++ show (length specs) ++ " external kernel(s)"
                    pure $ foldr registerKernel builtinRegistry specs

-- | Parse -> Lower -> Schedule -> Stream.
runGraph :: RunOptions -> FilePath -> IO ()
runGraph opts file = do
    mBackend <- resolveBackendOption (runBackend opts)
    registry <- buildRegistry mBackend
    let blockSize = runBlockSize opts
    hPutStrLn stderr $ "[musikell] parsing " ++ file
    parseResult <- parseFile file
    case parseResult of
        Left pe -> do
            hPutStrLn stderr $ "[musikell] parse error: " ++ show pe
            exitFailure
        Right ast -> do
            hPutStrLn stderr "[musikell] lowering"
            lowerResult <- lowerProgram registry ast
            case lowerResult of
                Left le -> do
                    hPutStrLn stderr $ "[musikell] lowering error: " ++ show le
                    exitFailure
                Right lr -> do
                    hPutStrLn stderr $ "[musikell] block size: " ++ show blockSize
                    hPutStrLn stderr "[musikell] streaming: stdin -> graph -> stdout"
                    hPutStrLn stderr "[musikell] press Ctrl-C to terminate"
                    runPipeline
                        (lowerPlan lr)
                        (lowerGraph lr)
                        (lowerRegistry lr)
                        (lowerInputNode lr)
                        (lowerOutputNode lr)
                        blockSize
                        defaultChannelCount

printVersion :: IO ()
printVersion = do
    putStrLn "musikell 0.1.0"
    putStrLn "ABI version: 1"

printHelp :: IO ()
printHelp = do
    putStrLn "musikell — deterministic, real-time, graph-based audio runtime"
    putStrLn ""
    putStrLn "commands:"
    putStrLn "  run <file.mkl>                        execute a graph file"
    putStrLn "    --block <size>                      block size in samples (default: 256)"
    putStrLn "    --backend <flake-ref>               use an external backend"
    putStrLn "    --no-backend                        builtins only (ignore default_backend)"
    putStrLn "  config set <key>=<value>              set a config value"
    putStrLn "  config get <key>                      get a config value"
    putStrLn "  version                               show version information"
    putStrLn "  help                                  show this help"
    putStrLn ""
    putStrLn "backends:"
    putStrLn "  musikell config set default_backend=github:User/repo"
    putStrLn "  musikell run --backend=github:User/repo graph.mkl"
    putStrLn ""
    putStrLn "streaming model:"
    putStrLn "  ./input.sh | musikell run graph.mkl | ./output.sh"
    putStrLn ""
    putStrLn "the runtime reads raw PCM (32-bit float, little-endian) from stdin,"
    putStrLn "processes it through the audio graph, and writes to stdout."
    putStrLn ""
    putStrLn "sample rate: 44100 Hz (configurable in .mkl)"
    putStrLn "block size:  256 samples (configurable via --block)"
