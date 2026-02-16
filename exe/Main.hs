-- |
-- Musikell — Deterministic, real-time, graph-based audio runtime
--
-- Usage:
--   musikell run <file.mkl>                       Execute a graph file
--   musikell run --backend <flake-ref> <file.mkl>  Execute with an external backend
--   musikell run --no-backend <file.mkl>           Execute with builtins only
--   musikell config set <key>=<value>             Set a config value
--   musikell config get <key>                     Get a config value
--   musikell version                              Show version information
--   musikell help                                 Show this help
--
-- Streaming model:
--   ./input.sh | musikell run graph.mkl | ./output.sh
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Musikell.CLI.Dispatch (dispatch)
import Musikell.CLI.Parse (parseArgs)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> hPutStrLn stderr ("error: " ++ show err) >> exitFailure
        Right cmd -> dispatch cmd
