-- File: src/Musikell/CLI/Parse.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/cli/parse.mdx
-- Module: Musikell.CLI.Parse
{-# LANGUAGE OverloadedStrings #-}

-- | Pure CLI argument parser for musikell.
--
-- Parses @[String]@ into a 'Command' with no IO.
-- Flags can appear in any order for the @run@ subcommand.
module Musikell.CLI.Parse (
    parseArgs,
) where

import qualified Data.Text as T

import Musikell.CLI.Command

-- | Parse command-line arguments into a Command.
parseArgs :: [String] -> Either ParseError Command
parseArgs [] = Right CmdHelp
parseArgs ("run" : rest) = parseRun rest
parseArgs ("config" : rest) = parseConfig rest
parseArgs ("version" : _) = Right CmdVersion
parseArgs ("help" : _) = Right CmdHelp
parseArgs ("-h" : _) = Right CmdHelp
parseArgs ("--help" : _) = Right CmdHelp
parseArgs (cmd : _) = Left (UnknownCommand cmd)

-- | Parse the @config@ subcommand.
parseConfig :: [String] -> Either ParseError Command
parseConfig ("set" : kv : _) = parseKeyValue kv
parseConfig ["set"] = Left (MissingArgument "config set requires key=value")
parseConfig ("get" : key : _) = Right (CmdConfigGet (T.pack key))
parseConfig ["get"] = Left (MissingArgument "config get requires a key")
parseConfig (sub : _) = Left (UnknownCommand ("config " ++ sub))
parseConfig [] = Left (MissingArgument "config requires a subcommand (set or get)")

-- | Parse a @key=value@ string into a 'CmdConfigSet'.
parseKeyValue :: String -> Either ParseError Command
parseKeyValue kv = case break (== '=') kv of
    (_, "") -> Left (MalformedKeyValue kv)
    (key, rest) -> Right (CmdConfigSet (T.pack key) (T.pack (drop 1 rest)))

-- | Parse the @run@ subcommand with order-independent flags.
parseRun :: [String] -> Either ParseError Command
parseRun args = do
    (opts, mFile) <- consumeRunArgs defaultRunOptions Nothing args
    case mFile of
        Nothing -> Left (MissingArgument "run requires a file argument")
        Just file -> Right (CmdRun opts file)

-- | Intermediate state while consuming backend flags.
-- Tracks whether we've seen --backend or --no-backend.
data BackendSeen = SeenNone | SeenExplicit | SeenNoBackend
    deriving (Eq)

-- | Recursively consume run flags and positional args.
consumeRunArgs ::
    RunOptions ->
    Maybe FilePath ->
    [String] ->
    Either ParseError (RunOptions, Maybe FilePath)
consumeRunArgs opts mFile [] = Right (opts, mFile)
consumeRunArgs opts mFile ("--block" : val : rest) =
    case safeReadInt val of
        Nothing -> Left (InvalidArgument "--block" val)
        Just n -> consumeRunArgs (opts {runBlockSize = n}) mFile rest
consumeRunArgs opts mFile ("--backend" : ref : rest) =
    case backendSeen opts of
        SeenNoBackend -> Left (AmbiguousFlag "--backend conflicts with --no-backend")
        _ -> consumeRunArgs (opts {runBackend = BackendExplicit (T.pack ref)}) mFile rest
consumeRunArgs opts mFile ("--no-backend" : rest) =
    case backendSeen opts of
        SeenExplicit -> Left (AmbiguousFlag "--no-backend conflicts with --backend")
        _ -> consumeRunArgs (opts {runBackend = BackendNone}) mFile rest
consumeRunArgs _ _ (('-' : '-' : flag) : _) =
    Left (InvalidArgument ("--" ++ flag) "unknown flag")
consumeRunArgs opts Nothing (arg : rest) =
    consumeRunArgs opts (Just arg) rest
consumeRunArgs _ (Just _) (arg : _) =
    Left (InvalidArgument arg "unexpected positional argument")

-- | Determine what backend flag has been seen so far.
backendSeen :: RunOptions -> BackendSeen
backendSeen opts = case runBackend opts of
    BackendDefault -> SeenNone
    BackendExplicit _ -> SeenExplicit
    BackendNone -> SeenNoBackend

-- | Safe integer parsing using 'reads'.
safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing
