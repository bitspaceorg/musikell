-- File: src/Musikell/CLI/Command.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/cli/command.mdx
-- Module: Musikell.CLI.Command

-- | CLI command types for musikell.
--
-- Pure data types representing parsed CLI commands.
-- No IO or parsing logic lives here.
module Musikell.CLI.Command (
    Command (..),
    RunOptions (..),
    BackendOption (..),
    ParseError (..),
    defaultRunOptions,
) where

import Data.Text (Text)

import Musikell.Core.Types (BlockSize, defaultBlockSize)

data Command
    = CmdRun !RunOptions !FilePath
    | CmdConfigSet !Text !Text
    | CmdConfigGet !Text
    | CmdVersion
    | CmdHelp
    deriving (Eq, Show)

data RunOptions = RunOptions
    { runBackend :: !BackendOption,
      runBlockSize :: !BlockSize
    }
    deriving (Eq, Show)

data BackendOption
    = BackendDefault
    | BackendExplicit !Text
    | BackendNone
    deriving (Eq, Show)

data ParseError
    = UnknownCommand String
    | MissingArgument String
    | InvalidArgument String String
    | AmbiguousFlag String
    | MalformedKeyValue String
    deriving (Eq, Show)

defaultRunOptions :: RunOptions
defaultRunOptions =
    RunOptions
        { runBackend = BackendDefault,
          runBlockSize = defaultBlockSize
        }
