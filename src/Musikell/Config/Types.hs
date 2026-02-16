-- File: src/Musikell/Config/Types.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/config/types.mdx
-- Module: Musikell.Config.Types
{-# LANGUAGE OverloadedStrings #-}

-- | Flat key=value configuration.
--
-- Stored at @~\/.config\/musikell\/config@ (XDG_CONFIG_HOME).
-- Format: one @key=value@ per line.
module Musikell.Config.Types (
    Config (..),
    configPath,
    readConfig,
    writeConfig,
    setConfigKey,
    getConfigKey,
) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype Config = Config
    { configDefaultBackend :: Maybe Text
    }
    deriving (Eq, Show)

configPath :: IO FilePath
configPath = do
    dir <- getXdgDirectory XdgConfig "musikell"
    pure (dir </> "config")

readConfig :: IO Config
readConfig = do
    path <- configPath
    exists <- doesFileExist path
    if not exists then
        pure (Config Nothing)
    else do
        kvs <- parseConfigFile <$> TIO.readFile path
        pure
            Config
                { configDefaultBackend = Map.lookup "default_backend" kvs
                }

writeConfig :: Config -> IO ()
writeConfig cfg = do
    path <- configPath
    dir <- getXdgDirectory XdgConfig "musikell"
    createDirectoryIfMissing True dir
    let kvs = maybe Map.empty (Map.singleton "default_backend") (configDefaultBackend cfg)
    TIO.writeFile path (serializeConfig kvs)

setConfigKey :: Text -> Text -> IO ()
setConfigKey key val = do
    path <- configPath
    dir <- getXdgDirectory XdgConfig "musikell"
    createDirectoryIfMissing True dir
    exists <- doesFileExist path
    kvs <-
        if exists then
            parseConfigFile <$> TIO.readFile path
        else
            pure Map.empty
    let kvs' = Map.insert key val kvs
    TIO.writeFile path (serializeConfig kvs')

getConfigKey :: Text -> IO (Maybe Text)
getConfigKey key = do
    path <- configPath
    exists <- doesFileExist path
    if not exists then
        pure Nothing
    else do
        kvs <- parseConfigFile <$> TIO.readFile path
        pure (Map.lookup key kvs)

-- Internal helpers

parseConfigFile :: Text -> Map Text Text
parseConfigFile = Map.fromList . concatMap parseLine . T.lines
    where
        parseLine line
            | T.null line = []
            | T.isPrefixOf "#" line = []
            | otherwise = case T.breakOn "=" line of
                (_, "") -> []
                (k, rest) -> [(T.strip k, T.strip (T.drop 1 rest))]

serializeConfig :: Map Text Text -> Text
serializeConfig = T.unlines . map (\(k, v) -> k <> "=" <> v) . Map.toAscList
