-- File: src/Musikell/Backend/Resolver.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/backend/resolver.mdx
-- Module: Musikell.Backend.Resolver
{-# LANGUAGE OverloadedStrings #-}

-- | Resolve a Nix flake reference to a shared library path.
--
-- Runs @nix build \<flake-ref\> --print-out-paths --no-link@ and
-- searches the resulting store path for a @.dylib@ or @.so@ file.
module Musikell.Backend.Resolver (
    resolveBackend,
    ResolveError (..),
) where

import Data.List (find)
import Data.Text (Text)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.Process (readProcess)

import qualified Data.Text as T

data ResolveError
    = NixBuildFailed String
    | NoLibraryFound FilePath
    deriving (Eq, Show)

-- | Build a flake ref and return the path to the shared library.
resolveBackend :: Text -> IO (Either ResolveError FilePath)
resolveBackend flakeRef = do
    let ref = T.unpack flakeRef
    result <- readProcess "nix" ["build", ref, "--print-out-paths", "--no-link"] ""
    let storePath = trimNewline result
    if null storePath then
        pure $ Left $ NixBuildFailed ("nix build returned empty output for: " ++ ref)
    else
        findLibrary storePath

findLibrary :: FilePath -> IO (Either ResolveError FilePath)
findLibrary storePath = do
    let libDir = storePath </> "lib"
    exists <- doesDirectoryExist libDir
    if not exists then
        pure $ Left $ NoLibraryFound storePath
    else do
        entries <- listDirectory libDir
        case find isSharedLib entries of
            Nothing -> pure $ Left $ NoLibraryFound libDir
            Just name -> pure $ Right (libDir </> name)

isSharedLib :: FilePath -> Bool
isSharedLib f = takeExtension f `elem` [".dylib", ".so"]

trimNewline :: String -> String
trimNewline = reverse . dropWhile (== '\n') . reverse
