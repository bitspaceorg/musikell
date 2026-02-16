-- File: src/Musikell/FFI/Exports.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/modules/ffi/exports.mdx
-- Module: Musikell.FFI.Exports
{-# LANGUAGE ForeignFunctionInterface #-}

module Musikell.FFI.Exports (
    -- * Initialization
    musikell_init,
    musikell_shutdown,

    -- * Version Information
    musikell_abi_version,
    musikell_version_string,

    -- * Runtime Control
    musikell_set_block_size,
    musikell_get_block_size,
) where

import Data.IORef
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import System.IO.Unsafe (unsafePerformIO)

import Musikell.Core.Types (BlockSize, defaultBlockSize)

-- | Global state for the runtime (mutable)
{-# NOINLINE globalBlockSize #-}
globalBlockSize :: IORef BlockSize
globalBlockSize = unsafePerformIO $ newIORef defaultBlockSize

-- | Initialize the Musikell runtime
-- Returns 0 on success, non-zero on failure
foreign export ccall musikell_init :: IO CInt

musikell_init :: IO CInt
musikell_init = do
    -- Reset to defaults
    writeIORef globalBlockSize defaultBlockSize
    -- Future: initialize backend, allocate buffers, etc.
    pure 0

-- | Shutdown the Musikell runtime
-- Returns 0 on success, non-zero on failure
foreign export ccall musikell_shutdown :: IO CInt

musikell_shutdown :: IO CInt
musikell_shutdown = do
    -- Future: release resources, shutdown backend
    pure 0

-- | Get the ABI version
foreign export ccall musikell_abi_version :: IO CUInt

musikell_abi_version :: IO CUInt
musikell_abi_version = pure 1

-- | Get the version string
-- Caller must not free the returned pointer
foreign export ccall musikell_version_string :: IO CString

musikell_version_string :: IO CString
musikell_version_string = newCString "0.1.0"

-- | Set the block size
foreign export ccall musikell_set_block_size :: CUInt -> IO CInt

musikell_set_block_size :: CUInt -> IO CInt
musikell_set_block_size size
    | size <= 0 = pure (-1) -- Invalid
    | size > 8192 = pure (-1) -- Too large
    | otherwise = do
        writeIORef globalBlockSize (fromIntegral size)
        pure 0

-- | Get the current block size
foreign export ccall musikell_get_block_size :: IO CUInt

musikell_get_block_size :: IO CUInt
musikell_get_block_size = fromIntegral <$> readIORef globalBlockSize
