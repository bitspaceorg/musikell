-- File: src/Musikell/IO/StreamOutput.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/io/stream-output.mdx
-- Module: Musikell.IO.StreamOutput

-- | Raw PCM output to stdout.
--
-- Format: 32-bit IEEE 754 float, little-endian, mono.
module Musikell.IO.StreamOutput (
    -- * Block writing
    writeBlock,

    -- * Conduit sink
    sinkStdout,

    -- * Conversion
    bufferToBytes,
    unsafeBufferToBytes,
    bufferIntoBs,
    bufferToBlock,
) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, await)
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import System.IO (hFlush, stdout)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Musikell.Memory.Buffer (Buffer, unsafeWithBuffer)

-- | Write raw PCM bytes to stdout and flush.
writeBlock :: ByteString -> IO ()
writeBlock bs = BS.hPut stdout bs >> hFlush stdout

-- | Conduit sink that writes blocks to stdout.
sinkStdout :: ConduitT ByteString () IO ()
sinkStdout = go
    where
        go = do
            mbs <- await
            case mbs of
                Nothing -> pure ()
                Just bs -> liftIO (writeBlock bs) >> go

-- | Convert a Buffer to raw PCM bytes by reinterpreting the float
--   data as bytes (IEEE 754, native byte order).
--   Safe version: copies the data into a new ByteString.
bufferToBytes :: Buffer -> IO ByteString
bufferToBytes buf = unsafeWithBuffer buf $ \ptr size ->
    BS.packCStringLen (castPtr ptr, size * 4)

-- | Zero-copy buffer to ByteString. Shares the underlying pointer —
--   the result is only valid until the next write into this buffer.
--   Use in the hot path where the ByteString is consumed (written to
--   stdout) before the next block overwrites the buffer.
unsafeBufferToBytes :: Buffer -> IO ByteString
unsafeBufferToBytes buf = unsafeWithBuffer buf $ \ptr size ->
    BSU.unsafePackCStringLen (castPtr ptr, size * 4)

-- | Write buffer contents into a pre-allocated output staging area.
--   Uses memcpy for maximum throughput.
bufferIntoBs :: Buffer -> Ptr Word8 -> IO ()
bufferIntoBs buf dstPtr = unsafeWithBuffer buf $ \ptr size ->
    copyBytes dstPtr (castPtr ptr) (size * 4)

-- | Alias for 'bufferToBytes'.
bufferToBlock :: Buffer -> IO ByteString
bufferToBlock = bufferToBytes
