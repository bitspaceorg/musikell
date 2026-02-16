-- File: src/Musikell/IO/StreamInput.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/io/stream-input.mdx
-- Module: Musikell.IO.StreamInput

-- | Raw PCM input from stdin.
--
-- Format: 32-bit IEEE 754 float, little-endian, mono.
-- This matches the output of most Unix audio tools (sox, ffmpeg -f f32le).
module Musikell.IO.StreamInput (
    -- * Block reading
    readBlock,
    readBlockStrict,

    -- * Conduit source
    sourceStdin,

    -- * Conversion
    bytesToBuffer,
    bytesIntoBuffer,
    blockToBuffer,
) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, yield)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import System.IO (isEOF, stdin)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

import Musikell.Core.Types (BlockSize)
import Musikell.Memory.Buffer (Buffer (..), allocateBuffer, unsafeWithBuffer)

-- | Bytes per sample (32-bit float).
sampleBytes :: Int
sampleBytes = 4

-- | Read one block of raw PCM from stdin.  Returns 'Nothing' on EOF.
readBlock :: BlockSize -> IO (Maybe ByteString)
readBlock blockSize = do
    eof <- isEOF
    if eof then
        pure Nothing
    else do
        let needed = blockSize * sampleBytes
        bs <- BS.hGet stdin needed
        if BS.null bs then pure Nothing else pure (Just bs)

-- | Read one block, zero-padding if the stream is short.
readBlockStrict :: BlockSize -> IO ByteString
readBlockStrict blockSize = do
    mbs <- readBlock blockSize
    let needed = blockSize * sampleBytes
    case mbs of
        Nothing -> pure $ BS.replicate needed 0
        Just bs
            | BS.length bs < needed -> pure $ bs <> BS.replicate (needed - BS.length bs) 0
            | otherwise -> pure bs

-- | Conduit source that reads fixed-size blocks from stdin.
sourceStdin :: BlockSize -> ConduitT () ByteString IO ()
sourceStdin blockSize = go
    where
        go = do
            mbs <- liftIO $ readBlock blockSize
            case mbs of
                Nothing -> pure ()
                Just bs -> yield bs >> go

-- | Convert raw PCM bytes to a Buffer by reinterpreting the memory
--   as IEEE 754 float32 (native byte order — correct on LE platforms,
--   which covers x86/ARM).
--   Allocates a new buffer. For hot-path use, prefer 'bytesIntoBuffer'.
bytesToBuffer :: ByteString -> IO Buffer
bytesToBuffer bs = do
    let sampleCount = BS.length bs `div` sampleBytes
    buf <- allocateBuffer sampleCount
    bytesIntoBuffer bs buf
    pure buf

-- | Zero-copy write of raw PCM bytes into an existing pre-allocated buffer.
--   Uses memcpy from the ByteString's underlying ForeignPtr directly into
--   the buffer's mutable vector. No per-sample loop, no allocation.
bytesIntoBuffer :: ByteString -> Buffer -> IO ()
bytesIntoBuffer bs buf = do
    let byteCount = BS.length bs
    let (fptr, off, _len) = BSI.toForeignPtr bs
    withForeignPtr fptr $ \basePtr ->
        unsafeWithBuffer buf $ \bufPtr _size ->
            copyBytes (castPtr bufPtr) (basePtr `plusPtr` off) byteCount

-- | Alias for 'bytesToBuffer'.
blockToBuffer :: ByteString -> IO Buffer
blockToBuffer = bytesToBuffer
