-- File: src/Musikell/Memory/Buffer.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/modules/memory/buffer.mdx
-- Module: Musikell.Memory.Buffer
{-# LANGUAGE BangPatterns #-}

module Musikell.Memory.Buffer (
    -- * Buffer Types
    Buffer (..),
    BufferRef,

    -- * Buffer Construction
    allocateBuffer,
    allocateBufferWith,
    zeroBuffer,

    -- * Buffer Operations
    bufferLength,
    readSample,
    writeSample,
    copyBuffer,
    copyBufferSlice,
    fillBuffer,

    -- * Unsafe Operations (no bounds checks — for hot-path kernels)
    unsafeReadSample,
    unsafeWriteSample,

    -- * Buffer Conversion
    bufferToList,
    listToBuffer,

    -- * Unsafe Operations (for FFI)
    unsafeBufferPtr,
    unsafeWithBuffer,
) where

import Data.IORef (IORef, newIORef)
import Data.Vector.Storable.Mutable (IOVector)
import Foreign.Ptr (Ptr)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Musikell.Core.Types (BlockSize)

-- | A buffer holds a fixed-size block of audio samples
data Buffer = Buffer
    { bufferData :: !(IOVector Float),
      bufferSize :: !BlockSize
    }

instance Show Buffer where
    show buf = "Buffer { size = " ++ show (bufferSize buf) ++ " }"

-- | Reference to a buffer (for scheduler state)
type BufferRef = IORef Buffer

-- | Allocate a new buffer of the given size (zero-initialized)
allocateBuffer :: BlockSize -> IO Buffer
allocateBuffer size = do
    vec <- MV.replicate size 0.0
    pure $ Buffer vec size

-- | Allocate a buffer with a specific initial value
allocateBufferWith :: BlockSize -> Float -> IO Buffer
allocateBufferWith size val = do
    vec <- MV.replicate size val
    pure $ Buffer vec size

-- | Create a buffer reference
zeroBuffer :: BlockSize -> IO BufferRef
zeroBuffer size = do
    buf <- allocateBuffer size
    newIORef buf

-- | Get the length of a buffer
bufferLength :: Buffer -> Int
bufferLength = bufferSize

-- | Read a sample at the given index
readSample :: Buffer -> Int -> IO Float
readSample buf idx
    | idx < 0 || idx >= bufferSize buf = pure 0.0 -- Safe bounds
    | otherwise = MV.read (bufferData buf) idx

-- | Write a sample at the given index
writeSample :: Buffer -> Int -> Float -> IO ()
writeSample buf idx val
    | idx < 0 || idx >= bufferSize buf = pure () -- Safe bounds
    | otherwise = MV.write (bufferData buf) idx val

-- | Read a sample without bounds checking (caller guarantees valid index)
unsafeReadSample :: Buffer -> Int -> IO Float
unsafeReadSample buf = MV.unsafeRead (bufferData buf)
{-# INLINE unsafeReadSample #-}

-- | Write a sample without bounds checking (caller guarantees valid index)
unsafeWriteSample :: Buffer -> Int -> Float -> IO ()
unsafeWriteSample buf = MV.unsafeWrite (bufferData buf)
{-# INLINE unsafeWriteSample #-}

-- | Copy contents from source to destination buffer (memcpy via storable vector)
copyBuffer :: Buffer -> Buffer -> IO ()
copyBuffer src dst = do
    let len = min (bufferSize src) (bufferSize dst)
        srcSlice = MV.unsafeSlice 0 len (bufferData src)
        dstSlice = MV.unsafeSlice 0 len (bufferData dst)
    MV.copy dstSlice srcSlice

-- | Copy a slice from source to destination buffer
-- copyBufferSlice src srcOff dst dstOff count
copyBufferSlice :: Buffer -> Int -> Buffer -> Int -> Int -> IO ()
copyBufferSlice src srcOff dst dstOff count = do
    let srcSlice = MV.unsafeSlice srcOff count (bufferData src)
        dstSlice = MV.unsafeSlice dstOff count (bufferData dst)
    MV.copy dstSlice srcSlice

-- | Fill buffer with a constant value
fillBuffer :: Buffer -> Float -> IO ()
fillBuffer buf = MV.set (bufferData buf)

-- | Convert buffer to a list (for testing/debugging)
bufferToList :: Buffer -> IO [Float]
bufferToList buf = do
    frozen <- V.unsafeFreeze (bufferData buf)
    let result = V.toList frozen
    -- Re-thaw so the buffer remains mutable for future use
    _ <- V.unsafeThaw frozen
    pure result

-- | Create a buffer from a list
listToBuffer :: [Float] -> IO Buffer
listToBuffer xs = do
    let !vec = V.fromList xs
    mvec <- V.thaw vec
    pure $ Buffer mvec (V.length vec)

-- | Get the raw pointer to buffer data (unsafe, for FFI)
unsafeBufferPtr :: Buffer -> IO (Ptr Float)
unsafeBufferPtr buf = do
    let vec = bufferData buf
    pure $ MV.unsafeToForeignPtr0 vec `seq` error "Use unsafeWithBuffer instead"

-- | Execute an action with the buffer's raw pointer
unsafeWithBuffer :: Buffer -> (Ptr Float -> Int -> IO a) -> IO a
unsafeWithBuffer buf action = MV.unsafeWith (bufferData buf) $ \ptr ->
    action ptr (bufferSize buf)
