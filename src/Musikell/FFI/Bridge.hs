-- File: src/Musikell/FFI/Bridge.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/ffi/bridge.mdx
-- Module: Musikell.FFI.Bridge
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Musikell.FFI.Bridge (
    -- * C Types
    CBuffer (..),
    CKernelDescriptor (..),
    CBackendDescriptor (..),
    CRuntimeInfo (..),
    CExecutionPlan (..),

    -- * Function Pointers
    KernelFnPtr,
    mkKernelFn,

    -- * Conversion
    bufferToCBuffer,
    cBufferToBuffer,

    -- * Tick loop FFI
    c_mkl_tick_once,

    -- * Constants
    mklOk,
    mklErrInvalid,
    mklErrOverflow,
    mklErrInternal,
    mklAbiVersion,
) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Musikell.Core.Types (BlockSize)
import Musikell.Memory.Buffer (Buffer, bufferToList, listToBuffer, unsafeWithBuffer)
import Musikell.Memory.Layout (defaultAlignment, defaultChannels)

-- | ABI version constant
mklAbiVersion :: CInt
mklAbiVersion = 1

-- | Return codes
mklOk, mklErrInvalid, mklErrOverflow, mklErrInternal :: CInt
mklOk = 0
mklErrInvalid = -1
mklErrOverflow = -2
mklErrInternal = -3

-- | C-compatible buffer structure
data CBuffer = CBuffer
    { cbData :: Ptr CFloat,
      cbSize :: CUInt,
      cbAlignment :: CUInt,
      cbChannels :: CUInt
    }

instance Storable CBuffer where
    sizeOf _ = 4 * 8 -- 4 fields, assume 8-byte alignment
    alignment _ = 8

    peek ptr = do
        d <- peekByteOff ptr 0
        s <- peekByteOff ptr 8
        a <- peekByteOff ptr 12
        c <- peekByteOff ptr 16
        pure $ CBuffer d s a c

    poke ptr (CBuffer d s a c) = do
        pokeByteOff ptr 0 d
        pokeByteOff ptr 8 s
        pokeByteOff ptr 12 a
        pokeByteOff ptr 16 c

-- | C kernel function pointer type
type KernelFnPtr = FunPtr (Ptr CBuffer -> CUInt -> Ptr CBuffer -> CUInt -> IO CInt)

-- | Convert function pointer to callable function
foreign import ccall "dynamic"
    mkKernelFn :: KernelFnPtr -> (Ptr CBuffer -> CUInt -> Ptr CBuffer -> CUInt -> IO CInt)

-- | C-compatible kernel descriptor
data CKernelDescriptor = CKernelDescriptor
    { ckdAbiVersion :: CUInt,
      ckdKernelName :: CString,
      ckdKernelVersion :: CString,
      ckdInputCount :: CUInt,
      ckdOutputCount :: CUInt,
      ckdExecute :: KernelFnPtr,
      ckdUserData :: Ptr ()
    }

instance Storable CKernelDescriptor where
    -- 7 fields: abi_version(4) + pad(4) + kernel_name(8) + kernel_version(8)
    --         + input_count(4) + output_count(4) + execute(8) + user_data(8) = 48
    sizeOf _ = 48
    alignment _ = 8

    peek ptr = do
        abi <- peekByteOff ptr 0
        name <- peekByteOff ptr 8
        ver <- peekByteOff ptr 16
        ic <- peekByteOff ptr 24
        oc <- peekByteOff ptr 28
        exe <- peekByteOff ptr 32
        ud <- peekByteOff ptr 40
        pure $ CKernelDescriptor abi name ver ic oc exe ud

    poke ptr (CKernelDescriptor abi name ver ic oc exe ud) = do
        pokeByteOff ptr 0 abi
        pokeByteOff ptr 8 name
        pokeByteOff ptr 16 ver
        pokeByteOff ptr 24 ic
        pokeByteOff ptr 28 oc
        pokeByteOff ptr 32 exe
        pokeByteOff ptr 40 ud

-- | C-compatible backend descriptor
data CBackendDescriptor = CBackendDescriptor
    { cbdAbiVersion :: CUInt,
      cbdBackendName :: CString,
      cbdBackendVersion :: CString
    }

-- | C-compatible runtime info
data CRuntimeInfo = CRuntimeInfo
    { criBlockSize :: CUInt,
      criSampleRate :: CUInt,
      criChannelCount :: CUInt
    }

instance Storable CRuntimeInfo where
    sizeOf _ = 12
    alignment _ = 4

    peek ptr = do
        bs <- peekByteOff ptr 0
        sr <- peekByteOff ptr 4
        cc <- peekByteOff ptr 8
        pure $ CRuntimeInfo bs sr cc

    poke ptr (CRuntimeInfo bs sr cc) = do
        pokeByteOff ptr 0 bs
        pokeByteOff ptr 4 sr
        pokeByteOff ptr 8 cc

-- | Convert a Haskell buffer to a C buffer (for passing to external kernels)
bufferToCBuffer :: Buffer -> IO CBuffer
bufferToCBuffer buf = unsafeWithBuffer buf $ \ptr size ->
    pure
        $ CBuffer
            { cbData = castPtr ptr,
              cbSize = fromIntegral size,
              cbAlignment = fromIntegral defaultAlignment,
              cbChannels = fromIntegral defaultChannels
            }

-- | Convert a C buffer back to a Haskell buffer
cBufferToBuffer :: CBuffer -> IO Buffer
cBufferToBuffer cbuf = do
    let size = fromIntegral (cbSize cbuf)
    samples <- peekArray size (castPtr $ cbData cbuf :: Ptr Float)
    listToBuffer samples

-- ---------------------------------------------------------------------------
-- C Execution Plan (for native tick loop)
-- ---------------------------------------------------------------------------

-- | Opaque handle to the C-side MklExecutionPlan.
-- The Haskell side allocates and fills this struct, then passes it
-- to mkl_tick_once via FFI.
newtype CExecutionPlan = CExecutionPlan (Ptr ())

-- | FFI binding to the C tick loop.
foreign import ccall "mkl_tick_once"
    c_mkl_tick_once :: Ptr () -> IO ()
