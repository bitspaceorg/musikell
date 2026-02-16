-- File: src/Musikell/Runtime/Kernel/Foreign.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/foreign.mdx
-- Module: Musikell.Runtime.Kernel.Foreign
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Load external kernels from a shared library via dlopen.
--
-- The shared library must export a function @mkl_register_kernels@
-- conforming to the 'MklRegisterKernelsFn' signature from Contract.h.
module Musikell.Runtime.Kernel.Foreign (
    loadBackend,
    ForeignLoadError (..),
) where

import Control.Monad (void)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (mallocArray, peekArray)
import Foreign.Ptr
import Foreign.Storable (peek, pokeElemOff)

import qualified Data.Text as T

import Musikell.Core.Types (KernelId (..))
import Musikell.FFI.Bridge (
    CBuffer,
    CKernelDescriptor (..),
    bufferToCBuffer,
    mkKernelFn,
    mklAbiVersion,
 )
import Musikell.Memory.Buffer (Buffer)
import Musikell.Runtime.Kernel (KernelSpec (..))

data ForeignLoadError
    = DlOpenFailed String
    | SymbolNotFound String
    | AbiVersionMismatch CUInt
    deriving (Eq, Show)

-- Raw FFI to dlopen / dlsym / dlerror
foreign import ccall "dlopen"
    c_dlopen :: CString -> CInt -> IO (Ptr ())

foreign import ccall "dlsym"
    c_dlsym :: Ptr () -> CString -> IO (FunPtr a)

foreign import ccall "dlerror"
    c_dlerror :: IO CString

-- RTLD_NOW = 2
rtldNow :: CInt
rtldNow = 2

-- | Load a shared library and extract all kernel specs.
loadBackend :: FilePath -> IO (Either ForeignLoadError [KernelSpec])
loadBackend path = do
    handle <- withCString path $ \cpath -> c_dlopen cpath rtldNow
    if handle == nullPtr then do
        err <- c_dlerror >>= peekCString
        pure $ Left $ DlOpenFailed err
    else do
        sym <- withCString "mkl_register_kernels" $ \csym -> c_dlsym handle csym
        if sym == nullFunPtr then
            pure $ Left $ SymbolNotFound "mkl_register_kernels"
        else do
            let registerFn = mkRegisterKernelsFn (castFunPtr sym)
            alloca $ \countPtr -> do
                descPtr <- registerFn countPtr
                count <- peek countPtr
                descs <- peekArray (fromIntegral count) descPtr
                specs <- mapM descriptorToKernelSpec descs
                case sequence specs of
                    Left e -> pure $ Left e
                    Right ss -> pure $ Right ss

-- | Convert a foreign MklRegisterKernelsFn to a callable Haskell function.
type RegisterKernelsFn = Ptr CUInt -> IO (Ptr CKernelDescriptor)

foreign import ccall "dynamic"
    mkRegisterKernelsFn :: FunPtr RegisterKernelsFn -> RegisterKernelsFn

-- | Convert a single C kernel descriptor to a KernelSpec.
descriptorToKernelSpec :: CKernelDescriptor -> IO (Either ForeignLoadError KernelSpec)
descriptorToKernelSpec desc = do
    let abi = ckdAbiVersion desc
    if abi /= fromIntegral mklAbiVersion then
        pure $ Left $ AbiVersionMismatch abi
    else do
        name <- peekCString (ckdKernelName desc)
        let kid = KernelId (T.pack name)
            nIn = fromIntegral (ckdInputCount desc)
            nOut = fromIntegral (ckdOutputCount desc)
            fnPtr = ckdExecute desc
            callFn = mkKernelFn fnPtr
        -- Pre-allocate CBuffer arrays once at load time, reuse per block.
        inArr <- mallocArray nIn
        outArr <- mallocArray nOut
        pure
            $ Right
                KernelSpec
                    { kernelId = kid,
                      kernelInputs = nIn,
                      kernelOutputs = nOut,
                      kernelExecute = \inputs outputs -> do
                        -- Fill pre-allocated arrays in-place (no allocation)
                        fillCBufferArray inArr inputs 0
                        fillCBufferArray outArr outputs 0
                        -- bufferToCBuffer exposes the raw underlying pointer, so
                        -- the C kernel writes directly into the Haskell buffer
                        -- memory. No copy-back is needed.
                        void $ callFn inArr (fromIntegral nIn) outArr (fromIntegral nOut)
                    }

-- | Fill a pre-allocated CBuffer array from a list of Buffers (no allocation).
fillCBufferArray :: Ptr CBuffer -> [Buffer] -> Int -> IO ()
fillCBufferArray _ [] _ = pure ()
fillCBufferArray arr (b : bs) i = do
    cb <- bufferToCBuffer b
    pokeElemOff arr i cb
    fillCBufferArray arr bs (i + 1)
