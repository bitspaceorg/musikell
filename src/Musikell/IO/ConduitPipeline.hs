-- File: src/Musikell/IO/ConduitPipeline.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/io/conduit-pipeline.mdx
-- Module: Musikell.IO.ConduitPipeline
{-# LANGUAGE BangPatterns #-}

-- | Conduit-based streaming pipeline.
--
-- The pipeline connects stdin → scheduler → stdout in a single
-- streaming loop.  Each iteration:
--
--   1. Read one block of raw PCM from stdin.
--   2. Copy it into the input node's output buffer in the pool.
--   3. Execute the full graph via 'executeBlock'.
--   4. Read the output node's buffer from the pool.
--   5. Write it as raw PCM to stdout.
--
-- The process repeats indefinitely until stdin closes (EOF) or the
-- process is terminated (Ctrl-C).
module Musikell.IO.ConduitPipeline (
    -- * Pipeline execution
    runPipeline,

    -- * Components (exported for testing)
    chunkedSource,
    processConduit,
) where

-- Note: runPipelineNative using mkl_tick_once will be added once
-- the C execution plan serialization is complete.

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Combinators (sinkHandle, sourceHandle)
import System.IO (BufferMode (..), hSetBinaryMode, hSetBuffering, stdin, stdout)

import qualified Data.ByteString as BS

import Musikell.Core.ExecutionPlan (ExecutionPlan)
import Musikell.Core.Graph (Graph)
import Musikell.Core.Scheduler (
    SchedulerConfig (..),
    SchedulerState (..),
    executeBlock,
    initScheduler,
 )
import Musikell.Core.Types (
    BlockSize,
    ChannelCount,
    NodeId,
    PortId (..),
    defaultChannelCount,
 )
import Musikell.IO.StreamInput (bytesIntoBuffer)
import Musikell.IO.StreamOutput (unsafeBufferToBytes)
import Musikell.Memory.Buffer (Buffer)
import Musikell.Memory.Pool (BufferPool, allocatePool, getPoolBuffer)
import Musikell.Runtime.Kernel (KernelRegistry)

-- | Bytes per sample (32-bit float).
sampleBytes :: Int
sampleBytes = 4

-- ---------------------------------------------------------------------------
-- Chunked source
-- ---------------------------------------------------------------------------

-- | Reads from stdin and emits fixed-size ByteString chunks.
--   Short final blocks are zero-padded.
chunkedSource :: BlockSize -> ConduitT () ByteString IO ()
chunkedSource blockSize = sourceHandle stdin .| chunker
    where
        chunkSize = blockSize * sampleBytes

        chunker :: ConduitT ByteString ByteString IO ()
        chunker = loop BS.empty
            where
                loop !acc = do
                    mbs <- await
                    case mbs of
                        Nothing
                            | BS.null acc -> pure ()
                            | otherwise -> yield (pad acc chunkSize)
                        Just bs -> emit (acc <> bs)

                emit !bs
                    | BS.length bs >= chunkSize =
                        let (chunk, rest) = BS.splitAt chunkSize bs
                        in  yield chunk >> emit rest
                    | otherwise = loop bs

                pad bs n
                    | BS.length bs >= n = bs
                    | otherwise = bs <> BS.replicate (n - BS.length bs) 0

-- ---------------------------------------------------------------------------
-- Process conduit
-- ---------------------------------------------------------------------------

-- | Core processing conduit: for each input chunk, runs the full
--   scheduler and emits the output chunk.
processConduit ::
    ExecutionPlan ->
    Graph ->
    KernelRegistry ->
    NodeId ->
    -- ^ input node
    NodeId ->
    -- ^ output node
    BufferPool ->
    SchedulerConfig ->
    ConduitT ByteString ByteString IO ()
processConduit plan graph registry inNode outNode pool config =
    loop (initScheduler config pool)
    where
        loop !state = do
            mbs <- await
            case mbs of
                Nothing -> pure ()
                Just inputBytes -> do
                    -- 1. Write input PCM directly into the input node's pool buffer.
                    case getPoolBuffer inNode (PortId 0) (statePool state) of
                        Nothing -> pure () -- should not happen if graph is valid
                        Just poolInBuf -> do
                            liftIO $ bytesIntoBuffer inputBytes poolInBuf

                            -- 2. Execute the full graph.
                            result <- liftIO $ executeBlock plan graph registry state
                            case result of
                                Left _err -> pure () -- stop on error
                                Right newState -> do
                                    -- 3. Read the output node's buffer (zero-copy).
                                    case getPoolBuffer outNode (PortId 0) (statePool newState) of
                                        Nothing -> pure ()
                                        Just poolOutBuf -> do
                                            outputBytes <- liftIO $ unsafeBufferToBytes poolOutBuf
                                            yield outputBytes
                                            loop newState

-- ---------------------------------------------------------------------------
-- Full pipeline
-- ---------------------------------------------------------------------------

-- | Run the complete streaming pipeline: stdin → graph → stdout.
--
-- This function:
--   1. Sets stdin/stdout to binary mode and block buffering.
--   2. Allocates the buffer pool.
--   3. Runs the conduit pipeline until EOF.
runPipeline ::
    ExecutionPlan ->
    Graph ->
    KernelRegistry ->
    NodeId ->
    -- ^ input node
    NodeId ->
    -- ^ output node
    BlockSize ->
    ChannelCount ->
    -- ^ channel count (1 = mono, 2 = stereo)
    IO ()
runPipeline plan graph registry inNode outNode blockSize channels = do
    -- Binary mode: no newline translation.
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    -- Block buffering for throughput.
    let chunkBytes = blockSize * channels * sampleBytes
    hSetBuffering stdin (BlockBuffering (Just chunkBytes))
    hSetBuffering stdout (BlockBuffering (Just chunkBytes))

    pool <- allocatePool blockSize graph
    let config = SchedulerConfig {configBlockSize = blockSize}

    runConduit
        $ chunkedSource blockSize
            .| processConduit plan graph registry inNode outNode pool config
            .| sinkHandle stdout
