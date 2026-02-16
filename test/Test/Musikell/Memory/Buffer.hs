-- |
-- Module      : Test.Musikell.Memory.Buffer
-- Description : Tests for Memory.Buffer module
-- License     : MIT
module Test.Musikell.Memory.Buffer (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Musikell.Core.Types (defaultBlockSize)
import Musikell.Memory.Buffer

tests :: TestTree
tests =
    testGroup
        "Memory.Buffer"
        [ unitTests,
          propertyTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "allocateBuffer creates buffer of correct size" $ do
            buf <- allocateBuffer 256
            bufferLength buf @?= 256,
          testCase "allocateBuffer initializes to zero" $ do
            buf <- allocateBuffer 10
            samples <- bufferToList buf
            samples @?= replicate 10 0.0,
          testCase "allocateBufferWith fills with value" $ do
            buf <- allocateBufferWith 5 1.0
            samples <- bufferToList buf
            samples @?= replicate 5 1.0,
          testCase "readSample/writeSample roundtrip" $ do
            buf <- allocateBuffer 10
            writeSample buf 5 0.75
            val <- readSample buf 5
            val @?= 0.75,
          testCase "readSample returns 0 for out of bounds" $ do
            buf <- allocateBuffer 10
            val <- readSample buf 100
            val @?= 0.0,
          testCase "writeSample ignores out of bounds" $ do
            buf <- allocateBuffer 10
            writeSample buf 100 1.0 -- Should not crash
            samples <- bufferToList buf
            samples @?= replicate 10 0.0,
          testCase "fillBuffer sets all samples" $ do
            buf <- allocateBuffer 5
            fillBuffer buf 0.5
            samples <- bufferToList buf
            samples @?= replicate 5 0.5,
          testCase "copyBuffer copies data" $ do
            src <- allocateBufferWith 5 0.25
            dst <- allocateBuffer 5
            copyBuffer src dst
            samples <- bufferToList dst
            samples @?= replicate 5 0.25,
          testCase "listToBuffer roundtrip" $ do
            let original = [0.1, 0.2, 0.3, 0.4, 0.5]
            buf <- listToBuffer original
            result <- bufferToList buf
            result @?= original,
          testCase "default block size buffer" $ do
            buf <- allocateBuffer defaultBlockSize
            bufferLength buf @?= 256
        ]

propertyTests :: TestTree
propertyTests =
    testGroup
        "Property Tests"
        [ testProperty "bufferLength matches allocation size"
            $ \(Positive n) -> ioProperty $ do
                buf <- allocateBuffer (min n 10000) -- Cap size for test performance
                pure $ bufferLength buf == min n 10000,
          testProperty "listToBuffer preserves length"
            $ \xs -> ioProperty $ do
                let samples = take 1000 xs -- Cap size
                buf <- listToBuffer samples
                pure $ bufferLength buf == length samples,
          testProperty "writeSample/readSample are consistent"
            $ \(Positive idx) val -> ioProperty $ do
                let i = idx `mod` 100
                buf <- allocateBuffer 100
                writeSample buf i val
                result <- readSample buf i
                pure $ result == val,
          testProperty "fillBuffer sets all samples to same value"
            $ \(Positive n) val -> ioProperty $ do
                let size = min n 100
                buf <- allocateBuffer size
                fillBuffer buf val
                samples <- bufferToList buf
                pure $ all (== val) samples
        ]
