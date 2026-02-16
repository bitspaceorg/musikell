-- File: src/Musikell/Memory/Layout.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/memory/layout.mdx
-- Module: Musikell.Memory.Layout

module Musikell.Memory.Layout (
    -- * Layout Specification
    BufferLayout (..),
    defaultLayout,

    -- * Layout Validation
    validateLayout,
    isAligned,

    -- * Layout Calculations
    requiredBytes,
    alignedSize,
    sampleSize,

    -- * Constants
    defaultAlignment,
    defaultChannels,
) where

import Data.Bits ((.&.))
import Foreign.Storable (sizeOf)

import Musikell.Core.Types (BlockSize, defaultBlockSize)

-- | Memory layout specification for buffers
data BufferLayout = BufferLayout
    { -- | Samples per block
      layoutBlockSize :: !BlockSize,
      -- | Memory alignment in bytes
      layoutAlignment :: !Int,
      -- | Number of audio channels
      layoutChannels :: !Int
    }
    deriving (Eq, Show)

-- | Default alignment: 16 bytes (SSE compatible)
defaultAlignment :: Int
defaultAlignment = 16

-- | Default number of channels: 1 (mono)
defaultChannels :: Int
defaultChannels = 1

-- | Default layout configuration
defaultLayout :: BufferLayout
defaultLayout =
    BufferLayout
        { layoutBlockSize = defaultBlockSize,
          layoutAlignment = defaultAlignment,
          layoutChannels = defaultChannels
        }

-- | Size of a single sample in bytes
sampleSize :: Int
sampleSize = sizeOf (undefined :: Float) -- 4 bytes

-- | Calculate required bytes for a layout
requiredBytes :: BufferLayout -> Int
requiredBytes layout =
    layoutBlockSize layout * layoutChannels layout * sampleSize

-- | Calculate aligned size (rounds up to alignment boundary)
alignedSize :: BufferLayout -> Int
alignedSize layout =
    let raw = requiredBytes layout
        align = layoutAlignment layout
    in  ((raw + align - 1) `div` align) * align

-- | Check if a size is properly aligned
isAligned :: Int -> Int -> Bool
isAligned size alignment = size `mod` alignment == 0

-- | Validate a layout specification
validateLayout :: BufferLayout -> Either String ()
validateLayout layout
    | layoutBlockSize layout <= 0 =
        Left "Block size must be positive"
    | layoutAlignment layout <= 0 =
        Left "Alignment must be positive"
    | not (isPowerOfTwo (layoutAlignment layout)) =
        Left "Alignment must be a power of two"
    | layoutChannels layout <= 0 =
        Left "Channel count must be positive"
    | otherwise =
        Right ()

-- | Check if a number is a power of two
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n > 0 && (n .&. (n - 1)) == 0
