-- File: src/Musikell/Runtime/Kernel/Builtin/Envelope.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/runtime/kernel/envelope.mdx
-- Module: Musikell.Runtime.Kernel.Builtin.Envelope
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ADSR envelope generator kernel.
--
-- Classic Attack-Decay-Sustain-Release envelope. The envelope multiplies
-- an input signal (amplitude modulation). Gate on/off is controlled by
-- a trigger mechanism.
module Musikell.Runtime.Kernel.Builtin.Envelope (
    mkADSR,
    EnvelopeStage (..),
) where

import Data.IORef

import Musikell.Core.Types (KernelId (..), SampleRate)
import Musikell.Memory.Buffer (Buffer, bufferSize, unsafeReadSample, unsafeWriteSample)
import Musikell.Runtime.Kernel (KernelSpec (..))
import Musikell.Runtime.Kernel.Builtin.Util (flushDenormals)

-- | ADSR envelope stages.
data EnvelopeStage
    = -- | Before gate on
      StageIdle
    | -- | Rising from 0 to 1
      StageAttack
    | -- | Falling from 1 to sustain level
      StageDecay
    | -- | Held at sustain level
      StageSustain
    | -- | Falling from sustain to 0
      StageRelease
    deriving (Eq, Show)

data ADSRState = ADSRState
    { adsrStage :: !EnvelopeStage,
      adsrLevel :: {-# UNPACK #-} !Float
    }

-- | Create an ADSR envelope kernel.
--
--   @mkADSR attack decay sustain release gateOn sampleRate@
--
--   - attack: seconds to rise from 0 to 1
--   - decay: seconds to fall from 1 to sustain level
--   - sustain: sustain level (0.0 - 1.0)
--   - release: seconds to fall from sustain to 0
--   - gateOn: whether the note is initially on
--
--   Input 0 = audio signal to envelope
--   Output 0 = enveloped signal
mkADSR :: Double -> Double -> Double -> Double -> Bool -> SampleRate -> IO KernelSpec
mkADSR attack decay sustain release gateOn sr = do
    let !attRate = if attack > 0 then realToFrac (1.0 / (attack * fromIntegral sr)) else 1.0 :: Float
        !decRate = if decay > 0 then realToFrac (1.0 / (decay * fromIntegral sr)) else 1.0 :: Float
        !susLvl = realToFrac sustain :: Float
        !relRate = if release > 0 then realToFrac (1.0 / (release * fromIntegral sr)) else 1.0 :: Float
    stRef <- newIORef (ADSRState (if gateOn then StageAttack else StageIdle) 0.0)
    pure
        $ KernelSpec
            { kernelId = KernelId "envelope.adsr",
              kernelInputs = 1,
              kernelOutputs = 1,
              kernelExecute = \ins outs -> case (ins, outs) of
                (i : _, o : _) -> do
                    st0 <- readIORef stRef
                    stFinal <- adsrLoop attRate decRate susLvl relRate i o (bufferSize i) st0 0
                    writeIORef stRef $! stFinal
                _ -> pure ()
            }

adsrLoop ::
    Float ->
    Float ->
    Float ->
    Float ->
    Buffer ->
    Buffer ->
    Int ->
    ADSRState ->
    Int ->
    IO ADSRState
adsrLoop !att !dec !sus !rel !i !o !n !st !idx
    | idx >= n = pure st
    | otherwise = do
        s <- unsafeReadSample i idx
        let !lvl = adsrLevel st
            (!lvl', !stage') = case adsrStage st of
                StageIdle -> (0.0, StageIdle)
                StageAttack ->
                    let !l = flushDenormals (lvl + att)
                    in  if l >= 1.0 then (1.0, StageDecay) else (l, StageAttack)
                StageDecay ->
                    let !l = flushDenormals (lvl - dec * (lvl - sus))
                    in  if l <= sus + 0.001 then (sus, StageSustain) else (l, StageDecay)
                StageSustain -> (sus, StageSustain)
                StageRelease ->
                    let !l = flushDenormals (lvl - rel * lvl)
                    in  if l < 0.001 then (0.0, StageIdle) else (l, StageRelease)
        unsafeWriteSample o idx (s * lvl')
        adsrLoop att dec sus rel i o n (ADSRState stage' lvl') (idx + 1)
