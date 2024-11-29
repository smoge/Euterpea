{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Euterpea.IO.Audio.Basics (
    outA,
    integral,
    countDown,
    countUp,
    upsample,
    pchToHz,
    apToHz,
    hertz,
    fromFrequency,
    calculateWaveLength,
)
where

import Control.Arrow 
import Control.Arrow.ArrowP
import Control.Arrow.Operations 
import Euterpea.IO.Audio.Types 
import Euterpea.Music




-- | Identity arrow that passes its input unchanged
outA :: (Arrow a) => a b b
outA = arr id


{- | Performs numerical integration over time.
    Uses a pre-calculated sampling period (dt) for better performance.

    Example:
    >>> let signal = integral >>> arr (* 2)  -- Integrate and double

    Note: Initial value is 0. For different initial values, compose with
    an arrow that adds the offset you need.
-}
integral :: forall a p. (ArrowCircuit a, Clock p) => ArrowP a p Double Double
integral = proc x -> do
    rec let i' = i + x * samplingPeriod
        i <- delay 0 -< i'
    outA -< i
  where
    samplingPeriod = recip . rate $ (undefined :: p)


{- | Creates a countdown arrow starting from the given value.

    Example:
    >>> let timer = countDown 10  -- Counts from 10 to -infinity

    Note: Does not stop at 0, will continue into negative numbers.
    Consider composing with (arr (max 0)) if non-negative output is needed.
-}
countDown :: (ArrowCircuit a) => Int -> a () Int
countDown x = proc _ -> do
    rec i <- delay x -< i - 1
    outA -< i


{- | Creates a count-up arrow starting from 0.

    Example:
    >>> let counter = countUp  -- Counts from 0 upward

    Note: Will eventually overflow at maxBound :: Int.
    Consider using Integer if unbounded counting is needed.
-}
countUp :: (ArrowCircuit a) => a () Int
countUp = proc _ -> do
    rec i <- delay 0 -< i + 1
    outA -< i

{- | Increases the sample rate of a signal.

    Properties:
    * Output rate must be higher than input rate
    * Interpolates between samples using zero-order hold

    Example:
    >>> let upsampled = upsample mySignal

    Throws: Error if output rate <= input rate
-}
upsample ::
    forall a b c p1 p2.
    (ArrowChoice a, ArrowCircuit a, Clock p1, Clock p2, AudioSample c) =>
    ArrowP a p1 b c ->
    ArrowP a p2 b c
upsample this_f = proc x -> do
    rec cc <- delay 0 -< if cc >= conversionRatio - 1 then 0 else cc + 1
        y <-
            if cc == 0
                then ArrowP (strip this_f) -< x
                else delay zero -< y
    outA -< y
  where
    conversionRatio =
        if outRate < inRate
            then error "Cannot upsample a signal of higher rate to lower rate"
            else outRate / inRate
    inRate = rate (undefined :: p1)
    outRate = rate (undefined :: p2)


{- | Converts an AbsPitch to frequency in Hz.

    The conversion uses A4 = 440Hz as reference and equal temperament tuning.

    Example:
    >>> apToHz 69  -- returns 440.0 (A4)
    >>> apToHz 60  -- returns ~261.63 (Middle C)
-}
apToHz :: AbsPitch -> Frequency
apToHz ap = Frequency $ 440 * 2 ** (fromIntegral (ap - absPitch (A, 4)) / 12)


{- | Converts a Pitch to frequency in Hz.

    Convenience wrapper around apToHz.

    Example:
    >>> pchToHz (A, 4)  -- returns 440.0
    >>> pchToHz (C, 4)  -- returns ~261.63
-}
pchToHz :: Pitch -> Frequency
pchToHz = apToHz . absPitch


hertz :: Double -> Frequency
hertz = Frequency

fromFrequency :: Frequency -> Double
fromFrequency (Frequency this_f) = this_f

calculateWaveLength :: Frequency -> Double -> Double
calculateWaveLength (Frequency freq) speedOfSound =
  speedOfSound / freq