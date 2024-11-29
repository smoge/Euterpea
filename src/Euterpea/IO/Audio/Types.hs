{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Euterpea.IO.Audio.Types where

import Control.Arrow.ArrowP
import Control.SF.SF

-- | The 'Clock' class is used to define types that can provide a sampling rate.
class Clock p where
    -- | Returns the sampling rate for the given clock type.
    --
    -- Example:
    -- >>> rate AudRate
    -- 44100.0
    rate :: p -> Double

{- | An instance of 'Clock' for 'AudRate', with a fixed sampling rate.
Audio rate often uses a standard of 44100 Hz, suitable for CD-quality audio.
-}
instance Clock AudRate where
    rate :: AudRate -> Double
    rate _ = 44100

{- | An instance of 'Clock' for 'CtrRate', with a lower sampling rate.
Control rate is usually much lower than audio rate; here it is set to 4410 Hz.
-}
instance Clock CtrRate where
    rate :: CtrRate -> Double
    rate _ = 4410

-- | 'AudRate' represents an audio rate clock, typically used for audio signals.
data AudRate

-- | 'CtrRate' represents a control rate clock, typically used for control signals.
data CtrRate

{- | 'Frequency' is a newtype wrapper for Double, representing frequency values in Hertz.
It derives useful numeric and display properties automatically.

Example:
>>> let f = Frequency 440.0
>>> getFrequency f
440.0
-}
newtype Frequency = Frequency {getFrequency :: Double}
    deriving (Show, Eq, Ord, Num, Fractional, Floating)

{- | The 'ToFrequency' class is for types that can be converted to a 'Frequency'.
This allows different representations of frequency to be standardized.
-}
class ToFrequency a where
    -- | Converts a value to a 'Frequency'.
    toFrequency :: a -> Frequency

instance ToFrequency Double where
    toFrequency = Frequency

instance ToFrequency Frequency where
    toFrequency = id

{- | 'SampleRate' is a newtype wrapper for Double, representing sample rates in Hertz.

Example:
>>> let sr = SampleRate 48000.0
>>> getSampleRate sr
48000.0
-}
newtype SampleRate = SampleRate {getSampleRate :: Double}
    deriving (Show, Eq, Ord, Num)

{- | Type synonym for a signal function with an audio rate clock.
Represents signal processing functions that operate at audio rate.
-}
type AudSF a b = SigFun AudRate a b

{- | Type synonym for a signal function with a control rate clock.
Represents signal processing functions that operate at control rate.
-}
type CtrSF a b = SigFun CtrRate a b

{- | 'Signal' represents a signal with a specified clock and input/output types.
Allows defining complex signal processing pipelines with specific clocking.
-}
type Signal clk a b = ArrowP SF clk a b

{- | 'SigFun' is a type synonym for generalized signal functions using 'ArrowP'.
A key abstraction for creating reusable components in signal processing.
-}
type SigFun clk a b = ArrowP SF clk a b

{- | The 'AudioSample' class defines operations for various audio sample formats.
Supports basic operations like mixing and channel management.
-}
class AudioSample a where
    zero ::
        -- | Represents silence or zero value for the audio sample.
        a
    mix ::
        a ->
        a ->
        -- | Combines two audio samples into one.
        a
    collapse ::
        a ->
        -- | Converts an audio sample to a list of doubles, typically representing channels.
        [Double]
    numChans ::
        a ->
        -- | Returns the number of channels in the audio sample.
        Int

-- | An instance of 'AudioSample' for single-channel (mono) audio represented by 'Double'.
instance AudioSample Double where
    zero = 0 -- Zero value for mono is 0.
    mix = (+) -- Mixes two mono samples by addition.
    collapse a = [a] -- Collapse returns a single-element list.
    numChans _ = 1 -- Mono has one channel.

-- | An instance of 'AudioSample' for stereo audio represented by a tuple of 'Double's.
instance AudioSample (Double, Double) where
    zero = (0, 0) -- Zero value for stereo is (0, 0).
    mix (a, b) (c, d) = (a + c, b + d) -- Mixes two stereo samples by adding corresponding channels.
    collapse (a, b) = [a, b] -- Collapse returns a list containing both channels.
    numChans _ = 2 -- Stereo has two channels.

{- | Type synonym for mono signals using the generic signal type.
Often used for single-channel (mono) audio processing.
-}
type Mono p = Signal p () Double

{- | Type synonym for stereo signals using the generic signal type.
Used for processing two-channel (stereo) audio signals.
-}
type Stereo p = Signal p () (Double, Double)
