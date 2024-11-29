{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Euterpea.IO.Audio.Types (
    -- * Clock types
    Clock (..),
    AudRate,
    CtrRate,

    -- * Error types
    AudioError (..),

    -- * Frequency
    Frequency (..),
    ToFrequency (..),

    -- * Sample rate
    SampleRate (..),

    -- * Signal types
    AudSF,
    CtrSF,
    Signal,
    SigFun,

    -- * Audio sample types
    AudioSample (..),
    MultiChannel (..),
    AudioChannel (..),

    -- * Channel configurations
    Mono,
    Stereo,
) where

import Control.Arrow.ArrowP
import Control.SF.SF
import Data.Maybe (fromMaybe)

-- | The 'Clock' class defines types that can provide a sampling rate.
class Clock p where
    -- | Returns the sampling rate in Hz for the given clock type.
    rate :: p -> Double

-- | Audio rate clock type (44.1 kHz)
data AudRate

-- | Control rate clock type (4.41 kHz)
data CtrRate

instance Clock AudRate where
    rate _ = 44100 -- Audio rate (44.1 kHz)

instance Clock CtrRate where
    rate _ = 4410 -- Control rate (4.41 kHz)

-- | Represents various types of audio processing errors
data AudioError
    = -- | Frequency outside valid range (20Hz-20kHz)
      InvalidFrequency Double
    | -- | Sample rate outside valid range (1Hz-192kHz)
      InvalidSampleRate Double
    | -- | Pan outside valid range (-1 to 1)
      InvalidPan Double
    deriving stock (Show, Eq)

-- | Represents frequency in Hz
newtype Frequency = Frequency
    { getFrequency :: Double
    -- ^ Get the frequency value in Hz
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Fractional, Floating)

-- | Types that can be converted to a frequency value
class ToFrequency a where
    -- | Converts a value to a 'Frequency' in Hz
    toFrequency :: a -> Frequency

instance ToFrequency Double where
    toFrequency = Frequency

instance ToFrequency Frequency where
    toFrequency = id

-- | Represents sample rate in Hz
newtype SampleRate = SampleRate
    { getSampleRate :: Double
    -- ^ Get the sample rate value in Hz
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num)

-- | Audio rate signal function
type AudSF a b = SigFun AudRate a b

-- | Control rate signal function
type CtrSF a b = SigFun CtrRate a b

-- | Generic signal type
type Signal clk a b = ArrowP SF clk a b

-- | Signal function type
type SigFun clk a b = ArrowP SF clk a b

-- | Type class for audio samples that can be mixed and processed
class AudioSample a where
    -- | Zero sample value
    zero :: a

    -- | Mix two samples together
    mix :: a -> a -> a

    -- | Convert sample to list of channel values
    collapse :: a -> [Double]

    -- | Get number of channels
    numChans :: a -> Int

instance AudioSample Double where
    zero = 0
    mix = (+)
    collapse a = [a]
    numChans _ = 1

instance AudioSample (Double, Double) where
    zero = (0, 0)
    mix (a, b) (c, d) = (a + c, b + d)
    collapse (a, b) = [a, b]
    numChans _ = 2

-- | Type class for multi-channel audio samples
class (AudioSample a) => MultiChannel a where
    -- | Apply function to all channels
    mapChannels :: (Double -> Double) -> a -> a

    -- | Get list of channel values
    channels :: a -> [Double]
    channels = collapse

    -- | Create from list of channel values
    fromChannels :: [Double] -> Maybe a

instance MultiChannel Double where
    mapChannels = id
    fromChannels [x] = Just x
    fromChannels _ = Nothing

instance MultiChannel (Double, Double) where
    mapChannels f (l, r) = (f l, f r)
    fromChannels [l, r] = Just (l, r)
    fromChannels _ = Nothing

-- | Type class for audio channels that support gain and panning
class (MultiChannel a) => AudioChannel a where
    -- | Apply gain to all channels
    gain :: Double -> a -> Either AudioError a
    gain g = Right . mapChannels (* g)

    -- | Apply panning (-1 to 1, where -1 is left, 0 is center, 1 is right)
    pan :: Double -> a -> Either AudioError a

instance AudioChannel Double where
    pan _ = Right

instance AudioChannel (Double, Double) where
    pan p x
        | p < -1 || p > 1 = Left $ InvalidPan p
        | otherwise =
            Right $
                fromMaybe (error "Impossible: stereo pan conversion failed") $
                    fromChannels $
                        doPan (channels x)
      where
        doPan [l, r] =
            let leftGain = cos ((p + 1) * pi / 4) -- Equal power panning
                rightGain = sin ((p + 1) * pi / 4)
             in [l * leftGain, r * rightGain]
        doPan _ = error "Impossible: stereo channels /= 2"

-- | Monophonic signal type
type Mono p = Signal p () Double

-- | Stereophonic signal type
type Stereo p = Signal p () (Double, Double)