{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Euterpea.IO.Audio.Types where

import Control.Arrow.ArrowP
import Control.SF.SF
import Data.Maybe (fromMaybe)

-- | The 'Clock' class is used to define types that can provide a sampling rate.
class Clock p where
    -- | Returns the sampling rate for the given clock type.
    rate :: p -> Double

data AudRate
data CtrRate

instance Clock AudRate where
    rate _ = 44100  -- Audio rate (44100 Hz)

instance Clock CtrRate where
    rate _ = 4410   -- Control rate (4410 Hz)

-- | Represents various types of audio processing errors
data AudioError
    = InvalidFrequency Double -- ^ Frequency outside valid range (20Hz-20kHz)
    | InvalidSampleRate Double -- ^ Sample rate outside valid range (1Hz-192kHz)
    | InvalidPan Double        -- ^ Pan outside valid range (-1 to 1)
    deriving (Show, Eq)

newtype Frequency = Frequency {getFrequency :: Double}
    deriving (Show, Eq, Ord, Num, Fractional, Floating)

class ToFrequency a where
    -- | Converts a value to a 'Frequency'.
    toFrequency :: a -> Frequency

instance ToFrequency Double where
    toFrequency = Frequency

instance ToFrequency Frequency where
    toFrequency = id

newtype SampleRate = SampleRate {getSampleRate :: Double}
    deriving (Show, Eq, Ord, Num)

type AudSF a b = SigFun AudRate a b
type CtrSF a b = SigFun CtrRate a b
type Signal clk a b = ArrowP SF clk a b
type SigFun clk a b = ArrowP SF clk a b

class AudioSample a where
    zero :: a
    mix :: a -> a -> a
    collapse :: a -> [Double]
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

class AudioSample a => MultiChannel a where
    mapChannels :: (Double -> Double) -> a -> a
    channels :: a -> [Double]
    channels = collapse
    fromChannels :: [Double] -> Maybe a

instance MultiChannel Double where
    mapChannels = id
    fromChannels [x] = Just x
    fromChannels _ = Nothing

instance MultiChannel (Double, Double) where
    mapChannels f (l, r) = (f l, f r)
    fromChannels [l, r] = Just (l, r)
    fromChannels _ = Nothing

class MultiChannel a => AudioChannel a where
    gain :: Double -> a -> Either AudioError a
    gain g = Right . mapChannels (* g)
    pan :: Double -> a -> Either AudioError a

instance AudioChannel Double where
    pan _ = Right

instance AudioChannel (Double, Double) where
    pan p x
        | p < -1 || p > 1 = Left $ InvalidPan p
        | otherwise = Right $ fromMaybe (error "Impossible: stereo pan conversion failed") $
            fromChannels $ doPan (channels x)
      where
        doPan [l, r] =
            let leftGain = cos ((p + 1) * pi / 4) -- Equal power panning
                rightGain = sin ((p + 1) * pi / 4)
             in [l * leftGain, r * rightGain]
        doPan _ = error "Impossible: stereo channels /= 2"

type Mono p = Signal p () Double
type Stereo p = Signal p () (Double, Double)
