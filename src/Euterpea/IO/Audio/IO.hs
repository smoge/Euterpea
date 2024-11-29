{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Euterpea.IO.Audio.IO (
    -- * File Output
    outFile,
    outFileNorm,
    -- * Analysis
    maxSample,
    -- * Helper Functions
    outFileHelp,
) where

import Codec.Wav
import Control.Arrow.ArrowP
import Control.SF.SF
import Data.Array.Unboxed
import Data.Audio
import Data.Int
import Data.List (foldl')
import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (fromMaybe)
import Euterpea.IO.Audio.Types hiding (Signal)

-- | Type synonym for a signal function with an audio clock.
type Signal clk a b = ArrowP SF clk a b

-- | Write audio data to a WAV file without normalization
outFile ::
    forall a p.
    (AudioSample a, Clock p) =>
    FilePath ->    -- ^ Output file path
    Double ->      -- ^ Duration in seconds
    Signal p () a -> -- ^ Audio signal
    IO ()
outFile = outFileHelp' id

-- | Normalize a list of audio samples to have maximum amplitude of 1.0
normList :: [Double] -> [Double]
normList xs = fmap (/ maxAmp) xs
  where
    maxAmp = max 1.0 $ fromMaybe 0.0 (safeMaximum (map abs xs))

-- | Safe maximum function that handles empty lists
safeMaximum :: [Double] -> Maybe Double
safeMaximum [] = Nothing
safeMaximum (x:xs) = Just (foldl' max x xs)

{- | Write audio data to a WAV file with automatic normalization.
     If the maximum amplitude exceeds 1.0, the signal is normalized.
     Otherwise, the signal is unchanged.
-}
outFileNorm ::
    forall a p.
    (AudioSample a, Clock p) =>
    FilePath ->    -- ^ Output file path
    Double ->      -- ^ Duration in seconds
    Signal p () a -> -- ^ Audio signal
    IO ()
outFileNorm = outFileHelp' normList

-- | Helper function for writing audio data with custom post-processing
outFileHelp ::
    forall a p.
    (AudioSample a, Clock p) =>
    ([Double] -> [Double]) -> -- ^ Post-processing function
    FilePath ->              -- ^ Output file path
    Double ->                -- ^ Duration in seconds
    Signal p () a ->        -- ^ Audio signal
    IO ()
outFileHelp f filepath dur sf = do
    let sr = rate (undefined :: p)
        numChannels = numChans (undefined :: a)
        numSamples = truncate (dur * sr) * numChannels
        -- Apply post-processing and convert to Int32 samples
        samples = toSamples dur sf
        processedSamples = f samples
        -- Prevent clipping by scaling slightly below maximum
        dat = fmap (fromSample . (* 0.999)) processedSamples :: [Int32]
        audioData = listArray (0, numSamples - 1) dat
        audioFile = Audio
            { sampleRate = truncate sr
            , channelNumber = numChannels
            , sampleData = audioData
            }
    exportFile filepath audioFile

-- | Helper function for writing audio data with clipping protection
outFileHelp' ::
    forall a p.
    (AudioSample a, Clock p) =>
    ([Double] -> [Double]) -> -- ^ Post-processing function
    FilePath ->              -- ^ Output file path
    Double ->                -- ^ Duration in seconds
    Signal p () a ->        -- ^ Audio signal
    IO ()
outFileHelp' f filepath dur sf = do
    let sr = rate (undefined :: p)
        numChannels = numChans (undefined :: a)
        numSamples = truncate (dur * sr) * numChannels
        -- Process and clip samples
        samples = toSamples dur sf
        processedSamples = f samples
        -- Apply clipping and scaling
        dat = fmap (fromSample . (* 0.999) . clipSample) processedSamples :: [Int32]
        audioData = listArray (0, numSamples - 1) dat
        audioFile = Audio
            { sampleRate = truncate sr
            , channelNumber = numChannels
            , sampleData = audioData
            }
    exportFile filepath audioFile
  where
    clipSample :: Double -> Double
    clipSample x = max (-1.0) (min 1.0 x)

-- | Convert a signal to a list of samples
toSamples ::
    forall a p.
    (AudioSample a, Clock p) =>
    Double ->      -- ^ Duration in seconds
    Signal p () a -> -- ^ Audio signal
    [Double]
toSamples dur sf =
    let sr = rate (undefined :: p)
        numChannels = numChans (undefined :: a)
        numSamples = truncate (dur * sr) * numChannels
    in take numSamples $ concatMap collapse $ unfold $ strip sf

-- | Compute the maximum absolute sample value in the signal
maxSample ::
    forall a p.
    (AudioSample a, Clock p) =>
    Double ->      -- ^ Duration in seconds
    Signal p () a -> -- ^ Audio signal
    Double
maxSample dur sf =
    let samples = fmap abs (toSamples dur sf)
    in case nonEmpty samples of
            Nothing -> 0.0  -- Return 0.0 for empty signals
            Just neList -> maximum (toList neList)