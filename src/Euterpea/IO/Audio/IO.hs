{- ORMOLU_DISABLE -}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE ExistentialQuantification #-}

module Euterpea.IO.Audio.IO
  ( outFile,
    outFileNorm,
    --    outFileA, outFileNormA, RecordStatus,
    maxSample,
  )
where

import           Codec.Wav
import           Control.Arrow.ArrowP
import           Control.SF.SF
import           Data.Array.Unboxed
import           Data.Audio
import           Data.Int
import           Data.Proxy              (Proxy (..))
import           Euterpea.IO.Audio.Types hiding (Signal)

{- ORMOLU_ENABLE -}

-- import Data.IORef
-- import Foreign.Cx
-- import Foreign.Marshal.Array
-- import Foreign.Marshal.Utils
-- import Foreign.Ptr
-- import Foreign.Storable
-- import Control.CCA.Types
-- import Control.Arrow
-- import Control.Concurrent.MonadIO
-- import Sound.RtAudio

-- | Type synonym for a signal function with an audio clock,
-- using ArrowP to establish type relationship with SF.
type Signal clk a b = ArrowP SF clk a b

-- | Writes sound to a wave file (.wav).
-- This uses the helper function outFileHelp' with identity function 'id'
outFile ::
  forall a p.
  (AudioSample a, Clock p) =>
  -- | Filename to write to.
  String ->
  -- | Duration of the wav in seconds.
  Double ->
  -- | Signal representing the sound.
  Signal p () a ->
  IO ()
outFile = outFileHelp' id

-- | Normalizes a list of Double values such that the maximum value is 1.0,
-- ensuring normalization only if the maximum is greater than 1.0.
normList :: [Double] -> [Double]
normList xs = fmap (/ mx) xs
  where
    mx = max 1.0 (maximum (fmap abs xs))

-- | Similar to outFile, but normalizes the output if amplitude exceeds 1,
-- requiring full in-memory storage of the signal before writing.
outFileNorm ::
  forall a p.
  ( AudioSample a,
    Clock p
  ) =>
  -- | Filename to write to.
  String ->
  -- | Duration of the wav in seconds.
  Double ->
  -- | Signal representing the sound.
  Signal p () a ->
  IO ()
outFileNorm = outFileHelp' normList

-- | Helper function for both outFile and outFileNorm handling preprocessing.
outFileHelp ::
  forall a p.
  ( AudioSample a,
    Clock p
  ) =>
  -- | Post-processing function.
  ([Double] -> [Double]) ->
  -- | Filename to write to.
  String ->
  -- | Duration of the wav in seconds.
  Double ->
  -- | Signal representing the sound.
  Signal p () a ->
  IO ()
outFileHelp f filepath dur sf =
  let sr = rateProxy (Proxy :: Proxy p)              -- Get sample rate from clock type.
      numChannels = numChansProxy (Proxy :: Proxy a) -- Determine number of audio channels.
      numSamples = truncate (dur * sr) * numChannels -- Total samples calculated from duration and sample rate.
      dat = fmap (fromSample . (* 0.999)) (f (toSamples dur sf)) :: [Int32]
      -- Multiply by 0.999 to avoid wraparound at 1.0.
      this_array = listArray (0, numSamples - 1) dat -- Create an array of the processed samples.
      aud =
        Audio
          { sampleRate = truncate sr,
            channelNumber = numChannels,
            sampleData = this_array
          }
   in exportFile filepath aud

{- 
Alternative definition enforcing clipping within [-1.0, 1.0] range. 
Prevents overflow issue by applying clipFix function to maintain stability, 
similar behavior to other audio software.
-}
outFileHelp' ::
  forall a p.
  ( AudioSample a,
    Clock p
  ) =>
  -- | Post-processing function.
  ([Double] -> [Double]) ->
  -- | Filename to write to.
  String ->
  -- | Duration of the wav in seconds.
  Double ->
  -- | Signal representing the sound.
  Signal p () a ->
  IO ()
outFileHelp' f filepath dur sf =
  let sr = rateProxy (Proxy :: Proxy p)
      numChannels = numChansProxy (Proxy :: Proxy a)
      numSamples = truncate (dur * sr) * numChannels
      dat =
        fmap (fromSample . (* 0.999) . clipFix) (f (toSamples dur sf)) :: [Int32]
        -- Apply clipping to ensure no sample exceeds [-1.0, 1.0] range.
      this_array = listArray (0, numSamples - 1) dat
      aud =
        Audio
          { sampleRate = truncate sr,
            channelNumber = numChannels,
            sampleData = this_array
          }
   in exportFile filepath aud
  where
    clipFix x = max (-1.0) (min 1.0 x) -- Ensure values are clamped between -1.0 and 1.0.

-- | Converts a Signal Function (SF) into a list of Double samples,
-- based on a given duration, extracting the sample data for further processing.
toSamples ::
  forall a p.
  (AudioSample a, Clock p) =>
  Double ->
  Signal p () a ->
  [Double]
toSamples dur sf =
  let sr = rateProxy (Proxy :: Proxy p)
      numChannels = numChansProxy (Proxy :: Proxy a)
      numSamples = truncate (dur * sr) * numChannels
   in take numSamples $ concatMap collapse $ unfold $ strip sf

-- | Calculate the maximum absolute sample value in the first 'dur' seconds,
-- useful for determining the peak amplitude of a signal.
maxSample ::
  forall a p.
  (AudioSample a, Clock p) =>
  Double ->
  Signal p () a ->
  Double
maxSample dur sf = safeMaximum 0 (fmap abs (toSamples dur sf))

safeMaximum :: Ord a => a -> [a] -> a
safeMaximum def xs = if null xs then def else maximum xs


------------------------



{-
writeWav :: AudioSample a => ([Double] -> [Double]) -> String -> Double -> Int -> [a] -> UI ()
writeWav f filepath sr numChannels adat =
  let dat         = map (fromSample . (*0.999))
                        (f (concatMap collapse adat)) :: [Int32]
                    -- multiply by 0.999 to avoid wraparound at 1.0
      array       = listArray (0, (length dat)-1) dat
      aud = Audio { sampleRate    = truncate sr,
                    channelNumber = numChannels,
                    sampleData    = array }
  in liftIO $ exportFile filepath aud
-}
