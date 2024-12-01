{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Euterpea.IO.Audio.IO
  ( outFile,
    outFileNorm,
    --    outFileA, outFileNormA, RecordStatus,
    maxSample,
  )
where

import Codec.Wav
import Control.Arrow.ArrowP
import Control.SF.SF
import Data.Array.Unboxed
import Data.Audio
import Data.Int
import Data.Proxy (Proxy (..))
import Euterpea.IO.Audio.Types hiding (Signal)

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

-- | Type synonym for a signal function with an audio clock.
type Signal clk a b = ArrowP SF clk a b

-- | Writes sound to a wave file (.wav)
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

normList :: [Double] -> [Double]
normList xs = fmap (/ mx) xs
  where
    mx = max 1.0 (maximum (fmap abs xs))

-- | Like outFile, but normalizes the output if the amplitude of
-- the signal goes above 1.  If the maximum sample is less than
-- or equal to 1, the output is not normalized.
-- Currently this requires storing the entire output stream in memory
-- before writing to the file.
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
  let sr = rateProxy (Proxy :: Proxy p)
      numChannels = numChansProxy (Proxy :: Proxy a)
      numSamples = truncate (dur * sr) * numChannels
      dat = fmap (fromSample . (* 0.999)) (f (toSamples dur sf)) :: [Int32]
      -- multiply by 0.999 to avoid wraparound at 1.0
      array = listArray (0, numSamples - 1) dat
      aud =
        Audio
          { sampleRate = truncate sr,
            channelNumber = numChannels,
            sampleData = array
          }
   in exportFile filepath aud

{-
Alternative definition of the above that enforces a clipping behavior when
the value exceeds the [-1.0, 1.0] range. The overflow behavior makes it
very hard to debug sound modeling problems that involve certain waveforms,
such as saw waves. Clipping is also a more common behavior in other audio
software rather than overflowing or wrap-around.
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
      array = listArray (0, numSamples - 1) dat
      aud =
        Audio
          { sampleRate = truncate sr,
            channelNumber = numChannels,
            sampleData = array
          }
   in exportFile filepath aud
  where
    clipFix x = max (-1.0) (min 1.0 x)

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

-- | Compute the maximum sample of an SF in the first 'dur' seconds.
maxSample ::
  forall a p.
  (AudioSample a, Clock p) =>
  Double ->
  Signal p () a ->
  Double
maxSample dur sf = maximum (fmap abs (toSamples dur sf))
