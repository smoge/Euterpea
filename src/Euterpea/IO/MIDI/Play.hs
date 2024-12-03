{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Euterpea.IO.MIDI.Play
  ( play, -- standard playback, allows infinite values
    playDev, -- play to a custom device, allows infinite values
    playS, -- play with strict timing (finite values only)
    playDevS, -- play to a custom device with strict timing (finite values only)
    playC, -- custom playback implementation to replace playA, playS, playDev, etc.
    devices, -- function that prints available MIDI device information
    musicToMsgs', -- music to MIDI message conversion
    linearCP, -- linear channel assignment policy
    dynamicCP, -- dynamic channel assignment policy
    predefinedCP, -- user-specified channel map (for MUIs)
    defParams,
    playM',
    PlayParams (..),
    ChannelMapFun,
    ChannelMap,
  )
where

import Codec.Midi
  ( Channel,
    FileType,
    Message
      ( ChannelPressure,
        ControlChange,
        KeyPressure,
        NoteOff,
        NoteOn,
        PitchWheel,
        ProgramChange,
        TempoChange
      ),
    Midi (Midi),
    Time,
    TimeDiv (..),
  )
import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (onException)
import Control.Monad (void)
import Data.List (insertBy)
import Euterpea.IO.MIDI.MEvent
  ( MEvent (eDur, eInst, ePitch, eTime, eVol),
    perform,
    perform1,
  )
import Euterpea.IO.MIDI.MidiIO
  ( DeviceInfo (name),
    MidiMessage (..),
    OutputDeviceID,
    defaultOutput,
    deliverMidiEvent,
    getAllDevices,
    initializeMidi,
    outputMidi,
    playMidi,
    terminateMidi,
    unsafeOutputID,
  )
import Euterpea.IO.MIDI.ToMidi (toMidi)
import Euterpea.Music
  ( Articulation (Legato, Staccato),
    Control (..),
    Dynamic (..),
    InstrumentName (Percussion),
    Mode,
    Music (..),
    Music1,
    NoteAttribute (..),
    Ornament,
    PhraseAttribute (..),
    PitchClass,
    Primitive (..),
    StdLoudness,
    Tempo (..),
    ToMusic1 (..),
  )
import Sound.PortMidi
  ( getDefaultOutputDeviceID,
    initialize,
    terminate,
  )

data PlayParams = PlayParams
  { strict :: Bool, -- strict timing (False for infinite values)
    chanPolicy :: ChannelMapFun, -- channel assignment policy
    devID :: Maybe OutputDeviceID, -- output device (Nothing means to use the OS default)
    closeDelay :: Time, -- offset in seconds to avoid truncated notes
    perfAlg :: Music1 -> [MEvent]
  }

defParams :: PlayParams
defParams = PlayParams False (linearCP 16 9) Nothing 1.0 perform1

play :: (ToMusic1 a, NFData a, Enum InstrumentName) => Music a -> IO ()
play = playC defParams {perfAlg = fixPerf}
  where
    fixPerf = fmap (\e -> e {eDur = max 0 (eDur e - 0.000001)}) . perform

playS :: (ToMusic1 a, NFData a, Enum InstrumentName) => Music a -> IO ()
playS = playC defParams {strict = True}

playDev :: (ToMusic1 a, NFData a, Enum InstrumentName) => Int -> Music a -> IO ()
playDev i = playC defParams {devID = Just $ unsafeOutputID i, perfAlg = fixPerf}
  where
    fixPerf = fmap (\e -> e {eDur = max 0 (eDur e - 0.000001)}) . perform

playDevS :: (ToMusic1 a, NFData a, Enum InstrumentName) => Int -> Music a -> IO ()
playDevS i = playC defParams {strict = True, devID = Just $ unsafeOutputID i}

playC :: (ToMusic1 a, NFData a, Enum InstrumentName) => PlayParams -> Music a -> IO ()
playC p = if strict p then playStrict p else playInf p

devices :: IO ()
devices = do
  (devsIn, devsOut) <- getAllDevices
  let f (devid, devname) = "  " <> (show devid <> ("\t" <> name devname <> "\n"))
      strIn = concatMap f devsIn
      strOut = concatMap f devsOut
  putStrLn "\nInput devices: " >> putStrLn strIn
  putStrLn "Output devices: " >> putStrLn strOut

playStrict :: (ToMusic1 a, NFData a) => PlayParams -> Music a -> IO ()
playStrict p m =
  m `deepseq`
    let x = toMidi (perfAlg p $ toMusic1 m)
     in x `deepseq` playM' (devID p) x

playM' :: Maybe OutputDeviceID -> Midi -> IO ()
playM' devID_ midi = handleCtrlC $ do
  initialize
  case devID_ of
    Nothing -> defaultOutput playMidi midi
    Just devID_ -> playMidi devID_ midi
  result <- terminate
  case result of
    Left err -> putStrLn ("Terminate failed with error: " <> show err)
    Right _ -> pure ()
  where
    handleCtrlC :: IO a -> IO a
    handleCtrlC op = onException op (void terminate)

playInf :: (ToMusic1 a, Enum InstrumentName) => PlayParams -> Music a -> IO ()
playInf p m = handleCtrlC $ do
  initializeMidi
  maybe (defaultOutput playRec) playRec (devID p) $ musicToMsgs' p m
  threadDelay $ round (closeDelay p * 1000000)
  terminateMidi
  pure ()
  where
    handleCtrlC :: IO a -> IO a
    handleCtrlC op = do
      dev <- resolveOutDev (devID p)
      onException op (stopMidiOut dev 16)

resolveOutDev :: Maybe OutputDeviceID -> IO OutputDeviceID
resolveOutDev Nothing = do
  outDevM <- getDefaultOutputDeviceID
  (_, outs) <- getAllDevices
  let allOutDevs = fmap fst outs
  let outDev = case outDevM of
        Nothing ->
          if null allOutDevs
            then error "No MIDI outputs!"
            else
              ( case allOutDevs of
                  x : _ -> x
                  [] -> error "resolveOutDev: allOutDev is an Empty List."
              )
        Just x -> unsafeOutputID x
  pure outDev
resolveOutDev (Just x) = pure x

stopMidiOut :: OutputDeviceID -> Channel -> IO ()
stopMidiOut dev i =
  if i < 0
    then threadDelay 1000000 >> terminateMidi
    else do
      deliverMidiEvent dev (0, Std $ ControlChange i 123 0)
      stopMidiOut dev (i - 1)

playRec :: (RealFrac a) => OutputDeviceID -> [(a, MidiMessage)] -> IO ()
playRec _ [] = pure ()
playRec dev (x@(t, m) : ms) =
  if t > 0
    then threadDelay (toMicroSec t) >> playRec dev ((0, m) : ms)
    else
      let mNow = x : takeWhile ((<= 0) . fst) ms
          mLater = drop (length mNow - 1) ms
       in doMidiOut dev (Just mNow) >> playRec dev mLater
  where
    doMidiOut dev Nothing = outputMidi dev
    doMidiOut dev (Just ms) = do
      outputMidi dev
      mapM_ (\(_, m) -> deliverMidiEvent dev (0, m)) ms
    toMicroSec x = round (x * 1000000)

type ChannelMap = [(InstrumentName, Channel)]

type ChannelMapFun = InstrumentName -> ChannelMap -> (Channel, ChannelMap)

musicToMsgs' :: (ToMusic1 a, Enum InstrumentName) => PlayParams -> Music a -> [(Time, MidiMessage)]
musicToMsgs' p m =
  let perf = perfAlg p $ toMusic1 m -- obtain the performance
      evsA = channelMap (chanPolicy p) [] perf -- time-stamped ANote values
      evs = stdMerge evsA -- merged On/Off events sorted by absolute time
      times = fmap fst evs -- absolute times in seconds
      newTimes =
        zipWith
          subtract
          ( ( case times of
                x : _ -> x
                [] -> error "Error Play.hs in musicToMsgs'"
            )
              : times
          )
          times -- relative times
   in zip newTimes (fmap snd evs)
  where
    -- stdMerge: converts ANotes into a sorted list of On/Off events
    stdMerge :: [(Time, MidiMessage)] -> [(Time, MidiMessage)]
    stdMerge [] = []
    stdMerge ((t, ANote c k v d) : es) =
      (t, Std $ NoteOn c k v)
        : stdMerge (insertBy (\(a, _) (x, _) -> compare a x) (t + d, Std $ NoteOff c k v) es)
    stdMerge (e1 : es) = e1 : stdMerge es
    -- channelMap: performs instrument assignment for a list of Events
    channelMap :: ChannelMapFun -> ChannelMap -> [MEvent] -> [(Time, MidiMessage)]
    channelMap _ _ [] = []
    channelMap cf cMap (e : es) =
      let i = eInst e
          ((chan, cMap'), newI) = case lookup i cMap of
            Nothing -> (cf i cMap, True)
            Just x -> ((x, cMap), False)
          e' =
            ( fromRational (eTime e),
              ANote chan (ePitch e) (eVol e) (fromRational $ eDur e)
            )
          es' = channelMap cf cMap' es
          iNum = if i == Percussion then 0 else fromEnum i
       in if newI
            then (fst e', Std $ ProgramChange chan iNum) : e' : es'
            else e' : es'

type NumChannels = Int -- maximum number of channels (i.e. 0-15 is 16 channels)

type PercChan = Int -- percussion channel, using indexing from zero

linearCP :: NumChannels -> PercChan -> ChannelMapFun
linearCP cLim pChan i cMap =
  if i == Percussion
    then (pChan, (i, pChan) : cMap)
    else
      let n = length $ filter ((/= Percussion) . fst) cMap
          newChan = if n >= pChan then n + 1 else n -- step over the percussion channel
       in if newChan < cLim
            then (newChan, (i, newChan) : cMap)
            else
              error ("Cannot use more than " <> (show cLim <> " instruments."))

dynamicCP :: NumChannels -> PercChan -> ChannelMapFun
dynamicCP cLim pChan i cMap =
  if i == Percussion
    then (pChan, (i, pChan) : cMap)
    else
      let cMapNoP = filter ((/= Percussion) . fst) cMap
          extra = ([(Percussion, pChan) | length cMapNoP /= length cMap])
          newChan =
            snd
              ( case reverse cMapNoP of
                  x : _ -> x
                  [] -> error  "Error: Play.hs: dynamicCP: cMapNoP is an empty list"
              )
       in if length cMapNoP < cLim - 1
            then linearCP cLim pChan i cMap
            else (newChan, (i, newChan) : (take (length cMapNoP - 1) cMapNoP <> extra))

predefinedCP :: ChannelMap -> ChannelMapFun
predefinedCP cMapFixed i _ = case lookup i cMapFixed of
  Nothing -> error (show i <> " is not included in the channel map.")
  Just c -> (c, cMapFixed)

instance NFData FileType where
  rnf x = ()

instance NFData TimeDiv where
  rnf (TicksPerBeat i) = rnf i
  rnf (TicksPerSecond i j) = rnf j `seq` rnf i

instance NFData Midi where
  rnf (Midi ft td ts) = rnf ft `seq` rnf td `seq` rnf ts

instance NFData Message where
  rnf (NoteOff c k v) = rnf c `seq` rnf k `seq` rnf v
  rnf (NoteOn c k v) = rnf c `seq` rnf k `seq` rnf v
  rnf (KeyPressure c k v) = rnf c `seq` rnf k `seq` rnf v
  rnf (ProgramChange c v) = rnf c `seq` rnf v
  rnf (ChannelPressure c v) = rnf c `seq` rnf v
  rnf (PitchWheel c v) = rnf c `seq` rnf v
  rnf (TempoChange t) = rnf t
  rnf _ = () -- no other message types are currently used by Euterpea

instance NFData MidiMessage where
  rnf (Std m) = rnf m
  rnf (ANote c k v d) = rnf c `seq` rnf k `seq` rnf v `seq` rnf d

instance (NFData a) => NFData (Music a) where
  rnf (a :+: b) = rnf a `seq` rnf b
  rnf (a :=: b) = rnf a `seq` rnf b
  rnf (Prim p) = rnf p
  rnf (Modify c m) = rnf c `seq` rnf m

instance (NFData a) => NFData (Primitive a) where
  rnf (Note d a) = rnf d `seq` rnf a
  rnf (Rest d) = rnf d

instance NFData Control where
  rnf :: Control -> ()
  rnf (Tempo t) = rnf t
  rnf (Transpose t) = rnf t
  rnf (Instrument i) = rnf i
  rnf (Phrase xs) = rnf xs
  rnf (Custom s) = rnf s
  rnf (KeySig r m) = rnf r `seq` rnf m

instance NFData PitchClass where
  rnf :: PitchClass -> ()
  rnf _ = ()

instance NFData Mode where
  rnf :: Mode -> ()
  rnf _ = ()

instance NFData PhraseAttribute where
  rnf (Dyn d) = rnf d
  rnf (Tmp t) = rnf t
  rnf (Art a) = rnf a
  rnf (Orn o) = rnf o

instance NFData Dynamic where
  rnf (Accent r) = rnf r
  rnf (Crescendo r) = rnf r
  rnf (Diminuendo r) = rnf r
  rnf (StdLoudness x) = rnf x
  rnf (Loudness r) = rnf r

instance NFData StdLoudness where
  rnf :: StdLoudness -> ()
  rnf _ = ()

instance NFData Articulation where
  rnf :: Articulation -> ()
  rnf (Staccato r) = rnf r
  rnf (Legato r) = rnf r
  rnf _ = ()

instance NFData Ornament where
  rnf :: Ornament -> ()
  rnf _ = ()

instance NFData Tempo where
  rnf :: Tempo -> ()
  rnf (Ritardando r) = rnf r
  rnf (Accelerando r) = rnf r

instance NFData InstrumentName where
  rnf :: InstrumentName -> ()
  rnf _ = ()

instance NFData NoteAttribute where
  rnf (Volume v) = rnf v
  rnf (Fingering f) = rnf f
  rnf (Dynamics d) = rnf d
  rnf (Params p) = rnf p
