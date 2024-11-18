{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Euterpea.IO.MIDI.MidiIO
  ( getAllDevices, -- isValidInputDevice, isValidOutputDevice, -- Used only by Euterpea.IO.MUI.MidiWidgets
    terminateMidi,
    initializeMidi, -- Used only by Euterpea.IO.MUI
    outputMidi,
    deliverMidiEvent, -- Used only by Euterpea.IO.MUI.MidiWidgets (particularly by midiOut)
    pollMidi, -- Used only by Euterpea.IO.MUI.MidiWidgets (particularly by midiIn)
    defaultOutput,
    defaultInput,
    playMidi,
    MidiMessage (ANote, Std),
    getTimeNow,
    DeviceInfo (..),
    InputDeviceID,
    OutputDeviceID,
    Message (..),
    Time,
    unsafeInputID,
    unsafeOutputID,
    EventQueue (..),
    DeviceState (..),
    playTrackRealTime,
    recordMidi,
    pollMidiCB,
    printAllDeviceInfo,
  )
where

import Codec.Midi
  ( Channel,
    Key,
    Message (..),
    Midi (..),
    Time,
    Track,
    Velocity,
    isTrackEnd,
    toAbsTime,
    toRealTime,
    toSingleTrack,
  )
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.DeepSeq (NFData)
import Control.Exception (finally)
import Control.Monad
import qualified Control.Monad as CM
import Control.Monad.STM (atomically)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Functor ((<&>))
import qualified Data.Heap as Heap
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Vector.Storable as VS
import Sound.PortMidi
  ( DeviceID,
    DeviceInfo (..),
    PMError (..),
    PMEvent (..),
    PMMsg (PMMsg),
    PMStream,
    PMSuccess (..),
    close,
    countDevices,
    decodeMsg,
    encodeMsg,
    getDefaultInputDeviceID,
    getDefaultOutputDeviceID,
    getDeviceInfo,
    getErrorText,
    initialize,
    openInput,
    openOutput,
    readEvents,
    terminate,
    time,
    writeShort,
  )
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

type MidiEvent = (Time, MidiMessage)

data MidiMessage
  = ANote
      {-# UNPACK #-} !Channel
      {-# UNPACK #-} !Key
      {-# UNPACK #-} !Velocity
      {-# UNPACK #-} !Time
  | Std !Message
  deriving (Show)

data EventQueue = EventQueue
  { queueBuffer :: {-# UNPACK #-} !(VS.Vector (Time, MidiMessage)),
    queueSize :: {-# UNPACK #-} !Int,
    queueHead :: {-# UNPACK #-} !Int,
    queueTail :: {-# UNPACK #-} !Int
  }

-- data MidiMessage = ANote { channel :: !Channel, key :: !Key,
--                           velocity :: !Velocity, duration :: !Time }
--                  | Std Message
--   deriving Show

newtype InputDeviceID = InputDeviceID DeviceID
  deriving (Eq, Show, NFData)

newtype OutputDeviceID = OutputDeviceID DeviceID
  deriving (Eq, Show, NFData)

-- Fixed-size bounded channels for better memory usage
type MidiChannel = TChan (Time, MidiMessage)

data DeviceState = DeviceState
  { inputBuffer :: {-# UNPACK #-} !MidiChannel,
    outputBuffer :: {-# UNPACK #-} !MidiChannel,
    deviceQueue :: {-# UNPACK #-} !EventQueue,
    lastEventTime :: {-# UNPACK #-} !(IORef Time)
  }

unsafeInputID :: Int -> InputDeviceID
unsafeInputID = InputDeviceID

unsafeOutputID :: Int -> OutputDeviceID
unsafeOutputID = OutputDeviceID

getTimeNow :: IO Time
getTimeNow = (/ 1000) . fromIntegral <$> time

-- getTimeNow :: IO Time
-- getTimeNow = do
--   t <- time
--   return (fromIntegral t / 1000)

getAllDevices :: IO ([(InputDeviceID, DeviceInfo)], [(OutputDeviceID, DeviceInfo)])
getAllDevices = do
  n <- countDevices
  deviceInfos <- mapM getDeviceInfo [0 .. n - 1]
  let devs = zip [0 .. n - 1] deviceInfos
  return
    ( [(InputDeviceID d, i) | (d, i) <- devs, input i],
      [(OutputDeviceID d, i) | (d, i) <- devs, output i]
    )

defaultOutput :: (OutputDeviceID -> a -> IO b) -> a -> IO b
defaultOutput f a = do
  i <- getDefaultOutputDeviceID
  case i of
    Nothing -> error "No MIDI output device found"
    Just i -> f (OutputDeviceID i) a

defaultInput :: (InputDeviceID -> a -> IO b) -> a -> IO b
defaultInput f a = do
  i <- getDefaultInputDeviceID
  case i of
    Nothing -> error "No MIDI input device found"
    Just i -> f (InputDeviceID i) a

data PrioChannel a b = PrioChannel
  { get :: IO (Heap.MinPrioHeap a b),
    push :: a -> b -> IO (),
    pop :: IO (Maybe (a, b)),
    peek :: IO (Maybe (a, b))
  }

makePriorityChannel :: IO (PrioChannel Time Message)
makePriorityChannel = do
  heapRef <- newIORef (Heap.empty :: Heap.MinPrioHeap Time Message)
  let getHeap = readIORef heapRef
      push a b = modifyIORef heapRef (Heap.insert (a, b))
      pop = do
        h <- getHeap
        case Heap.view h of
          Just ((a, b), h') -> do
            writeIORef heapRef h'
            return (Just (a, b))
          Nothing -> return Nothing
      peek = do
        fmap fst . Heap.view <$> getHeap

  return $ PrioChannel getHeap push pop peek

outDevMap ::
  IORef
    [ ( OutputDeviceID,
        ( PrioChannel Time Message, -- priority channel
          (Time, Message) -> IO (), -- sound output function
          IO () -- stop/terminate function
        )
      )
    ]
{-# NOINLINE outDevMap #-}
outDevMap = unsafePerformIO $ newIORef []

outPort :: IORef [(OutputDeviceID, PMStream)]
inPort :: IORef [(InputDeviceID, PMStream)]
{-# NOINLINE outPort #-}
outPort = unsafePerformIO (newIORef [])

{-# NOINLINE inPort #-}
inPort = unsafePerformIO (newIORef [])

lookupPort :: (Eq deviceid) => IORef [(deviceid, PMStream)] -> deviceid -> IO (Maybe PMStream)
lookupPort p i = readIORef p <&> lookup i

addPort :: IORef [(deviceid, PMStream)] -> (deviceid, PMStream) -> IO ()
addPort p is = modifyIORef p (is :)

initializeMidi :: IO ()
initializeMidi = do
  e <- initialize
  case e of
    Right _ -> return ()
    Left e' -> reportError "initializeMidi" e'

terminateMidi :: IO ()
terminateMidi = do
  inits <- readIORef outDevMap
  mapM_ (\(_, (_, _out, stop)) -> stop) inits
  result <- terminate
  case result of
    Left err -> putStrLn ("Error during termination: " ++ show err)
    Right _ -> return ()
  writeIORef outDevMap []
  writeIORef outPort []
  writeIORef inPort []

getOutDev :: OutputDeviceID -> IO (PrioChannel Time Message, (Time, Message) -> IO (), IO ())
getOutDev devId = do
  inits <- readIORef outDevMap
  case lookup devId inits of
    Just f -> return f
    Nothing -> do
      x <- midiOutRealTime' devId -- Changes made by Donya Quick: this line used to pattern match against Just.
      pChan <- makePriorityChannel
      case x of
        Just (mout, stop) -> do
          -- Case statement added.
          modifyIORef outDevMap ((devId, (pChan, mout, stop)) :)
          return (pChan, mout, stop)
        Nothing -> return (pChan, const (return ()), return ()) -- Nothing case added

pollMidiCB :: InputDeviceID -> ((Time, [Message]) -> IO ()) -> IO ()
pollMidiCB idid@(InputDeviceID devId) callback = do
  s <- lookupPort inPort idid
  case s of
    Nothing -> do
      r <- openInput devId
      case r of
        Left e -> reportError "pollMidiCB" e
        Right s -> addPort inPort (idid, s) >> input s
    Just s -> input s
  where
    input :: PMStream -> IO ()
    input s = do
      e <- readEvents s
      case e of
        Left e -> reportError "pollMidiCB" e
        Right l -> do
          now <- getTimeNow
          case mapMaybe (msgToMidi . decodeMsg . message) l of
            [] -> return ()
            ms -> callback (now, ms)

pollMidi :: InputDeviceID -> IO (Maybe (Time, [Message]))
pollMidi idid@(InputDeviceID devId) = do
  s <- lookupPort inPort idid
  case s of
    Nothing -> do
      r <- openInput devId
      case r of
        Left e -> reportError "pollMIDI" e >> return Nothing
        Right s -> addPort inPort (idid, s) >> input s
    Just s -> input s
  where
    input :: PMStream -> IO (Maybe (Time, [Message]))
    input s = do
      e <- readEvents s
      case e of
        Left e -> reportError "pollMIDI" e >> return Nothing
        Right l -> do
          now <- getTimeNow
          case mapMaybe (msgToMidi . decodeMsg . message) l of
            [] -> return Nothing
            ms -> return $ Just (now, ms)

deliverMidiEvent :: OutputDeviceID -> MidiEvent -> IO ()
deliverMidiEvent devId (t, m) = do
  (pChan, out, _stop) <- getOutDev devId
  now <- getTimeNow
  let deliver t' m' =
        if t' == 0
          then out (now, m')
          else push pChan (now + t') m'

  case m of
    Std m' -> deliver t m'
    ANote c k v d -> do
      deliver t (NoteOn c k v)
      deliver (t + d) (NoteOff c k v)

outputMidi :: OutputDeviceID -> IO ()
outputMidi devId = do
  (pChan, out, _stop) <- getOutDev devId
  let loop = do
        r <- peek pChan
        case r of
          Nothing -> return ()
          Just (t, m) -> do
            now <- getTimeNow
            CM.when (t <= now) $ out (now, m) >> pop pChan >> loop
  loop
  return ()

-- playMidi :: OutputDeviceID -> Midi -> IO ()
-- playMidi device midi@(Midi _ division _) = do
--   let track = toRealTime division (toAbsTime (head (tracks (toSingleTrack midi))))
--   out <- midiOutRealTime device
--   case out of
--     Nothing -> return ()
--     Just (out, stop) -> do
--       t0 <- getTimeNow
--       finally (playTrack t0 0 out track) stop
--   where
--     playTrack t0 t' out [] = out (t0 + t', TrackEnd)
--     playTrack t0 _ out (e@(t, m) : s) = do
--       out (t0 + t, m)
--       if isTrackEnd m
--         then return ()
--         else playTrack t0 t out s

playMidi :: OutputDeviceID -> Midi -> IO ()
playMidi device midi@(Midi _ division _) = do
  let track = toRealTime division (toAbsTime (head (tracks (toSingleTrack midi))))
  midiOutRealTime device >>= maybe (return ()) (`playMIDIImplementation` track)
  where
    playMIDIImplementation (out, stop) track = do
      t0 <- getTimeNow
      finally (playTrack t0 0 out track) stop
    playTrack t0 _ out [] = out (t0, TrackEnd)
    playTrack t0 _ out ((t, m) : s) = out (t0 + t, m) >> unless (isTrackEnd m) (playTrack t0 t out s)

midiOutRealTime' :: OutputDeviceID -> IO (Maybe ((Time, Message) -> IO (), IO ()))
midiOutRealTime' odid@(OutputDeviceID devId) = do
  s <- openOutput devId 1
  case s of
    Left e -> reportError "Unable to open output device in midiOutRealTime'" e >> return Nothing
    Right s -> do
      addPort outPort (odid, s)
      return $ Just (process odid, finalize odid)
  where
    process odid (t, msg) = do
      s <- lookupPort outPort odid
      case s of
        Nothing -> error ("midiOutRealTime': port " ++ show odid ++ " is not open for output")
        Just s -> do
          if isTrackEnd msg
            then return ()
            else case midiEvent msg of
              Just m -> writeMsg s t $ encodeMsg m
              Nothing -> return ()
    writeMsg s t m = do
      e <- writeShort s (PMEvent m (round (t * 1e3)))
      case e of
        Left e' -> reportError "midiOutRealTime'" e'
        Right _ -> return ()
    finalize odid = do
      s <- lookupPort outPort odid
      e <- maybe (return (Right NoError'NoData)) close s
      case e of
        Left e' -> reportError "midiOutRealTime'" e'
        Right _ -> return ()

midiOutRealTime :: OutputDeviceID -> IO (Maybe ((Time, Message) -> IO (), IO ()))
midiOutRealTime (OutputDeviceID devId) = do
  s <- openOutput devId 1
  case s of
    Left e -> reportError "outputMidi" e >> return Nothing
    Right s -> do
      ch <- atomically newTChan
      wait <- newEmptyMVar
      fin <- newEmptyMVar
      _ <- forkIO (pump s ch wait fin)
      pure $ Just (output s ch wait, stop ch fin)
  where
    stop ch fin = atomically (unGetTChan ch Nothing) >> takeMVar fin
    output _ ch wait evt@(_, m) = do
      atomically $ writeTChan ch (Just evt)
      CM.when (isTrackEnd m) $ takeMVar wait
    pump s ch wait fin = loop
      where
        loop = do
          e <- atomically $ readTChan ch
          case e of
            Nothing -> close s >> putMVar fin ()
            Just (t, msg) -> do
              now <- getTimeNow
              if t > now + 5
                then atomically (unGetTChan ch e) >> threadDelay 10000 >> loop
                else do
                  done <- process t msg
                  if done
                    then waitUntil (t + 1)
                    else loop
          where
            waitUntil t = do
              now <- getTimeNow
              if t > now
                then do
                  threadDelay $ min 10000 (round ((t - now) * 1E6))
                  empty <- atomically $ isEmptyTChan ch
                  if empty
                    then waitUntil t
                    else do
                      e <- atomically $ readTChan ch
                      case e of
                        Nothing -> finishup
                        _ -> waitUntil t
                else finishup
            finishup = putMVar wait () >> close s >> putMVar fin ()
            process t msg =
              if isTrackEnd msg
                then return True
                else case midiEvent msg of
                  Just m -> writeMsg t $ encodeMsg m
                  Nothing -> return False
            writeMsg t m = do
              e <- writeShort s (PMEvent m (round (t * 1e3)))
              case e of
                Left BufferOverflow -> putStrLn "overflow" >> threadDelay 10000 >> writeMsg t m
                Left e' -> reportError "outputMidi" e' >> return True
                Right _ -> return False

midiEvent :: Message -> Maybe PMMsg
midiEvent (NoteOff c p v) = Just $ PMMsg (128 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
midiEvent (NoteOn c p v) = Just $ PMMsg (144 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral v)
midiEvent (KeyPressure c p pr) = Just $ PMMsg (160 .|. (fromIntegral c .&. 0xF)) (fromIntegral p) (fromIntegral pr)
midiEvent (ControlChange c cn cv) = Just $ PMMsg (176 .|. (fromIntegral c .&. 0xF)) (fromIntegral cn) (fromIntegral cv)
midiEvent (ProgramChange c pn) = Just $ PMMsg (192 .|. (fromIntegral c .&. 0xF)) (fromIntegral pn) 0
midiEvent (ChannelPressure c pr) = Just $ PMMsg (208 .|. (fromIntegral c .&. 0xF)) (fromIntegral pr) 0
midiEvent (PitchWheel c pb) = Just $ PMMsg (224 .|. (fromIntegral c .&. 0xF)) (fromIntegral lo) (fromIntegral hi)
  where
    (hi, lo) = (pb `shiftR` 8, pb .&. 0xFF)
midiEvent _ = Nothing

msgToMidi :: PMMsg -> Maybe Message
msgToMidi (PMMsg m d1 d2) =
  let k = (m .&. 0xF0) `shiftR` 4
      c = fromIntegral (m .&. 0x0F)
   in case k of
        0x8 -> Just $ NoteOff c (fromIntegral d1) (fromIntegral d2)
        0x9 -> Just $ NoteOn c (fromIntegral d1) (fromIntegral d2)
        0xA -> Just $ KeyPressure c (fromIntegral d1) (fromIntegral d2)
        0xB -> Just $ ControlChange c (fromIntegral d1) (fromIntegral d2)
        0xC -> Just $ ProgramChange c (fromIntegral d1)
        0xD -> Just $ ChannelPressure c (fromIntegral d1)
        0xE -> Just $ PitchWheel c (fromIntegral (d1 + d2 `shiftL` 8))
        0xF -> Nothing -- SysEx event not handled
        _ -> Nothing

reportError :: String -> PMError -> IO ()
reportError prompt e = do
  err <- getErrorText e
  hPutStrLn stderr $ prompt ++ ": " ++ err

-- Prints all DeviceInfo found by getAllDevices.
printAllDeviceInfo :: IO ()
printAllDeviceInfo = do
  (indevs, outdevs) <- getAllDevices
  mapM_ (print . snd) indevs
  mapM_ (print . snd) outdevs

playTrackRealTime :: OutputDeviceID -> [(t, Message)] -> IO ()
playTrackRealTime device track = do
  out <- midiOutRealTime device
  case out of
    Nothing -> return ()
    Just (out, stop) -> finally (playTrack out track) stop
  where
    playTrack out [] = do
      t <- getTimeNow
      out (t, TrackEnd)
    playTrack out (e@(_, m) : s) = do
      t <- getTimeNow
      out (t, m)
      if isTrackEnd m
        then return ()
        else playTrack out s

{-
    ticksPerBeat = case division of
      TicksPerBeat n -> n
      TicksPerSecond mode nticks -> (256 - mode - 128) * nticks `div` 2
-}
{-
runTrack tpb = runTrack' 0 0 120                 -- 120 beat/s is the default tempo
  where
    runTrack' t t0 bps ((_, TempoChange tempo) : l) =
      let bps' = 1000000 `div` fromIntegral tempo
      in runTrack' t t0 bps' l
    runTrack' t t0 bps ((t1, m) : l) =
      let t' = t + 1000 * fromIntegral (t1 - t0) `div` (tpb * bps)
      in (t', m) : runTrack' t' t1 bps l
    runTrack' _ _ _ [] = []

playTrack s ch t0 = playTrack' 0
  where
    playTrack' t [] = putStrLn "done" >> putMVar ch Nothing >> return (round (t * 1.0E3))
    playTrack' _ ((t, e):es) = putMVar ch (Just io) >> playTrack' t es
      where
        io = case midiEvent e of
          Just m  -> writeShort s (PMEvent m (t0 + round (t * 1.0E3)))
          Nothing -> return NoError
-}
recordMidi :: DeviceID -> (Track Time -> IO ()) -> IO ()
recordMidi device f = do
  ch <- newChan
  final <- midiInRealTime device (\e -> writeChan ch e >> return False)
  case final of
    Nothing -> return ()
    Just fin -> do
      track <- getChanContents ch
      done <- newEmptyMVar
      _ <- forkIO (f track >> putMVar done ())
      putStrLn "Start recording, hit ENTER when you are done."
      _ <- getLine
      fin
      takeMVar done
      return ()

midiInRealTime :: DeviceID -> ((Time, Message) -> IO Bool) -> IO (Maybe (IO ()))
midiInRealTime device callback = do
  r <- openInput device
  case r of
    Left e -> reportError "midiInRealTime" e >> return Nothing
    Right s -> do
      fin <- newEmptyMVar
      _ <- forkIO (loop Nothing s fin)
      pure $ Just (putMVar fin () >> putMVar fin ())
  where
    loop start s fin = do
      done <- tryTakeMVar fin
      t <- getTimeNow
      case done of
        Just _ -> close s >> callback (t, TrackEnd) >> takeMVar fin >> return ()
        Nothing -> do
          e <- readEvents s
          case e of
            Left e -> do
              reportError "midiInRealTime" e
              _ <- callback (t, TrackEnd)
              return ()
            Right l -> do
              t <- getTimeNow
              sendEvts start t l
      where
        sendEvts start now [] = loop start s fin
        sendEvts start now (e@(PMEvent m t) : l) = do
          let t0 = Data.Maybe.fromMaybe t start
          case msgToMidi $ decodeMsg m of
            Just m' -> do
              done <- callback (now + fromIntegral (t - t0) / 1E3, m')
              if done then CM.void (close s) else sendEvts (Just t0) now l
            Nothing -> sendEvts (Just t0) now l

{-
module OptimizedMidiIO where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as BS
import Control.Concurrent.BoundedChan
import Control.Monad.ST
import Data.IORef.Unboxed
import Foreign.Storable
import GHC.Conc (setNumCapabilities)
import System.IO.Unsafe (unsafePerformIO)

-- Use strict data types for messages
data MidiMessage' =
    ANote' {-# UNPACK #-} !Channel
           {-# UNPACK #-} !Key
           {-# UNPACK #-} !Velocity
           {-# UNPACK #-} !Time
  | Std' !Message
  deriving Show

-- Optimized priority queue for real-time events
data EventQueue = EventQueue {
    queueBuffer :: {-# UNPACK #-} !(VS.Vector (Time, MidiMessage')),
    queueSize   :: {-# UNPACK #-} !Int,
    queueHead   :: {-# UNPACK #-} !Int,
    queueTail   :: {-# UNPACK #-} !Int
}

-- Fixed-size bounded channels for better memory usage
type MidiChannel = BoundedChan (Time, MidiMessage')

-- Optimized device management
data DeviceState = DeviceState {
    inputBuffer  :: {-# UNPACK #-} !MidiChannel,
    outputBuffer :: {-# UNPACK #-} !MidiChannel,
    deviceQueue  :: {-# UNPACK #-} !EventQueue,
    lastEventTime :: {-# UNPACK #-} !UnboxedIORef Time
}

-- Create a new event queue with pre-allocated buffer
newEventQueue :: Int -> IO EventQueue
newEventQueue size = do
    let vec = VS.replicate size (0, Std' (NoteOff 0 0 0))
    return EventQueue {
        queueBuffer = vec,
        queueSize = size,
        queueHead = 0,
        queueTail = 0
    }

-- Optimized event insertion using circular buffer
insertEvent :: EventQueue -> (Time, MidiMessage') -> IO EventQueue
insertEvent q@EventQueue{..} event = do
    let newTail = (queueTail + 1) `mod` queueSize
    if newTail == queueHead
        then error "Queue full"
        else do
            let vec' = VS.modify (\v -> VS.write v queueTail event) queueBuffer
            return q { queueBuffer = vec', queueTail = newTail }

-- Optimized event retrieval
getNextEvent :: EventQueue -> IO (Maybe (Time, MidiMessage'), EventQueue)
getNextEvent q@EventQueue{..} =
    if queueHead == queueTail
        then return (Nothing, q)
        else do
            let event = queueBuffer `VS.unsafeIndex` queueHead
                newHead = (queueHead + 1) `mod` queueSize
            return (Just event, q { queueHead = newHead })

-- Optimized MIDI input handling
optimizedMidiInput :: InputDeviceID -> DeviceState -> IO ()
optimizedMidiInput devId state = do
    -- Pre-allocate buffers
    let bufferSize = 1024
    inBuf <- VS.new bufferSize

    let loop = do
            -- Read events in batch
            events <- readEventsOptimized devId inBuf

            -- Process events in bulk
            processEventsBatch events

            -- Continue loop
            loop

    loop
  where
    processEventsBatch events = do
        now <- getTimeNow
        -- Process multiple events at once using vectors
        VS.forM_ events $ \event -> do
            writeBoundedChan (inputBuffer state) (now, event)

-- Optimized MIDI output handling
optimizedMidiOutput :: OutputDeviceID -> DeviceState -> IO ()
optimizedMidiOutput devId state = do
    -- Pre-allocate output buffer
    outBuf <- VS.new 1024

    let loop lastTime = do
            now <- getTimeNow
            events <- drainEvents (outputBuffer state) 64  -- Process up to 64 events at once

            -- Bulk process events
            processEventsBatch events outBuf

            -- Calculate optimal sleep time
            let nextWakeup = calculateNextWakeup events
            threadDelay nextWakeup

            loop now

    loop 0
  where
    processEventsBatch events outBuf = do
        VS.unsafeWith outBuf $ \ptr -> do
            -- Direct memory manipulation for better performance
            mapM_ (writeEvent ptr) events
            flushEvents ptr (VS.length events)

-- Low-level optimized MIDI event writing
writeEvent :: Ptr Word8 -> (Time, MidiMessage') -> IO ()
writeEvent ptr (time, msg) = do
    -- Direct memory writing without bounds checking
    pokeByteOff ptr 0 (encodeMsg msg)
    pokeByteOff ptr 4 (round (time * 1000) :: Int32)

-- Optimized initialization
initializeOptimizedMidi :: Int -> IO DeviceState
initializeOptimizedMidi bufferSize = do
    -- Set number of capabilities for optimal threading
    setNumCapabilities 2  -- One for input, one for output

    -- Create bounded channels with fixed size
    inChan <- newBoundedChan bufferSize
    outChan <- newBoundedChan bufferSize

    -- Create event queue with pre-allocated buffer
    queue <- newEventQueue bufferSize

    -- Use unboxed IORef for better performance
    timeRef <- newUnboxedIORef 0

    return DeviceState {
        inputBuffer = inChan,
        outputBuffer = outChan,
        deviceQueue = queue,
        lastEventTime = timeRef
    }

-- Helper functions for batch processing
drainEvents :: MidiChannel -> Int -> IO [(Time, MidiMessage')]
drainEvents chan maxEvents = do
    -- Read multiple events at once
    events <- readAvailable chan maxEvents
    return events
  where
    readAvailable chan 0 = return []
    readAvailable chan n = do
        empty <- isEmptyChan chan
        if empty
            then return []
            else do
                event <- readChan chan
                rest <- readAvailable chan (n-1)
                return (event : rest)

-- Calculate optimal sleep time based on event timing
calculateNextWakeup :: [(Time, MidiMessage')] -> Int
calculateNextWakeup events =
    case events of
        [] -> 1000  -- Default sleep time when no events
        xs -> min 1000 $ round $ (minimum (map fst xs) * 1000)

-- Main processing loop with optimizations
processOptimizedMidi :: DeviceState -> IO ()
processOptimizedMidi state = do
    -- Start input and output threads with high priority
    inputThread <- forkOS $ optimizedMidiInput inDev state
    outputThread <- forkOS $ optimizedMidiOutput outDev state

    -- Set thread priorities
    setThreadPriority inputThread high
    setThreadPriority outputThread high

    -- Monitor and adjust performance
    monitorPerformance state
  where
    monitorPerformance state = do
        -- Collect performance metrics
        stats <- gatherPerformanceStats state
        -- Adjust parameters based on stats
        adjustParameters state stats
        -- Continue monitoring
        threadDelay 1000000  -- Check every second
        monitorPerformance state

-}
