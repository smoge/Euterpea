{-# LANGUAGE DeriveGeneric #-}

module Euterpea.IO.MIDI.MEvent where

import Euterpea.Music
import GHC.Generics (Generic)

data MEvent = MEvent
  { eTime :: {-# UNPACK #-} !PTime,
    eInst :: !InstrumentName,
    ePitch :: {-# UNPACK #-} !AbsPitch,
    eDur :: {-# UNPACK #-} !DurT,
    eVol :: {-# UNPACK #-} !Volume,
    eParams :: ![Double]
  }
  deriving (Eq, Generic)

-- | A performance is a list of musical events (MEvents).
type Performance = [MEvent]

-- | Type alias for onset time, represented as a Rational value.
type PTime = Rational

-- | Type alias for duration, represented as a Rational value.
type DurT = Rational

-- | Merge two performances (lists of MEvents), sorting them by event time.
merge :: Performance -> Performance -> Performance
merge [] es2 = es2
-- \^ If the first list is empty, return the second list.
merge es1 [] = es1
-- \^ If the second list is empty, return the first list.
merge a@(e1 : es1) b@(e2 : es2) =
  if eTime e1 < eTime e2
    then e1 : merge es1 b
    -- \^ If the first event is earlier, add it to the result.
    else e2 : merge a es2

-- \^ Otherwise, add the second event to the result.

-- | Context for musical events, maintaining current state like time, instrument, duration, and volume.
data MContext = MContext
  { -- | Current time in the context.
    mcTime :: !PTime,
    -- | Current instrument in the context.
    mcInst :: !InstrumentName,
    -- | Current note duration in the context.
    mcDur :: !DurT,
    -- | Current volume in the context.
    mcVol :: !Volume
  }
  deriving (Show)

-- | Convert a generic Music structure into a Performance.
perform :: (ToMusic1 a) => Music a -> Performance
perform = perform1 . toMusic1

-- \^ First convert to Music1, then perform.

-- | Perform a Music1 structure to obtain a Performance.
perform1 :: Music1 -> Performance
perform1 = fst . perform1Dur

-- \^ Perform and ignore the returned duration.

-- | Perform a Music1 structure and also return its total duration.
perform1Dur :: Music1 -> (Performance, DurT)
perform1Dur = musicToMEvents defCon . applyControls
  where
    defCon = MContext {mcTime = 0, mcInst = AcousticGrandPiano, mcDur = metro 120 qn, mcVol = 127}
    -- \^ Default context, using Acoustic Grand Piano and default tempo.

    -- \| Calculate duration based on metronome setting.
    metro :: Int -> Dur -> DurT
    metro setting dur_ = 60 / (fromIntegral setting * dur_)

-- | Apply musical controls such as tempo and transposition recursively to a Music1 structure.
applyControls :: Music1 -> Music1
applyControls (Modify (Tempo r) m) = scaleDurations r $ applyControls m
applyControls (Modify (Transpose k) m) = shiftPitches1 k $ applyControls m
applyControls (Modify x m) = Modify x $ applyControls m
applyControls (m1 :+: m2) = applyControls m1 :+: applyControls m2
applyControls (m1 :=: m2) = applyControls m1 :=: applyControls m2
applyControls x = x

-- | Convert a Music1 structure to a list of MEvents and compute its duration.
musicToMEvents :: MContext -> Music1 -> (Performance, DurT)
musicToMEvents c@MContext {mcTime = _, mcDur = dt} (Prim (Note d p)) = ([noteToMEvent c d p], d * dt)
musicToMEvents c@MContext {mcTime = _, mcDur = dt} (Prim (Rest d)) = ([], d * dt)
musicToMEvents c@MContext {mcTime = t, mcDur = _} (m1 :+: m2) =
  let (evs1, d1) = musicToMEvents c m1
      (evs2, d2) = musicToMEvents c {mcTime = t + d1} m2
   in (evs1 ++ evs2, d1 + d2)
musicToMEvents c@MContext {mcTime = t, mcDur = dt} (m1 :=: m2) =
  let (evs1, d1) = musicToMEvents c m1
      (evs2, d2) = musicToMEvents c m2
   in (merge evs1 evs2, max d1 d2)
musicToMEvents c (Modify (Instrument i) m) = musicToMEvents c {mcInst = i} m
musicToMEvents c (Modify (Phrase pas) m) = phraseToMEvents c pas m
musicToMEvents c (Modify (KeySig x y) m) = musicToMEvents c m
-- \^ Key signature change causes no actual change here.
musicToMEvents c (Modify (Custom x) m) = musicToMEvents c m
-- \^ Custom modifications are ignored.
musicToMEvents c m@(Modify x m') = musicToMEvents c $ applyControls m

-- \^ Transpose and tempo addressed by applyControls.

-- | Convert a single note with duration and pitch into an MEvent, considering the current context.
noteToMEvent :: MContext -> Dur -> (Pitch, [NoteAttribute]) -> MEvent
noteToMEvent MContext {mcTime = ct, mcInst = ci, mcDur = cdur, mcVol = cvol} d (p, nas) =
  foldr applyNoteAttribute initialEvent nas
  where
    initialEvent =
      MEvent
        { eTime = ct,
          ePitch = absPitch p,
          eInst = ci,
          eDur = d * cdur,
          eVol = cvol,
          eParams = []
        }

    applyNoteAttribute :: NoteAttribute -> MEvent -> MEvent
    applyNoteAttribute (Volume v) ev = ev {eVol = v}
    applyNoteAttribute (Params pms) ev = ev {eParams = pms}
    applyNoteAttribute _ ev = ev

-- | Convert a phrase into a set of MEvents, applying various phrase attributes like dynamics or tempo changes.
phraseToMEvents :: MContext -> [PhraseAttribute] -> Music1 -> (Performance, DurT)
phraseToMEvents c [] m = musicToMEvents c m
phraseToMEvents c@MContext {mcTime = t, mcInst = i, mcDur = dt} (pa : pas) m =
  let pfd@(pf, dur) = phraseToMEvents c pas m

      loudnessLevel x = phraseToMEvents c (Dyn (Loudness x) : pas) m

      adjustTimeAndDuration x =
        let t0 = eTime (head pf)
            r = x / dur
            updateEvent e@MEvent {eTime = t, eDur = d} =
              let dt = t - t0
                  t' = (1 + dt * r) * dt + t0
                  d' = (1 + (2 * dt + d) * r) * d
               in e {eTime = t', eDur = d'}
         in (map updateEvent pf, (1 + x) * dur)

      adjustVolume x =
        let t0 = eTime (head pf)
            r = x / dur
            updateEvent e@MEvent {eTime = t, eVol = v} =
              e {eVol = round ((1 + (t - t0) * r) * fromIntegral v)}
         in (map updateEvent pf, dur)

      adjustDuration factor = (map (\e -> e {eDur = factor * eDur e}) pf, dur)

      applySlurred x =
        let lastStartTime = maximum $ map eTime pf
            setDuration e =
              if eTime e < lastStartTime
                then e {eDur = x * eDur e}
                else e
         in (map setDuration pf, dur)
   in case pa of
        Dyn (Accent x) ->
          (map (\e -> e {eVol = round (x * fromIntegral (eVol e))}) pf, dur)
        Dyn (StdLoudness l) ->
          loudnessLevel $ case l of
            PPP -> 40
            PP -> 50
            P -> 60
            MP -> 70
            SF -> 80
            MF -> 90
            NF -> 100
            FF -> 110
            FFF -> 120
        Dyn (Loudness x) -> phraseToMEvents c {mcVol = round x} pas m
        Dyn (Crescendo x) -> adjustVolume x
        Dyn (Diminuendo x) -> adjustVolume (-x)
        Tmp (Ritardando x) -> adjustTimeAndDuration x
        Tmp (Accelerando x) -> adjustTimeAndDuration (-x)
        Art (Staccato x) -> adjustDuration x
        Art (Legato x) -> adjustDuration x
        Art (Slurred x) -> applySlurred x
        Art _ -> pfd
        Orn _ -> pfd
