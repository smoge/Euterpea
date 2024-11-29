{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Render a Music object to a audio signal function that can be further
-- manipulated or saved to a file.  It is channel-agnostic in that it is
-- able to deal with instruments of arbitrary number of channels.

module Euterpea.IO.Audio.Render (
    Instr,
    InstrMap,
    renderSF,
)
where

import Control.Arrow
import Control.Arrow.ArrowP
import Control.Arrow.Operations
import Control.SF.SF
import qualified Data.IntMap as M
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Euterpea.IO.Audio.Basics
import Euterpea.IO.Audio.Types
import Euterpea.IO.MIDI.MEvent
import Euterpea.Music

-- Every instrument is a function that takes a duration, absolute
-- pitch, volume, and a list of parameters (Doubles).  What the function
-- actually returns is implementation independent.
type Instr a = Dur -> AbsPitch -> Volume -> [Double] -> a

type InstrMap a = [(InstrumentName, Instr a)]

lookupInstr :: InstrumentName -> InstrMap a -> Instr a
lookupInstr ins im =
    case lookup ins im of
        Just i -> i
        Nothing ->
            error
                ( "Instrument "
                    <> ( show ins
                            <> " does not have a matching Instr in the supplied InstrMap."
                       )
                )

-- Each note in a Performance is tagged with a unique NoteId, which
-- helps us keep track of the signal function associated with a note.
type NoteId = Int

-- In this particular implementation, 'a' is the signal function that
-- plays the given note.
data NoteEvt a
    = NoteOn NoteId a
    | NoteOff NoteId

type Evt a = (Double, NoteEvt a) -- Timestamp in seconds, and the note event

-- | Turn an Event into a NoteOn and a matching NoteOff with the same NodeId.
eventToEvtPair :: InstrMap a -> MEvent -> Int -> [Evt a]
eventToEvtPair imap (MEvent{eTime, eInst, ePitch, eDur, eVol, eParams}) nid =
    let instr = lookupInstr eInst imap
        tOn = fromRational eTime
        tDur = fromRational eDur :: Double
        sf = instr eDur ePitch eVol eParams
     in [(tOn, NoteOn nid sf), (tOn + tDur, NoteOff nid)]

{- | Turn a Performance into an SF of NoteOn/NoteOffs.
For each note, generate a unique id to tag the NoteOn and NoteOffs.
The tag is used as the key to the collection of signal functions
for efficient insertion/removal.
-}
toEvtSF :: (Clock p) => [MEvent] -> InstrMap a -> Signal p () [Evt a]
toEvtSF pf imap =
    let evts =
            sortBy (comparing fst) $
                concat $
                    zipWith (eventToEvtPair imap) pf [0 ..]
     in -- Sort all NoteOn/NoteOff events by timestamp.
        proc _ -> do
            rec t <- integral -< 1
                es <- delay evts -< next
                let (evs, next) = span ((<= t) . fst) es
            -- Trim events that are due off the list and output them,
            -- retaining the rest
            outA -< evs

---

{- |
Modify the collection of signal functions upon receiving events.

This function processes a list of `Evt` (event type) and modifies
an input map of signal functions (`M.IntMap a`). The modifications are:
  - Insert new signal functions into the map when a `NoteOn` event is received.
  - Remove signal functions from the map when a `NoteOff` event is received.
  - In all other cases, leave the map unchanged for safety.

* The timestamps associated with these events are expected
  but not used within this function.
-}
modSF :: M.IntMap a -> [Evt a] -> M.IntMap a
modSF = foldl' modification
  where
    modification m (_, NoteOn nid sf) = M.insert nid sf m
    modification m (_, NoteOff nid) = M.delete nid m

{- |
Simplified version of a parallel switcher for signal functions.

This function manages the dynamic modification and execution
of a collection of signal functions (`SF`) based on incoming events.

Parameters:
  * An initial collection of signal functions (`col (Signal p () a)`).
  * A stream of input events that can modify the signal function collection.
  * A modifying function to apply changes to the collection based on events.

Notes:
  - This function requires the implementation of `runSF` specific to 
    the signal functions in use, as it executes all SFs in the provided collection.
  - The output is a collection of results produced by each signal function
    after processing the input stream.
-}
pSwitch ::
    forall p col a.
    (Functor col) =>
    col (Signal p () a) -> -- Initial SF collection.
    Signal p () [Evt (Signal p () a)] -> -- Input event stream.
    (col (Signal p () a) -> [Evt (Signal p () a)] -> col (Signal p () a)) ->
    -- Function to modify the collection of SFs based on current events.
    Signal p () (col a)
pSwitch col esig modification =
    proc _ -> do
        evts <- esig -< ()
        rec -- Consider reducing frequency using upsample if needed
            sfcol <- delay col -< modification sfcol' evts
            let rs = fmap (\s -> runSF (strip s) ()) sfcol :: col (a, SF () a)
                (as_, sfcol' :: col (Signal p () a)) = (fmap fst rs, fmap (ArrowP . snd) rs)
        outA -< as_

{- |
Render a sequence of musical notes into a signal function audio output.

Given a music piece and an instrument map, this function converts the music
into a signal function that outputs audio signals over its duration.

Parameters:
  * `Music a`: The music piece to be converted.
  * `InstrMap (Signal p () b)`: Mapping from instruments to their respective signal functions.

Returns:
  - Duration of the music piece in seconds.
  - A signal function that plays the music over time.
-}
renderSF ::
    (Clock p, ToMusic1 a, AudioSample b) =>
    Music a ->
    InstrMap (Signal p () b) ->
    (Double, Signal p () b)
renderSF m im =
    let (pf, this_dur) = perform1Dur $ toMusic1 m -- Updated 16-Dec-2015
        evtsf = toEvtSF pf im
        allsf = pSwitch M.empty evtsf modSF
        sf = allsf >>> arr (foldl' mix zero . M.elems) -- Mix all samples together
     in (fromRational this_dur, sf)
