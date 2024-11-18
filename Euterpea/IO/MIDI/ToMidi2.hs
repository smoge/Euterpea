module Euterpea.IO.MIDI.ToMidi2 (writeMidi2, resolveInstrumentName) where

import Codec.Midi
import Data.List
import Euterpea.IO.MIDI.ExportMidiFile
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MEvent
import Euterpea.IO.MIDI.ToMidi
import Euterpea.Music

instNameOnly :: String -> String
instNameOnly [] = []
instNameOnly (x : xs) = if x == ' ' then [] else x : instNameOnly xs

resolveInstrumentName :: InstrumentName -> InstrumentName
resolveInstrumentName x@(CustomInstrument s) =
  let iName = instNameOnly s
      allInsts = take 128 $ enumFrom AcousticGrandPiano
      i = maybe (-1) id $ findIndex (== iName) $ map show $ allInsts
   in if i >= 0 then allInsts !! i else x
resolveInstrumentName x = x

resolveMEventInsts :: [(InstrumentName, [MEvent])] -> [(InstrumentName, [MEvent])]
resolveMEventInsts = map f1
  where
    f1 (iname, mevs) = (resolveInstrumentName iname, map f2 mevs)
    f2 mev = mev {eInst = resolveInstrumentName (eInst mev)}

writeMidi2 :: (ToMusic1 a) => FilePath -> Music a -> IO ()
writeMidi2 fn m = exportMidiFile fn $ toMidiUPM2 defUpm $ perform m

toMidiUPM2 :: UserPatchMap -> [MEvent] -> Midi
toMidiUPM2 upm pf =
  let split = resolveMEventInsts $ splitByInst pf
      insts = map fst split
      rightMap =
        if allValid upm insts
          then upm
          else makeGMMap insts
   in Midi
        ( if length split == 1
            then SingleTrack
            else MultiTrack
        )
        (TicksPerBeat division)
        (map (fromAbsTime . mevsToMessages rightMap) split)
