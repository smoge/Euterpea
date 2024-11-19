module Euterpea.IO.MIDI.FromMidi2 (fromMidi2, restructure, Chunk, chunkEvents, chunkToMusic) where

import Codec.Midi
import Data.List
import Euterpea.IO.MIDI.FromMidi
import Euterpea.IO.MIDI.GeneralMidi
import Euterpea.IO.MIDI.MEvent
import Euterpea.IO.MIDI.ToMidi
import Euterpea.Music hiding (E)

fromMidi2 :: Midi -> Music (Pitch, Volume)
fromMidi2 = restructure . fromMidi

restructure :: (ToMusic1 a) => Music a -> Music (Pitch, Volume)
restructure = parseFeaturesI

type Onset = Dur -- to clarify some type signatures

data Chunk = Par [Chunk] | Seq [Chunk] | Chord [Chunk] | E MEvent | R Onset Dur
  deriving (Eq)

initChunk :: [MEvent] -> [Chunk]
initChunk mevs =
  let mevs' = sortBy sortFun mevs
   in map E mevs'

chunkChord :: [Chunk] -> [Chunk]
chunkChord [] = []
chunkChord (c : cs) =
  let cChord = filter (chordWith c) cs
      notInChord = filter (\v -> not $ elem v cChord) cs
   in if null cChord
        then c : chunkChord cs
        else Chord (c : cChord) : chunkChord notInChord

chordWith :: Chunk -> Chunk -> Bool
chordWith c0 c = chunkOnset c == chunkOnset c0 && chunkDur c == chunkDur c0

chunkMel :: [Chunk] -> [Chunk]
chunkMel [] = []
chunkMel x@(c : cs) =
  let cMel = buildMelFrom (chunkOnset c) x -- get ALL possible melody elements
      notInMel = filter (`notElem` cMel) x
   in if null cMel
        then c : chunkMel cs
        else Seq cMel : chunkMel notInMel

buildMelFrom :: Onset -> [Chunk] -> [Chunk]
buildMelFrom t [] = []
buildMelFrom t (c : cs) =
  if chunkOnset c == t
    then c : buildMelFrom (t + chunkDur c) cs
    else buildMelFrom t cs

chunkSeqs :: [Chunk] -> [Chunk]
chunkSeqs [] = []
chunkSeqs x@(c : cs) =
  let s = seqWithRests (chunkOnset c) x
      notInS = filter (`notElem` s) x
   in if s == [c]
        then c : chunkSeqs cs
        else Seq s : chunkSeqs notInS

seqWithRests :: Onset -> [Chunk] -> [Chunk]
seqWithRests _ [] = []
seqWithRests t x@(c : cs) =
  let tc = chunkOnset c
      dt = tc - t
   in if dt == 0
        then c : seqWithRests (tc + chunkDur c) cs
        else
          if dt > 0
            then R t dt : c : seqWithRests (tc + chunkDur c) cs
            else seqWithRests t cs

chunkEvents :: [MEvent] -> Chunk
chunkEvents = Par . chunkSeqs . chunkMel . chunkChord . initChunk

chunkToMusic :: Chunk -> Music (Pitch, Volume)
chunkToMusic (E e) = note (eDur e / 2) (pitch $ ePitch e, eVol e)
chunkToMusic (R o d) = rest (d / 2)
chunkToMusic (Seq x) = line (map chunkToMusic x)
chunkToMusic (Chord x) = chord (map chunkToMusic x)
chunkToMusic (Par x) = chord $ map (\v -> rest (chunkOnset v / 2) :+: chunkToMusic v) x

parseFeatures :: (ToMusic1 a) => Music a -> Music (Pitch, Volume)
parseFeatures = removeZeros . chunkToMusic . chunkEvents . perform

parseFeaturesI :: (ToMusic1 a) => Music a -> Music (Pitch, Volume)
parseFeaturesI m =
  let mevs = perform m
      (iList, mevsI) = unzip $ splitByInst mevs
      parsesI = map (removeZeros . chunkToMusic . chunkEvents) mevsI
   in chord $ zipWith instrument iList parsesI

doubleShow :: Rational -> String
doubleShow x = show (fromRational x :: Double)

pcShow :: AbsPitch -> String
pcShow = show . fst . pitch

listShow, listShowN :: (Show a) => [a] -> String
listShow x = "[" ++ (concat $ intersperse ", " $ map show x) ++ "]"
listShowN x = "[\n    " ++ (concat $ intersperse ",\n    " $ map show x) ++ "\n]"

listShowX :: (Show a) => Int -> [a] -> String
listShowX i x =
  let v = concat (take i (repeat " "))
   in "[\n" ++ v ++ (concat $ intersperse (",\n" ++ v) $ map show x) ++ "\n" ++ v ++ "]"

instance Show Chunk where
  show (E e) = "E " ++ doubleShow (eTime e) ++ " " ++ pcShow (ePitch e) ++ " " ++ doubleShow (eDur e)
  show s@(Seq x) = "S " ++ doubleShow (chunkOnset s) ++ " " ++ listShowX 4 x
  show c@(Chord x) = "C " ++ doubleShow (chunkOnset c) ++ " " ++ listShowX 6 x
  show p@(Par x) = "P " ++ doubleShow (chunkOnset p) ++ " " ++ listShowX 2 x
  show (R o d) = "R " ++ doubleShow o ++ " " ++ doubleShow d

instance Ord Chunk where
  compare x1 x2 = compare (chunkOnset x1) (chunkOnset x2)

chunkOnset :: Chunk -> Onset
chunkOnset (Seq x) = if null x then error "Empty Seq!" else chunkOnset (head x)
chunkOnset (Chord x) = if null x then error "Empty Chord!" else chunkOnset (head x)
chunkOnset (Par x) = if null x then 0 else minimum $ map chunkOnset x
chunkOnset (E e) = eTime e
chunkOnset (R o d) = o

chunkEnd :: Chunk -> Onset
chunkEnd (Seq x) = if null x then error "Empty Seq!" else chunkEnd (last x)
chunkEnd (Chord x) = if null x then error "Empty Chord!" else chunkEnd (head x)
chunkEnd (Par x) = if null x then 0 else maximum $ map chunkEnd x
chunkEnd (E e) = eTime e + eDur e
chunkEnd (R o d) = o + d

chunkDur :: Chunk -> Dur
chunkDur (Seq x) = if null x then error "Empty Seq!" else sum $ map chunkDur x
chunkDur (Chord x) = if null x then error "Empty Chord!" else chunkDur (head x)
chunkDur c@(Par x) =
  if null x
    then 0
    else
      let o = chunkOnset c
          e = chunkEnd c
       in e - o
chunkDur (E e) = eDur e
chunkDur (R o d) = d

sortFun :: MEvent -> MEvent -> Ordering
sortFun e1 e2 =
  if eTime e1 == eTime e2
    then compare (ePitch e1) (ePitch e2)
    else compare (eTime e1) (eTime e2)
