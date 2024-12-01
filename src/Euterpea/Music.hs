{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}

module Euterpea.Music where

import           Data.Bifunctor  (first)
import           Data.List       (uncons)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           GHC.Generics    (Generic)


infixr 5 :+:, :=:

type AbsPitch = Int

type Octave = Int

type Pitch = (PitchClass, Octave)

type Dur = Rational

{- ORMOLU_DISABLE -}
data PitchClass = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds | Ef | Fff | Dss
                 | E | Ff | Es | F | Gff | Ess | Fs | Gf | Fss | G | Aff | Gs | Af | Gss
                 | A | Bff | As | Bf | Ass | B | Bs | Bss
  deriving (Show, Eq, Ord, Read, Bounded)
{- ORMOLU_ENABLE -}

data Primitive a where
  Note :: Dur -> a -> Primitive a
  Rest :: Dur -> Primitive a
  deriving (Show, Eq, Ord, Functor)

data Music a where
  Prim :: (Primitive a) -> Music a
  (:+:) :: Music a -> Music a -> Music a
  (:=:) :: Music a -> Music a -> Music a
  Modify :: Control -> (Music a) -> Music a
  deriving (Show, Eq)

instance Functor Music where
  fmap :: (a -> b) -> Music a -> Music b
  fmap func (Prim p)     = Prim (fmap func p) --  uses Primitive's Functor instance
  fmap func (m1 :+: m2)  = fmap func m1 :+: fmap func m2
  fmap func (m1 :=: m2)  = fmap func m1 :=: fmap func m2
  fmap func (Modify x m) = Modify x (fmap func m)

data Control where
  Tempo :: Rational -> Control
  Transpose :: AbsPitch -> Control
  Instrument :: InstrumentName -> Control
  Phrase :: [PhraseAttribute] -> Control
  KeySig :: PitchClass -> Mode -> Control
  Custom :: String -> Control
  deriving (Show, Eq)

data Mode
  = Ionian
  | Dorian
  | Major
  | Minor
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  | CustomMode !String
  deriving (Show, Eq, Ord, Read)


data PhraseAttribute
  = Dyn Dynamic
  | Tmp Tempo
  | Art Articulation
  | Orn Ornament
  deriving (Show, Eq, Ord)

data Dynamic where
  Accent :: Rational -> Dynamic
  Crescendo :: Rational -> Dynamic
  Diminuendo :: Rational -> Dynamic
  StdLoudness :: StdLoudness -> Dynamic
  Loudness :: Rational -> Dynamic
  deriving (Show, Eq, Ord)

data StdLoudness = PPP | PP | P | MP | SF | MF | NF | FF | FFF
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

data Tempo = Ritardando Rational | Accelerando Rational
  deriving (Show, Eq, Ord)

data Articulation
  = Staccato Rational
  | Legato Rational
  | Slurred Rational
  | Tenuto
  | Marcato
  | Pedal
  | Fermata
  | FermataDown
  | Breath
  | DownBow
  | UpBow
  | Harmonic
  | Pizzicato
  | LeftPizz
  | BartokPizz
  | Swell
  | Wedge
  | Thumb
  | Stopped
  deriving (Show, Eq, Ord)

data Ornament
  = Trill
  | Mordent
  | InvMordent
  | DoubleMordent
  | Turn
  | TrilledTurn
  | ShortTrill
  | Arpeggio
  | ArpeggioUp
  | ArpeggioDown
  | Instruction String
  | Head NoteHead
  | DiatonicTrans Int
  deriving (Show, Eq, Ord)

data NoteHead
  = DiamondHead
  | SquareHead
  | XHead
  | TriangleHead
  | TremoloHead
  | SlashHead
  | ArtHarmonic
  | NoHead
  deriving (Show, Eq, Ord)

type Volume = Int

-- addVolume    :: Volume -> Music Pitch -> Music (Pitch,Volume)
-- addVolume v  = mMap (\p -> (p,v))

addVolume :: Volume -> Music Pitch -> Music (Pitch, Volume)
addVolume v = fmap (,v)

data NoteAttribute
  = Volume {-# UNPACK #-} !Int
  | Fingering {-# UNPACK #-} !Int
  | Dynamics !String
  | Params ![Double]
  deriving (Eq, Show, Generic)

type Note1 = (Pitch, [NoteAttribute])

type Music1 = Music Note1

class ToMusic1 a where
  toMusic1 :: Music a -> Music1

instance ToMusic1 Pitch where
  toMusic1 :: Music Pitch -> Music1
  toMusic1 = fmap (,[])

instance ToMusic1 (Pitch, Volume) where
  toMusic1 :: Music (Pitch, Volume) -> Music1
  toMusic1 = fmap (\(p, v) -> (p, [Volume v]))

instance ToMusic1 Note1 where
  toMusic1 :: Music Note1 -> Music1
  toMusic1 = id

instance ToMusic1 AbsPitch where
  toMusic1 :: Music AbsPitch -> Music1
  toMusic1 = fmap (\x -> (pitch x, []))

instance ToMusic1 (AbsPitch, Volume) where
  toMusic1 :: Music (AbsPitch, Volume) -> Music1
  toMusic1 = fmap (\(p, v) -> (pitch p, [Volume v]))

note :: Dur -> a -> Music a
note this_dur p = Prim (Note this_dur p)

rest :: Dur -> Music a
rest this_dur = Prim (Rest this_dur)

tempo :: Dur -> Music a -> Music a
tempo r = Modify (Tempo r)

transpose :: AbsPitch -> Music a -> Music a
transpose i = Modify (Transpose i)

instrument :: InstrumentName -> Music a -> Music a
instrument i = Modify (Instrument i)

phrase :: [PhraseAttribute] -> Music a -> Music a
phrase pa = Modify (Phrase pa)

keysig :: PitchClass -> Mode -> Music a -> Music a
keysig pc mo = Modify (KeySig pc mo)

{- ORMOLU_DISABLE -}
cff, cf, c, cs, css, dff, df, d, ds, dss, eff, ef, e, es, ess, fff, ff, f, fs, fss,
  gff, gf, g, gs, gss, aff, af, a, as, ass, bff, bf, b, bs, bss :: Octave -> Dur -> Music Pitch

cff o this_dur = note this_dur (Cff, o)
cf o this_dur  = note this_dur (Cf, o)
c o this_dur   = note this_dur (C, o)
cs o this_dur  = note this_dur (Cs, o)
css o this_dur = note this_dur (Css, o)
dff o this_dur = note this_dur (Dff, o)
df o this_dur  = note this_dur (Df, o)
d o this_dur   = note this_dur (D, o)
ds o this_dur  = note this_dur (Ds, o)
dss o this_dur = note this_dur (Dss, o)
eff o this_dur = note this_dur (Eff, o)
ef o this_dur  = note this_dur (Ef, o)
e o this_dur   = note this_dur (E, o)
es o this_dur  = note this_dur (Es, o)
ess o this_dur = note this_dur (Ess, o)
fff o this_dur = note this_dur (Fff, o)
ff o this_dur  = note this_dur (Ff, o)
f o this_dur   = note this_dur (F, o)
fs o this_dur  = note this_dur (Fs, o)
fss o this_dur = note this_dur (Fss, o)
gff o this_dur = note this_dur (Gff, o)
gf o this_dur  = note this_dur (Gf, o)
g o this_dur   = note this_dur (G, o)
gs o this_dur  = note this_dur (Gs, o)
gss o this_dur = note this_dur (Gss, o)
aff o this_dur = note this_dur (Aff, o)
af o this_dur  = note this_dur (Af, o)
a o this_dur   = note this_dur (A, o)
as o this_dur  = note this_dur (As, o)
ass o this_dur = note this_dur (Ass, o)
bff o this_dur = note this_dur (Bff, o)
bf o this_dur  = note this_dur (Bf, o)
b o this_dur   = note this_dur (B, o)
bs o this_dur  = note this_dur (Bs, o)
bss o this_dur = note this_dur (Bss, o)


bn, wn, hn, qn, en, sn, tn, sfn, dwn, dhn, dqn, den, dsn, dtn, ddhn, ddqn, dden :: Dur
bnr, wnr, hnr, qnr, enr, snr, tnr, sfnr, dwnr, dhnr, dqnr, denr, dsnr, dtnr, ddhnr, ddqnr, ddenr :: Music Pitch

bn = 2
bnr = rest bn   -- brevis rest

wn = 1
wnr = rest wn   -- whole note rest

hn = 1 / 2
hnr = rest hn   -- half note rest

qn = 1 / 4
qnr = rest qn   -- quarter note rest

en = 1 / 8
enr = rest en   -- eighth note rest

sn = 1 / 16
snr = rest sn   -- sixteenth note rest

tn = 1 / 32
tnr = rest tn   -- thirty-second note rest

sfn = 1 / 64
sfnr = rest sfn -- sixty-fourth note rest

dwn = 3 / 2
dwnr = rest dwn -- dotted whole note rest

dhn = 3 / 4
dhnr = rest dhn -- dotted half note rest

dqn = 3 / 8
dqnr = rest dqn -- dotted quarter note rest

den = 3 / 16
denr = rest den -- dotted eighth note rest

dsn = 3 / 32
dsnr = rest dsn -- dotted sixteenth note rest

dtn = 3 / 64
dtnr = rest dtn -- dotted thirty-second note rest

ddhn = 7 / 8
ddhnr = rest ddhn -- double-dotted half note rest

ddqn = 7 / 16
ddqnr = rest ddqn -- double-dotted quarter note rest

dden = 7 / 32
ddenr = rest dden -- double-dotted eighth note rest
{- ORMOLU_ENABLE -}

absPitch :: Pitch -> AbsPitch
absPitch (pc, oct) = 12 * (oct + 1) + pcToInt pc

pcToInt :: PitchClass -> Int
pcToInt pc = case pc of
  Cff -> -2
  Cf  -> -1
  C   -> 0
  Cs  -> 1
  Css -> 2
  Dff -> 0
  Df  -> 1
  D   -> 2
  Ds  -> 3
  Dss -> 4
  Eff -> 2
  Ef  -> 3
  E   -> 4
  Es  -> 5
  Ess -> 6
  Fff -> 3
  Ff  -> 4
  F   -> 5
  Fs  -> 6
  Fss -> 7
  Gff -> 5
  Gf  -> 6
  G   -> 7
  Gs  -> 8
  Gss -> 9
  Aff -> 7
  Af  -> 8
  A   -> 9
  As  -> 10
  Ass -> 11
  Bff -> 9
  Bf  -> 10
  B   -> 11
  Bs  -> 12
  Bss -> 13

pitch :: AbsPitch -> Pitch
pitch ap =
  let (oct, n) = divMod ap 12
   in ([C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B] !! n, oct - 1)

trans :: Int -> Pitch -> Pitch
trans i p = pitch (absPitch p + i)


-- [[Note]] Applying foldr to infinite structures usually doesn't terminate

line, chord :: [Music a] -> Music a
line = foldr (:+:) (rest 0)
chord = foldr (:=:) (rest 0)

line1 :: [Music a] -> Music a
line1 = foldr (:+:) (rest 0)

chord1 :: [Music a] -> Music a
chord1 = foldr (:=:) (rest 0)


offset :: Dur -> Music a -> Music a
offset this_dur m = rest this_dur :+: m

times :: Int -> Music a -> Music a
times 0 _ = rest 0
times n m = m :+: times (n - 1) m

forever :: Music a -> Music a
forever m = m :+: forever m

lineToList :: Music a -> [Music a]
lineToList (Prim (Rest 0)) = []
lineToList (n :+: ns) = n : lineToList ns
lineToList _ =
  error "lineToList: argument not created by function line"

invertAt :: Pitch -> Music Pitch -> Music Pitch
invertAt pRef = fmap (\p -> pitch (2 * absPitch pRef - absPitch p))

invertAt1 :: Pitch -> Music (Pitch, a) -> Music (Pitch, a)
invertAt1 pRef = fmap (\(p, x) -> (pitch (2 * absPitch pRef - absPitch p), x))

invert :: Music Pitch -> Music Pitch
invert m =
  let pRef = mFold pFun (++) (++) (\_ x -> x) m
   in case uncons pRef of
        Nothing     -> m -- no pitches in the structure!
        Just (p, _) -> invertAt p m
  where
    pFun (Note _ p) = [p]
    pFun _          = []

invert1 :: Music (Pitch, a) -> Music (Pitch, a)
invert1 m =
  let pRef = mFold pFun (++) (++) (\_ x -> x) m
   in case uncons pRef of
        Nothing     -> m -- no pitches!
        Just (p, _) -> invertAt1 p m
  where
    pFun (Note _ (p, _)) = [p]
    pFun _               = []

retro :: Music a -> Music a
retro n@(Prim _) = n
retro (Modify c m) = Modify c (retro m)
retro (m1 :+: m2) = retro m2 :+: retro m1
retro (m1 :=: m2) =
  let d1 = dur m1
      d2 = dur m2
   in if d1 > d2
        then retro m1 :=: (rest (d1 - d2) :+: retro m2)
        else (rest (d2 - d1) :+: retro m1) :=: retro m2

retroInvert, invertRetro :: Music Pitch -> Music Pitch
retroInvert = retro . invert
invertRetro = invert . retro

dur :: Music a -> Dur
dur music = case music of
  Prim p             -> primDur p
  m1 :+: m2          -> dur m1 + dur m2
  m1 :=: m2          -> dur m1 `max` dur m2
  Modify (Tempo r) m -> dur m / r
  Modify _ m         -> dur m
  where
    primDur :: Primitive a -> Dur
    primDur (Note this_dur _) = this_dur
    primDur (Rest this_dur)   = this_dur

-- | The 'cut' function trims a 'Music' structure to a specified duration 'd'.
-- It effectively limits the music piece's playtime to the provided duration.
--
-- This function processes different musical constructs:
-- - Simple notes and rests are truncated to not exceed 'd'
-- - Parallel compositions have both parts cut to 'd'
-- - Sequential compositions are cut with remaining duration passed to subsequent parts
-- - Tempo modifications adjust 'd' proportionally
--
-- Special Cases:
-- - If 'd' <= 0, returns a rest with zero duration
-- - Notes/rests beyond the cut point get zero duration
-- - Preserves structure but may result in zero-duration elements
--
-- Examples:
-- >>> cut 2 (note 1 (C,4) :+: note 2 (D,4))
-- Prim (Note (1 % 1) (C,4)) :+: Prim (Note (1 % 1) (D,4))
-- WAS WAS WAS Note: second note duration reduced from 2 to 1
--
-- >>> cut 1 (note 2 (C,4) :+: note 1 (D,4))
-- Prim (Note (1 % 1) (C,4)) :+: Prim (Rest (0 % 1))
-- WAS WAS WAS Note: first note truncated, second note replaced with zero rest
cut :: Dur -> Music a -> Music a
cut this_dur _ | this_dur <= 0 = rest 0
cut this_dur (Prim (Note oldD p)) =
  let d' = max (min oldD this_dur) 0
   in if d' > 0 then note d' p else rest 0
cut this_dur (Prim (Rest oldD)) = rest (max (min oldD this_dur) 0)
cut this_dur (m1 :=: m2) = cut this_dur m1 :=: cut this_dur m2
cut this_dur (m1 :+: m2) =
  let m'1 = cut this_dur m1
      m'2 = cut (this_dur - dur m'1) m2
   in m'1 :+: m'2
cut this_dur (Modify (Tempo r) m) = tempo r (cut (this_dur * r) m)
cut this_dur (Modify cu m) = Modify cu (cut this_dur m)

-- | The 'remove' function subtracts a given duration 'd' from a Music structure.
-- Notes and rests at the start are shortened by 'd', potentially being eliminated
-- if their duration is less than or equal to 'd'.
--
-- This function processes:
-- - Notes and rests are shortened by 'd'
-- - Parallel compositions have 'd' removed from both parts
-- - Sequential compositions subtract 'd' progressively
-- - Tempo modifications scale 'd' appropriately
--
-- Special Cases:
-- - If 'd' <= 0, music remains unchanged
-- - Elements completely consumed by 'd' become zero duration
-- - Maintains structure while adjusting durations
--
-- Examples:
-- >>> remove 0.5 (note 2 (C,4) :+: note 1 (D,4))
-- Prim (Note (3 % 2) (C,4)) :+: Prim (Note (1 % 1) (D,4))
-- WAS WAS Note: first note shortened by 0.5, second unchanged
--
-- >>> remove 1 (note 2 (C,4) :+: note 1 (D,4))
-- Prim (Note (1 % 1) (C,4)) :+: Prim (Note (1 % 1) (D,4))
-- WAS WAS Note: first note shortened by 1, second unchanged
remove :: Dur -> Music a -> Music a
remove this_dur m | this_dur <= 0 = m
remove this_dur (Prim (Note oldD p)) =
  let d' = max (oldD - this_dur) 0
   in if d' > 0 then note d' p else rest 0
remove this_dur (Prim (Rest oldD)) = rest (max (oldD - this_dur) 0)
remove this_dur (m1 :=: m2) = remove this_dur m1 :=: remove this_dur m2
remove this_dur (m1 :+: m2) =
  let m'1 = remove this_dur m1
      m'2 = remove (this_dur - dur m1) m2
   in m'1 :+: m'2
remove this_dur (Modify (Tempo r) m) = tempo r (remove (this_dur * r) m)
remove this_dur (Modify c m) = Modify c (remove this_dur m)

-- | The 'removeZeros' function simplifies a 'Music' structure by eliminating
--  zero-duration notes and rests. It retains the same musical structure but
--  without elements that contribute nothing to the duration of the piece.
--
--  This operation can optimize musical pieces by omitting unnecessary components,
--  leading to potentially more efficient processing or simpler music structures.
--
--  Parameters:
--  - m: The 'Music' object from which zero-duration notes and rests will be removed.
--
--  Returns:
--  A new 'Music' object that excludes all zero-duration elements while preserving
--  the original musical relationships and hierarchy.
-- >>> removeZeros (note 0 (C,4) :+: note 1 (D,4))
-- Prim (Note (1 % 1) (D,4))
removeZeros :: Music a -> Music a
removeZeros = \case
  Prim p -> Prim p
  m1 :+: m2 -> combineMusicWithZeros (:+:) m1 m2
  m1 :=: m2 -> combineMusicWithZeros (:=:) m1 m2
  Modify c_ m -> Modify c_ (removeZeros m)
  where
    combineMusicWithZeros ::
      (Music a -> Music a -> Music a) ->
      Music a ->
      Music a ->
      Music a
    combineMusicWithZeros op m1 m2 =
      case (removeZeros m1, removeZeros m2) of
        (Prim (Note 0 _), m) -> m
        (Prim (Rest 0), m)   -> m
        (m, Prim (Note 0 _)) -> m
        (m, Prim (Rest 0))   -> m
        (m1', m2')           -> m1' `op` m2'

-- | The 'durL' function calculates the lazy list of durations for each segment
-- within a 'Music' composition. It provides a timeline view, allowing us to
-- determine cumulative durations efficiently.
--
-- For parallel compositions, this function merges individual duration lists,
-- recognizing that they play simultaneously. Sequential compositions concatenate
-- the durations, incrementing them as segments are processed in sequence.
--
-- Examples:
-- >>> durL (note 1 (C,4) :+: note 2 (D,4))
-- NOW [1 % 1,3 % 1]
-- >>> durL (note qn (C,4))
-- NOW [1 % 4]
-- >>>  durL (note qn (C,4) :+: note hn (D,4))
-- [1 % 4,3 % 4]
type LazyDur = [Dur]

durL :: Music a -> LazyDur
durL = \case
  -- Single primitive: just its duration
  Prim p -> [dur (Prim p)]
  -- Sequential composition: accumulate durations
  m1 :+: m2 ->
    let d1 = durL m1
        lastD = last d1
     in d1 <> fmap (+ lastD) (durL m2)
  -- Parallel composition: merge duration lists
  m1 :=: m2 -> mergeLD (durL m1) (durL m2)
  -- Tempo modification: scale all durations
  Modify (Tempo r) m -> fmap (/ r) (durL m)
  -- Other modifications: pass through
  Modify _ m -> durL m

-- | Merges two duration lists for parallel composition, preserving
-- all timing points from both sequences. The result contains all
-- durations from both lists in order, maintaining the timeline of
-- both parts playing simultaneously.
--
-- Parameters:
-- - List 1: First duration list (LazyDur)
-- - List 2: Second duration list (LazyDur)
--
-- Returns:
-- A merged LazyDur containing all timing points from both lists
-- in chronological order.
--
-- Examples:
-- >>> mergeLD [1/4, 1/2] [1/4, 3/4]
-- WAS WAS [1 % 4, 1 % 4, 1 % 2, 3 % 4]
-- WAS NOW [1 % 4,1 % 4,1 % 2,3 % 4]
-- NOW [1 % 4,1 % 4,1 % 2,3 % 4]
-- >>> mergeLD [1/2] [1/4, 1/2]
-- WAS WAS [1 % 4, 1 % 2, 1 % 2]
-- WAS NOW [1 % 4,1 % 2,1 % 2]
-- NOW [1 % 4,1 % 2,1 % 2]
--
-- Note: Duplicate timing points are preserved to maintain the structure
-- of both original timelines.
mergeLD :: LazyDur -> LazyDur -> LazyDur
mergeLD [] ld = ld -- If first list empty, use second
mergeLD ld [] = ld -- If second list empty, use first
mergeLD ld1@(d1 : ds1) ld2@(d2 : ds2) =
  if d1 < d2
    then d1 : mergeLD ds1 ld2 -- Take smaller duration and continue
    else d2 : mergeLD ld1 ds2 -- Preserves all timing points

-- | The 'minL' function computes the minimum duration from a list of durations
--  ('LazyDur') comparing each to a given reference duration ('Dur').
--
--  This function serves to determine the smallest duration that should be used
--  when modifying or generating new music constructs.
--
minL :: LazyDur -> Dur -> Dur
minL [] this_dur'       = this_dur'
minL [this_dur] this_dur'      = min this_dur this_dur'
minL (this_dur : this_durs) this_dur' = if this_dur < this_dur' then minL this_durs this_dur' else this_dur'

-- | Cut a piece of music according to a list of durations
-- This function adjusts the duration of each segment of music
-- based on the provided timeline
-- >>> cutL [1 / 1, 1 / 1] (note 1 (C,4) :+: note 1 (D,4))
-- Prim (Note (1 % 1) (C,4)) :+: Prim (Rest (0 % 1))
-- >>> cutL [2 / 1, 1 / 1] (note 1 (C,4) :+: note 2 (D,4))
-- WAS WAS NOW Prim (Note (1 % 1) (C,4)) :+: Prim (Note (0 % 1) (D,4))
-- WAS NOW Prim (Note (1 % 1) (C,4)) :+: Prim (Note (0 % 1) (D,4))
-- NOW Prim (Note (1 % 1) (C,4)) :+: Prim (Note (0 % 1) (D,4))
cutL :: LazyDur -> Music a -> Music a
cutL [] _ = rest 0
cutL (d : ds) m | d <= 0 = cutL ds m
cutL ld m = case m of
  -- Base cases: Notes and Rests
  Prim (Note oldD p) -> note (minL ld oldD) p
  Prim (Rest oldD) -> rest (minL ld oldD)
  -- Parallel composition: cut both parts to same timeline
  m1 :=: m2 -> cutL ld m1 :=: cutL ld m2
  -- Sequential composition: adjust timeline for second part
  m1 :+: m2 ->
    let m'1 = cutL ld m1
        remainingDur = fmap (subtractDur (dur m'1)) ld
        m'2 = cutL remainingDur m2
     in m'1 :+: m'2
  -- Handle tempo modifications
  Modify (Tempo r) m -> tempo r (cutL (fmap (* r) ld) m)
  Modify c m -> Modify c (cutL ld m)
  where
    subtractDur :: Dur -> Dur -> Dur
    subtractDur d1 d2 = d2 - d1

-- | Combine two pieces of music in parallel, cutting each to match
-- the duration of the other part
(/=:) :: Music a -> Music a -> Music a
m1 /=: m2 = cutL (durL m2) m1 :=: cutL (durL m1) m2

-- cutL :: LazyDur -> Music a -> Music a
-- cutL [] m                     = rest 0
-- cutL (d:ds) m | d <= 0        = cutL ds m
-- cutL ld (Prim (Note oldD p))  = note (minL ld oldD) p
-- cutL ld (Prim (Rest oldD))    = rest (minL ld oldD)
-- cutL ld (m1 :=: m2)           = cutL ld m1 :=: cutL ld m2
-- cutL ld (m1 :+: m2)           =
--    let  m'1 = cutL ld m1
--         m'2 = cutL (map (\d -> d - dur m'1) ld) m2
--    in m'1 :+: m'2
-- cutL ld (Modify (Tempo r) m)  = tempo r (cutL (map (*r) ld) m)
-- cutL ld (Modify c m)          = Modify c (cutL ld m)

-- (/=:)      :: Music a -> Music a -> Music a
-- m1 /=: m2  = cutL (durL m2) m1 :=: cutL (durL m1) m2

pMap :: (a -> b) -> Primitive a -> Primitive b
pMap func (Note this_dur x) = Note this_dur (func x)
pMap _ (Rest this_dur)   = Rest this_dur

mMap :: (a -> b) -> Music a -> Music b
mMap func (Prim p)     = Prim (pMap func p)
mMap func (m1 :+: m2)  = fmap func m1 :+: fmap func m2
mMap func (m1 :=: m2)  = fmap func m1 :=: fmap func m2
mMap func (Modify c m) = Modify c (fmap func m)

-- instance Functor Primitive where
--     fmap = pMap

-- instance Functor Music where
--     fmap = mMap

-- | Fold over a Music structure, allowing transformation of the entire music tree
-- into a single value by providing functions to handle each constructor.
--
-- Parameters:
-- - f: handles Primitive values
-- - (+:): combines sequential compositions
-- - (=:): combines parallel compositions
-- - g: handles modifications
mFold ::
  (Primitive a -> b) ->
  (b -> b -> b) ->
  (b -> b -> b) ->
  (Control -> b -> b) ->
  Music a ->
  b
mFold f (+:) (=:) g m =
  let rec = mFold f (+:) (=:) g
   in case m of
        Prim p     -> f p
        m1 :+: m2  -> rec m1 +: rec m2
        m1 :=: m2  -> rec m1 =: rec m2
        Modify c m -> g c (rec m)

-- Calculate total duration
dur' :: Music a -> Dur
dur' = mFold primDur (+) max tempoMod
  where
    primDur (Note d _) = d
    primDur (Rest d)   = d
    tempoMod (Tempo r) d = d / r
    tempoMod _ d         = d

-- Count all notes
countNotes :: Music a -> Int
countNotes = mFold f (+) (+) (\_ n -> n)
  where
    f (Note _ _) = 1
    f (Rest _)   = 0

-- Collect all pitches
getPitches :: Music Pitch -> [Pitch]
getPitches = mFold f (++) (++) (\_ ps -> ps)
  where
    f (Note _ p) = [p]
    f (Rest _)   = []

-- Convert to simple string representation
showMusic :: (Show a) => Music a -> String
showMusic = mFold f (+++) (|||) g
  where
    f (Note d p) = "Note " <> show d <> " " <> show p
    f (Rest d)   = "Rest " <> show d
    (+++) s1 s2 = s1 <> " :+: " <> s2
    (|||) s1 s2 = s1 <> " :=: " <> s2
    g c s = "Modify " <> show c <> " (" <> s <> ")"

shiftPitches :: AbsPitch -> Music Pitch -> Music Pitch
shiftPitches k = fmap (trans k)

-- shiftPitches1 :: AbsPitch -> Music (Pitch, b) -> Music (Pitch, b)
-- shiftPitches1 k = mMap (\(p,xs) -> (trans k p, xs))

shiftPitches1 :: AbsPitch -> Music (Pitch, b) -> Music (Pitch, b)
shiftPitches1 k = fmap (first (trans k))

scaleDurations :: Rational -> Music a -> Music a
scaleDurations r (Prim (Note this_dur p)) = note (this_dur / r) p
scaleDurations r (Prim (Rest this_dur)) = rest (this_dur / r)
scaleDurations r (m1 :+: m2) = scaleDurations r m1 :+: scaleDurations r m2
scaleDurations r (m1 :=: m2) = scaleDurations r m1 :=: scaleDurations r m2
scaleDurations r (Modify c m) = Modify c (scaleDurations r m)

changeInstrument :: InstrumentName -> Music a -> Music a
changeInstrument i m = Modify (Instrument i) $ removeInstruments m

removeInstruments :: Music a -> Music a
removeInstruments (Modify (Instrument i) m) = removeInstruments m
removeInstruments (Modify c m) = Modify c $ removeInstruments m
removeInstruments (m1 :+: m2) = removeInstruments m1 :+: removeInstruments m2
removeInstruments (m1 :=: m2) = removeInstruments m1 :=: removeInstruments m2
removeInstruments m = m

-- | Map over the primitive values in a Music structure while preserving the overall structure.
--
-- Parameters:
-- - f: Function to transform primitive values
--
-- Examples:
-- >>> .mapM (\(Note d p) -> Note d (p + 1)) (note 1 (C,4))
-- WAS Prim (Note (1 % 1) (D,4))
-- NOW parse error on input `.'
mapM :: (Primitive a -> Primitive b) -> Music a -> Music b
mapM func = mFold (Prim . func) (:+:) (:=:) Modify

-- | Bottom-up fold over a Music structure, accumulating a value while traversing
-- from leaves to root, similar to foldl for lists.
--
-- Parameters:
-- - f: Function to process primitive values with accumulator
-- - (+:): Function to combine sequential compositions
-- - (=:): Function to combine parallel compositions
-- - g: Function to handle modifications
-- - z: Initial accumulator value
--
-- Examples:
-- >>> mFoldl (\acc p -> case p of Note _ _ -> acc + 1; Rest _ -> acc) (+) (+) const 0 (note 1 (C,4) :+: note 1 (D,4))
-- WAS WAS 2
-- WAS NOW Variable not in scope: c4
-- WAS NOW Variable not in scope: d4
-- NOW 2
--
-- Examples:
-- >>> mFoldl (\acc (Note _ _) -> acc + 1) (+) (+) (\_ n -> n) 0 (note 1 C :+: note 1 D)
-- No instance for `Num Control' arising from a use of `+'
-- In the expression: acc_aP91V + 1
-- In the first argument of `mFoldl', namely
--  `(\ acc_aP91V (Note _ _) -> acc_aP91V + 1)'
-- In the expression:
--  mFoldl
--    (\ acc_aP91V (Note _ _) -> acc_aP91V + 1) (+) (+)
--    (\ _ n_aP91W -> n_aP91W) 0 (note 1 C :+: note 1 D)
mFoldl ::
  (b -> Primitive a -> b) ->
  (b -> b -> b) ->
  (b -> b -> b) ->
  (b -> Control -> b) ->
  b ->
  Music a ->
  b
mFoldl func (+:) (=:) g z m = case m of
  Prim p -> func z p
  m1 :+: m2 ->
    let z' = mFoldl func (+:) (=:) g z m1
     in mFoldl func (+:) (=:) g z' m2
  m1 :=: m2 ->
    let z1 = mFoldl func (+:) (=:) g z m1
        z2 = mFoldl func (+:) (=:) g z m2
     in z1 =: z2
  Modify cu m' -> g (mFoldl func (+:) (=:) g z m') cu

-- | Traverse a Music structure with effects while maintaining the structure.
-- Useful for operations that might fail or have side effects.
--
-- Parameters:
-- - f: Function to process primitives with effects
--
-- Examples:
-- >>> let checkDuration (Note d p) = if d > 0 then Just (Note d p) else Nothing; checkDuration r@(Rest _) = Just r
-- >>> mTraverse checkDuration (note 1 (C,4) :+: note (-1) (D,4))
-- Nothing
mTraverse :: (Applicative f) => (Primitive a -> f (Primitive b)) -> Music a -> f (Music b)
mTraverse func m = case m of
  Prim p       -> Prim <$> func p
  m1 :+: m2    -> (:+:) <$> mTraverse func m1 <*> mTraverse func m2
  m1 :=: m2    -> (:=:) <$> mTraverse func m1 <*> mTraverse func m2
  Modify cu m' -> Modify cu <$> mTraverse func m'

-- | Monadic fold
mFoldM ::
  (Monad m) =>
  (Primitive a -> m b) ->
  (b -> b -> m b) ->
  (b -> b -> m b) ->
  (Control -> b -> m b) ->
  Music a ->
  m b
mFoldM f (+:) (=:) g m = case m of
  Prim p -> f p
  m1 :+: m2 -> do
    x1 <- mFoldM f (+:) (=:) g m1
    x2 <- mFoldM f (+:) (=:) g m2
    x1 +: x2
  m1 :=: m2 -> do
    x1 <- mFoldM f (+:) (=:) g m1
    x2 <- mFoldM f (+:) (=:) g m2
    x1 =: x2
  Modify c m' -> do
    x <- mFoldM f (+:) (=:) g m'
    g c x

-- | Accumulating fold (with state)
mFoldS ::
  (s -> Primitive a -> (b, s)) ->
  (b -> b -> b) ->
  (b -> b -> b) ->
  (Control -> b -> b) ->
  s ->
  Music a ->
  (b, s)
mFoldS f (+:) (=:) g s m = case m of
  Prim p -> f s p
  m1 :+: m2 ->
    let (x1, s1) = mFoldS f (+:) (=:) g s m1
        (x2, s2) = mFoldS f (+:) (=:) g s1 m2
     in (x1 +: x2, s2)
  m1 :=: m2 ->
    let (x1, s1) = mFoldS f (+:) (=:) g s m1
        (x2, s2) = mFoldS f (+:) (=:) g s1 m2
     in (x1 =: x2, s2)
  Modify c m' ->
    let (x, s') = mFoldS f (+:) (=:) g s m'
     in (g c x, s')

-- -- Using mapM to transpose all notes
-- transpose2 :: Int -> Music Pitch -> Music Pitch
-- transpose2 n = mapM f
--   where f (Note d p) = Note d (p + n)
--         f (Rest d) = Rest d

-- Using mFoldM for validation
-- validateDurations :: Music Pitch -> Either String Dur
-- validateDurations = mFoldM f (+) max g
--   where
--     f (Note d _) | d < 0 = Left "Negative duration"
--                  | otherwise = Right d
--     f (Rest d) | d < 0 = Left "Negative duration"
--                | otherwise = Right d
--     g (Tempo r) d | r <= 0 = Left "Invalid tempo"
--                   | otherwise = Right (d/r)
--     g _ d = Right d

-- Using mFoldS to number all notes
-- >>> numberNotes (note qn (C,4) :+: note qn (D,4))
-- (Prim (Note (1 % 4) (1,(C,4))) :+: Prim (Note (1 % 4) (2,(D,4))),3)
numberNotes :: Music Pitch -> (Music (Int, Pitch), Int)
numberNotes = mFoldS func (:+:) (:=:) Modify 1
  where
    func n (Note durac p) = (Prim (Note durac (n, p)), n + 1)
    func n (Rest durac)   = (Prim (Rest durac), n)

data InstrumentName
  = AcousticGrandPiano
  | BrightAcousticPiano
  | ElectricGrandPiano
  | HonkyTonkPiano
  | RhodesPiano
  | ChorusedPiano
  | Harpsichord
  | Clavinet
  | Celesta
  | Glockenspiel
  | MusicBox
  | Vibraphone
  | Marimba
  | Xylophone
  | TubularBells
  | Dulcimer
  | HammondOrgan
  | PercussiveOrgan
  | RockOrgan
  | ChurchOrgan
  | ReedOrgan
  | Accordion
  | Harmonica
  | TangoAccordion
  | AcousticGuitarNylon
  | AcousticGuitarSteel
  | ElectricGuitarJazz
  | ElectricGuitarClean
  | ElectricGuitarMuted
  | OverdrivenGuitar
  | DistortionGuitar
  | GuitarHarmonics
  | AcousticBass
  | ElectricBassFingered
  | ElectricBassPicked
  | FretlessBass
  | SlapBass1
  | SlapBass2
  | SynthBass1
  | SynthBass2
  | Violin
  | Viola
  | Cello
  | Contrabass
  | TremoloStrings
  | PizzicatoStrings
  | OrchestralHarp
  | Timpani
  | StringEnsemble1
  | StringEnsemble2
  | SynthStrings1
  | SynthStrings2
  | ChoirAahs
  | VoiceOohs
  | SynthVoice
  | OrchestraHit
  | Trumpet
  | Trombone
  | Tuba
  | MutedTrumpet
  | FrenchHorn
  | BrassSection
  | SynthBrass1
  | SynthBrass2
  | SopranoSax
  | AltoSax
  | TenorSax
  | BaritoneSax
  | Oboe
  | Bassoon
  | EnglishHorn
  | Clarinet
  | Piccolo
  | Flute
  | Recorder
  | PanFlute
  | BlownBottle
  | Shakuhachi
  | Whistle
  | Ocarina
  | Lead1Square
  | Lead2Sawtooth
  | Lead3Calliope
  | Lead4Chiff
  | Lead5Charang
  | Lead6Voice
  | Lead7Fifths
  | Lead8BassLead
  | Pad1NewAge
  | Pad2Warm
  | Pad3Polysynth
  | Pad4Choir
  | Pad5Bowed
  | Pad6Metallic
  | Pad7Halo
  | Pad8Sweep
  | FX1Train
  | FX2Soundtrack
  | FX3Crystal
  | FX4Atmosphere
  | FX5Brightness
  | FX6Goblins
  | FX7Echoes
  | FX8SciFi
  | Sitar
  | Banjo
  | Shamisen
  | Koto
  | Kalimba
  | Bagpipe
  | Fiddle
  | Shanai
  | TinkleBell
  | Agogo
  | SteelDrums
  | Woodblock
  | TaikoDrum
  | MelodicDrum
  | SynthDrum
  | ReverseCymbal
  | GuitarFretNoise
  | BreathNoise
  | Seashore
  | BirdTweet
  | TelephoneRing
  | Helicopter
  | Applause
  | Gunshot
  | Percussion
  | CustomInstrument String
  deriving (Show, Eq, Ord)

data PercussionSound
  = AcousticBassDrum --  MIDI Key 35
  | BassDrum1 --  MIDI Key 36
  | SideStick --  ...
  | AcousticSnare
  | HandClap
  | ElectricSnare
  | LowFloorTom
  | ClosedHiHat
  | HighFloorTom
  | PedalHiHat
  | LowTom
  | OpenHiHat
  | LowMidTom
  | HiMidTom
  | CrashCymbal1
  | HighTom
  | RideCymbal1
  | ChineseCymbal
  | RideBell
  | Tambourine
  | SplashCymbal
  | Cowbell
  | CrashCymbal2
  | VibraslapSpec
  | LowConga
  | HighTimbale
  | LowTimbale
  | HighAgogo
  | LowAgogo
  | Cabasa
  | Maracas
  | ShortWhistle
  | LongWhistle
  | ShortGuiro
  | LongGuiro
  | Claves
  | HiWoodBlock
  | LowWoodBlock
  | MuteCuica
  | OpenCuica
  | MuteTriangle
  | OpenTriangle --  MIDI Key 82
  deriving (Show, Eq, Ord, Enum)
