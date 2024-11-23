{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Euterpea.MicroMusic
  ( -- * Types
    Accidental (..),
    NoteName (..),
    PitchClass (..),
    Octave (..),
    Pitch (..),
    Rule (..),
    OctaveChange (..),

    -- * Functions
    mkOctave,
    mkPitchClass,
    mkPitch,
    mkPitch',
    GetPitch,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List (minimumBy, uncons)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Ratio
import GHC.Generics (Generic)
)

-- | Represents a pitch alteration as a simple fraction of a semitone. The
-- fraction is stored in unreduced form since we only use fixed, small
-- denominators and never perform arithmetic between Rats.
data Rat = Rat
  { -- | The top number of the fraction
    numerator :: !Integer,
    -- | The bottom number of the fraction
    denominator :: !Integer
  }
  deriving (Eq, Show)

{- ORMOLU_DISABLE -}
-- | Represents musical accidentals with microtonal precision up to 1/4th,
-- 1/6th, and 1/8th tones. The accidentals range from double flat (-200 cents)
-- to double sharp (+200 cents). Each value includes a rational representation
-- of how many semitones it alters the pitch, where 1 represents a semitone,
-- negative values flatten the pitch, and positive values sharpen it.
data Accidental
  = DoubleFlat         -- -2/1 = -200 cents
  | SevenEighthsFlat   -- -7/4 = -175 cents
  | ThreeQuartersFlat  -- -3/2 = -150 cents
  | FiveThirdsFlat     -- -5/3 ≈ -133.33 cents
  | FiveEighthsFlat    -- -5/4 = -125 cents
  | Flat               -- -1/1 = -100 cents
  | FiveSixthsFlat     -- -5/6 ≈ -83.33 cents
  | ThreeEighthsFlat   -- -3/4 = -75 cents
  | TwoThirdsFlat      -- -2/3 ≈ -66.67 cents
  | QuarterFlat        -- -1/2 = -50 cents
  | OneSixthFlat       -- -1/6 ≈ -33.33 cents
  | EighthFlat         -- -1/4 = -25 cents
  | Natural            --  0/1  = 0 cents
  | EighthSharp        -- +1/4 = +25 cents
  | OneSixthSharp      -- +1/6 ≈ +33.33 cents
  | QuarterSharp       -- +1/2 = +50 cents
  | TwoThirdsSharp     -- +2/3 ≈ +66.67 cents
  | ThreeEighthsSharp  -- +3/4 = +75 cents
  | FiveSixthsSharp    -- +5/6 ≈ +83.33 cents
  | Sharp              -- +1/1 = +100 cents
  | FiveEighthsSharp   -- +5/4 = +125 cents
  | FiveThirdsSharp    -- +5/3 ≈ +133.33 cents
  | ThreeQuartersSharp -- +3/2 = +150 cents
  | SevenEighthsSharp  -- +7/4 = +175 cents
  | DoubleSharp        -- +2/1 = +200 cents
  deriving (Show, Eq, Ord, Enum, Bounded, Read)


-- | Converts an 'Accidental' to its custom rational representation in semitones.
-- For example:
--
-- >>> toRat QuarterSharp
-- Rat 1 2   -- represents 1/2 semitone sharp
--
-- >>> toRat ThreeQuartersFlat
-- Rat (-3) 2   -- represents -3/2 semitones (flat)
--
-- >>>  all (\acc -> denominator (toRat acc) == 1) halfTones
--
-- The resulting 'Rat' can be converted to cents by:
-- cents = ratToRational (toRat acc) * 100
toRat :: Accidental -> Rat
toRat acc = case acc of
  DoubleFlat         -> Rat (-2) 1
  SevenEighthsFlat   -> Rat (-7) 4
  ThreeQuartersFlat  -> Rat (-3) 2
  FiveThirdsFlat     -> Rat (-5) 3
  FiveEighthsFlat    -> Rat (-5) 4
  Flat               -> Rat (-1) 1
  FiveSixthsFlat     -> Rat (-5) 6
  ThreeEighthsFlat   -> Rat (-3) 4
  TwoThirdsFlat      -> Rat (-2) 3
  QuarterFlat        -> Rat (-1) 2
  OneSixthFlat       -> Rat (-1) 6
  EighthFlat         -> Rat (-1) 4
  Natural            -> Rat 0 1
  EighthSharp        -> Rat 1 4
  OneSixthSharp      -> Rat 1 6
  QuarterSharp       -> Rat 1 2
  TwoThirdsSharp     -> Rat 2 3
  ThreeEighthsSharp  -> Rat 3 4
  FiveSixthsSharp    -> Rat 5 6
  Sharp              -> Rat 1 1
  FiveEighthsSharp   -> Rat 5 4
  FiveThirdsSharp    -> Rat 5 3
  ThreeQuartersSharp -> Rat 3 2
  SevenEighthsSharp  -> Rat 7 4
  DoubleSharp        -> Rat 2 1
{- ORMOLU_ENABLE -}

-- | List of all quarter-tone accidentals in ascending order
quarterToneAccidentals :: [Accidental]
quarterToneAccidentals =
  [ DoubleFlat,
    ThreeQuartersFlat,
    Flat,
    QuarterFlat,
    Natural,
    QuarterSharp,
    Sharp,
    ThreeQuartersSharp,
    DoubleSharp
  ]

-- | List of all eighth-tone accidentals in ascending order
eighthToneAccidentals :: [Accidental]
eighthToneAccidentals =
  [ DoubleFlat,
    SevenEighthsFlat,
    ThreeQuartersFlat,
    FiveEighthsFlat,
    Flat,
    ThreeEighthsFlat,
    QuarterFlat,
    EighthFlat,
    Natural,
    EighthSharp,
    QuarterSharp,
    ThreeEighthsSharp,
    Sharp,
    FiveEighthsSharp,
    ThreeQuartersSharp,
    SevenEighthsSharp,
    DoubleSharp
  ]

-- | Convert our Rat to Rational for cents calculation
ratToRational :: Rat -> Rational
ratToRational (Rat n d) = fromIntegral n % fromIntegral d

allAccidentals :: [Accidental]
allAccidentals = [minBound .. maxBound]

filterByDenominator :: Integer -> [Accidental]
filterByDenominator d = filter (\acc -> denominator (toRat acc) == d) allAccidentals

halfTones :: [Accidental]
halfTones = filter (\acc -> denominator (toRat acc) == 1) allAccidentals

quarterTones :: [Accidental]
quarterTones = filter (\acc -> denominator (toRat acc) <= 2) allAccidentals

eighthTones :: [Accidental]
eighthTones = filter (\acc -> denominator (toRat acc) <= 4) allAccidentals

sixthTones :: [Accidental]
sixthTones = filter (\acc -> denominator (toRat acc) <= 6) allAccidentals

nearestIn :: [Accidental] -> Accidental -> Accidental
nearestIn subset acc = minimumBy (comparing (\x -> abs (cents x - cents acc))) subset
  where
    cents a = ratToRational (toRat a) * 100

roundToHalfTones :: Accidental -> Accidental
roundToHalfTones = nearestIn halfTones

roundToQuarterTones :: Accidental -> Accidental
roundToQuarterTones = nearestIn quarterTones

roundToEighthTones :: Accidental -> Accidental
roundToEighthTones = nearestIn eighthTones

roundToSixthTones :: Accidental -> Accidental
roundToSixthTones = nearestIn sixthTones

customSubset :: Integer -> [Accidental]
customSubset maxDenom = filter (\acc -> denominator (toRat acc) <= maxDenom) allAccidentals

-- | Get the next quarter-tone accidental.
-- Returns Nothing if there is no next quarter-tone accidental.
--
-- >>> succQuarter Natural
-- Just QuarterSharp
--
-- >>> succQuarter ThreeQuartersSharp
-- Just Sharp
--
-- >>> succQuarter DoubleSharp
-- Nothing
succQuarter :: Accidental -> Maybe Accidental
succQuarter acc =
  let nextQuarterTones = dropWhile (<= acc) quarterTones
   in if null nextQuarterTones
        then Nothing
        else Just (head nextQuarterTones)

-- | Get the previous quarter-tone accidental.
-- Returns Nothing if there is no previous quarter-tone accidental.
--
-- >>> predQuarter Natural
-- Just QuarterFlat
--
-- >>> predQuarter QuarterFlat
-- Just Flat
--
-- >>> predQuarter DoubleFlat
-- Nothing
predQuarter :: Accidental -> Maybe Accidental
predQuarter acc =
  let prevQuarterTones = dropWhile (>= acc) (reverse quarterTones)
   in if null prevQuarterTones
        then Nothing
        else Just (head prevQuarterTones)

-- | Get the next eighth-tone accidental.
-- Returns Nothing if there is no next eighth-tone accidental.
--
-- >>> succEighth Natural
-- Just EighthSharp
--
-- >>> succEighth EighthSharp
-- Just QuarterSharp
--
-- >>> succEighth DoubleSharp
-- Nothing
succEighth :: Accidental -> Maybe Accidental
succEighth acc =
  let nextEighthTones = dropWhile (<= acc) eighthToneAccidentals
   in if null nextEighthTones
        then Nothing
        else Just (head nextEighthTones)

-- | Get the previous eighth-tone accidental.
-- Returns Nothing if there is no previous eighth-tone accidental.
--
-- >>> predEighth Natural
-- Just EighthFlat
--
-- >>> predEighth EighthFlat
-- Just QuarterFlat
--
-- >>> predEighth DoubleFlat
-- Nothing
predEighth :: Accidental -> Maybe Accidental
predEighth acc =
  let prevEighthTones = dropWhile (>= acc) (reverse eighthToneAccidentals)
   in if null prevEighthTones
        then Nothing
        else Just (head prevEighthTones)

-- | The interval in Rational between two Accidentals
intervalBetweenAccidentals :: Accidental -> Accidental -> Rational
intervalBetweenAccidentals a1 a2 = ratToRational (toRat a2) - ratToRational (toRat a1)

-- | Get centls (Double) from an Accidental
accidentalToCents :: Accidental -> Double
accidentalToCents acc = fromRational (ratToRational (toRat acc) * 100)

--------------------------------------------------------------------------
-- PitchClass
--------------------------------------------------------------------------

data NoteName = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Generic)

data PitchClass = PitchClass
  { noteName :: !NoteName,
    accidental :: !Accidental
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Octave = Octave
  { getOctave :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Pitch = Pitch
  { pitchClass :: !PitchClass,
    octave :: !Octave
  }
  deriving (Eq, Show, Read, Generic)

mkOctave :: Int -> Octave
mkOctave = Octave

mkPitchClass :: NoteName -> Accidental -> PitchClass
mkPitchClass = PitchClass

mkPitch :: PitchClass -> Octave -> Pitch
mkPitch = Pitch

mkPitch' :: NoteName -> Accidental -> Int -> Pitch
mkPitch' n a o = Pitch (PitchClass n a) (Octave o)

-- | A type function that returns the pitch type associated with a given type.
--
-- For simple types, this is identity
--
-- > GetPitch Pitch = Pitch
--
-- For containers, this is a morphism
--
-- > GetPitch (Voice Pitch) = PitchOf Pitch = Pitch
type family GetPitch (s :: Type) :: Type

--------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------

-- | Rule that specifies how to choose enharmonic pitch options.
--
-- Rules define transformations between enharmonically equivalent pitches -
-- notes that sound the same but are written differently. Each rule consists of:
--
--  - A source pitch class ('fromPitchClass')
--  - A target pitch class ('toPitchClass')
--  - Any octave change needed ('octaveChange')
--
-- For example:
--
-- > Rule (PitchClass G Sharp) (PitchClass A Flat) NoChange     -- G♯ to A♭
-- > Rule (PitchClass B Sharp) (PitchClass C Natural) OctaveUp  -- B♯ to C (next octave)
-- > Rule (PitchClass F Flat) (PitchClass E Natural) NoChange   -- F♭ to E
--
-- Rules should maintain pitch equivalence - the input and output should represent
-- the same musical pitch, accounting for any octave changes.
data Rule = Rule
  { -- | The source pitch class to transform from
    fromPitchClass :: !PitchClass,
    -- | The target pitch class to transform to
    toPitchClass :: !PitchClass,
    -- | Any octave shift needed for equivalence
    octaveChange :: !OctaveChange
  }
  deriving (Show, Eq)

-- | Specifies how a pitch transformation affects the octave number.
--
-- Some enharmonic equivalents require moving to an adjacent octave to maintain
-- the same pitch. For example, B♯ is enharmonically equivalent to C in the next
-- octave up.
data OctaveChange
  = -- | Stay in the same octave (e.g., G♯ to A♭)
    NoChange
  | -- | Move up one octave (e.g., B♯ to C)
    OctaveUp
  | -- | Move down one octave (e.g., C♭ to B)
    OctaveDown
  deriving (Show, Eq)

-- | Maps octave changes to their numeric shifts for calculation.
--
-- > fromEnum NoChange == 0
-- > fromEnum OctaveUp == 1
-- > fromEnum OctaveDown == -1
instance Enum OctaveChange where
  fromEnum = \case
    NoChange -> 0
    OctaveUp -> 1
    OctaveDown -> -1

  -- \| Convert integer octave shifts to OctaveChange values.
  --
  -- > toEnum 0  == NoChange
  -- > toEnum 1  == OctaveUp
  -- > toEnum -1 == OctaveDown
  --
  -- Throws an error for any other integer input.
  toEnum = \case
    0 -> NoChange
    1 -> OctaveUp
    -1 -> OctaveDown
    _ -> error "OctaveChange.toEnum: expected -1, 0, or 1"
