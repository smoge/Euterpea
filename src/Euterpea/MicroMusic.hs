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
import Data.Kind
import Data.List (uncons)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data Accidental
  = DoubleFlat
  | ThreeQuartersFlat
  | Flat
  | QuarterFlat
  | Natural
  | QuarterSharp
  | Sharp
  | ThreeQuartersSharp
  | DoubleSharp
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

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
