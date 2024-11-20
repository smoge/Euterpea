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
-- For simple types, this is identity (they are their own pitch).
--
-- > GetPitch Pitch = Pitch
--
-- For containers, this is a morphism
--
-- > GetPitch (Voice Pitch) = PitchOf Pitch = Pitch
type family GetPitch (s :: Type) :: Type

-- Move to another module
data Rule = Rule
  { fromPitchClass :: !PitchClass,
    toPitchClass :: !PitchClass,
    octaveChange :: !OctaveChange
  }
  deriving (Show, Eq)

data OctaveChange = NoChange | OctaveUp | OctaveDown
  deriving (Show, Eq)

instance Enum OctaveChange where
  fromEnum = \case
    NoChange -> 0
    OctaveUp -> 1
    OctaveDown -> -1
  toEnum = \case
    0 -> NoChange
    1 -> OctaveUp
    -1 -> OctaveDown
    _ -> error "OctaveChange.toEnum: bad argument"
