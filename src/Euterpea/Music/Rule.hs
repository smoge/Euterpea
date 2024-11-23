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

module Euterpea.Music.Rule
  ( Rule (..),
    OctaveChange (..),
  )
where

import Euterpea.MicroMusic

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
