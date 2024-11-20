{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Euterpea.MicroMusic
  ( Accidental(..)
  , NoteName(..)
  , PitchClass(..)
  , Pitch(..)
  , Octave
  , makeOctave
  , getOctave
  , Rule(..)
  , OctaveChange(..)
  ) where

import Data.Bifunctor (first)
import Data.List (uncons)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

infixr 5 :+:, :=:

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
  { noteName :: NoteName
  , accidental :: Accidental
  } deriving (Eq, Ord, Show, Enum, Bounded, Read, Generic)

newtype Octave = Octave { getOctave :: Int }
  deriving (Eq, Ord, Show, Enum, Read, Generic)

data Pitch = Pitch
  { pitchClass :: PitchClass
  , octave :: Octave
  } deriving (Eq, Ord, Show, Enum, Bounded, Read, Generic)

makeOctave :: Int -> Octave
makeOctave = Octave

data Rule = Rule
  { fromPitchClass :: !PitchClass
  , toPitchClass :: !PitchClass
  , octaveChange :: !OctaveChange
  }

data OctaveChange = NoChange | OctaveUp | OctaveDown
  deriving (Show, E)q
