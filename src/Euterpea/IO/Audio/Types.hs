{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Euterpea.IO.Audio.Types where

import           Control.Arrow.ArrowP
import           Control.SF.SF
import           Data.Proxy           (Proxy (..))

class Clock p where
  rate :: p -> Double
  rateProxy :: Proxy p -> Double

class AudioSample a where
  numChans :: a -> Int
  numChansProxy :: Proxy a -> Int
  zero :: a
  mix :: a -> a -> a
  collapse :: a -> [Double]

data AudRate

data CtrRate

instance Clock AudRate where
  rate _ = 44100
  rateProxy _ = 44100

instance Clock CtrRate where
  rate _ = 4410
  rateProxy _ = 4410

type AudSF a b = SigFun AudRate a b

type CtrSF a b = SigFun CtrRate a b

type Signal clk a b = ArrowP SF clk a b

type SigFun clk a b = ArrowP SF clk a b

-- Arbitrary number of channels (say, 5.1) can be supported by just adding more
-- instances of the AudioSample type class.
-- numChans :: a -> Int
-- allows us to reify the number of channels from the type.

instance AudioSample Double where
  zero = 0
  mix = (+)
  collapse a = [a]
  numChans _ = 1
  numChansProxy _ = 1

instance AudioSample (Double, Double) where
  zero = (0, 0)
  mix (a, b) (c, d) = (a + c, b + d)
  collapse (a, b) = [a, b]
  numChans _ = 2
  numChansProxy _ = 2

-- Some useful type synonyms:
type Mono p = Signal p () Double

type Stereo p = Signal p () (Double, Double)
