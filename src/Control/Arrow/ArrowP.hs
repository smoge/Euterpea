{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Arrow.ArrowP (
  -- | The ArrowP type wrapper and associated functions
  ArrowP(..),
  strip
) where

import Control.Arrow
import Control.Arrow.Operations
import Control.Category
import Prelude hiding (id, (.))

-- | A wrapper type for arrows that adds an additional phantom type parameter 'p'.
-- This is useful for tagging arrows with extra type information without affecting
-- their runtime behavior.
--
-- Examples:
-- >>> let f = arr (+1) :: ArrowP (->) Int Int Int
-- >>> strip f 5
-- 6
newtype ArrowP a p b c = ArrowP {
  -- | Unwraps an ArrowP to get the underlying arrow.
  --
  -- Example:
  -- >>> strip (ArrowP (arr (+1))) 5
  -- 6
  strip :: a b c
}

-- | Category instance enables composition of ArrowP arrows
--
-- Examples:
-- >>> let f = ArrowP (arr (+1))
-- >>> let g = ArrowP (arr (*2))
-- >>> strip (g . f) 5  -- Applies f then g: (5+1)*2
-- 12
instance (Category a) => Category (ArrowP a p) where
  id = ArrowP id
  ArrowP g . ArrowP f = ArrowP (g . f)

-- | Arrow instance enables lifting functions and working with pairs
--
-- Examples:
-- >>> strip (arr (+1)) 5
-- 6
-- >>> strip (first (arr (+1))) (5,10)  -- Only affects first component
-- (6,10)
instance (Arrow a) => Arrow (ArrowP a p) where
  arr f = ArrowP (arr f)
  first (ArrowP f) = ArrowP (first f)

-- | ArrowLoop instance enables recursive computations
--
-- Example:
-- >>> let sumLoop = loop (arr (\(x,s) -> (x+s, x+s)))  -- Accumulates sum
-- >>> strip sumLoop [1,2,3]
-- [1,3,6]
instance (ArrowLoop a) => ArrowLoop (ArrowP a p) where
  loop (ArrowP f) = ArrowP (loop f)

-- | ArrowCircuit instance enables stateful computations with delays
--
-- Example:
-- >>> let delayBy1 = delay 0  -- Delays input by 1 step, initially outputs 0
-- >>> strip delayBy1 [1,2,3]
-- [0,1,2]
instance (ArrowCircuit a) => ArrowCircuit (ArrowP a p) where
  delay i = ArrowP (delay i)

-- | ArrowChoice instance enables conditional computations with Either
--
-- Examples:
-- >>> let f = arr (+1)
-- >>> let g = arr (*2)
-- >>> strip (f ||| g) (Left 5)   -- Uses f on Left values
-- Left 6
-- >>> strip (f ||| g) (Right 5)  -- Uses g on Right values
-- Right 10
instance (ArrowChoice a) => ArrowChoice (ArrowP a p) where
  left (ArrowP f) = ArrowP (left f)
  ArrowP f ||| ArrowP g = ArrowP (f ||| g)