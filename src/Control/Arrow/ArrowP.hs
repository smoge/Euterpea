{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Arrow.ArrowP (
    ArrowP(..)
  , strip
) where

-- Import necessary modules for Arrow, Category, and related operations.
import Control.Arrow
import Control.Arrow.Operations

import Control.Category
import Prelude hiding (id, (.)) -- Hide the default Prelude id and (.) to use Category versions.

-- Define a newtype ArrowP, which is a wrapper around an arrow 'a'.
-- The 'strip' function extracts the underlying arrow from ArrowP.
newtype ArrowP a p b c = ArrowP {strip :: a b c}

-- Make ArrowP an instance of Category.
instance (Category a) => Category (ArrowP a p) where
  id = ArrowP id                         -- Define identity: wraps the identity of the underlying category.
  ArrowP g . ArrowP f = ArrowP (g . f)   -- Define composition: wraps the composition of two underlying arrows.
instance (Arrow a) => Arrow (ArrowP a p) where
  arr f = ArrowP (arr f)                 -- Lift a function to ArrowP using arr.
  first (ArrowP f) = ArrowP (first f)    -- Apply Arrow's first to ArrowP, affecting only the first component of a pair.

-- Make ArrowP an instance of ArrowLoop.
instance (ArrowLoop a) => ArrowLoop (ArrowP a p) where
  loop (ArrowP f) = ArrowP (loop f)      -- Use loop from ArrowLoop to create feedback loops in ArrowP.

-- Make ArrowP an instance of ArrowCircuit.
instance (ArrowCircuit a) => ArrowCircuit (ArrowP a p) where
  delay i = ArrowP (delay i)             -- Introduce a delay element for ArrowP using the ArrowCircuit interface.

-- Make ArrowP an instance of ArrowChoice.
instance (ArrowChoice a) => ArrowChoice (ArrowP a p) where
  left (ArrowP f) = ArrowP (left f)      -- Apply the left operation to ArrowP, focusing on Either type's Left value.
  ArrowP f ||| ArrowP g = ArrowP (f ||| g) -- Combine two ArrowP arrows with either-or choice, handling both sides.52