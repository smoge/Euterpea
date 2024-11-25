{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | This module provides the 'ArrowP' wrapper for augmenting arrows with
additional phantom type parameters. These parameters can be used for tracking
effects, encoding protocols, or applying domain-specific constraints at the
type level without affecting runtime performance.
-}
module Control.Arrow.ArrowP (
    ArrowP (..),
)
where

import Control.Arrow (Arrow (arr, first), ArrowChoice (left, (|||)), ArrowLoop (..))
import Control.Arrow.Operations (ArrowCircuit (..))
import Control.Category (Category (..))
import Prelude hiding (id, (.))

{- | A wrapper type for arrows that adds an additional phantom type parameter 'p'.
The phantom type can be used to encode additional type-level information
without affecting the arrow's runtime behavior.

The type parameters are:
* @a@: The underlying arrow type
* @p@: The phantom type parameter
* @b@: The input type
* @c@: The output type

>>> :{
let incrementTagged :: ArrowP (->) "math" Int Int
  incrementTagged = arr (+1)
in strip incrementTagged 5
:}
6

>>> :{
let doubleTagged :: ArrowP (->) "math" Int Int
  doubleTagged = arr (*2)
in strip doubleTagged 5
:}
10

Unwraps an ArrowP to get the underlying arrow.
The 'strip' function unwraps the 'ArrowP' type to reveal the underlying
arrow. This is useful when you need to work with the arrow directly or
pass it to functions expecting the base arrow type.

>>> :{
let f = ArrowP (arr (+1))
in strip f 5
:}
6
-}
newtype ArrowP a p b c = ArrowP {strip :: a b c}

{- | The Category instance enables composition of 'ArrowP' arrows while preserving
the phantom type parameter. The `(.)` operator combines arrows sequentially,
applying the first function and then the second.


>>> :{
let f = ArrowP (arr (+1))
  g = ArrowP (arr (*2))
in strip (g . f) 5  -- Composes f and g: (5+1)*2
:}
12

>>> :{
let f = ArrowP (arr show)
  g = ArrowP (arr length)
in strip (g . f) 123  -- Converts to string then counts chars
:}
3
-}
instance (Category a) => Category (ArrowP a p) where
    id = ArrowP id
    ArrowP g . ArrowP f = ArrowP (g . f)

{- | Arrow instance enables lifting pure functions and working with pairs.
The phantom type parameter is preserved across all operations.

>>> :{
let addPair = arr (\(x,y) -> x + y)
  incrementFirst = first (arr (+1))
in (strip addPair (5,3), strip incrementFirst (5,3))
:}
(8,(6,3))
-}
instance (Arrow a) => Arrow (ArrowP a p) where
    arr f = ArrowP (arr f)
    first (ArrowP f) = ArrowP (first f)

{- | ArrowLoop instance enables recursive computations through feedback loops.

>>> :{
let sumLoop = loop (arr (\(x,s) -> let next = x + s in (next, next)))
in strip sumLoop [1,2,3]  -- Running sum: [1, 1+2, 1+2+3]
:}
[1,3,6]
-}
instance (ArrowLoop a) => ArrowLoop (ArrowP a p) where
    loop (ArrowP f) = ArrowP (loop f)

{- | ArrowCircuit instance enables stateful computations with delay elements.

>>> :{
let delayOne = delay 0  -- Delays by one step, initial output is 0
in strip delayOne [1,2,3]
:}
[0,1,2]
-}
instance (ArrowCircuit a) => ArrowCircuit (ArrowP a p) where
    delay i = ArrowP (delay i)

{- | ArrowChoice instance enables conditional computations using Either values.
Both branches preserve the phantom type parameter.


>>> :{
let f = arr (+1)  -- Increment for Left
  g = arr (*2)  -- Double for Right
  choice = f ||| g
in (strip choice (Left 5), strip choice (Right 5))  -- Process Left or Right
:}
(Left 6,Right 10)

>>> :{
let process = left (arr (+1))  -- Only modifies Left values
in (strip process (Left 5), strip process (Right 10))
:}
(Left 6,Right 10)
-}
instance (ArrowChoice a) => ArrowChoice (ArrowP a p) where
    left (ArrowP f) = ArrowP (left f)
    ArrowP f ||| ArrowP g = ArrowP (f ||| g)
