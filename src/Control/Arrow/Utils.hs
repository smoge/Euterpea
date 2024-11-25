{-# LANGUAGE Arrows #-}

module Control.Arrow.Utils (
    EventoArrow (..),
    traverseArr,
    traverseArr_,
    sequenceArr_,
    sequenceArr,
    zipSequenceArrVec,
    zipSequenceArrList,
    whenArr,
    unlessArr,
    constantly,
)
where

import Control.Arrow (
    Arrow (arr, (***)),
    ArrowChoice,
    returnA,
    (>>>),
 )
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Data.Vector.Sized (Vector, fromList, toList)
import qualified Data.Vector.Sized as Vec
import GHC.TypeLits (KnownNat)

{- | A newtype wrapper for arrows that provides useful instances like Functor
and Applicative.

The Functor instance allows mapping over the output:
>>> let add1 = EventoArrow (arr (+1))
>>> fmap (*2) add1  -- First adds 1, then multiplies by 2

The Applicative instance enables parallel execution:
>>> let add1 = EventoArrow (arr (+1))
>>> let add10 = EventoArrow (arr (+10))
>>> let combined = (+) <$> add1 <*> add10  -- Runs both arrows on the same input
-}
newtype EventoArrow a b c = EventoArrow {unEventoArrow :: a b c}

instance (Arrow a) => Functor (EventoArrow a b) where
    fmap f a = EventoArrow (unEventoArrow a >>> arr f)

instance (Arrow a) => Applicative (EventoArrow a b) where
    pure c = EventoArrow $ constantly c
    f <*> a = EventoArrow $ proc input -> do
        fres <- unEventoArrow f -< input
        ares <- unEventoArrow a -< input
        returnA -< fres ares

{- | Traverses a structure with an arrow function, collecting results.

>>> traverseArr (\x -> arr (+ x)) [1,10] 5
[6,15]
-}
traverseArr :: (Traversable t, Arrow a) => (x -> a b c) -> t x -> a b (t c)
traverseArr f xs = unEventoArrow $ traverse (EventoArrow . f) xs

{- | Like 'traverseArr' but discards results.

> traverseArr_ (\x -> arr (\y -> print (x + y))) [1,10] 5
Prints: 6
Prints: 15
-}
traverseArr_ :: (Foldable t, Arrow a) => (x -> a b c) -> t x -> a b ()
traverseArr_ f xs = unEventoArrow $ traverse_ (EventoArrow . f) xs

{- | Runs a collection of arrows on the same input, collecting results.

> sequenceArr [arr (*2), arr (+3)] 5
[10,8]
-}
sequenceArr :: (Traversable t, Arrow a) => t (a b c) -> a b (t c)
sequenceArr = traverseArr id

{- | Like 'sequenceArr' but discards results.

> sequenceArr_ [arr print, arr print] "hello"
Prints: "hello" twice
-}
sequenceArr_ :: (Foldable t, Arrow a) => t (a b any) -> a b ()
sequenceArr_ = traverseArr_ id

{- | Runs arrows in parallel with corresponding inputs from a fixed-size vector.

> let v = fromJust $ fromList [arr (*2), arr (+1)]
> zipSequenceArrVec v (fromJust $ fromList [3,4])
fromList [6,5]
-}
zipSequenceArrVec :: (Arrow a, KnownNat n) => Vector n (a b c) -> a (Vector n b) (Vector n c)
zipSequenceArrVec cells = arr toList >>> zipSequenceArrListUnsafe (toList cells) >>> arr (fromJust . fromList)
  where
    -- Internal helper - safe when used with equal-sized vectors
    zipSequenceArrListUnsafe :: (Arrow a) => [a b c] -> a [b] [c]
    zipSequenceArrListUnsafe [] = constantly []
    zipSequenceArrListUnsafe (x : xs) = proc (y : ys) -> do
        xres <- x -< y
        xsres <- zipSequenceArrListUnsafe xs -< ys
        returnA -< (xres : xsres)

{- | Like 'zipSequenceArrVec' but for lists, safely handling mismatched lengths.

> zipSequenceArrList [arr (*2), arr (+1)] [3,4,5]
[6,5]
-}
zipSequenceArrList :: (Arrow a, ArrowChoice a) => [a b c] -> a [b] [c]
zipSequenceArrList [] = constantly []
zipSequenceArrList (a : as) = proc bs -> case bs of
    [] -> returnA -< []
    b : bs' -> do
        c <- a -< b
        cs <- zipSequenceArrList as -< bs'
        returnA -< c : cs

{- | Conditionally executes an arrow based on a boolean input.

> whenArr (arr print) (True, "hello")
Prints: "hello"
> whenArr (arr print) (False, "hello")
Does nothing
-}
whenArr :: (ArrowChoice a) => a b () -> a (Bool, b) ()
whenArr cell = proc (b, input) ->
    if b
        then cell -< input
        else constantly () -< input

{- | Like 'whenArr' but executes when the condition is False.

> unlessArr (arr print) (False, "hello")
Prints: "hello"
> unlessArr (arr print) (True, "hello")
Does nothing
-}
unlessArr :: (ArrowChoice a) => a b () -> a (Bool, b) ()
unlessArr cell = arr not *** arr id >>> whenArr cell

{- | Creates an arrow that always outputs the same value.

>>> constantly "hello" ()
"hello"
-}
constantly :: (Arrow a) => b -> a any b
constantly = arr . const
