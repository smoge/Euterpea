{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{- | SF (signal function) data structure, which is used to represent reactive
systems where inputs are transformed into outputs in a time-dependent manner.
The module leverages Haskell's Arrow abstraction to structure the
computation.
-}
module Control.SF.SF where

import Control.Arrow (
    Arrow (arr, first, (&&&), (***)),
    ArrowChoice (left),
    ArrowLoop (..),
    (>>>),
 )
import Control.Arrow.ArrowP ()
import Control.Arrow.Operations (ArrowCircuit (..))
import Control.Category (Category (..))
import Prelude hiding (id, (.))

{- | SF represents a signal function that takes an input of type a and produces
an output of type b along with a new signal function for subsequent inputs.
-}
newtype SF a b = SF {runSF :: (a -> (b, SF a b))}

{- | Category Instance

id: A signal function that returns its input as output.

(.): Composes two signal functions f and g. It evaluates f first, passes its
output to g, and returns the result. The use of seq ensures that intermediate
computations are evaluated strictly.
-}
instance Category SF where
    id = SF h where h x = (x, SF h)
    g . f = SF (h f g)
      where
        h f g x =
            let (y, f') = runSF f x
                (z, g') = runSF g y
             in f' `seq` g' `seq` (z, SF (h f' g'))

{- | Arrow Instance
arr f: Lifts a pure function f into an SF.
first f: Applies f to the first component of a pair.
(&&&): Splits input into two, applies f and g independently, and combines outputs into a pair.
(***): Like (&&&) but operates on a pair of inputs.
-}
instance Arrow SF where
    arr f = g
      where
        g = SF (\x -> (f x, g))
    first f = SF (g f)
      where
        g f (x, z) = f' `seq` ((y, z), SF (g f'))
          where
            (y, f') = runSF f x
    f &&& g = SF (h f g)
      where
        h f g x =
            let (y, f') = runSF f x
                (z, g') = runSF g x
             in ((y, z), SF (h f' g'))
    f *** g = SF (h f g)
      where
        h f g x =
            let (y, f') = runSF f (fst x)
                (z, g') = runSF g (snd x)
             in ((y, z), SF (h f' g'))

-- | Implements feedback loops where part of the output is fed back as input.
instance ArrowLoop SF where
    loop sf = SF (g sf)
      where
        g f x = f' `seq` (y, SF (g f'))
          where
            ((y, z), f') = runSF f (x, z)

{- | Implements conditional branching: applies the signal function to Left
inputs and passes Right inputs through unchanged.
-}
instance ArrowChoice SF where
    left sf = SF (g sf)
      where
        g f x = case x of
            Left a -> let (y, f') = runSF f a in f' `seq` (Left y, SF (g f'))
            Right b -> (Right b, SF (g f))

-- | Introduces a one-step delay with an initial value i.
instance ArrowCircuit SF where
    delay i = SF (f i)
      where
        f i x = (i, SF (f x))

---------------------
-- Utilitu Functions
---------------------

-- | Runs a signal function on a list of inputs, producing a list of outputs.
run :: SF a b -> [a] -> [b]
run _ [] = []
run (SF f) (x : xs) =
    let (y, f') = f x
     in y `seq` f' `seq` (y : run f' xs)

-- | Runs a signal function on a list of inputs, producing a list of outputs.
unfold :: SF () a -> [a]
unfold = flip run inp
  where
    inp = () : inp

-- | Extracts the nth output from an unfolding signal function.
nth :: Int -> SF () a -> a
nth n (SF f) = x `seq` if n == 0 then x else nth (n - 1) f'
  where
    (x, f') = f ()

nth' :: Int -> (b, ((), b) -> (a, b)) -> a
nth' !n (i, f) = n `seq` i `seq` f `seq` aux n i
  where
    aux !n !i = x `seq` i' `seq` if n == 0 then x else aux (n - 1) i'
      where
        (x, i') = f ((), i)
