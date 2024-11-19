import Euterpea

createMajorChord :: (Octave -> Dur -> Music Pitch) -> Music Pitch
createMajorChord root =
  root 4 qn :=: third :=: fifth
  where
    third = transpose 4 (root 4 qn)
    fifth = transpose 7 (root 4 qn)

createMajorChord :: Music a -> Music a
createMajorChord root =
  root :=: third :=: fifth
  where
    third = transpose 4 root
    fifth = transpose 7 root

createMinorChord :: Music a -> Music a
createMinorChord root =
  root :=: third :=: fifth
  where
    third = transpose 3 root
    fifth = transpose 7 root

-- Before I learned of "line" and "chord"
foldMusicSet :: ConcatMusicSet a -> [Music a] -> Music a
foldMusicSet foldFn = foldr foldFn (rest 0)

-- Equivalent
line', chord' :: [Music a] -> Music a
line' = foldR (:+:) (rest 0)
chord' = foldR (:=:) (rest 0)

------------ References from Exercises! ------------
-- Start
addEachPair :: [(Int, Int)] -> [Int]
addEachPair [] = []
addEachPair ((n, m) : rest) = n + m : addEachPair rest

addEachPair' :: [(Int, Int)] -> [Int]
addEachPair' = map (\(n, m) -> n + m)

-- Finished Abstraction
addEachPair' :: [(Int, Int)] -> [Int]
addEachPair' = map (uncurry (+))

-- Abstraction to handle both min and max and curryable
makeGetAbsPitch :: String -> PitchSpace -> AbsPitch
makeGetAbsPitch [] = error "Empty Lists can not be handled"
makeGetAbsPitch "max" = last . sort
makeGetAbsPitch "min" = head . sort
makeGetAbsPitch _ = error "Pass 'min' or 'max' ONLY"

---- Errors come caked into this via the closure
minPitch = makeGetAbsPitch "min"

maxPitch = makeGetAbsPitch "max"

-- Initial Abstraction, eventually just derived from Enum and used fromEnum
genScale :: Pitch -> ScaleMode -> [AbsPitch]
genScale p mode =
  mkScale p (getByMode mode)
  where
    getByMode Ionian' = cycleScaleNTimes 0
    getByMode Dorian' = cycleScaleNTimes 1
    getByMode Phrygian' = cycleScaleNTimes 2
    getByMode Lydian' = cycleScaleNTimes 3
    getByMode Mixolydian' = cycleScaleNTimes 4
    getByMode Aeolian' = cycleScaleNTimes 5
    getByMode Locrian' = cycleScaleNTimes 6

-- Determining best way to nest wheres/let
--- 'where' is better when there's "guards" (ie. pred | resultIfTrue)
---- This is because where scopes to the FUNCTION, not locally
--- 'let-in' is a local scope so anything in the let is only available in "in"
genScale' :: (Enum a) => Pitch -> a -> PitchSpace
genScale' p = mkScale p . cycleScaleNTimes . fromEnum
  where
    cycleScaleNTimes =
      let steps = majSteps -- Swap out values here to test different step sets
          cycleScale xs = [head $ tail xs] ++ tail (tail xs) ++ [head xs]
       in (iterate cycleScale steps !!)

genScale'' :: (Enum a) => Pitch -> a -> PitchSpace
genScale'' p =
  let cycleScaleNTimes = (iterate cycleScale steps !!)
        where
          steps = majSteps -- Swap out values here to test different step sets
          cycleScale xs = [head $ tail xs] ++ tail (tail xs) ++ [head xs]
   in mkScale p . cycleScaleNTimes . fromEnum