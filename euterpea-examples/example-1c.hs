{-# LANGUAGE DerivingStrategies #-}

import Data.List (sort)
import Euterpea

type PitchSpace = [AbsPitch]

data Scale = Scale
  { scaleRoot :: Pitch,
    scaleSteps :: [Int]
  }
  deriving stock (Show, Eq)

-- | Different types of musical modes
data ScaleMode
  = -- | Major scale
    Ionian
  | -- | Minor scale with raised 6th
    Dorian
  | -- | Minor scale with lowered 2nd
    Phrygian
  | -- | Major scale with raised 4th
    Lydian
  | -- | Major scale with lowered 7th
    Mixolydian
  | -- | Natural minor scale
    Aeolian
  | -- | Minor scale with lowered 2nd and 5th
    Locrian
  deriving stock (Show, Eq, Enum, Bounded)

-- | Common scale patterns
scalePatterns :: [(ScaleMode, [Int])]
scalePatterns =
  [ (Ionian, [2, 2, 1, 2, 2, 2, 1]),
    (Dorian, [2, 1, 2, 2, 2, 1, 2]),
    (Phrygian, [1, 2, 2, 2, 1, 2, 2]),
    (Lydian, [2, 2, 2, 1, 2, 2, 1]),
    (Mixolydian, [2, 2, 1, 2, 2, 1, 2]),
    (Aeolian, [2, 1, 2, 2, 1, 2, 2]),
    (Locrian, [1, 2, 2, 1, 2, 2, 2])
  ]

-- | Apply a list of functions to a single value
applyEach :: [a -> b] -> a -> [b]
applyEach fs x = map ($ x) fs

-- | Compose a list of functions into a single function
applyAll :: [a -> a] -> a -> a
applyAll = foldr (.) id

-- | Get length of a list without using recursive pattern matching
listLength :: [a] -> Int
listLength = sum . fmap (const 1)

-- | Double each number in a list
doubleEach :: (Num a) => [a] -> [a]
doubleEach = fmap (* 2)

-- | Create pairs of consecutive numbers
pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne = fmap (\n -> (n, n + 1))

-- | Add numbers in each pair
addEachPair :: [(Int, Int)] -> [Int]
addEachPair = fmap (uncurry (+))

-- | Add corresponding elements in a list of pairs
addPairsPointwise :: [(Int, Int)] -> (Int, Int)
addPairsPointwise = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)

-- | Find maximum absolute pitch in a pitch space
maxAbsPitch :: PitchSpace -> AbsPitch
maxAbsPitch [] = error "Cannot find maximum of empty pitch space"
maxAbsPitch xs = maximum xs

-- | Find minimum absolute pitch in a pitch space
minAbsPitch :: PitchSpace -> AbsPitch
minAbsPitch [] = error "Cannot find minimum of empty pitch space"
minAbsPitch xs = minimum xs

-- | Create a chromatic scale between two pitches
chromaticScale :: Pitch -> Pitch -> Music Pitch
chromaticScale start end
  | start == end = note qn start
  | start < end = line $ map (note qn) [start .. end]
  | otherwise = line $ map (note qn) $ reverse [end .. start]

-- | Create a scale from a root pitch and interval pattern
mkScale :: Pitch -> [Int] -> [AbsPitch]
mkScale root = scanl (+) (absPitch root)

-- | Generate a scale in a specific mode
genScale :: Pitch -> ScaleMode -> [AbsPitch]
genScale root mode =
  case lookup mode scalePatterns of
    Nothing -> error $ "Unknown scale mode: " ++ show mode
    Just steps -> mkScale root steps

-- | Implementation of "Frère Jacques"
frereJacques :: Music Pitch
frereJacques =
  let -- Define the melody patterns
      pattern1 = [c 4 qn, d 4 qn, e 4 qn, c 4 qn]
      pattern2 = [e 4 qn, f 4 qn, g 4 hn]
      pattern3 = [g 4 en, a 4 en, g 4 en, f 4 en, e 4 qn, c 4 qn]
      pattern4 = [c 4 qn, g 3 qn, c 4 hn]

      -- Combine patterns into a phrase
      phrase =
        line $
          concat
            [ pattern1,
              pattern1,
              pattern2,
              pattern2,
              pattern3,
              pattern3,
              pattern4,
              pattern4
            ]

      -- Create a round with four voices
      delays = [0, wn, wn * 2, wn * 3]
      voices = map (\d -> rest d :+: phrase) delays
   in foldr (:=:) (rest 0) voices

-- Helper functions
trans :: Int -> Pitch -> Pitch
trans i = pitch . (+ i) . absPitch