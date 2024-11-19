import Data.List (sort)
import Euterpea

type PitchSpace = [AbsPitch]

createMajorChord :: Music Pitch -> Music Pitch
createMajorChord root = chord [root, transpose 4 root, transpose 7 root]

createMinorChord :: Music Pitch -> Music Pitch
createMinorChord root = chord [root, transpose 3 root, transpose 7 root]

createChord :: [Int] -> Music Pitch -> Music Pitch
createChord intervals root =
  chord $ root : map (`transpose` root) intervals

data MusicFold = Horizontal | Vertical

foldMusic :: MusicFold -> [Music a] -> Music a
foldMusic fold = foldr connector (rest 0)
  where
    connector = case fold of
      Horizontal -> (:+:)
      Vertical -> (:=:)

line' :: [Music a] -> Music a
line' = foldMusic Horizontal

chord' :: [Music a] -> Music a
chord' = foldMusic Vertical

data ScaleMode
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  deriving (Eq, Show, Enum)

type ScaleSteps = [Int]

majorScaleSteps :: ScaleSteps
majorScaleSteps = [2, 2, 1, 2, 2, 2, 1]

genScale :: Pitch -> ScaleMode -> PitchSpace
genScale root mode = mkScale root $ rotateSteps (fromEnum mode) majorScaleSteps
  where
    rotateSteps n xs = take (length xs) $ drop n $ cycle xs

data PitchOperation = MinPitch | MaxPitch

getPitchFromSpace :: PitchOperation -> PitchSpace -> AbsPitch
getPitchFromSpace op space = case (op, space) of
  (_, []) -> error "Empty pitch space"
  (MinPitch, xs) -> head $ sort xs
  (MaxPitch, xs) -> last $ sort xs

addPairs :: [(Int, Int)] -> [Int]
addPairs = fmap (uncurry (+))

mkScale :: Pitch -> ScaleSteps -> PitchSpace
mkScale root steps = scanl (+) (absPitch root) steps