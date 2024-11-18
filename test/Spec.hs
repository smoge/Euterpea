{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Applicative (liftA2)
import Data.Functor ((<&>))
import Data.Ratio ((%))
import Euterpea.Music
import GHC.Generics (Generic)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Args (maxDiscardRatio, maxSize, maxSuccess),
    Gen,
    Property,
    Testable,
    choose,
    counterexample,
    elements,
    forAll,
    generate,
    oneof,
    resize,
    scale,
    sized,
    stdArgs,
    vectorOf,
    withMaxSuccess,
    (==>),
  )

testConfig :: Args
testConfig =
  stdArgs
    { maxSuccess = 1000,
      maxSize = 100,
      maxDiscardRatio = 10
    }

propertyWith :: (Testable prop) => prop -> Property
propertyWith = withMaxSuccess testConfig.maxSuccess

generateTestCases :: Gen a -> IO [a]
generateTestCases gen = generate $ vectorOf 1000 gen

data MusicTestError
  = InvalidDuration !Dur
  | InvalidPitch !Pitch
  | InvalidStructure !String
  | InvalidModification !Control
  deriving (Show, Eq, Generic)

-----------------------------------
-- Helper Types and Functions
-----------------------------------

newtype ValidPitch = ValidPitch {getPitch :: Pitch}
  deriving (Eq, Show)

newtype TestDur = TestDur {getDur :: Rational}
  deriving stock (Eq, Show, Ord)
  deriving newtype (Enum, Num, Fractional)

instance Semigroup TestDur where
  (<>) :: TestDur -> TestDur -> TestDur
  (<>) (TestDur x) (TestDur y) = TestDur (x + y)

instance Monoid TestDur where
  mempty :: TestDur
  mempty = TestDur 0

newtype SimpleNote = SimpleNote {getNote :: Music Pitch}
  deriving (Show, Eq, Generic)

newtype NonRestMusic = NonRestMusic {getMusic :: Music ValidPitch}
  deriving (Eq, Show, Generic)

-- evaluatePitch :: Music Pitch -> [(PitchClass, Octave)]
-- evaluatePitch = \case
--     Prim (Note _ p) -> [pitchToTuple p]
--     Prim (Rest _) -> []
--     Modify (Transpose n) m -> fmap (pitchToTuple . trans n) (evaluatePitch m)
--     Modify _ m -> evaluatePitch m
--     m1 :+: m2 -> evaluatePitch m1 <> evaluatePitch m2
--     m1 :=: m2 -> evaluatePitch m1 <> evaluatePitch m2

evaluatePitch :: Music Pitch -> [(PitchClass, Octave)]
evaluatePitch = go 0
  where
    go :: Int -> Music Pitch -> [(PitchClass, Octave)]
    go transposition = \case
      Prim (Note _ p) -> [pitchToTuple (trans transposition p)]
      Prim (Rest _) -> []
      Modify (Transpose n) m -> go (transposition + n) m
      Modify _ m -> go transposition m
      m1 :+: m2 -> go transposition m1 <> go transposition m2
      m1 :=: m2 -> go transposition m1 <> go transposition m2

pitchToTuple :: Pitch -> (PitchClass, Octave)
pitchToTuple (p, o) = (p, o)

fromValidPitch :: Music ValidPitch -> Music Pitch
fromValidPitch = fmap getPitch

makeDuration :: Integer -> Integer -> Rational
makeDuration num deno = num % deno

-- Normalize music by simplifying zero transpositions
normalizeMusic :: Music Pitch -> Music Pitch
normalizeMusic (Modify (Transpose 0) m) = normalizeMusic m
normalizeMusic (Prim p) = Prim p
normalizeMusic (m1 :+: m2) = normalizeMusic m1 :+: normalizeMusic m2
normalizeMusic (m1 :=: m2) = normalizeMusic m1 :=: normalizeMusic m2
normalizeMusic (Modify x m) = Modify x (normalizeMusic m)

-- Check if two pieces of music are the same after normalization
sameMusic :: Music Pitch -> Music Pitch -> Bool
sameMusic m1 m2 = sameMusicHelper (normalizeMusic m1) (normalizeMusic m2)

sameMusicHelper :: Music Pitch -> Music Pitch -> Bool
sameMusicHelper (Prim p1) (Prim p2) = samePrim p1 p2
sameMusicHelper (m1 :+: m2) (n1 :+: n2) = sameMusicHelper m1 n1 && sameMusicHelper m2 n2
sameMusicHelper (m1 :=: m2) (n1 :=: n2) = sameMusicHelper m1 n1 && sameMusicHelper m2 n2
sameMusicHelper (Modify c1 m1') (Modify c2 m2') = c1 == c2 && sameMusicHelper m1' m2'
sameMusicHelper _ _ = False

samePrim :: Primitive Pitch -> Primitive Pitch -> Bool
samePrim (Note d1 p1) (Note d2 p2) = d1 == d2 && p1 == p2
samePrim (Rest d1) (Rest d2) = d1 == d2
samePrim _ _ = False

collectPitches :: Music Pitch -> [Pitch]
collectPitches music = collectHelper music []

collectHelper :: Music Pitch -> [Pitch] -> [Pitch]
collectHelper m acc = case m of
  Prim (Note _ p) -> p : acc
  Modify (Transpose n) music' -> foldr (transAndAccumulate n) acc (collectPitches music')
  m1 :+: m2 -> collectHelper m1 (collectHelper m2 acc)
  m1 :=: m2 -> collectHelper m1 (collectHelper m2 acc)
  Prim (Rest _) -> acc
  Modify (Tempo _) music' -> collectHelper music' acc
  Modify (Instrument _) music' -> collectHelper music' acc
  Modify (Phrase _) music' -> collectHelper music' acc
  Modify (KeySig _ _) music' -> collectHelper music' acc
  Modify (Custom _) music' -> collectHelper music' acc

-- Helper function to apply transpositions and accumulate
transAndAccumulate :: Int -> Pitch -> [Pitch] -> [Pitch]
transAndAccumulate n pipi acc = trans n pipi : acc

sameStructure :: Music Pitch -> Music Pitch -> Bool
sameStructure m1 m2 = case (m1, m2) of
  (Prim (Rest _), Prim (Rest _)) -> True
  (Prim _, Prim _) -> True
  (m11 :+: m22, n1 :+: n2) -> sameStructure m11 n1 && sameStructure m22 n2
  (m11 :=: m22, n1 :=: n2) -> sameStructure m11 n1 && sameStructure m22 n2
  (Modify c1 m1', Modify c2 m2') -> c1 == c2 && sameStructure m1' m2'
  _ -> False

-- For all valid values generated, check a given property
forAllValid ::
  (Show a, Testable prop) =>
  Gen a ->
  (a -> Bool) ->
  (a -> prop) ->
  Property
forAllValid gen valid prop = forAll gen $ \x ->
  valid x ==> prop x

class HasDur a where
  getDur' :: a -> Dur

instance HasDur TestDur where
  getDur' :: TestDur -> Dur
  getDur' (TestDur dd) = dd

------------------------
-- Arbitrary Instances
------------------------

durationDenominators :: [Int]
durationDenominators = [2 ^ (n :: Int) | n <- [0 .. 12]]

dotOptions :: [Int]
dotOptions = [0 .. 6]

-- just sharps for now
instance Arbitrary PitchClass where
  arbitrary :: Gen PitchClass
  arbitrary = elements [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]

-- instance Arbitrary ValidPitch where
--     arbitrary = do
--         pc <- arbitrary
--         oct <- choose (-1, 9)
--         pure $ ValidPitch (pc, oct)

instance Arbitrary ValidPitch where
  arbitrary = ValidPitch <$> ((,) <$> arbitrary <*> choose (-1, 9))

numOptions :: [Integer]
numOptions = [2 ^ n | n <- [0 .. 10 :: Integer]]

instance Arbitrary SimpleNote where
  arbitrary = do
    pc <- arbitrary
    oct <- choose (1, 7)
    num <- elements numOptions
    deno <- elements durationDenominators
    dots <- elements dotOptions
    let baseDuration = makeDuration num (fromIntegral deno)
        dur_ = applyDots baseDuration dots
    pure $ SimpleNote $ note dur_ (pc, oct)

applyDots :: Dur -> Int -> Dur
applyDots duration n = duration * ((2 ^ n + 1) / (2 ^ n))

instance Arbitrary TestDur where
  arbitrary :: Gen TestDur
  arbitrary = do
    n <- elements numOptions
    deno <- elements durationDenominators
    dotz <- elements dotOptions
    pure . TestDur $ (n % (2 ^ deno)) * ((2 ^ dotz + 1) / (2 ^ dotz))

-- Arbitrary instance for generating primitive music components
instance (Arbitrary a) => Arbitrary (Primitive a) where
  arbitrary = do
    dura <- getDur <$> arbitrary
    oneof [Note dura <$> arbitrary, pure $ Rest dura]

-- Arbitrary instance for generating random music  with valid pitches
instance Arbitrary (Music ValidPitch) where
  arbitrary :: Gen (Music ValidPitch)
  arbitrary = sized arbitraryMusic
    where
      arbitraryMusic 0 = Prim <$> arbitrary
      arbitraryMusic n =
        oneof
          [ Prim <$> arbitrary,
            (:+:) <$> subMusic <*> subMusic,
            (:=:) <$> subMusic <*> subMusic,
            Modify <$> arbitrary <*> subMusic
          ]
        where
          subMusic = arbitraryMusic (n `div` 2)

-- , liftM2 (:+:) subMusic subMusic
-- , liftM2 (:=:) subMusic subMusic

-- Arbitrary instance for generating control modifications
instance Arbitrary Control where
  arbitrary :: Gen Control
  arbitrary =
    oneof
      [ Tempo . getDur <$> arbitrary,
        Transpose <$> choose (-24, 24)
        -- Instrument <$> elements [AcousticGrandPiano, Violin, Flute]
      ]

  -- instance Arbitrary Mode where
  --   arbitrary = arbitraryBoundedEnum
    
-- Arbitrary instance for generating musical modes
-- instance Arbitrary Mode where
  -- arbitrary :: Gen Mode
  -- arbitrary = elements [Major, Minor, Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian]




-- Arbitrary instance for generating non-rest music elements
instance Arbitrary NonRestMusic where
  arbitrary = sized $ \n -> do
    vp <- arbitrary
    durr <- getDur <$> arbitrary
    if n == 0
      then pure $ NonRestMusic $ Prim $ Note durr vp
      else do
        m1 <- resize (n `div` 2) arbitrary
        m2 <- resize (n `div` 2) arbitrary
        oneof
          [ pure $ NonRestMusic $ Prim $ Note durr vp,
            pure $ NonRestMusic $ getMusic m1 :+: getMusic m2,
            pure $ NonRestMusic $ getMusic m1 :=: getMusic m2,
            (Modify <$> arbitrary <*> pure (getMusic m1)) <&> NonRestMusic
          ]

instance Arbitrary (Music Pitch) where
  arbitrary = sized genMusic

genMusic :: Int -> Gen (Music Pitch)
genMusic n =
  if n == 0
    then Prim <$> arbitrary
    else
      oneof
        [ Prim <$> arbitrary,
          combineWith (:+:) halfSizedArb,
          combineWith (:=:) halfSizedArb,
          Modify <$> arbitrary <*> halfSizedArb
        ]
  where
    halfSizedArb :: Gen (Music Pitch)
    halfSizedArb = scale (`div` 2) arbitrary

    combineWith :: (Music Pitch -> Music Pitch -> Music Pitch) -> Gen (Music Pitch) -> Gen (Music Pitch)
    combineWith op arb = liftA2 op arb arb

-- Properties
------------------------

-- Property testing sequential associativity of music composition
prop_SequentialAssociativity :: (Eq a) => Music a -> Music a -> Music a -> Bool
prop_SequentialAssociativity m1 m2 m3 =
  (m1 :+: m2) :+: m3 == m1 :+: (m2 :+: m3)

-- Property testing parallel commutativity of music composition
prop_ParallelCommutativity :: (Eq a) => Music a -> Music a -> Bool
prop_ParallelCommutativity m1 m2 =
  m1 :=: m2 == m2 :=: m1

-- Property checking that rest with zero duration is an identity element
prop_ZeroRestIdentity :: (Eq a) => Music a -> Bool
prop_ZeroRestIdentity m =
  m :+: rest 0 == m && rest 0 :+: m == m

-- Property testing the inverse operation of transpose function
prop_TransposeInverse :: AbsPitch -> Music Pitch -> Bool
prop_TransposeInverse n m =
  (transpose n . transpose (-n)) m == m

-- Property ensuring all durations in music are non-negative
prop_NonNegativeDuration :: Music a -> Bool
prop_NonNegativeDuration m =
  dur m >= 0

-- Property verifying normalization of music removes zero transpositions
prop_NormalizeZeroTransposition :: Music Pitch -> Bool
prop_NormalizeZeroTransposition m =
  normalizeMusic (transpose 0 m) == m

-- Property ensuring proper folding and summation of durations
prop_FoldDurations :: Music a -> Bool
prop_FoldDurations m =
  dur' m == sum (durL m)

-- Property asserting retrograde operation is its own inverse
prop_RetroInverse :: (Eq a) => Music a -> Bool
prop_RetroInverse m =
  retro (retro m) == m

-- Property testing invert operation as its own inverse
prop_InvertInverse :: Music Pitch -> Bool
prop_InvertInverse m =
  invert (invert m) == m

-- Property confirming cutting music by its entire duration yields the same music
prop_CutEntireDuration :: (Eq a) => Music a -> Bool
prop_CutEntireDuration m =
  cut (dur m) m == m

-- Property ensuring valid pitches remain within octave bounds after transpose
prop_ValidPitchAfterTranspose :: ValidPitch -> AbsPitch -> Bool
prop_ValidPitchAfterTranspose (ValidPitch p) n =
  let limitedN = limitNToOctave n
      (_, octave) = trans limitedN p
   in isValidOctave octave

-- Helper function to limit the range of n
limitNToOctave :: AbsPitch -> Int
limitNToOctave n = n `mod` 24 - 12

-- Helper function to check if the octave is within the valid range
isValidOctave :: Int -> Bool
isValidOctave o = o >= -1 && o <= 9

------------------------
-- Test Specifications
------------------------

baseTests :: SpecWith ()
baseTests = describe "Basic Pitch Operations" $ do
  let absPitchTestCases =
        [ ((C, 4), 60),
          ((A, 4), 69),
          ((C, 5), 72),
          ((Cs, 5), 73),
          ((B, 3), 59),
          ((As, 3), 58),
          ((G, 3), 55)
        ]

      pitchTestCases =
        [ (60, (C, 4)),
          (69, (A, 4)),
          (72, (C, 5)),
          (73, (Cs, 5)),
          (59, (B, 3)),
          (46, (As, 2))
        ]

      transTestCases =
        [ (2, (C, 4), (D, 4)),
          (12, (C, 4), (C, 5)),
          (-1, (C, 4), (B, 3))
        ]

  describe "absPitch" $
    it "converts a pitch to an absolute pitch number" $
      mapM_ (\(input, expected) -> absPitch input `shouldBe` expected) absPitchTestCases

  describe "pitch" $
    it "returns the correct Pitch for a given AbsPitch" $
      mapM_ (\(input, expected) -> pitch input `shouldBe` expected) pitchTestCases

  describe "trans" $
    it "transposes a pitch by a specified number of semitones" $
      mapM_ (\(semitones, input, expected) -> trans semitones input `shouldBe` expected) transTestCases

-- Correcting the type used in createNote:
createNote :: Dur -> PitchClass -> Music Pitch
createNote dura pitchClass = note dura (pitchClass, 4 :: Int)

-- Specification of tests for music structure operations
structureTests :: SpecWith ()
structureTests = describe "Music Structure Operations" $ do
  -- Transpose operation tests
  describe "transpose" $ do
    it "transposes music correctly" $ do
      evaluatePitch (transpose 2 (createNote qn C)) `shouldBe` [(D, 4)]
      evaluatePitch (transpose 2 (createNote qn C :+: createNote qn E)) `shouldBe` [(D, 4), (Fs, 4)]

  -- Duration calculation tests
  describe "duration" $ do
    it "calculates durations accurately" $ do
      dur (createNote qn C) `shouldBe` 1 / 4
      dur (createNote qn C :+: note hn (D, 4 :: Int)) `shouldBe` 3 / 4
      dur (createNote qn C :=: note hn (D, 4 :: Int)) `shouldBe` 1 / 2

  -- Tempo modification tests
  describe "tempo" $ it "modifies the duration of notes correctly" $ do
    dur (tempo 2 (createNote qn C)) `shouldBe` 1 / 8
    dur (tempo 0.5 (createNote qn C)) `shouldBe` 1 / 2

-- Specification of tests for music manipulation functions
manipulationTests :: SpecWith ()
manipulationTests = describe "Music Manipulation" $ do
  -- Cut operation tests
  describe "cut" $ do
    it "truncates music to specified duration" $ dur (cut (1 / 8) (note qn (C, 4 :: Integer))) `shouldBe` 1 / 8

    it "handles sequential composition" $ do
      let music = note qn (C, 4 :: Integer) :+: note qn (D, 4)
      dur (cut (3 / 8) music) `shouldBe` 3 / 8

  -- Remove operation tests
  describe "remove" $ do
    it "removes duration from the beginning" $ dur (remove (1 / 8) (note qn (C, 4 :: Integer))) `shouldBe` 1 / 8

    it "handles sequential composition" $ do
      let music = note qn (C, 4 :: Integer) :+: note qn (D, 4)
      dur (remove (1 / 8) music) `shouldBe` 3 / 8

    it "another test" $ do
      let music = note hn (C, 4 :: Integer) :+: note hn (D, 4)
      dur (remove (1 / 4) music) `shouldBe` 3 / 4

-- Specification of tests for combinators in music
combinatorTests :: SpecWith ()
combinatorTests = describe "Music Combinators" $ do
  describe "line" $ it "combines notes sequentially" $ do
    let notes = [note qn (C, 4 :: Integer), note qn (D, 4)]
    dur (line notes) `shouldBe` 1 / 2

  describe "chord" $ it "combines notes in parallel" $ do
    let notes = [note qn (C, 4 :: Integer), note hn (E, 4)]
    dur (chord notes) `shouldBe` 1 / 2

transformationTests :: SpecWith ()
transformationTests = describe "Music Transformations" $ do
  describe "retro" $ it "reverses the order of notes" $ do
    let music = note qn (C, 4) :+: note qn (D, 4)
    evaluatePitch (retro music) `shouldBe` [(D, 4), (C, 4)]

  describe "invert" $ it "inverts the pitch of notes" $ do
    let music = note qn (C, 4) :+: note qn (D, 4)
    evaluatePitch (invert music) `shouldBe` [(C, 4), (As, 3)]

------------------------
-- Property Tests
------------------------

propertyTests :: SpecWith ()
propertyTests = describe "QuickCheck Properties" $ do
  it "pitch . absPitch is identity for valid pitches" $
    propertyWith $
      \(ValidPitch p) -> pitch (absPitch p) == p

  it "duration is always non-negative" $
    propertyWith $
      \(m :: Music Pitch) -> dur m >= 0

  it "transpose 0 preserves structure" $
    propertyWith $ \m ->
      counterexample
        ( "Original: "
            <> show m
            <> "\n"
            <> "Transposed: "
            <> show (transpose 0 m)
            <> "\n"
            <> "Normalized transposed: "
            <> show (normalizeMusic (transpose 0 m))
        )
        (sameMusic m (transpose 0 m))

  describe "Music normalization" $ do
    it "removes zero transposition" $ do
      let m = note qn (C, 4)
          transposed = transpose 0 m
      normalizeMusic transposed `shouldBe` m

    it "preserves non-zero transposition" $ do
      let m = note qn (C, 4)
          transposed = transpose 1 m
      normalizeMusic transposed `shouldBe` transposed

    it "normalizes nested zero transpositions" $ do
      let m = note qn (C, 4)
          nested = transpose 0 (transpose 0 m)
      normalizeMusic nested `shouldBe` m

  describe "Basic Music Properties" $ do
    it "pitch . absPitch is identity for valid pitches" $
      propertyWith $ \(ValidPitch p) ->
        pitch (absPitch p) == p

    it "duration is always non-negative" $
      propertyWith $ \(SimpleNote m) ->
        dur m >= 0

    it "trans is reversible" $
      propertyWith $ \(ValidPitch p) n ->
        trans (-n) (trans n p) == p

  describe "Transformation Properties" $ do
    it "transpose preserves duration" $
      propertyWith $ \(m :: NonRestMusic) (n :: Int) ->
        let music = getMusic m
            converted = fromValidPitch music
         in dur converted == dur (transpose n converted)

    it "retro preserves duration" $
      propertyWith $ \m ->
        let music = getMusic m :: Music ValidPitch
            converted = fromValidPitch music
         in dur converted == dur (retro converted)

    it "retro is its own inverse" $
      propertyWith $ \m ->
        let music = getMusic m :: Music ValidPitch
            converted = fromValidPitch music
         in sameMusic converted (retroFixed (retroFixed converted))

    it "invert preserves duration" $
      propertyWith $ \m ->
        let music = getMusic m :: Music ValidPitch
            converted = fromValidPitch music
         in dur converted == dur (invert converted)

  describe "Cut and Remove Properties" $ do
    it "cut preserves or reduces duration" $
      propertyWith $ \m du ->
        let music = getMusic m :: Music ValidPitch
            converted = fromValidPitch music
            d' = abs du
         in dur (cut d' converted) <= min d' (dur converted)

    it "remove preserves structure of remaining music" $
      propertyWith $ \m du ->
        let music = getMusic m :: Music ValidPitch
            converted = fromValidPitch music
            d' = abs du
         in dur (remove d' converted) == max 0 (dur converted - d')

  describe "Tempo Properties" $ do
    it "tempo scaling is multiplicative" $
      propertyWith $ \m r1 r2 ->
        r1 > 0
          && r2
            > 0
          ==> let music = getMusic m :: Music ValidPitch
                  converted = fromValidPitch music
               in abs
                    ( dur (tempo r1 (tempo r2 converted))
                        - dur (tempo (r1 * r2) converted)
                    )
                    < 0.000001

  describe "Musical Laws" $ do
    it "line preserves total duration" $
      propertyWith $ \m1 m2 ->
        let music1 = getMusic m1 :: Music ValidPitch
            music2 = getMusic m2
            convert = fromValidPitch
         in dur (line [convert music1, convert music2])
              == dur (convert music1) + dur (convert music2)

    it "chord takes maximum duration" $
      propertyWith $ \m1 m2 ->
        let music1 = getMusic m1 :: Music ValidPitch
            music2 = getMusic m2
            convert = fromValidPitch
         in dur (chord [convert music1, convert music2])
              == max (dur (convert music1)) (dur (convert music2))

------------------------
-- Main Test Runner
------------------------

main :: IO ()
main = hspec $ do
  baseTests
  structureTests
  manipulationTests
  combinatorTests
  transformationTests
  propertyTests

------------------------
-- Fixes
------------------------

retroFixed :: Music a -> Music a
retroFixed = \case
  --  parallel composition (already provided)
  m1 :=: m2 -> retroFixed m1 :=: retroFixed m2
  --  primitive music elements
  prim@(Prim _) -> prim
  --  sequential composition
  -- retrograde
  m1 :+: m2 -> retroFixed m2 :+: retroFixed m1
  --  modified music elements
  Modify control m -> Modify control (retroFixed m)
