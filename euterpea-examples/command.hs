import Euterpea
import Control.Monad (join)

data Command a = Add a
               | Use1 Int (a -> a)
               | Use2 Int Int (a -> a -> a)
               deriving Show

run :: [Command a] -> [a] -> Maybe [a]
run [] l' = Just l'
run ((Add x):l) l' = run l (x:l')
run ((Use1 i f):l) l' | i < 0 || i >= length l' = Nothing
                      | otherwise = run l l' >>= (\r -> return $ f (l'!!i):r)
run ((Use2 i j f):l) l' | i < 0 || j < 0 || i >= length l' || j >= length l' = Nothing
                        | otherwise = run l l' >>= (\r -> return $ f (l'!!i) (l'!!j):r)

transposeUp :: Music Pitch -> Music Pitch
transposeUp = transpose 12

transposeDown :: Music Pitch -> Music Pitch
transposeDown = transpose (-12) 

speedUp :: Music Pitch -> Music Pitch
speedUp = tempo 2.0

slowDown :: Music Pitch -> Music Pitch
slowDown = tempo 0.5 

reverseMusic :: Music Pitch -> Music Pitch
reverseMusic = retro 

simpleExample :: Maybe [Music Pitch]
simpleExample = run [
    Add (line [c 4 qn, d 4 qn, e 4 qn, f 4 qn, g 4 qn, a 4 qn, b 4 qn, c 5 qn]),
    Use1 0 transposeUp,
    Use1 1 id,
    Use2 0 1 (:=:)
    ] []

roundExample :: Maybe [Music Pitch]
roundExample = let
    melody = line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]
    in run [
        Add melody,
        Use1 0 (delay (1/2)),
        Use1 1 (delay 1),
        Use2 0 1 (:=:),
        Use2 0 2 (:=:)
    ] []

themeAndVariations :: Maybe [Music Pitch]
themeAndVariations = let
    theme = line [c 4 qn, e 4 qn, g 4 qn, e 4 qn]
    in run [
        Add theme,                            -- Original theme
        Use1 0 transposeUp,                   -- Variation 1: octave up
        Use1 1 reverseMusic,                  -- Variation 2: reversed
        Use1 2 speedUp,                       -- Variation 3: faster
        Use1 0 (instrument Flute),            -- Change instrument
        Use2 3 4 (:+:),
        Use2 2 5 (:+:),
        Use2 1 6 (:+:)
    ] []

layeredExample :: Maybe [Music Pitch]
layeredExample = let
    melody = line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]
    bass = line [c 3 hn, g 3 hn]
    in run [
        Add melody,
        Add bass,
        Use1 0 (instrument Violin),
        Use1 1 (instrument Bass),
        Use1 2 (transpose 4),
        Use2 2 3 (:=:),
        Use2 1 4 (:=:)
    ] []

patternExample :: Maybe [Music Pitch]
patternExample = let
    motif = line [c 4 en, d 4 en, e 4 en, g 4 en]
    in run [
        Add motif,
        Use1 0 transposeUp,
        Use1 1 reverseMusic,
        Use1 0 speedUp,
        Use2 2 3 (:+:),
        Use2 1 4 (:=:),
        Use1 5 (delay 0.25),
        Use2 5 6 (:=:)
    ] []

chordProgression :: Maybe [Music Pitch]
chordProgression = let
    makeChord root = chord [root, transpose 4 root, transpose 7 root]
    in run [
        -- Create basic chords
        Add (makeChord (c 4 hn)),    -- C major
        Add (makeChord (g 3 hn)),    -- G major
        Add (makeChord (a 3 hn)),    -- A minor
        Add (makeChord (f 3 hn)),    -- F major
        -- Combine them sequentially
        Use2 2 3 (:+:),
        Use2 1 4 (:+:),
        Use2 0 5 (:+:),
        -- Add a bass line
        Add (line [c 3 hn, g 2 hn, a 2 hn, f 2 hn]),
        -- Combine with chords
        Use2 6 7 (:=:)
    ] []

-- Helper function to play the result
playResult :: Maybe [Music Pitch] -> IO ()
playResult Nothing = putStrLn "Error in music composition"
playResult (Just (m:_)) = play m
playResult (Just []) = putStrLn "Empty composition"

-- Example usage of all patterns together
complexComposition :: Maybe [Music Pitch]
complexComposition = run [
    -- Start with chord progression
    Add (chord [c 4 qn, e 4 qn, g 4 qn]),
    Add (chord [g 3 qn, b 3 qn, d 4 qn]),
    Use2 0 1 (:+:),
    -- Add melody on top
    Add (line [c 5 qn, d 5 qn, e 5 qn, g 5 qn]),
    Use1 3 (instrument Flute),
    -- Combine melody with chords
    Use2 2 4 (:=:),
    -- Add bass line
    Add (line [c 3 hn, g 2 hn]),
    Use1 6 (instrument Bass),
    -- Combine everything
    Use2 5 7 (:=:),
    -- Create variations
    Use1 8 (delay 0.5),
    Use1 9 transposeUp,
    -- Final combination
    Use2 9 10 (:=:)
] []