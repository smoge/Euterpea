{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Criterion.Main
import Criterion.Types
import Euterpea.Music  
import Control.DeepSeq
import Control.Exception (evaluate)
import GHC.Generics (Generic)
import Data.List (foldl')


-- Sample music pieces
sampleMelody :: Music Pitch
sampleMelody = line [c 4 qn, g 4 qn, e 4 qn, f 4 qn, g 4 qn, a 4 qn, b 4 qn, c 5 qn]

complexPiece :: Music Pitch
complexPiece = let
    melody = line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]
    harmony = chord [c 3 wn, e 3 wn, g 3 wn]
    bass = line [c 2 hn, g 2 hn]
    in tempo 1.2 $ melody :=: harmony :=: bass

longPiece :: Music Pitch
longPiece = times 100 sampleMelody

parallelPiece :: Music Pitch
parallelPiece = foldl1 (:=:) $ replicate 10 sampleMelody

-- Helper for debugging evaluation
debugEvaluation :: (NFData a, Show a) => String -> a -> IO ()
debugEvaluation label x = do
    putStrLn $ "Starting evaluation of: " ++ label
    evaluated <- evaluate (force x)
    putStrLn $ "Completed evaluation of: " ++ label
    putStrLn $ "Result: " ++ show evaluated
    putStrLn "------------------------"

-- Benchmark groups
basicOperations :: Benchmark
basicOperations = bgroup "Basic Operations"
    [ bench "dur/simple" $ nf dur sampleMelody
    , bench "dur/complex" $ nf dur complexPiece
    , bench "dur/parallel" $ nf dur parallelPiece
    ]

transformations :: Benchmark
transformations = bgroup "Transformations"
    [ bench "transpose/simple" $ nf (transpose 12) sampleMelody
    , bench "transpose/complex" $ nf (transpose 12) complexPiece
    , bench "retro/simple" $ nf retro sampleMelody
    , bench "retro/complex" $ nf retro complexPiece
    ]

compositionOps :: Benchmark
compositionOps = bgroup "Composition Operations"
    [ bench "sequential" $ nf (sampleMelody :+:) sampleMelody
    , bench "parallel" $ nf (sampleMelody :=:) sampleMelody
    , bench "chord" $ nf chord [c 4 qn, e 4 qn, g 4 qn]
    , bench "line" $ nf line [c 4 qn, f 4 qn, e 4 qn, f 4 qn]
    ]

complexOps :: Benchmark
complexOps = bgroup "Complex Operations"
    [ bench "cut" $ nf (cut 2.0) longPiece
    , bench "remove" $ nf (remove 1.0) longPiece
    , bench "removeZeros" $ nf removeZeros complexPiece
    ]

main :: IO ()
main = do
    putStrLn "Testing evaluation of sample pieces..."
    mapM_ (uncurry debugEvaluation)
        [ ("Sample Melody", sampleMelody)
        , ("Complex Piece", complexPiece)
        , ("Long Piece (start)", line $ take 5 $ lineToList longPiece)
        ]

    defaultMainWith config
        [ basicOperations
        , transformations
        , compositionOps
        , complexOps
        ]
  where
    config = defaultConfig
        { reportFile = Just "music-benchmarks.html"
        , csvFile = Just "music-benchmarks.csv"
        , timeLimit = 3.0
        , resamples = 100
        , verbosity = Normal
        }