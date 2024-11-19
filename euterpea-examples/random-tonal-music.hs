-- https://github.com/Euterpea/Euterpea2-Examples/blob/master/NoteLevel/RandomTonalMusic.lhs

import Euterpea
import System.Random

choose :: [a] -> StdGen -> (a, StdGen)
choose [] g = error "Nothing to choose from!"
choose xs g =
  let (i, g') = next g
   in (xs !! (i `mod` length xs), g')

randomMel :: [AbsPitch] -> [Dur] -> Volume -> StdGen -> Music (AbsPitch, Volume)
randomMel pitches durs thresh g0 =
  let (p, g1) = choose pitches g0
      (d, g2) = choose durs g1
      (v, g3) = randomR (0, 127) g2
      x = if v < thresh then rest d else note d (p, v)
   in x :+: randomMel pitches durs thresh g3

pitches1, pitches2 :: [AbsPitch]
pitches1 = [60, 62, 63, 65, 67, 68, 70, 72, 74, 75, 77, 79] -- C-minor
pitches2 = [36, 43, 46, 48] -- also C-minor (root, 5th, 7th, root)

mel1, mel2, duet :: Music (AbsPitch, Volume)
mel1 = randomMel pitches1 [qn, en, en, en] 40 (mkStdGen 500)
mel2 = randomMel pitches2 [hn, qn, qn, qn] 20 (mkStdGen 501)
duet = tempo 1.75 (instrument Celesta mel1 :=: instrument AcousticBass mel2)

main = play duet
