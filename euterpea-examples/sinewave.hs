{-# LANGUAGE Arrows #-}

-- | Code by Donya Quick
-- https://github.com/Euterpea/Euterpea2-Examples
module SineDemos where

import Euterpea

sineTable :: Table
sineTable = tableSinesN 4096 [1]

sine440 :: AudSF () Double
sine440 = proc _ -> do
  y <- osc sineTable 0 -< 440
  returnA -< y

testSine440 :: IO ()
testSine440 = outFile "sine440.wav" 2.0 sine440

sineInstr :: Instr (Mono AudRate)
sineInstr dur pch vol params =
  let freq = apToHz pch
   in proc _ -> do
        y <- osc sineTable 0 -< freq
        returnA -< y

sineName, sineName2 :: InstrumentName
sineName = CustomInstrument "Sine"
sineName2 = CustomInstrument "Sine2"

instrMap :: InstrMap (Mono AudRate)
instrMap = [(sineName, sineInstr), (sineName2, sineInstr2)]

myMel =
  instrument sineName $
    line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]

testMyMel = writeWav "mymel.wav" instrMap myMel

sineInstr2 :: Instr (Mono AudRate)
sineInstr2 dur pch vol params =
  let freq = apToHz pch
      d = fromRational dur
   in proc _ -> do
        y <- osc sineTable 0 -< freq
        e <- envLineSeg [0, 1, 1, 0] [0.01 * d, 0.94 * d, 0.05 * d] -< ()
        returnA -< y * e

myMel2 =
  instrument sineName2 $
    line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]

testMyMel2 = writeWav "mymel2.wav" instrMap myMel2
