{-# LANGUAGE Arrows #-}

module Main where


import Control.Arrow
import Euterpea
import System.Directory (createDirectoryIfMissing)

-- Define a sine wave table with 4096 samples
sineTable :: Table
sineTable = tableSinesN 4096 [1]

-- Defines the bell instrument using additive synthesis with multiple oscillators
bellInstr :: Instr (AudSF () Double)
bellInstr dur ap vol pfields =
  let dur' = fromRational dur -- Convert duration to a rational number
      f = apToHz ap -- Convert pitch to frequency
   in proc () -> do
        -- Create oscillators with different frequency multipliers
        x1 <- osc sineTable 0 -< f
        x2 <- osc sineTable 0 -< f * 4.1
        x3 <- osc sineTable 0 -< f * 6.05
        x4 <- osc sineTable 0 -< f * 8.2

        -- Envelopes for amplitude modulation of each oscillator
        env1 <- envLineSeg [1.0, 0.2, 0, 0] [1, 2, 100] -< ()
        env2 <- envLineSeg [0, 0.8, 0, 0] [0.05, 2, 100] -< ()
        env3 <- envLineSeg [0, 0.5, 0, 0] [0.08, 2, 100] -< ()
        env4 <- envLineSeg [0, 0.3, 0, 0] [0.015, 1, 100] -< ()

        -- Additional envelopes for final sound shaping
        envx1 <- envLineSeg [0, 1, 1, 0] [0.0001 * dur', 0.9999 * dur', 0.0001 * dur'] -< ()
        envx2 <- envLineSeg [1, 0.5, 0.2, 0, 0] [0.05, 0.2, 3, 100] -< ()

        -- Mix partials and apply the final envelope
        let envs = envx2
            partials = ((x1 * env1) + (x2 * env2) + (x3 * env3) + (x4 * env4)) / 4
        outA -< 0.95 * envs * partials

-- Define the custom instrument name
bellName :: InstrumentName
bellName = CustomInstrument "Bell Instrument"

-- Map the custom instrument name to its definition
myInstrMap :: InstrMap (AudSF () Double)
myInstrMap = [(bellName, bellInstr)]

-- Function to write a single bell note to a WAV file
bellNote =
  writeWav
    "bellNote.wav"
    myInstrMap
    (tempo 0.5 $ transpose 12 $ instrument bellName (g 5 wn))

-- Define a melody consisting of a scale
mel1 = toMusic1 $ line $ map ($ en) [c 5, d 5, e 5, f 5, g 5, a 5, b 5, c 6]

-- Write the melody to a WAV file with truncated note durations
bellMel =
  writeWav
    "bellMel.wav"
    myInstrMap
    (tempo 0.5 $ transpose 12 $ instrument bellName mel1)

-- Write the melody to a WAV file allowing notes to resonate
bellMel2 =
  writeWav
    "bellMel2.wav"
    myInstrMap
    (parMod wn $ tempo 0.5 $ transpose 12 $ instrument bellName mel1)

-- Modify music to parallelize notes and extend their durations
parMod d (Prim (Rest d')) = rest d
parMod d (Prim (Note d' x)) = note (d + d') x
parMod d (m1 :+: m2) = parMod d m1 :=: (rest (dur m1) :+: parMod d m2) :=: (rest (dur m1) :+: parMod d m2)
parMod d (m1 :=: m2) = parMod d m1 :=: parMod d m2
parMod d (Modify c m) = Modify c $ parMod d m

-- Main function to manage audio file generation
main :: IO ()
main = do
  -- Ensure output directory exists
  createDirectoryIfMissing True "output"

  -- Generate and log the creation of WAV files

  -- Single bell note
  bellNote
  putStrLn "Created bellNote.wav"

  -- Melody with truncated notes
  bellMel
  putStrLn "Created bellMel.wav"

  -- Melody allowing note resonance
  bellMel2
  putStrLn "Created bellMel2.wav"

  -- Generate a chord of bells as additional demonstration
  let bellChord =
        writeWav
          "output/bellChord.wav"
          myInstrMap
          ( tempo 0.5 $
              transpose 12 $
                instrument
                  bellName
                  (c 5 wn :=: e 5 wn :=: g 5 wn)
          )

  bellChord
  putStrLn "Created bellChord.wav"

  putStrLn "All wave files have been generated successfully!"
