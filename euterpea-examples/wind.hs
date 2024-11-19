{-# LANGUAGE Arrows #-}

-- | Code by Donya Quick
-- https://github.com/Euterpea/Euterpea2-Examples
module Main where

import Euterpea
import System.Environment
import System.Random

randomPoints :: StdGen -> Double -> Double -> Double -> Double -> [Double]
randomPoints g lastY maxDelta limitL limitU =
  let (r, g') = randomR (-maxDelta, maxDelta) g -- generate an amount to move
      nextY = min limitU (max (lastY + r) limitL) -- stay within lower/upper bounds
   in lastY : randomPoints g' nextY maxDelta limitL limitU
  where

randEnv :: Double -> Double -> Double -> Double -> Double -> Double -> StdGen -> AudSF () Double
randEnv secs spacing y0 maxDelta limitL limitU g =
  let n = ceiling (secs / spacing) -- how many envelope points do we need? (Must be finite)
      ys = take n $ randomPoints g y0 maxDelta limitL limitU
      xs = take (n - 1) $ repeat spacing
   in proc () -> do
        y <- envLineSeg ys xs -< ()
        returnA -< y

splitN :: StdGen -> [StdGen]
splitN g = let (g1, g2) = split g in g1 : splitN g2

wind :: Double -> StdGen -> AudSF () Double
wind secs g =
  let gs = splitN g
   in proc _ -> do
        n <- noiseWhite (fst $ next (gs !! 0)) -< () -- white noise source
        vEnv <- randEnv secs 0.05 0 0.1 (0.1) 1.0 (gs !! 2) -< () -- volume envelope
        bpF <- randEnv secs 0.05 2000 50 100 5000 (gs !! 3) -< () -- bandpass center frequency
        bw <- randEnv secs 0.05 50 5 5 100 (gs !! 4) -< () -- bandpass bandwidth
        nBP <- filterBandPassBW -< (n, bpF, bw) -- filtered noise
        returnA -< vEnv * nBP

genWind :: StdGen -> Double -> FilePath -> IO ()
genWind g seconds filename = outFile filename seconds (wind seconds g)

main = do
  args <- getArgs
  rio <- randomIO
  let r = if length args == 3 then read (args !! 2) else rio
      g = mkStdGen r
  putStr ("Random seed: " ++ show r)
  if length args >= 2
    then do
      let secs = read (args !! 0) :: Double
      genWind g secs (args !! 1)
    else genWind g 30.0 "wind.wav"
