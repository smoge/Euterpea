{-# LANGUAGE Arrows #-}

module Main where

import Euterpea
import System.Environment
import System.Random
import Data.List (unfoldr)
import Control.Arrow
import Data.Function ((&))

-- Generate an infinite list of random points with constraints.
randomPoints :: StdGen -> Double -> Double -> Double -> Double -> [Double]
randomPoints g lastY maxDelta limitL limitU = 
  iterate (\(y, gen) -> let (r, nextG) = randomR (-maxDelta, maxDelta) gen
                        in (clamp (y + r) limitL limitU, nextG)) (lastY, g)
  & map fst
  where
    -- Clamp the value x between low and high limits.
    clamp x low high = max low (min x high)

-- Infinite list generation of split random generators.
splitN :: StdGen -> [StdGen]
splitN = unfoldr (Just . split)

-- Create a random envelope for modulation using specified parameters.
randEnv :: Double -> Double -> Double -> Double -> Double -> Double -> StdGen -> AudSF () Double
randEnv secs spacing y0 maxDelta limitL limitU g =
  let n = ceiling (secs / spacing)
      ys = take n $ randomPoints g y0 maxDelta limitL limitU
      xs = replicate (n - 1) spacing
  in proc () -> do
       envLineSeg ys xs -< ()

-- Wind parameters data type to specify different wind characteristics.
data WindParams = WindParams {
    volumeRange :: (Double, Double),   -- Range for volume modulation
    freqRange :: (Double, Double),     -- Range for frequency modulation
    bandwidthRange :: (Double, Double),-- Range for bandwidth modulation
    modSpeed :: Double                 -- Speed of modulation changes
}

-- Predefined wind parameter sets for various wind types.
highWindParams, lowWindParams, midWindParams, gustyWindParams :: WindParams
highWindParams = WindParams (0.1, 0.8) (2000, 15000) (50, 200) 0.9
lowWindParams = WindParams (0.1, 0.6) (50, 600) (20, 80) 0.18
midWindParams = WindParams (0.1, 0.4) (80, 2500) (30, 120) 0.9
gustyWindParams = WindParams (0.0, 1.0) (150, 19000) (100, 300) 0.32

-- Generate wind sound based on given parameters and random generator.
windWithParams :: Double -> WindParams -> StdGen -> AudSF () Double
windWithParams secs params g =
  let (volMin, volMax) = volumeRange params
      (freqMin, freqMax) = freqRange params
      (bwMin, bwMax) = bandwidthRange params
      spacing = modSpeed params
      [gNoise, gVolEnv, gBpF, gBw] = take 4 $ splitN g
      avg x y = (x + y) / 2
  in proc _ -> do
       n <- noiseWhite (fst $ next gNoise) -< ()
       vEnv <- randEnv secs spacing 0 0.1 volMin volMax gVolEnv -< ()
       bpF <- randEnv secs spacing (avg freqMin freqMax) 50 freqMin freqMax gBpF -< ()
       bw <- randEnv secs spacing (avg bwMin bwMax) 5 bwMin bwMax gBw -< ()
       nBP <- filterBandPassBW -< (n, bpF, bw)
       returnA -< vEnv * nBP

-- Mix two audio signals by averaging their amplitudes.
mixTwo :: AudSF () Double -> AudSF () Double -> AudSF () Double
mixTwo sf1 sf2 = proc () -> do
    (x1, x2) <- (sf1 &&& sf2) -< ()
    returnA -< (x1 + x2) * 0.5

-- Scale a wind stream by a specified volume factor.
scaleWind :: Double -> AudSF () Double -> AudSF () Double
scaleWind vol w = proc () -> do
    x <- w -< ()
    returnA -< vol * x

-- Mix multiple wind streams together, scaling each by its provided volume.
mixWinds :: [(Double, AudSF () Double)] -> AudSF () Double
mixWinds [] = proc () -> returnA -< 0.0
mixWinds [single] = uncurry scaleWind single
mixWinds (x:xs) = proc () -> do
    first <- uncurry scaleWind x -< ()
    rest <- mixWinds xs -< ()
    -- Calculate average of mixed signals
    let count = fromIntegral (length xs + 1)
    returnA -< (first + rest) / count

-- Generate layered wind sound and save it to a file.
genLayeredWind :: StdGen -> Double -> FilePath -> IO ()
genLayeredWind g0 seconds filename = do
    -- Split the generator for different wind layers.
    let [g3, g4, g5, g6] = take 4 $ splitN g0

    -- Create different wind layers with individual characteristics.
    let winds = [ (0.7, windWithParams seconds highWindParams g3)  -- High whistling winds
                , (1.0, windWithParams seconds lowWindParams g4)   -- Low rumbling winds
                , (0.8, windWithParams seconds midWindParams g5)   -- Mid-range winds
                , (0.5, windWithParams seconds gustyWindParams g6) -- Gusty wind bursts
                ]

    -- Mix all created wind layers into one.
    let mixedWind = mixWinds winds

    -- Write the generated sound to the specified file.
    outFile filename seconds mixedWind

main :: IO ()
main = do
    args <- getArgs
    rio <- randomIO
    
    -- Determine random seed based on arguments or system random number.
    let seed = if length args == 3 
               then read (args !! 2) 
               else rio
        g = mkStdGen seed
    
    putStrLn $ "Using random seed: " ++ show seed

    -- Determine duration in seconds, defaulting to 30 if not provided.
    let secs = if not (null args) then read (head args) else 30.0

    if length args >= 2
        then do
            putStrLn $ "Generating " ++ show secs ++ " seconds of layered wind..."
            genLayeredWind g secs (args !! 1)
            putStrLn "Done!"
        else putStrLn "Usage: <program> <seconds> <output-file> [seed]"
