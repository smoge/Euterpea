{-# LANGUAGE Arrows #-}

module Main where

import Euterpea
import System.Environment
import System.Random
import Data.List (unfoldr)
import Control.Arrow
import Data.Function ((&))

-- Optimized randomPoints using iterate for infinite list generation
randomPoints :: StdGen -> Double -> Double -> Double -> Double -> [Double]
randomPoints g lastY maxDelta limitL limitU = 
  iterate (\(y, gen) -> let (r, nextG) = randomR (-maxDelta, maxDelta) gen
                            newY = min limitU (max (y + r) limitL)
                        in (newY, nextG)) (lastY, g)
  & map fst

-- Improved splitN to produce an infinite list of StdGens more concisely
splitN :: StdGen -> [StdGen]
splitN = unfoldr (Just . split)

randEnv :: Double -> Double -> Double -> Double -> Double -> Double -> StdGen -> AudSF () Double
randEnv secs spacing y0 maxDelta limitL limitU g =
  let n = ceiling (secs / spacing)
      ys = take n $ randomPoints g y0 maxDelta limitL limitU
      xs = replicate (n - 1) spacing
  in proc () -> do
       y <- envLineSeg ys xs -< ()
       returnA -< y

data WindParams = WindParams {
    volumeRange :: (Double, Double),
    freqRange :: (Double, Double),
    bandwidthRange :: (Double, Double),
    modSpeed :: Double
}

highWindParams, lowWindParams, midWindParams, gustyWindParams :: WindParams
highWindParams = WindParams (0.1, 0.8) (2000, 15000) (50, 200) 0.9
lowWindParams = WindParams (0.1, 0.6) (50, 600) (20, 80) 0.18
midWindParams = WindParams (0.1, 0.4) (80, 2500) (30, 120) 0.9
gustyWindParams = WindParams (0.0, 1.0) (150, 19000) (100, 300) 0.32

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


-- Mix two audio signals together
mixTwo :: AudSF () Double -> AudSF () Double -> AudSF () Double
mixTwo sf1 sf2 = proc () -> do
    (x1, x2) <- (sf1 &&& sf2) -< ()
    returnA -< (x1 + x2) * 0.5

-- Scale a wind stream by volume
scaleWind :: Double -> AudSF () Double -> AudSF () Double
scaleWind vol w = proc () -> do
    x <- w -< ()
    returnA -< vol * x

-- Mix multiple wind streams together
mixWinds :: [(Double, AudSF () Double)] -> AudSF () Double
mixWinds [] = proc () -> returnA -< 0.0
mixWinds [single] = uncurry scaleWind single
mixWinds (x:xs) = proc () -> do
    first <- (uncurry scaleWind x) -< ()
    rest <- mixWinds xs -< ()
    let count = fromIntegral (length xs + 1)
    returnA -< (first + rest) / count

-- Generate layered wind and save to file
genLayeredWind :: StdGen -> Double -> FilePath -> IO ()
genLayeredWind g0 seconds filename = 
    let -- Split the generator for different layers
        (g1, g2) = split g0
        (g3, g4) = split g1
        (g5, g6) = split g2
        
        -- Create wind layers with different characteristics
        winds = [ (0.7, windWithParams seconds highWindParams g3)  -- High whistling winds
                , (1.0, windWithParams seconds lowWindParams g4)   -- Low rumbling winds
                , (0.8, windWithParams seconds midWindParams g5)   -- Mid-range winds
                , (0.5, windWithParams seconds gustyWindParams g6) -- Gusty wind bursts
                ]
        
        -- Mix all wind layers
        mixedWind = mixWinds winds
        
    in outFile filename seconds mixedWind

main :: IO ()
main = do
    args <- getArgs
    rio <- randomIO
    
    let r = if length args == 3 
            then read (args !! 2) 
            else rio
        g = mkStdGen r
    
    putStrLn $ "Using random seed: " ++ show r

    if length args >= 2
        then do
            let secs = read (head args) :: Double
            putStrLn $ "Generating " ++ show secs ++ " seconds of layered wind..."
            genLayeredWind g secs (args !! 1)
            putStrLn "Done!"
        else do
            putStrLn "Generating 30 seconds of layered wind..."
            genLayeredWind g 30.0 "layered-wind.wav"
            putStrLn "Done!"