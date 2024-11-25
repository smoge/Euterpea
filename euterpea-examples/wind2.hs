{-# LANGUAGE Arrows #-}

module Main where

import Euterpea
import System.Environment
import System.Random

-- Keep all the previous helper functions and data types unchanged until windWithParams
randomPoints :: StdGen -> Double -> Double -> Double -> Double -> [Double]
randomPoints g lastY maxDelta limitL limitU =
    let (r, g') = randomR (-maxDelta, maxDelta) g
        nextY = min limitU (max (lastY + r) limitL)
     in lastY : randomPoints g' nextY maxDelta limitL limitU

splitN :: StdGen -> [StdGen]
splitN g =
    let (g1, g2) = split g
     in g1 : splitN g2

randEnv :: Double -> Double -> Double -> Double -> Double -> Double -> StdGen -> AudSF () Double
randEnv secs spacing y0 maxDelta limitL limitU g =
    let n = ceiling (secs / spacing)
        ys = take n $ randomPoints g y0 maxDelta limitL limitU
        xs = replicate (n - 1) spacing
     in proc () -> do
            y <- envLineSeg ys xs -< ()
            returnA -< y

data WindParams = WindParams
    { volumeRange :: (Double, Double)
    , freqRange :: (Double, Double)
    , bandwidthRange :: (Double, Double)
    , modSpeed :: Double
    }

highWindParams :: WindParams
highWindParams =
    WindParams
        { volumeRange = (0.1, 0.8)
        , freqRange = (2000, 15000)
        , bandwidthRange = (50, 200)
        , modSpeed = 0.09
        }

lowWindParams :: WindParams
lowWindParams =
    WindParams
        { volumeRange = (0.1, 0.6)
        , freqRange = (100, 1000)
        , bandwidthRange = (20, 80)
        , modSpeed = 0.18
        }

midWindParams :: WindParams
midWindParams =
    WindParams
        { volumeRange = (0.1, 0.4)
        , freqRange = (800, 2500)
        , bandwidthRange = (30, 120)
        , modSpeed = 0.09
        }

gustyWindParams :: WindParams
gustyWindParams =
    WindParams
        { volumeRange = (0.0, 1.0)
        , freqRange = (1500, 14000)
        , bandwidthRange = (100, 300)
        , modSpeed = 0.12
        }

windWithParams :: Double -> WindParams -> StdGen -> AudSF () Double
windWithParams secs params g =
    let gs = splitN g
        (volMin, volMax) = volumeRange params
        (freqMin, freqMax) = freqRange params
        (bwMin, bwMax) = bandwidthRange params
        spacing = modSpeed params
     in proc _ -> do
            n <- noiseWhite (fst $ next (head gs)) -< ()
            vEnv <- randEnv secs spacing 0 0.1 volMin volMax (gs !! 2) -< ()
            bpF <- randEnv secs spacing (freqMin + (freqMax - freqMin) / 2) 50 freqMin freqMax (gs !! 3) -< ()
            bw <- randEnv secs spacing ((bwMin + bwMax) / 2) 5 bwMin bwMax (gs !! 4) -< ()
            nBP <- filterBandPassBW -< (n, bpF, bw)
            returnA -< vEnv * nBP

-- Mix two audio signals together
mixTwo :: AudSF () Double -> AudSF () Double -> AudSF () Double
mixTwo sf1 sf2 = proc () -> do
    x1 <- sf1 -< ()
    x2 <- sf2 -< ()
    returnA -< (x1 + x2) / 2

-- Scale a wind stream by volume
scaleWind :: Double -> AudSF () Double -> AudSF () Double
scaleWind vol w = proc () -> do
    x <- w -< ()
    returnA -< vol * x

-- Mix multiple wind streams together
mixWinds :: [(Double, AudSF () Double)] -> AudSF () Double
mixWinds [] = proc () -> returnA -< 0.0
mixWinds [single] = scaleWind (fst single) (snd single)
mixWinds (x : xs) = proc () -> do
    first <- scaleWind (fst x) (snd x) -< ()
    rest <- mixWinds xs -< ()
    let count = fromIntegral (length xs + 1)
    returnA -< (first + rest) / count

-- Generate layered wind and save to file
genLayeredWind :: StdGen -> Double -> FilePath -> IO ()
genLayeredWind g0 seconds filename =
    let
        -- Split the generator for different layers
        (g1, g2) = split g0
        (g3, g4) = split g1
        (g5, g6) = split g2

        -- Create wind layers with different characteristics
        winds =
            [ (0.7, windWithParams seconds highWindParams g3) -- High whistling winds
            , (1.0, windWithParams seconds lowWindParams g4) -- Low rumbling winds
            , (0.8, windWithParams seconds midWindParams g5) -- Mid-range winds
            , (0.5, windWithParams seconds gustyWindParams g6) -- Gusty wind bursts
            ]

        -- Mix all wind layers
        mixedWind = mixWinds winds
     in
        outFile filename seconds mixedWind

main :: IO ()
main = do
    args <- getArgs
    rio <- randomIO

    let r =
            if length args == 3
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
