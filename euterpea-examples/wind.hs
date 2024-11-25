{-# LANGUAGE Arrows #-}

-- | Code by Donya Quick
-- https://github.com/Euterpea/Euterpea2-Examples
module Main where

import Euterpea
import System.Environment
import System.Random


-- | Generate a list of random points starting from an initial value.
-- | The movement between consecutive points is bounded by `maxDelta`.
-- | The resulting points are kept within the specified limits `limitL` and `limitU`.
randomPoints :: StdGen     -- ^ Random number generator
             -> Double     -- ^ Initial Y value
             -> Double     -- ^ Maximum change (delta) allowed per step
             -> Double     -- ^ Lower limit for Y values
             -> Double     -- ^ Upper limit for Y values
             -> [Double]   -- ^ Resulting list of Y values
randomPoints g lastY maxDelta limitL limitU =
  let 
      -- Generate a random amount of movement within the range [-maxDelta, maxDelta].
      (r, g') = randomR (-maxDelta, maxDelta) g
      
      -- Calculate the next Y value, ensuring it stays within the bounds [limitL, limitU].
      nextY = min limitU (max (lastY + r) limitL)
      
   in 
      -- Prepend the current Y value to the list and recursively generate more points.
      lastY : randomPoints g' nextY maxDelta limitL limitU


-- | Generate a random envelope sound function over time.
-- | The envelope is constructed from random points and spans a set duration.
randEnv :: Double     -- ^ Total duration of the envelope in seconds
        -> Double     -- ^ Spacing between envelope points
        -> Double     -- ^ Initial Y value
        -> Double     -- ^ Maximum change (delta) allowed per step
        -> Double     -- ^ Lower limit for Y values
        -> Double     -- ^ Upper limit for Y values
        -> StdGen     -- ^ Random number generator
        -> AudSF () Double  -- ^ Resulting audio signal function
randEnv secs spacing y0 maxDelta limitL limitU g =
  let 
      -- Calculate the number of envelope points required for the entire duration.
      n = ceiling (secs / spacing)
      
      -- Generate 'n' random Y values for the envelope points using the `randomPoints` function.
      ys = take n $ randomPoints g y0 maxDelta limitL limitU
      
      -- Create equal spacing between all consecutive points except the last one.
      xs = replicate (n - 1) spacing
      
   in 
      -- Define a signal processing function using arrow notation.
      proc () -> do
        -- Linearly interpolate between the generated envelope points.
        y <- envLineSeg ys xs -< ()
        
        -- Return the current interpolated value as the output of the signal function.
        returnA -< y


-- | Function to create an infinite list of random number generators by recursively splitting.
splitN :: StdGen      -- ^ Initial random number generator
       -> [StdGen]    -- ^ Infinite list of split random number generators
splitN g =
  let 
      -- Split the generator into two new generators.
      (g1, g2) = split g 
  in 
      -- Construct a list where g1 is the head and recursively call splitN on g2 for the tail.
      g1 : splitN g2

-- | Simulate wind noise as an audio signal function over time.
wind :: Double        -- ^ Total duration of the simulation in seconds
     -> StdGen        -- ^ Random number generator for producing noise
     -> AudSF () Double  -- ^ Resulting audio signal function that mimics wind noise
wind secs g =
  let 
      -- Create an infinite list of random generators from the initial generator.
      gs = splitN g
   in 
      -- Define a signal processing function using arrow notation.
      proc _ -> do
        
        -- Generate white noise using a random seed derived from the first generator.
        n <- noiseWhite (fst $ next (head gs)) -< ()
        
        -- Create a volume envelope with random variability over the specified duration.
        vEnv <- randEnv secs 0.05 0 0.1 0.1 1.0 (gs !! 2) -< ()
        
        -- Create a center frequency envelope for bandpass filtering with randomness.
        bpF <- randEnv secs 0.05 2000 50 100 5000 (gs !! 3) -< ()
        
        -- Create a bandwidth envelope for bandpass filtering with randomness.
        bw <- randEnv secs 0.05 50 5 5 100 (gs !! 4) -< ()
        
        -- Apply bandpass filtering on the white noise using the generated envelopes.
        nBP <- filterBandPassBW -< (n, bpF, bw)
        
        -- Return the product of the volume envelope and filtered noise as the output signal.
        returnA -< vEnv * nBP


-- | Function to generate wind noise and write it to a file.
genWind :: StdGen      -- ^ Random number generator for producing noise
        -> Double      -- ^ Duration of wind noise generation in seconds
        -> FilePath    -- ^ Path for the output audio file
        -> IO ()       -- ^ IO action to produce a file with the generated wind noise
genWind g seconds filename =
  -- Write the wind noise to the specified file over the given duration.
  outFile filename seconds (wind seconds g)


main :: IO ()
main = do
  -- Fetch command-line arguments. Expected: <duration> <file_path> [<optional_seed>]
  args <- getArgs

  -- Generate a random seed if not supplied through command-line arguments.
  rio <- randomIO

  let 
      -- Determine the random seed based on input arguments or use a random one.
      r = if length args == 3 then read (args !! 2) else rio
      
      -- Create a random number generator from the seed.
      g = mkStdGen r
  
  -- Display the random seed being used.
  putStr ("Random seed: " ++ show r)

  if length args >= 2
    then do
      -- Read the duration from command-line and call `genWind` with provided file path.
      let secs = read (head args) :: Double
      genWind g secs (args !! 1)
    else
      -- Use default values if insufficient arguments; 30 seconds duration and default filename.
      genWind g 30.0 "wind.wav"
