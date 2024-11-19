{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Euterpea.IO.Audio.BasicSigFuns
  ( Table,
    pluck,
    PluckDecayMethod (..),
    balance,
    tableExponN,
    tableExpon,
    tableLinearN,
    tableLinear,
    tableSines3N,
    tableSines3,
    tableSinesN,
    tableSines,
    tableBesselN,
    tableBessel,
    filterLowPass,
    filterHighPass,
    filterBandPass,
    filterBandStop,
    filterLowPassBW,
    filterHighPassBW,
    filterBandPassBW,
    filterBandStopBW,
    filterComb,
    osc,
    oscI,
    oscFixed,
    oscDur,
    oscDurI,
    oscPartials,
    envLine,
    envExpon,
    envLineSeg,
    envExponSeg,
    envASR,
    envCSEnvlpx,
    noiseWhite,
    noiseBLI,
    noiseBLH,
    delayLine,
    delayLine1,
    delayLineT,
    samples,
    milliseconds,
    seconds,
    countTime,
  )
where

import Control.Arrow
import Control.Arrow.ArrowP
import Control.Arrow.Operations
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import Euterpea.IO.Audio.Basics
import Euterpea.IO.Audio.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.IO
import System.Random

-- Conventions:

-- (1) Optional arguments in some csound unit generators sometimes carry
-- different semantics depending on the way the generator is called.
-- Here they are encoded as algebraic datatypes instead (see 'pluck' for
-- example).  A single optional argument is normally encoded using
-- Haskell's Maybe type.

-- (2) csound's i-type is updated only once on every note's
-- initialization pass.  They are represented as unlifted arguments here
-- (i.e. non-signal).

-- (3) Many unit generators in csound take a signal 'amp' as input, which
-- scales its result by 'amp'.  Since this feature induces computational
-- overhead when scaling is not needed, and is easily expressed using
-- arrow syntax when needed, we omit that functionality from Eutperpea's
-- versions of the unit generators.

type SEvent a = Maybe a

constA = arr . const

wrap :: (Ord n, Num n) => n -> n -> n
wrap val bound = if val > bound then wrap val (val - bound) else val

clip :: (Ord n) => n -> n -> n -> n
clip val lower upper
  | val <= lower = lower
  | val >= upper = upper
  | otherwise = val

pow :: (Floating a) => a -> a -> a
pow a b = exp (log a * b)

frac :: (RealFrac r) => r -> r
frac = snd . properFraction

data Table
  = Table
      !Int -- size
      !(UArray Int Double) -- table implementation
      !Bool -- Whether the table is normalized

instance Show Table where
  show (Table sz a n) = "Table with " ++ show sz ++ " entries: " ++ show a

funToTable :: (Double -> Double) -> Bool -> Int -> Table
funToTable f normalize size =
  let delta = 1 / fromIntegral size
      ys = take size (map f [0, delta ..]) ++ [head ys]
      -- make table one size larger as an extended guard point
      zs = if normalize then map (/ maxabs ys) ys else ys
      maxabs = maximum . map abs
   in Table size (listArray (0, size) zs) normalize

readFromTable :: Table -> Double -> Double
readFromTable (Table sz array _) pos =
  let idx = truncate (fromIntegral sz * pos) -- range must be [0,size]
   in array `unsafeAt` idx
{-# INLINE [0] readFromTable #-}

readFromTableA :: (Arrow a) => Table -> a Double Double
readFromTableA = arr . readFromTable

readFromTableRaw :: Table -> Int -> Double
readFromTableRaw (Table _ a _) idx = a `unsafeAt` idx

readFromTablei :: Table -> Double -> Double
readFromTablei (Table sz array _) pos =
  let idx = fromIntegral sz * pos -- fractional "index" in table ([0,sz])
      idx0 = (truncate idx) `mod` sz :: Int
      idx1 = idx0 + 1 :: Int
      val0 = array `unsafeAt` idx0
      val1 = array `unsafeAt` idx1
   in val0 + (val1 - val0) * (idx - fromIntegral idx0)
{-# INLINE [0] readFromTablei #-}

readFromTableiA :: (Arrow a) => Table -> a Double Double
readFromTableiA = arr . readFromTablei

tablei ::
  (Clock p, Arrow a) =>
  Table -> -- Table to read from.
  Bool -> -- Whether to wrap around index;
  --   if not, index is clipped within bounds
  ArrowP a p Double Double
tablei tab True =
  proc pos -> do
    outA -< readFromTablei tab (wrap pos 1)
tablei tab False =
  proc pos -> do
    outA -< readFromTablei tab (clip pos 0 1)

table :: (Clock p, Arrow a) => Table -> Bool -> ArrowP a p Double Double
table tab True =
  proc pos -> do
    outA -< readFromTable tab (wrap pos 1)
table tab False =
  proc pos -> do
    outA -< readFromTable tab (clip pos 0 1)

tableiIx ::
  (Clock p, Arrow a) =>
  Table ->
  Bool ->
  ArrowP a p Double Double
tableiIx tab@(Table sz array _) True =
  proc idx -> do
    let idx0 = (truncate idx) `mod` sz
        val0 = readFromTableRaw tab idx0
        val1 = readFromTableRaw tab (idx0 + 1)
    outA -< val0 + (val1 - val0) * (idx - fromIntegral idx0)
tableiIx tab@(Table sz _ _) False =
  proc idx -> do
    let pos = idx / fromIntegral (sz - 1)
    outA -< readFromTablei tab (clip pos 0 1)

tableIx :: (Clock p, Arrow a) => Table -> Bool -> ArrowP a p Double Double
tableIx tab@(Table sz array _) True =
  proc idx -> do
    outA -< readFromTableRaw tab (truncate idx `mod` (sz - 1))
tableIx tab@(Table sz array _) False =
  proc idx -> do
    outA -< readFromTableRaw tab (clip (truncate idx) 0 (sz - 1))

-- Oscillators
-- -----------

-- 'osc' generates periodic signals consisting of the values returned
-- from sampling a stored function table. The internal phase is
-- simultaneously advanced in accordance with the input signal 'freq'.

osc ::
  (Clock p, ArrowCircuit a) =>
  Table ->
  Double -> -- Initial phase of sampling, expressed as a
  -- fraction of a cycle (0 to 1).
  ArrowP a p Double Double
osc table iphs = osc_ iphs >>> readFromTableA table

oscI ::
  (Clock p, ArrowCircuit a) =>
  Table ->
  Double ->
  ArrowP a p Double Double
oscI table iphs = osc_ iphs >>> readFromTableiA table

osc_ ::
  forall p a.
  (Clock p, ArrowCircuit a) =>
  Double ->
  ArrowP a p Double Double
osc_ phs =
  let sr = rate (undefined :: p)
   in proc freq -> do
        rec let delta = 1 / sr * freq
                phase = if next > 1 then frac next else next
            next <- delay phs -< frac (phase + delta)
        outA -< phase

-- Simple, fast sine oscillator, that uses only one multiply and two add
-- operations to generate one sample of output, and does not require a
-- function table.

oscFixed ::
  forall p a.
  (Clock p, ArrowCircuit a) =>
  Double ->
  ArrowP a p () Double
oscFixed freq =
  let omh = 2 * pi * freq / sr
      d = sin omh
      c = 2 * cos omh
      sr = rate (undefined :: p)
      sf = proc () -> do
        rec let r = c * d2 - d1
            d1 <- delay 0 -< d2
            d2 <- delay d -< r
        outA -< r
   in sf

-- 'oscDur' accesses values by sampling once through the function table
-- at a rate determined by 'dur'. For the first 'del' seconds, the point
-- of scan will reside at the first location of the table; it will then
-- begin moving through the table at a constant rate, reaching the end in
-- another 'dur' seconds; from that time on (i.e. after 'del' + 'dur'
-- seconds) it will remain pointing at the last location.

oscDur ::
  (Clock p, ArrowChoice a, ArrowCircuit a) =>
  Table ->
  Double ->
  -- delay in seconds before 'oscDur' incremental sampling begins
  Double ->
  -- duration in seconds to sample through the table just once.
  ArrowP a p () Double
oscDur = oscDur_ osc

oscDurI ::
  (Clock p, ArrowChoice a, ArrowCircuit a) =>
  Table ->
  Double ->
  -- delay in seconds before 'oscDur' incremental sampling begins.
  Double ->
  -- duration in seconds to sample through the table just once.
  ArrowP a p () Double
oscDurI = oscDur_ oscI

oscDur_ ::
  forall p a.
  (Clock p, ArrowChoice a, ArrowCircuit a) =>
  (Table -> Double -> ArrowP a p Double Double) ->
  Table ->
  Double ->
  Double ->
  ArrowP a p () Double
oscDur_ osc table@(Table sz _ _) del dur =
  let sr = rate (undefined :: p)
      t1 = del * sr
      t2 = t1 + dur * sr
      v0 = readFromTableRaw table 0
      v2 = readFromTableRaw table (sz - 1)
   in proc () -> do
        i <- countUp -< ()
        let i' = fromIntegral i
        y <- case (i' < t1, i' < t2) of
          (True, _) -> outA -< v0
          (False, True) -> osc table 0 -< 1 / dur
          (False, False) -> outA -< v2
        outA -< y

foscil,
  foscili ::
    (Clock p, Arrow a) =>
    Table ->
    ArrowP a p (Double, Double, Double, Double) Double
foscil table =
  proc (freq, carfreq, modfreq, modindex) -> do
    outA -< 0
foscili table =
  proc (freq, carfreq, modfreq, modindex) -> do
    outA -< 0

loscil :: (Clock p, Arrow a) => Table -> ArrowP a p Double Double
loscil table =
  proc freq -> do
    outA -< 0

oscPartials ::
  forall p.
  (Clock p) =>
  Table -> -- table containing a sine wave;
  -- a table size of at least 8192 is recommended.
  Double -> -- initial phase of the fundamental frequency,
  -- expressed as a fraction of a cycle (0 to 1).
  Signal p (Double, Int) Double
-- 'freq' is the fundamental frequency in cycles per
-- second; 'nharms' is the number of harmonics requested.
oscPartials table initialPhase =
  let sr = rate (undefined :: p)
   in proc (freq, nharms) -> do
        rec let delta = 1 / sr * freq
                phase = if next > 1 then frac next else next
            next <- delay initialPhase -< frac (phase + delta)
        outA
          -<
            sum
              [ readFromTable table (frac (phase * fromIntegral pn))
                | pn <- [1 .. nharms]
              ]
              / fromIntegral nharms

data PluckDecayMethod
  = SimpleAveraging
  | -- A simple smoothing process.
    StretchedAveraging Double
  | -- Smoothing time stretched by a factor.
    SimpleDrum Double
  | -- The range from pitch to noise is controlled by a 'roughness
    -- factor' (0 to 1). Zero gives the plucked string effect, while
    -- 1 reverses the polarity of every sample (octave down, odd
    -- harmonics). The setting .5 gives an optimum snare drum.
    StretchedDrum Double Double
  | -- Combines both roughness and stretch factors. parm1 is
    -- roughness (0 to 1), and parm2 the stretch factor (=1).
    WeightedAveraging Double Double
  | -- As SimpleAveraging, with parm1 weighting the current sample
    -- (the status quo) and iparm2 weighting the previous adjacent
    -- one. iparm1 + iparm2must be <= 1.
    RecursiveFilter

-- 1st order recursive filter, with coefs .5. Unaffected by
-- parameter values.

pluck ::
  forall p.
  (Clock p) =>
  Table ->
  Double ->
  PluckDecayMethod ->
  Signal p Double Double
pluck table pitch method =
  let sr = rate (undefined :: p)
   in proc cps -> do
        rec z <- delayLineT (max 64 (truncate (sr / pitch))) table -< y
            z' <- delay 0 -< z
            let y = case method of
                  SimpleAveraging -> 0.5 * (z + z')
                  -- or is this "RecursiveFilter?"
                  WeightedAveraging a b -> z * a + z' * b
                  _ -> error "pluck: method not implemented"
        outA -< y

grain ::
  Table ->
  -- Grain waveform. This can be just a sine wave or a sampled sound.
  Table ->
  -- Amplitude envelope used for the grains.
  Double ->
  -- Maximum grain duration in seconds. This is the biggest
  -- value to be assigned to 'gdur'.
  Bool ->
  -- If 'True', all grains will begin reading from the
  -- beginning of the 'gfn' table.  If 'False', grains
  -- will start reading from random 'gfn' table positions.
  Signal p (Double, Double, Double, Double, Double) Double
grain gfn wfn mgdur grnd =
  proc (pitch, dens, ampoff, pitchoff, gdur) -> do
    outA -< 0

-- Delay Lines
-- -----------

-- csound's delayr and delayw are not implemented -- instead, one can use
-- a fixed-time delay with native recursive arrow syntax to achieve
-- modified feedback loops.

data Buf = Buf !Int !(Ptr Double)

updateBuf :: Buf -> Int -> Double -> IO Double
updateBuf (Buf _ a) i u =
  a `seq` i `seq` u `seq` do
    let p = a `advancePtr` i
    x' <- peek p
    poke p u
    return x'

peekBuf (Buf sz a) i = peek (a `advancePtr` (min (sz - 1) i))

mkArr :: Int -> Buf
mkArr n =
  n `seq`
    Buf
      n
      ( unsafePerformIO $
          Foreign.Marshal.newArray (replicate n 0)
      )

mkArrWithTable size t =
  Buf
    size
    ( unsafePerformIO $
        Foreign.Marshal.newArray (map (readFromTable t) [0, (1 / sz) .. ((sz - 1) / sz)])
    )
  where
    sz = fromIntegral size

delayLineT ::
  forall p.
  (Clock p) =>
  Int ->
  Table ->
  Signal p Double Double
delayLineT size table =
  let sr = rate (undefined :: p)
      buf = mkArrWithTable size table
   in proc x -> do
        rec let i' = if i == size - 1 then 0 else i + 1
            i <- delay 0 -< i'
            y <- delay 0 -< x
        -- TODO: this proc can't be strict on x, but how can we
        --       deal with strictness better without this hack?
        outA -< unsafePerformIO $ updateBuf buf i y

delayLine ::
  forall p.
  (Clock p) =>
  Double ->
  Signal p Double Double
delayLine maxdel =
  let sr = rate (undefined :: p)
      sz = truncate (sr * maxdel)
      buf = mkArr sz
   in proc x -> do
        rec let i' = if i == sz - 1 then 0 else i + 1
            i <- delay 0 -< i'
            y <- delay 0 -< x
        outA -< unsafePerformIO $ updateBuf buf i y

delayLine1 :: forall p. (Clock p) => Double -> Signal p (Double, Double) Double
delayLine1 maxdel =
  let sr = rate (undefined :: p)
      sz = truncate (sr * maxdel)
      buf = mkArr sz
   in proc (sig, dlt) -> do
        rec let i' = if i == sz - 1 then 0 else i + 1
                dl = min maxdel dlt
                tap = i - truncate (sr * dl)
                tapidx = if tap < 0 then sz + tap else tap
            i <- delay 0 -< i'
            y <- delay 0 -< sig
        outA
          -< unsafePerformIO $ do
            s <- peekBuf buf tapidx
            _ <- updateBuf buf i y
            return s

delay2 :: Double -> Signal p (Double, Double, Double) Double
delay2 maxdel =
  proc (sig, dlt1, dlt2) -> do
    outA -< 0

delay3 :: Double -> Signal p (Double, Double, Double, Double) Double
delay3 maxdel =
  proc (sig, dlt1, dlt2, dlt3) -> do
    outA -< 0

delay4 :: Double -> Signal p (Double, Double, Double, Double, Double) Double
delay4 maxdel =
  proc (sig, dlt1, dlt2, dlt3, dlt4) -> do
    outA -< 0

-- Noise Generators
-- ----------------

-- Analogous to rand, randi, and randh in csound.

-- Generate uniform white noise with an R.M.S value of 1 / sqrt 2, where
-- 'seed' is the random seed.

noiseWhite :: Int -> Signal p () Double
noiseWhite seed =
  let gen = mkStdGen seed
   in proc () -> do
        rec let (a, g') = random g :: (Double, StdGen)
            g <- delay gen -< g'
        outA -< a * 2 - 1

-- Controlled band-limited noise with interpolation between each new
-- number, and with an RMS value of 1 / sqrt 2.
-- 'cps' controls how fast the new numbers are generated.
-- 'seed' is the random seed.

noiseBLI :: forall p. (Clock p) => Int -> Signal p Double Double
noiseBLI seed =
  let sr = rate (undefined :: p)
      gen = mkStdGen seed
      (i_n1, i_g1) = random gen :: (Double, StdGen)
      (i_n2, i_g2) = random i_g1 :: (Double, StdGen)
      i_pr = (i_n1, i_n2, i_g2)
   in proc cps -> do
        let bound = sr / cps
        rec state <- delay (0, i_pr) -< state'
            let (cnt, pr@(n1, n2, g)) = state
                n = n1 + (n2 - n1) * cnt / bound
                state' =
                  if cnt + 1 < bound
                    then (cnt + 1, pr)
                    else
                      let (n3, g') = random g :: (Double, StdGen)
                       in (0, (n2, n3, g'))
        outA -< n * 2 - 1

-- Controlled band-limited noise without interpolation (holds
-- previous value instead), and with an RMS value of 1 / sqrt 2.
-- 'cps' controls how fast the new numbers are generated.
-- 'seed' is the random seed.

noiseBLH :: forall p. (Clock p) => Int -> Signal p Double Double
noiseBLH seed =
  let sr = rate (undefined :: p)
      gen = mkStdGen seed
      (i_n1, i_g) = random gen :: (Double, StdGen)
      i_pr = (i_n1, i_g)
   in proc cps -> do
        let bound = sr / cps
        rec state <- delay (0, i_pr) -< state'
            let (cnt, pr@(n, g)) = state
                state' =
                  if cnt + 1 < bound
                    then (cnt + 1, pr)
                    else
                      let (n', g') = random g :: (Double, StdGen)
                       in (0, (n', g'))
        outA -< n * 2 - 1

balance ::
  forall p.
  (Clock p) =>
  Int ->
  Signal p (Double, Double) Double
balance ihp =
  proc (sig, ref) -> do
    rec (sqrsum, refsum) <- delay (0, 0) -< (sqrsum', refsum')
        let sqrsum' = c1 * sig * sig + c2 * sqrsum
            refsum' = c1 * ref * ref + c2 * refsum
            ratio =
              if sqrsum == 0
                then sqrt $ refsum
                else sqrt $ refsum / sqrsum
    outA -< sig * ratio
  where
    sr = rate (undefined :: p)
    tpidsr = 2 * pi / sr -- tpidsr = two-pi over sr
    b = 2 - cos (fromIntegral ihp * tpidsr)
    c1 = 1 - c2
    c2 = b - sqrt (b * b - 1)

data BandPassData = BandPassData
  { rsnKcf :: !Double,
    rsnKbw :: !Double,
    rsnCosf :: !Double,
    rsnC1 :: !Double,
    rsnC2 :: !Double,
    rsnC3 :: !Double,
    rsnYt1 :: !Double,
    rsnYt2 :: !Double
  }

rsnDefault :: BandPassData
rsnDefault = BandPassData (-1) (-1) 0 0 0 0 0 0

filterBandPass ::
  forall p.
  (Clock p) =>
  Int -> -- 'scale': 1 signifies a peak response factor of 1, i.e. all
  -- frequencies other than kcf are attenuated in accordance with
  -- the (normalized) response curve; 2 raises the response
  -- factor so that its overall RMS value equals 1; 0 ignifies
  -- no scaling of the signal, leaving that to some later
  -- adjustment (like balance).
  Signal p (Double, Double, Double) Double
-- 'sig' is the signal to be filtered,
-- 'kcf' is the center frequency of the filter,
-- and 'kbw' is the bandwidth of it.
filterBandPass scale =
  proc (sig, kcf, kbw) -> do
    rec rsnData <- delay rsnDefault -< rsnData'
        currData <-
          if kcf == rsnKcf rsnData && kbw == rsnKbw rsnData
            then outA -< rsnData
            else update -< (rsnData, kcf, kbw)
        let BandPassData
              { rsnC1 = c1,
                rsnC2 = c2,
                rsnC3 = c3,
                rsnYt1 = yt1,
                rsnYt2 = yt2
              } = currData
            a = c1 * sig + c2 * yt1 - c3 * yt2
            rsnData' = currData {rsnYt1 = a, rsnYt2 = yt1}
    outA -< a
  where
    sr = rate (undefined :: p)
    tpidsr = 2 * pi / sr -- tpidsr = two-pi over sr
    update = proc (rsnData, kcf, kbw) -> do
      -- kcf or kbw changed, recalc consts
      let cosf = cos $ kcf * tpidsr -- cos (2pi * freq / rate)
          c3 = exp $ -kbw * tpidsr -- exp (-2pi * bwidth / rate)
          -- (note on csound code) mtpdsr = -tpidsr
          -- c1   Gain for input signal.
          -- c2   (Minused) gain for output of delay 1.
          -- c3   Gain for output of delay 2.
          c3p1 = c3 + 1
          c3t4 = c3 * 4
          c2 = c3t4 * cosf / c3p1
          omc3 = 1 - c3
          c2sqr = c2 * c2
          c1 = case scale of
            1 -> omc3 * sqrt (1 - c2sqr / c3t4)
            2 -> sqrt $ (c3p1 * c3p1 - c2sqr) * omc3 / c3p1
            _ -> 1.0
      outA
        -<
          rsnData
            { rsnKcf = kcf,
              rsnKbw = kbw,
              rsnCosf = cosf,
              rsnC1 = c1,
              rsnC2 = c2,
              rsnC3 = c3
            }

filterBandStop ::
  forall p.
  (Clock p) =>
  Int ->
  Signal p (Double, Double, Double) Double
filterBandStop scale = proc (sig, kcf, kbw) -> do
  r <- filterBandPass scale -< (sig, kcf, kbw)
  outA -< sig - r

data ButterData = ButterData !Double !Double !Double !Double !Double

sqrt2 :: Double
sqrt2 = sqrt 2

blpset :: Double -> Double -> ButterData
blpset freq sr = ButterData a1 a2 a3 a4 a5
  where
    c = 1 / tan (pidsr * freq)
    csq = c * c
    pidsr = pi / sr
    a1 = 1 / (1 + sqrt2 * c + csq)
    a2 = 2 * a1
    a3 = a1
    a4 = 2 * (1 - csq) * a1
    a5 = (1 - sqrt2 * c + csq) * a1

bhpset :: Double -> Double -> ButterData
bhpset freq sr = ButterData a1 a2 a3 a4 a5
  where
    c = tan (pidsr * freq)
    csq = c * c
    pidsr = pi / sr
    a1 = 1 / (1 + sqrt2 * c + csq)
    a2 = (-2) * a1
    a3 = a1
    a4 = 2 * (csq - 1) * a1
    a5 = (1 - sqrt2 * c + csq) * a1

bbpset :: Double -> Double -> Double -> ButterData
bbpset freq band sr = ButterData a1 a2 a3 a4 a5
  where
    c = 1 / tan (pidsr * band)
    d = 2 * cos (2 * pidsr * freq)
    pidsr = pi / sr
    a1 = 1 / (1 + c)
    a2 = 0
    a3 = negate a1
    a4 = negate (c * d * a1)
    a5 = (c - 1) * a1

bbrset :: Double -> Double -> Double -> ButterData
bbrset freq band sr = ButterData a1 a2 a3 a4 a5
  where
    c = tan (pidsr * band)
    d = 2 * cos (2 * pidsr * freq)
    pidsr = pi / sr
    a1 = 1 / (1 + c)
    a2 = negate d * a1
    a3 = a1
    a4 = a2
    a5 = (1 - c) * a1

filterLowPassBW :: forall p. (Clock p) => Signal p (Double, Double) Double
filterLowPassBW =
  let sr = rate (undefined :: p)
   in proc (sig, freq) -> do
        butter -< (sig, blpset freq sr)

filterHighPassBW :: forall p. (Clock p) => Signal p (Double, Double) Double
filterHighPassBW =
  let sr = rate (undefined :: p)
   in proc (sig, freq) -> do
        butter -< (sig, bhpset freq sr)

filterBandPassBW ::
  forall p.
  (Clock p) =>
  Signal p (Double, Double, Double) Double
filterBandPassBW =
  let sr = rate (undefined :: p)
   in proc (sig, freq, band) -> do
        butter -< (sig, bbpset freq band sr)

filterBandStopBW ::
  forall p.
  (Clock p) =>
  Signal p (Double, Double, Double) Double
filterBandStopBW =
  let sr = rate (undefined :: p)
   in proc (sig, freq, band) -> do
        butter -< (sig, bbrset freq band sr)

butter :: (Clock p) => Signal p (Double, ButterData) Double
butter = proc (sig, ButterData a1 a2 a3 a4 a5) -> do
  rec let t = sig - a4 * y' - a5 * y''
          y = t * a1 + a2 * y' + a3 * y''
      y' <- delay 0 -< t
      y'' <- delay 0 -< y'
  outA -< y

filterComb ::
  (Clock p) =>
  Double -> -- loop time in seconds, which determines the "echo
  -- density" of the reverberation. This in turn
  -- characterizes the "color" of the filter whose
  -- frequency response curve will contain 'looptime' *
  -- sr/2 peaks spaced evenly between 0 and sr/2 (the
  -- Nyquist frequency).  Loop time can be as large as
  -- available memory will permit.
  Signal p (Double, Double) Double
filterComb looptime =
  let log001 = -6.9078
      del = delayLine looptime
   in proc (sig, rvt) -> do
        let gain = exp (log001 * looptime / rvt)
        rec r <- del -< sig + r * gain
        outA -< r

filterLowPass :: forall p. (Clock p) => Signal p (Double, Double) Double
filterLowPass =
  let sr = rate (undefined :: p)
   in proc (sig, hp) -> do
        rec let y' = c1 * sig + c2 * y
                b = 2 - cos (2 * pi * hp / sr)
                c2 = b - sqrt (b * b - 1.0)
                c1 = 1 - c2
            y <- delay 0 -< y'
        outA -< y

filterHighPass :: (Clock p) => Signal p (Double, Double) Double
filterHighPass = proc (sig, hp) -> do
  y <- filterLowPass -< (sig, hp)
  outA -< sig - y

envLine ::
  forall p.
  (Clock p) =>
  Double -> -- Starting value.
  Double -> -- Duration in seconds.
  Double -> -- Value after 'dur' seconds.
  Signal p () Double
envLine a dur b =
  let sr = rate (undefined :: p)
   in proc () -> do
        rec y <- delay a -< y + (b - a) * (1 / sr / dur)
        outA -< y

envExpon ::
  forall p.
  (Clock p) =>
  Double -> -- Starting value.  Zero is illegal for exponentials.
  Double -> -- Duration in seconds.
  Double -> -- Value after 'dur' seconds.  For exponentials,
  -- must be non-zero and must agree in sign with 'a'.
  Signal p () Double
envExpon a dur b =
  let sr = rate (undefined :: p)
   in proc () -> do
        rec y <- delay a -< y * pow (b / a) (1 / sr / dur)
        outA -< y

data Tab = Tab [Double] !Int !(UArray Int Double)

aAt :: Tab -> Int -> Double
aAt (Tab _ sz a) i = unsafeAt a (min (sz - 1) i)

seghlp ::
  forall p.
  (Clock p) =>
  [Double] -> -- List of points to trace through.
  [Double] -> -- List of durations for each line segment.
  -- Needs to be one element fewer than 'iamps'.
  Signal p () (Double, Double, Double, Double)
seghlp iamps idurs =
  let sr = rate (undefined :: p)
      sz = length iamps
      amps = Tab iamps sz (listArray (0, sz - 1) iamps)
      durs = Tab idurs (sz - 1) (listArray (0, sz - 2) (map (* sr) idurs))
   in proc _ -> do
        -- TODO: this is better defined using 'integral', but which is faster?
        rec let (t', i') =
                  if t >= durs `aAt` i
                    then if i == sz - 2 then (t + 1, i) else (0, i + 1)
                    else (t + 1, i)
            i <- delay 0 -< i'
            t <- delay 0 -< t'
        let a1 = aAt amps i
            a2 = aAt amps (i + 1)
            d = aAt durs i
        outA -< (a1, a2, t, d)

envLineSeg ::
  (Clock p) =>
  [Double] -> -- List of points to trace through.
  [Double] -> -- List of durations for each line segment.
  -- Needs to be one element fewer than 'amps'.
  Signal p () Double
envLineSeg amps durs =
  let sf = seghlp amps durs
   in proc () -> do
        (a1, a2, t, d) <- sf -< ()
        outA -< a1 + (a2 - a1) * (t / d)

envExponSeg ::
  (Clock p) =>
  [Double] -> -- List of points to trace through.
  [Double] -> -- List of durations for each line segment.
  -- Needs to be one element fewer than 'amps'.
  Signal p () Double
envExponSeg ampinps durs =
  let amps' = case ampinps of
        (a : amps) -> max 0.001 a : amps
        [] -> []
      sf = seghlp amps' durs
   in proc () -> do
        (a1, a2, t, d) <- sf -< ()
        outA -< a1 * pow (a2 / a1) (t / d)

envASR ::
  (Clock p) =>
  Double -> -- rise time in seconds.
  Double -> -- overall duration in seconds.
  Double -> -- decay time in seconds.
  Signal p () Double
envASR rise dur dec =
  let sf = envLineSeg [0, 1, 1, 0] [rise, dur - rise - dec, dec]
   in proc () -> do
        env <- sf -< ()
        outA -< env

envCSEnvlpx ::
  forall p.
  (Clock p) =>
  Double -> -- rise time in seconds.
  Double -> -- overall duration in seconds.
  Double -> -- decay time in seconds.
  Table -> -- table of stored rise shape.
  Double ->
  -- attenuation factor, by which the last value of the
  -- 'envCSEnvlpx' rise is modified during the note's pseudo
  -- steady state. A factor greater than 1 causes an
  -- exponential growth and a factor less than 1 creates an
  -- exponential decay. A factor of 1 will maintain a true
  -- steady state at the last rise value. Note that this
  -- attenuation is not by fixed rate (as in a piano), but
  -- is sensitive to a note's duration. However, if 'atss'
  -- is negative (or if steady state < 4 k-periods) a fixed
  -- attenuation rate of 'abs' 'atss' per second will be
  -- used. 0 is illegal.
  Double ->
  -- attenuation factor by which the closing steady state
  -- value is reduced exponentially over the decay
  -- period. This value must be positive and is normally of
  -- the order of .01. A large or excessively small value is
  -- apt to produce a cutoff which is audible. A zero or
  -- negative value is illegal.
  Signal p () Double
envCSEnvlpx rise dur dec tab atss atdec =
  let sr = rate (undefined :: p)
      cnt1 = (dur - rise - dec) * sr + 0.5
      -- num of samples in steady state
      mlt1 = pow atss (1 / cnt1)
      mlt2 = pow atdec (1 / sr / dec)
   in proc () -> do
        rec i <- countUp -< ()
            let i' = fromIntegral i
            y <- delay (readFromTableRaw tab 0) -< y'
            y' <- case (i' < rise * sr, i' < (dur - dec) * sr) of
              (True, _) -> table tab False -< i' / (rise * sr + 0.5)
              (False, True) -> outA -< y * mlt1
              (False, False) -> outA -< y * mlt2
        outA -< y'

type TableSize = Int

type PartialNum = Double

type PartialStrength = Double

type PhaseOffset = Double

type StartPt = Double

type SegLength = Double

type EndPt = Double

type DoubleSegFun =
  (Double, StartPt) -> [(SegLength, EndPt)] -> Double -> Double

tableExponN ::
  TableSize ->
  -- The size of the table to be produced.
  StartPt ->
  -- The y-coordinate for the start point, (0,y).
  [(SegLength, EndPt)] ->
  -- Pairs of segment lengths and y-coordinates. The segment
  -- lengths are the projection along the x-axis. The first
  -- pair will define the line from (0, startPt) to (segLength,
  -- endPt).
  Table
tableExponN size sp segs = tableExp_ sp segs True size

tableExpon :: Int -> StartPt -> [(SegLength, EndPt)] -> Table
tableExpon size sp segs = tableExp_ sp segs False size

tableExp_ :: StartPt -> [(SegLength, EndPt)] -> Bool -> Int -> Table
tableExp_ sp segs = funToTable (interpLine sp segs interpExpLine)

tableLinearN ::
  TableSize ->
  -- The size of the table to be produced.
  StartPt ->
  -- The y-coordinate for the start point, (0,y).
  [(SegLength, EndPt)] ->
  -- Pairs of segment lengths and y-coordinates. The segment
  -- lengths are the projection along the x-axis. The first
  -- pair will define the line from (0, startPt) to (segLength,
  -- endPt).
  Table
tableLinearN size sp segs = tableLin_ sp segs True size

tableLinear :: Int -> StartPt -> [(SegLength, EndPt)] -> Table
tableLinear size sp segs = tableLin_ sp segs False size

tableLin_ :: StartPt -> [(SegLength, EndPt)] -> Bool -> Int -> Table
tableLin_ sp segs = funToTable (interpLine sp segs interpStraightLine)

tableSines3N ::
  TableSize ->
  -- The size of the table to be produced.
  [(PartialNum, PartialStrength, PhaseOffset)] ->
  -- List of triples of the partial (0,1,...), partial
  -- strength on [0,1], and phase delay on [0,360].
  Table
tableSines3N size ps = tableSines3_ ps True size

tableSines3 :: Int -> [(PartialNum, PartialStrength, PhaseOffset)] -> Table
tableSines3 size ps = tableSines3_ ps False size

tableSines3_ :: [(PartialNum, PartialStrength, PhaseOffset)] -> Bool -> Int -> Table
tableSines3_ ps = funToTable (makeCompositeSineFun ps)

tableSinesF :: (Floating a, Enum a) => [a] -> a -> a
tableSinesF pss x =
  let phase = 2 * pi * x
   in sum (zipWith (*) [sin (phase * pn) | pn <- [1 ..]] pss)

tableSinesN :: TableSize -> [PartialStrength] -> Table
tableSinesN size pss = tableSinesN_ pss True size

tableSines :: Int -> [Double] -> Table
tableSines size pss = tableSinesN_ pss False size

tableSinesN_ :: [Double] -> Bool -> Int -> Table
tableSinesN_ pss = funToTable (tableSinesF pss)

tableBesselN ::
  TableSize ->
  Double -> -- specifies the x interval [0 to +xint] over which
  -- the function is defined.
  Table
tableBesselN size xint = tableBess_ xint True size

tableBessel :: Int -> Double -> Table
tableBessel size xint = tableBess_ xint False size

tableBess_ :: Double -> Bool -> Int -> Table
tableBess_ xint = funToTable (tableBessF xint)

tableBessF :: (Floating s) => s -> s -> s
tableBessF xint x =
  log $
    1
      + let tsquare = x * x * xint * xint / 3.75 / 3.75
         in sum
              $ zipWith
                (*)
                [ 3.5156229,
                  3.0899424,
                  1.2067492,
                  0.2659732,
                  0.0360768,
                  0.0045813
                ]
              $ iterate (* tsquare) tsquare

normalizeSegs :: [(SegLength, entPt)] -> [(SegLength, entPt)]
normalizeSegs segs =
  let s = sum (map fst segs)
      fact = if (s > 1) then (1 / s) else 1 -- don't force max<1 up to max=1
   in map (\(x, y) -> (x * fact, y)) segs

interpLine ::
  StartPt ->
  -- The y-coordinate for the start point (0,y).
  [(SegLength, EndPt)] ->
  -- Pairs of segment lengths (projected on the x-axis)
  -- and y-coordinates (end points).
  DoubleSegFun ->
  -- The function to use for interpolation
  Double ->
  -- The x-coordinate for which to find the
  -- corresponding f(x)=y.
  Double
interpLine sp [] d f = 0 -- catchall case
interpLine sp points f d = f (0, sp) (normalizeSegs points) d

interpExpLine ::
  (Double, StartPt) ->
  -- The startpoing as (x,y)
  [(SegLength, EndPt)] ->
  -- A list of line segments with (x',y) where x' is
  -- a length projected on the x-axis
  Double ->
  -- The target x-coordinate to find a corresponding
  -- y value for
  Double
interpExpLine (s1, e1) [] d = e1 -- termination case, end of list
interpExpLine (s1, e1) ((s2, e2) : t) d =
  if d > s2
    then interpExpLine (s2, e2) t (d - s2)
    else
      let h = e2 - e1
          x = if h < 0 then s2 - d else d
       in if s2 <= 0
            then e2 -- accomodate discontinuities
            else
              (abs h) * ((exp (x / s2)) - 1) / ((exp 1) - 1) + (min e1 e2)

interpStraightLine ::
  (Double, StartPt) ->
  -- The startpoing as (x,y)
  [(SegLength, EndPt)] ->
  -- A list of line segments with (x',y) where x' is
  -- a length projected on the x-axis
  Double ->
  -- The target x-coordinate to find a corresponding
  -- y value for
  Double
interpStraightLine (s1, e1) [] d = e1 -- termination case, end of list
interpStraightLine (s1, e1) ((s2, e2) : t) d =
  if d > s2
    then interpStraightLine (s2, e2) t (d - s2)
    else
      let h = e2 - e1 -- height of triangle
          s = h / s2 -- slope of triangle
       in if s2 <= 0
            then e2
            else
              e1 + (s * d) -- start point plus slope times distance

makeSineFun ::
  (PartialNum, PartialStrength, PhaseOffset) ->
  -- Triple of the partial (0,1,...), partial strength
  -- on [0,1], and phase delay on [0,360].
  Double ->
  -- The x coordinate for which to find f(x)=y
  Double
makeSineFun (pNum, pStrength, pOffset) x =
  let x' = x * 2 * pi -- convert [0,1] to [0,pi] radians
      po = (pOffset / 360) * 2 * pi -- convert [0,360] to [0,pi] radians
   in pStrength * sin (x' * pNum + po)

makeCompositeSineFun ::
  [(PartialNum, PartialStrength, PhaseOffset)] ->
  -- List of triples of the partial (0,1,...),
  -- partial strength on [0,1], and phase delay
  -- on [0,360].
  Double ->
  -- The x coordinate for which to find f(x)=y
  Double
makeCompositeSineFun [] x = 0
makeCompositeSineFun (p : ps) x = makeSineFun p x + makeCompositeSineFun ps x

samples :: forall p. (Clock p) => Signal p () (SEvent ())
samples = constA (Just ())

timeBuilder :: forall p. (Clock p) => Double -> Signal p () (SEvent ())
timeBuilder d =
  let r = (rate (undefined :: p)) * d
   in proc _ -> do
        rec i <- delay 0 -< if i >= r then i - r else i + 1
        outA -< if i < 1 then Just () else Nothing

milliseconds :: (Clock p) => Signal p () (SEvent ())
milliseconds = timeBuilder (1 / 1000)

seconds :: (Clock p) => Signal p () (SEvent ())
seconds = timeBuilder 1

countTime :: (Clock p) => Int -> Signal p () (SEvent ()) -> Signal p () (SEvent ())
countTime n t = proc _ -> do
  e <- t -< ()
  rec i <- delay 0 -< maybe i' (const $ i' + 1) e
      let (i', o) = if i == n then (0, Just ()) else (i, Nothing)
  outA -< o
