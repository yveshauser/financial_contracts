{-# LANGUAGE RankNTypes #-}
-- | Cross-validation of the pricing models (the model set of Miletus.jl).
--
-- Two layers:
--
--   * sanity checks pinning the models to analytic and literature values,
--     and to each other (Monte Carlo and LSMC have no reference data)
--
--   * Miletus.jl's reference data (test/data/*.csv, generated with the R
--     fOptions package by Miletus' test/ref.r), checked row by row with the
--     same tolerances as Miletus (rtol = 1e-3, atol = 1e-6)
--
-- All Monte Carlo runs use fixed seeds, so the suite is deterministic.
module Main where

import Contracts (Currency (..), Asset (..), Stock (..), one)
import Control.Monad (unless)
import Data.Int (Int8, Int64)
import qualified Data.Massiv.Array as M
import Data.Number.Erf (normcdf)
import Derivatives (american, european, OptionKind (..))
import Models (ModelChoice (..), Market (..), runValue, runPaths, priceWith, defaultMarket)
import Models.Futhark (FutT, getContext, runFutTIn, fromFuthark)
import qualified Models.Futhark.Entries as E
import System.Exit (exitFailure)
import Text.Printf (printf)
import Valuation (evalC)

-- | Run a Futhark computation in the shared context
type Runner = forall a. (forall c. FutT c IO a) -> IO a

main :: IO ()
main = do
  ctx <- getContext []
  let runIn :: Runner
      runIn = runFutTIn ctx
  oks <- sequence
    [ sanityChecks runIn
    , greekChecks runIn
    , csvSuite "bs.csv (Black-Scholes vs fOptions)" (bsRow runIn)
    , csvSuite "crr.csv (CRR tree vs fOptions)" (treeRow runIn 0)
    , csvSuite "jr.csv (Jarrow-Rudd tree vs fOptions)" (treeRow runIn 1)
    , csvSuite "tian.csv (Tian tree vs fOptions)" (treeRow runIn 3)
    ]
  unless (and oks) exitFailure

-- * Sanity checks

data Check = Check
  { label    :: String
  , actual   :: Double
  , expected :: Double
  , tol      :: Double
  }

sanityChecks :: Runner -> IO Bool
sanityChecks runIn = do
  -- at-the-money option: s0 = k = 100, r = 5%, no carry, sigma = 30%, t = 1y
  -- flags: kind 1 = call / 0 = put; exercise 1 = american / 0 = european;
  -- variant 0 = CRR, 1 = JR, 2 = JRrn, 3 = Tian
  let (s0, k, r, b, sigma, t) = (100, 100, 0.05, 0, 0.3, 1)
  bs     <- runIn (E.blackScholes 1 s0 k r b sigma t)
  bsPut  <- runIn (E.blackScholes 0 s0 k r b sigma t)
  jrrn   <- runIn (E.binomial 2 0 1 s0 k r b sigma t 1000)
  mc     <- runIn (E.mcEuropean 1 s0 k r b sigma t 100000 100 42)
  lsmc   <- runIn (E.lsmcAmerican 1 s0 k r b sigma t 100000 50 3 42)
  crrAm  <- runIn (E.binomial 0 1 0 s0 k r b sigma t 1000)
  lsmcAm <- runIn (E.lsmcAmerican 0 s0 k r b sigma t 100000 50 3 42)
  -- the lattice value process, threaded through Futhark end to end via evalC,
  -- priced against the per-asset market (Stk X: spot 100, vol 0.3)
  latCall <- runIn (runValue (evalC (Cur CHF) (european Call 1 10 CHF (one (Stk X)))) defaultMarket)
  latSpot <- runIn (runValue (evalC (Cur CHF) (one (Stk X))) defaultMarket)
  bsITM   <- runIn (E.blackScholes 1 100 10 r b sigma t)  -- the same call, analytic
  -- priceWith dispatch: a vanilla European goes to the chosen model's scalar
  -- pricer; a non-vanilla contract falls back to the lattice value process
  pwCrr   <- runIn (priceWith (Cur CHF) CRR defaultMarket (european Call 1 10 CHF (one (Stk X))))
  crrEu   <- runIn (E.binomial 0 0 1 100 10 r b sigma t 500)  -- CRR european, 500 steps
  pwShare <- runIn (priceWith (Cur CHF) CRR defaultMarket (one (Stk X)))
  -- the path-based (Monte Carlo / LSMC) value process, end to end via evalC
  pathsEu   <- runIn (runPaths (evalC (Cur CHF) (european Call 1 10 CHF (one (Stk X)))) defaultMarket)
  mcEu      <- runIn (E.mcEuropean 1 100 10 r b sigma t 100000 50 42)  -- same paths, analytic disc
  pathsSpot <- runIn (runPaths (evalC (Cur CHF) (one (Stk X))) defaultMarket)
  let amPut = american Put (0, 1) 100 CHF (one (Stk X))
  pathsAm   <- runIn (runPaths (evalC (Cur CHF) amPut) defaultMarket)
  latAm50   <- runIn (runValue (evalC (Cur CHF) amPut) defaultMarket { steps = 50 })
  oks <- mapM report
    [ Check "Black-Scholes call matches reference value" bs 14.2313 1e-3
    , Check "put-call parity" (bs - bsPut) (s0 - k * exp (-r*t)) 1e-9
    , Check "JRrn tree converges to Black-Scholes" jrrn bs 1e-2
    , Check "Monte Carlo within 3 standard errors of Black-Scholes" mc bs 0.25
    , Check "LSMC american call equals european call (no dividends)" lsmc bs 0.1
    , Check "CRR american put matches reference value" crrAm 9.8687 1e-2
    , Check "LSMC american put agrees with CRR tree" lsmcAm crrAm 0.1
    , Check "Lattice value process (european call) matches Black-Scholes" latCall bsITM 0.1
    , Check "Lattice value process (one share) equals spot" latSpot 100 1e-9
    , Check "priceWith routes a vanilla European to the chosen model (CRR)" pwCrr crrEu 1e-9
    , Check "priceWith falls back to the lattice for non-vanilla contracts" pwShare 100 1e-9
    , Check "Paths disc (european call) matches Monte Carlo" pathsEu mcEu 1e-6
    , Check "Paths exch (one share) equals spot" pathsSpot 100 1e-9
    , Check "Paths snell (american put) agrees with the 50-step lattice" pathsAm latAm50 0.15
    ]
  pure (and oks)

report :: Check -> IO Bool
report c = do
  let ok = abs (actual c - expected c) <= tol c
  printf "%s %s (got %.6f, expected %.6f +/- %g)\n"
         (if ok then "PASS " else "FAIL ") (label c) (actual c) (expected c) (tol c)
  pure ok

-- | Greeks computed by forward-mode AD in Futhark, validated against the
-- closed-form Black-Scholes greeks (b = 0). Tree and Monte Carlo AD greeks
-- must approximate the analytic ones.
greekChecks :: Runner -> IO Bool
greekChecks runIn = do
  let (s0, k, r, b, sigma, t) = (100, 100, 0.05, 0, 0.3, 1)
      sq    = sigma * sqrt t
      d1    = (log (s0/k) + (r + sigma*sigma/2)*t) / sq
      d2    = d1 - sq
      npdf x = exp (-x*x/2) / sqrt (2*pi)
      deltaRef = normcdf d1
      gammaRef = npdf d1 / (s0 * sq)
      vegaRef  = s0 * npdf d1 * sqrt t
      thetaRef = -(s0 * npdf d1 * sigma / (2 * sqrt t)) - r*k*exp (-r*t) * normcdf d2
      rhoRef   = k*t*exp (-r*t) * normcdf d2
  bsG  <- runIn (E.blackScholesGreeks 1 s0 k r b sigma t >>= fromFuthark)
            :: IO (M.Array M.S M.Ix1 Double)
  crrG <- runIn (E.binomialGreeks 0 0 1 s0 k r b sigma t 1000 >>= fromFuthark)
            :: IO (M.Array M.S M.Ix1 Double)
  mcG  <- runIn (E.mcGreeks 1 s0 k r b sigma t 100000 100 42 >>= fromFuthark)
            :: IO (M.Array M.S M.Ix1 Double)
  let [_, bsDelta, bsGamma, bsVega, bsTheta, bsRho] = M.toList bsG
      [_, crrDelta, crrVega, crrTheta, crrRho]      = M.toList crrG
      [_, mcDelta, mcVega, _mcTheta, _mcRho]        = M.toList mcG
  oks <- mapM report
    [ Check "AD delta matches closed form (Black-Scholes)" bsDelta deltaRef 1e-9
    , Check "AD gamma matches closed form (Black-Scholes)" bsGamma gammaRef 1e-9
    , Check "AD vega matches closed form (Black-Scholes)"  bsVega  vegaRef  1e-9
    , Check "AD theta matches closed form (Black-Scholes)" bsTheta thetaRef 1e-9
    , Check "AD rho matches closed form (Black-Scholes)"   bsRho   rhoRef   1e-9
    -- the tree price is piecewise linear in s0, so its AD delta is a step
    -- function oscillating around the continuous delta with amplitude of
    -- roughly one terminal-node weight (~0.02 at 1000 steps)
    , Check "AD delta through CRR tree" crrDelta deltaRef 0.03
    , Check "AD vega through CRR tree"  crrVega  vegaRef  0.5
    , Check "AD theta through CRR tree" crrTheta thetaRef 0.2
    , Check "AD rho through CRR tree"   crrRho   rhoRef   0.5
    , Check "AD pathwise delta (Monte Carlo)" mcDelta deltaRef 0.02
    , Check "AD pathwise vega (Monte Carlo)"  mcVega  vegaRef  1.0
    ]
  pure (and oks)

-- * Miletus reference data

-- | Same as Julia's isapprox with Miletus' test tolerances
approx :: Double -> Double -> Bool
approx x y = abs (x - y) <= max atol (rtol * max (abs x) (abs y))
  where
    rtol = 1e-3
    atol = 1e-6

-- | bs.csv row: startprice,strikeprice,days,interestrate,carryrate,sigma,c,p
bsRow :: Runner -> [Double] -> IO [String]
bsRow runIn [s0, k, days, r, b, sigma, c, p] = do
  let t = days / 365
  cv <- runIn (E.blackScholes 1 s0 k r b sigma t)
  pv <- runIn (E.blackScholes 0 s0 k r b sigma t)
  pure $ mismatch "call" [s0, k, days, r, b, sigma] cv c
      ++ mismatch "put"  [s0, k, days, r, b, sigma] pv p
bsRow _ row = pure ["malformed row: " ++ show row]

-- | tree csv row: startprice,strikeprice,days,interestrate,carryrate,sigma,
--   nsteps,ce,pe,ca,pa
treeRow :: Runner -> Int8 -> [Double] -> IO [String]
treeRow runIn variant [s0, k, days, r, b, sigma, n, ce, pe, ca, pa] = do
  let t = days / 365
      steps = round n :: Int64
      price exercise kind = runIn (E.binomial variant exercise kind s0 k r b sigma t steps)
      params = [s0, k, days, r, b, sigma, n]
  ce' <- price 0 1
  pe' <- price 0 0
  ca' <- price 1 1
  pa' <- price 1 0
  pure $ mismatch "european call" params ce' ce
      ++ mismatch "european put"  params pe' pe
      ++ mismatch "american call" params ca' ca
      ++ mismatch "american put"  params pa' pa
treeRow _ _ row = pure ["malformed row: " ++ show row]

mismatch :: String -> [Double] -> Double -> Double -> [String]
mismatch what params got want
  | approx got want = []
  | otherwise = [printf "  %s %s: got %.10g, want %.10g" what (show params) got want]

csvSuite :: String -> ([Double] -> IO [String]) -> IO Bool
csvSuite name checkRow = do
  rows <- parseCsv ("test/data/" ++ takeWhile (/= ' ') name)
  failures <- concat <$> mapM checkRow rows
  mapM_ putStrLn (take 10 failures)
  unless (null (drop 10 failures)) $
    printf "  ... and %d more\n" (length failures - 10)
  printf "%s %s: %d rows, %d mismatches\n"
         (if null failures then "PASS " else "FAIL ") name (length rows) (length failures)
  pure (null failures)

parseCsv :: FilePath -> IO [[Double]]
parseCsv fp = do
  content <- readFile fp
  case lines content of
    (_header : rows) -> pure (map (map read . splitOn ',') (filter (not . null) rows))
    [] -> pure []

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (field, _ : rest) -> field : splitOn c rest
  (field, [])       -> [field]
