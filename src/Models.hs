{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Models
 ( ModelChoice(..)
 , Quote(..)
 , Market(..)
 , defaultMarket
 , Lattice
 , Paths
 , Proc
 , runValue
 , runPaths
 , priceWith
 , greeksWith
 , runFutT
 )
where

import Contracts
import Data.Int
import Data.Maybe (fromMaybe)
import qualified Data.Massiv.Array as M
import Models.Futhark (Output(..), Input(..))
import qualified Models.Futhark.Entries as E
import Models.Futhark.Types (F64_1d)
import Models.Futhark.Fut
import Valuation

-- | The pricing model backing the value process (the model set of Miletus.jl)
data ModelChoice
  = BlackScholes -- ^ analytic Black-Scholes        (Miletus: GeomBMModel)
  | CRR          -- ^ Cox-Ross-Rubinstein tree      (Miletus: CRRModel)
  | JR           -- ^ Jarrow-Rudd tree              (Miletus: JRModel)
  | JRrn         -- ^ Jarrow-Rudd risk-neutral tree (Miletus: JRrnModel)
  | Tian         -- ^ Tian tree                     (Miletus: TianModel)
  | MonteCarlo   -- ^ GBM Monte Carlo               (Miletus: MonteCarloModel)
  | LSMC         -- ^ Longstaff-Schwartz            (Miletus: LeastSquaresMonteCarlo)
  | Lattice      -- ^ "Composing contracts" value-process lattice (Peyton Jones et al. §8)
  deriving (Show, Eq, Enum, Bounded)

-- | A value process is a binomial lattice of @a@-values: slice (row) i holds
-- the states reachable at step i, column j being j up-moves. Booleans live in
-- their own lattice; the model operators encode them as 1/0 to thread them
-- through Futhark. The upper triangle (j > i) is unused.
type Proc a = M.Array M.D M.Ix2 a

-- | Per-asset market data: spot price, volatility and carry (e.g. a dividend
-- yield). The risk-neutral drift of the asset is @rate - carry@.
data Quote = Quote { spot :: Double, vol :: Double, carry :: Double }

-- | The market environment a contract is priced against: per-asset quotes, the
-- risk-free rate, and the lattice horizon/resolution. The contract supplies the
-- structure (payoffs, strikes, exercise dates); the 'Market' supplies the data.
data Market = Market
  { quote      :: Asset -> Quote  -- ^ market data per asset
  , rate       :: Double          -- ^ risk-free rate (of the reference currency)
  , horizon    :: Double          -- ^ lattice horizon in years; contract times must not exceed it
  , steps      :: Int             -- ^ number of lattice steps
  , variant    :: Int8            -- ^ binomial variant: 0 = CRR, 1 = JR, 2 = JRrn, 3 = Tian
  , underlying :: Asset           -- ^ the asset whose dynamics drive the lattice probability
  , npaths     :: Int             -- ^ number of Monte Carlo paths
  , nsteps     :: Int             -- ^ number of Monte Carlo time steps
  , seed       :: Int             -- ^ Monte Carlo RNG seed
  }

-- | A sample environment: one stock per ticker, the reference currency worth 1,
-- a Jarrow-Rudd lattice on Stk X.
-- (FIXME: the horizon should come from the contract's own dates; the lattice is
-- single-underlying, named by 'underlying'.)
defaultMarket :: Market
defaultMarket = Market
  { quote = \a -> case a of
      Stk X -> Quote 100 0.30 0
      Stk Y -> Quote  50 0.20 0
      Stk Z -> Quote 120 0.25 0
      Cur _ -> Quote   1 0    0
  , rate       = 0.05
  , horizon    = 1
  , steps      = 500
  , variant    = 1        -- JR (matches the previous fixed lattice)
  , underlying = Stk X
  , npaths     = 100000
  , nsteps     = 50
  , seed       = 42
  }

-- | The lattice value process: given the market environment, a Futhark
-- computation yielding the lattice of @a@-values. This is one 'Model' instance;
-- a path-based instance (Monte Carlo / LSMC) would be a sibling type.
newtype Lattice c m a = Lat (Market -> FutT c m (Proc a))

-- | The lattice shape for an environment.
latSize :: Market -> M.Sz M.Ix2
latSize mkt = M.Sz2 (steps mkt + 1) (steps mkt + 1)

-- | The lattice time step (years) for an environment.
dtStep :: Market -> Double
dtStep mkt = horizon mkt / fromIntegral (steps mkt)

-- | The lattice row (step) for a contract time, in years.
stepOf :: Market -> Time -> Int
stepOf mkt t = max 0 (min n (round (fromIntegral t / horizon mkt * fromIntegral n)))
  where n = steps mkt

instance Monad m => Functor (Lattice c m) where
  fmap f (Lat g) = Lat (fmap (M.map f) . g)

instance Monad m => Applicative (Lattice c m) where
  pure a = Lat (\mkt -> pure (M.makeArrayR M.D M.Seq (latSize mkt) (const a)))
  (<*>) = zipP ($)

instance Monad m => Process (Lattice c m) where
  zipP f (Lat g1) (Lat g2) = Lat (\mkt -> M.zipWith f <$> g1 mkt <*> g2 mkt)
  ifP (Lat gb) (Lat g1) (Lat g2) =
    Lat (\mkt -> M.zipWith3 sel <$> gb mkt <*> g1 mkt <*> g2 mkt)
    where sel b x y = if b then x else y
  -- a boolean process comparing the time of each slice to the threshold t
  obs cmp t = Lat (\mkt -> pure (M.makeArrayR M.D M.Seq (latSize mkt) (node mkt)))
    where node mkt (i M.:. _) = cmp (toInteger i) (toInteger (stepOf mkt t))

-- | The model operators of the lattice value process, threaded with the
-- *actual* boolean and value processes (no placeholder option) and the market:
--   @one@   (exch):   the asset's price lattice (from its 'Quote'), in the
--                     market's binomial variant;
--   @when@  (disc),   @anytime@ (snell), @until@ (absorb): the backward
--   induction in @Lattice.fut@, fed the real processes as n*n lattices and the
--   variant's risk-neutral up-probability.
instance Monad m => Model (Lattice c m) where
  exch k k1
    | k == k1   = Lat (\mkt -> pure (M.makeArrayR M.D M.Seq (latSize mkt) (const 1)))
    | otherwise = Lat (\mkt -> priceProc mkt k1)
  disc   _ = latModel 0
  snell  _ = latModel 1
  absorb _ = latModel 2

-- | @one k@ for a non-reference asset: the price process of that asset, taken
-- from its quote in the market, built in Futhark and brought back as a lattice.
priceProc :: forall c m. Monad m => Market -> Asset -> FutT c m (Proc Double)
priceProc mkt a = do
  let q = quote mkt a
  fa  <- E.priceLattice (variant mkt) (spot q) (rate mkt) (carry q) (vol q) (horizon mkt)
                        (fromIntegral (steps mkt + 1))
  arr <- fromFuthark fa :: FutT c m (M.Array M.S M.Ix2 Double)
  pure (M.delay arr)

-- | Thread the boolean process @b@ and value process @v@ through one of the
-- Futhark lattice operators (0 = disc, 1 = snell, 2 = absorb): encode @b@ as
-- 1/0, ship both lattices over, run the backward induction with the variant's
-- up-probability (from the underlying's dynamics), bring it back.
latModel :: forall c m. Monad m => Int8 -> (Lattice c m Bool, Lattice c m Double) -> Lattice c m Double
latModel mode (Lat gb, Lat gv) = Lat $ \mkt -> do
  b <- gb mkt
  v <- gv mkt
  fb  <- toFuthark (M.computeAs M.S (M.map bit b))
  fv  <- toFuthark (M.computeAs M.S v)
  let u     = quote mkt (underlying mkt)
      kappa = rate mkt - carry u
  out <- op (variant mkt) kappa (vol u) (rate mkt) (dtStep mkt) fb fv
  res <- fromFuthark out :: FutT c m (M.Array M.S M.Ix2 Double)
  pure (M.delay res)
  where
    bit x = if x then 1 else 0
    op = case mode of 1 -> E.snell; 2 -> E.absorb; _ -> E.disc

-- | Run a lattice value process in a market and read its value at the root.
runValue :: Monad m => Lattice c m Double -> Market -> FutT c m Double
runValue (Lat g) mkt = (\a -> M.evaluate' a (0 M.:. 0)) <$> g mkt

-- * The path-based value process (Monte Carlo / Longstaff-Schwartz)
--
-- A sibling 'Model' instance: a value process is an [npaths][nsteps+1] array
-- (row = path, column = time). exch = simulated price paths; disc/absorb = plain
-- Monte Carlo; snell = Longstaff-Schwartz. The elementwise combinators are the
-- same Massiv operations as the lattice; only the shape and operators differ.

-- | The path-based value process: given the market, a Futhark computation
-- yielding an [npaths][nsteps+1] array of @a@-values.
newtype Paths c m a = Pth (Market -> FutT c m (Proc a))

-- | The path-array shape for an environment.
pathSize :: Market -> M.Sz M.Ix2
pathSize mkt = M.Sz2 (npaths mkt) (nsteps mkt + 1)

-- | The Monte Carlo time step (years).
pdt :: Market -> Double
pdt mkt = horizon mkt / fromIntegral (nsteps mkt)

-- | The path column (time step) for a contract time, in years.
pstepOf :: Market -> Time -> Int
pstepOf mkt t = max 0 (min n (round (fromIntegral t / horizon mkt * fromIntegral n)))
  where n = nsteps mkt

instance Monad m => Functor (Paths c m) where
  fmap f (Pth g) = Pth (fmap (M.map f) . g)

instance Monad m => Applicative (Paths c m) where
  pure a = Pth (\mkt -> pure (M.makeArrayR M.D M.Seq (pathSize mkt) (const a)))
  (<*>) = zipP ($)

instance Monad m => Process (Paths c m) where
  zipP f (Pth g1) (Pth g2) = Pth (\mkt -> M.zipWith f <$> g1 mkt <*> g2 mkt)
  ifP (Pth gb) (Pth g1) (Pth g2) =
    Pth (\mkt -> M.zipWith3 sel <$> gb mkt <*> g1 mkt <*> g2 mkt)
    where sel b x y = if b then x else y
  -- a boolean process comparing each time column to the threshold t
  obs cmp t = Pth (\mkt -> pure (M.makeArrayR M.D M.Seq (pathSize mkt) (node mkt)))
    where node mkt (_ M.:. s) = cmp (toInteger s) (toInteger (pstepOf mkt t))

-- | The model operators of the path-based value process: @one@ (exch) = the
-- asset's price paths; @when@ (disc) and @until@ (absorb) = plain Monte Carlo;
-- @anytime@ (snell) = Longstaff-Schwartz (the only operator that looks across
-- paths, by regression).
instance Monad m => Model (Paths c m) where
  exch k k1
    | k == k1   = Pth (\mkt -> pure (M.makeArrayR M.D M.Seq (pathSize mkt) (const 1)))
    | otherwise = Pth (\mkt -> pathProc mkt k1)
  disc   _ = pathModel 0
  snell  _ = pathModel 1
  absorb _ = pathModel 2

-- | @one k@: the simulated price paths of the asset, from its quote.
pathProc :: forall c m. Monad m => Market -> Asset -> FutT c m (Proc Double)
pathProc mkt a = do
  let q = quote mkt a
  fa  <- E.pricePaths (spot q) (rate mkt) (carry q) (vol q) (horizon mkt)
                      (fromIntegral (npaths mkt)) (fromIntegral (nsteps mkt + 1))
                      (fromIntegral (seed mkt))
  arr <- fromFuthark fa :: FutT c m (M.Array M.S M.Ix2 Double)
  pure (M.delay arr)

-- | Thread the boolean and value processes through a path operator (0 = disc,
-- 1 = snell, 2 = absorb). snell is degree-3 Longstaff-Schwartz.
pathModel :: forall c m. Monad m
          => Int8 -> (Paths c m Bool, Paths c m Double) -> Paths c m Double
pathModel mode (Pth gb, Pth gv) = Pth $ \mkt -> do
  b <- gb mkt
  v <- gv mkt
  fb  <- toFuthark (M.computeAs M.S (M.map bit b))
  fv  <- toFuthark (M.computeAs M.S v)
  out <- case mode of
           1 -> E.snellPaths 3 (rate mkt) (pdt mkt) fb fv
           2 -> E.absorbPaths (rate mkt) (pdt mkt) fb fv
           _ -> E.discPaths (rate mkt) (pdt mkt) fb fv
  res <- fromFuthark out :: FutT c m (M.Array M.S M.Ix2 Double)
  pure (M.delay res)
  where
    bit x = if x then 1 else 0

-- | Run a path-based value process and read off its price: the mean over paths
-- of the value at time 0 (column 0).
runPaths :: Monad m => Paths c m Double -> Market -> FutT c m Double
runPaths (Pth g) mkt = mean <$> g mkt
  where
    mean a = M.sum (col a) / fromIntegral (npaths mkt)
    col a  = M.computeAs M.S
               (M.makeArrayR M.D M.Seq (M.Sz1 (npaths mkt)) (\w -> M.evaluate' a (w M.:. 0)))

-- | A vanilla European option recovered from a contract's structure: its kind
-- (1 = call, 0 = put), underlying asset, strike and expiry.
data Vanilla = Vanilla { vKind :: Int8, vUnder :: Asset, vStrike :: Double, vExpiry :: Time }

-- | Recognise the @european@ shape (Derivatives.european) so its parameters can
-- be fed to the AD pricers. Anything else (american, exotics) yields 'Nothing'.
vanilla :: Contract -> Maybe Vanilla
vanilla (When (At t) (Or body Zero)) = case body of
    And (One a) (Give (Scale (Konst s) (One (Cur _)))) -> Just (Vanilla 1 a s t)
    And (Give (One a)) (Scale (Konst s) (One (Cur _))) -> Just (Vanilla 0 a s t)
    _                                                  -> Nothing
vanilla _ = Nothing

-- | Price a contract with the chosen model.
--
-- The binomial family (CRR/JR/JRrn/Tian) are value-process models: they run the
-- 'Lattice' instance over the *whole* contract, with that variant — so they
-- handle arbitrary contracts, not just vanillas, and agree with the scalar trees
-- on vanillas. 'Lattice' uses the market's own variant. 'MonteCarlo' and 'LSMC'
-- run the path-based 'Paths' instance (disc/absorb = plain Monte Carlo, snell =
-- Longstaff-Schwartz). Black-Scholes has no value process, so it takes a
-- recognised vanilla European (see 'vanilla') through its closed form and
-- otherwise falls back to the lattice. @ref@ is the currency the value is in.
priceWith :: Monad m => Asset -> ModelChoice -> Market -> Contract -> FutT c m Double
priceWith ref mc mkt con = case mc of
    CRR          -> lattice 0
    JR           -> lattice 1
    JRrn         -> lattice 2
    Tian         -> lattice 3
    Lattice      -> runValue (evalC ref con) mkt
    MonteCarlo   -> runPaths (evalC ref con) mkt
    LSMC         -> runPaths (evalC ref con) mkt
    BlackScholes -> fromMaybe (runValue (evalC ref con) mkt) (vanilla con >>= bs)
  where
    lattice v = runValue (evalC ref con) (mkt { variant = v })
    bs o      = Just (E.blackScholes (vKind o) (s0 o) (vStrike o) (rate mkt) (cb o) (sg o) (texp o))
    q o       = quote mkt (vUnder o)
    s0 o      = spot (q o)
    cb o      = carry (q o)
    sg o      = vol (q o)
    texp o    = fromIntegral (vExpiry o)

-- | Greeks of a contract, by automatic differentiation of the model (forward-
-- mode AD in Futhark). The option's parameters come from the contract and the
-- market; only vanilla European options are supported (the AD pricers are the
-- scalar option pricers), so other contracts and LSMC/Lattice yield none.
greeksWith :: forall c m. Monad m
           => ModelChoice -> Market -> Contract -> FutT c m [(String, Double)]
greeksWith mc mkt con = case vanilla con of
  Nothing  -> pure []
  Just opt ->
    let q     = quote mkt (vUnder opt)
        s0    = spot q
        b     = carry q
        sigma = vol q
        k     = vStrike opt
        r     = rate mkt
        t     = fromIntegral (vExpiry opt)
        kind  = vKind opt
        n     = fromIntegral (steps mkt)
    in case mc of
        BlackScholes -> labels6 <$> arr (E.blackScholesGreeks kind s0 k r b sigma t)
        CRR          -> labels5 <$> arr (E.binomialGreeks 0 0 kind s0 k r b sigma t n)
        JR           -> labels5 <$> arr (E.binomialGreeks 1 0 kind s0 k r b sigma t n)
        JRrn         -> labels5 <$> arr (E.binomialGreeks 2 0 kind s0 k r b sigma t n)
        Tian         -> labels5 <$> arr (E.binomialGreeks 3 0 kind s0 k r b sigma t n)
        MonteCarlo   -> labels5 <$> arr (E.mcGreeks kind s0 k r b sigma t npaths 100 seed)
        LSMC         -> pure []
        Lattice      -> pure []  -- no AD entry; the lattice price matches JR's greeks
  where
    arr :: FutT c m (F64_1d c) -> FutT c m [Double]
    arr e = M.toList <$> (e >>= fromFuthark :: FutT c m (M.Array M.S M.Ix1 Double))
    labels6 = zip ["value", "delta", "gamma", "vega", "theta", "rho"]
    labels5 = zip ["value", "delta", "vega", "theta", "rho"]
    npaths = 100000
    seed = 42
