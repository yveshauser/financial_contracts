{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Models
 ( ModelChoice(..)
 , P(..)
 , greeksWith
 , runFutT
 )
where

import Contracts
import Control.Applicative (Alternative, liftA2)
import Control.Monad.Identity (Identity, runIdentity)
import Derivatives
import Data.Int
import qualified Data.Massiv.Array as M
import Models.Futhark (getContext, runFutIn, runFutTIn, Output(..))
import qualified Models.Futhark.Entries as E
import Models.Futhark.Context (Context(..))
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
  deriving (Show, Eq, Enum, Bounded)

type Arr' b = M.Array M.D M.Ix1 b
newtype P c m a = P (ModelChoice -> FutT c m (Arr' a))

instance Functor m => Functor (P c m) where
  fmap f (P x) = P (mapFutT g . x)
    where
      g = fmap (fmap f)

instance Applicative m => Applicative (P c m) where
  pure a = P $ \_ -> pure $ M.makeArray M.Seq (M.Sz 1600) (const a)
  (<*>) = zipP ($)

instance Applicative m => Process (P c m) where
  ifP pb p1 p2 = g <$> zipP (,) pb (zipP (,) p1 p2)
    where
      g (True, p) = fst p
      g (False, p) = snd p

  zipP g (P p1) (P p2) = P $ \mc -> let g' = liftA2 (M.zipWith g) in map2FutT g' (p1 mc) (p2 mc)
  obs = undefined

instance (Monad m, Process (P c m)) => Model (P c m) where
   exch = undefined
   disc a (p1,p2) = P run
   snell = undefined
   absorb = undefined

run :: Monad m => ModelChoice -> FutT c m (Arr' Double)
run mc = do
  price <- priceWith mc
  pure $ M.makeArray M.Seq (M.Sz 1600) (const price)

-- | Price one option with the chosen model.
-- FIXME: option parameters are placeholders (an at-the-money European call:
-- s0 = k = 100, r = 5%, no carry, sigma = 30%, t = 1y), not taken from the
-- contract. All models should agree on ~14.23 (LSMC values the American
-- call, which equals the European one in the absence of dividends).
priceWith :: Monad m => ModelChoice -> FutT c m Double
priceWith mc = case mc of
    BlackScholes -> E.blackScholes call s0 k r b sigma t
    CRR          -> E.binomial 0 european call s0 k r b sigma t steps
    JR           -> E.binomial 1 european call s0 k r b sigma t steps
    JRrn         -> E.binomial 2 european call s0 k r b sigma t steps
    Tian         -> E.binomial 3 european call s0 k r b sigma t steps
    MonteCarlo   -> E.mcEuropean call s0 k r b sigma t npaths 100 seed
    LSMC         -> E.lsmcAmerican call s0 k r b sigma t npaths 50 3 seed
  where
    (s0, k, r, b, sigma, t) = (100, 100, 0.05, 0, 0.3, 1)
    (call, european) = (1, 0)
    steps = 1000
    npaths = 100000
    seed = 42

-- | Greeks of the same placeholder option, by automatic differentiation of
-- the model (forward-mode AD in Futhark). LSMC has none: differentiating
-- through the regression and exercise decisions is not meaningful.
greeksWith :: forall c m. Monad m => ModelChoice -> FutT c m [(String, Double)]
greeksWith mc = case mc of
    BlackScholes -> labels6 <$> arr (E.blackScholesGreeks call s0 k r b sigma t)
    CRR          -> labels5 <$> arr (E.binomialGreeks 0 european call s0 k r b sigma t steps)
    JR           -> labels5 <$> arr (E.binomialGreeks 1 european call s0 k r b sigma t steps)
    JRrn         -> labels5 <$> arr (E.binomialGreeks 2 european call s0 k r b sigma t steps)
    Tian         -> labels5 <$> arr (E.binomialGreeks 3 european call s0 k r b sigma t steps)
    MonteCarlo   -> labels5 <$> arr (E.mcGreeks call s0 k r b sigma t npaths 100 seed)
    LSMC         -> pure []
  where
    arr :: FutT c m (F64_1d c) -> FutT c m [Double]
    arr e = M.toList <$> (e >>= fromFuthark :: FutT c m (M.Array M.S M.Ix1 Double))
    labels6 = zip ["value", "delta", "gamma", "vega", "theta", "rho"]
    labels5 = zip ["value", "delta", "vega", "theta", "rho"]
    (s0, k, r, b, sigma, t) = (100, 100, 0.05, 0, 0.3, 1)
    (call, european) = (1, 0)
    steps = 1000
    npaths = 100000
    seed = 42
