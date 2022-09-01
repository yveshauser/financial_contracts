{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Examples where

import Contracts hiding (lift)
import Derivatives
import Valuation

import Prelude hiding (and, or)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Data.Functor ((<&>))
import Data.Number.Erf
import GHC.Float

{-# ANN module "HLint: ignore Use camelCase" #-}

-------------------
-- Distributions --
-------------------
std_normal :: MonadSample m => Process m Double
std_normal = normal 0 1 <&> const

wiener :: MonadSample m => Process m Double
wiener = normal 0 1 >>= (return . (!!) . scanl1 (+)) . repeat

geometric_brownian_motion :: MonadSample m => Double -> Double -> Double -> Process m Double
geometric_brownian_motion μ σ s_0 = do
  w_t <- wiener
  return $ \t -> s_0 * exp (((μ-(σ*σ)/2) * int2Double t) + (σ * w_t t))

example_model :: MonadSample m => Model m
example_model = Model {
    modelStart = 0
  , exch = example_exch
  , disc = example_disc
  , snell = example_snell
  , absorb = example_absorb
}

-- FIXME: Make sure, no-arbitrage conditions hold:
-- example_exch k1 k2 * example_exch k2 k3 = example_exch k1 k3
example_exch :: MonadSample m => Asset -> Asset -> Process m Double
example_exch k1 k2 | k1 == k2 = return $ const 1
example_exch (Cur CHF) (Cur EUR) = return $ const 1
example_exch (Cur EUR) (Cur CHF) = return $ const 1
example_exch k1 k2 | k2 < k1 = example_exch k2 k1
example_exch (Cur CHF) (Stk X) = return $ const 12
example_exch (Cur CHF) (Stk Y) = geometric_brownian_motion 0.1 0.1 10.0
example_exch (Cur CHF) (Stk Z) = geometric_brownian_motion 0.1 0.1 100.0
example_exch _ _ = undefined

intrest_rate :: MonadSample m => Process m Double
intrest_rate = geometric_brownian_motion 0.1 0.1 0.0

example_disc :: MonadSample m => Asset -> (Process m Bool, Process m Double) -> Process m Double
example_disc _ (b, d) = do
  p1 <- d
  p2 <- intrest_rate
  pb <- b
  return $ \t -> if pb t then p1 t else p1 t/(1 + p2 t/100)

example_snell :: MonadSample m => Asset -> (Process m Bool, Process m Double) -> Process m Double
example_snell _  = undefined

example_absorb :: MonadSample m => Asset -> (Process m Bool, Process m Double) -> Process m Double
example_absorb _ = undefined

sampleGBM :: IO ()
sampleGBM = do
  f <- sampleIO (geometric_brownian_motion 1.0 1.0 100.0)
  print $ map f [1 .. 100]

-- analytical pricing using Black-Scholes Model

evalModel :: Contract -> Maybe Double
evalModel (When (At t) (Or (And a (Give (Scale (Konst k) (One (Cur c))))) Zero)) =
  do σ <- vola a
     s <- price a
     let r = 0.0
     Just $ call s 0 k r σ 1
evalModel (When (At t) (Or (And (Give (Scale (Konst k) (One (Cur c)))) a) Zero)) =
  do σ <- vola a
     s <- price a
     let r = 0.0
     Just $ call s 0 k r σ 1
evalModel (When (At t) (Or (And (Give a) ((Scale (Konst k) (One (Cur c))))) Zero)) =
  do σ <- vola a
     s <- price a
     let r = 0.0
     Just $ put s 0 k r σ 1
evalModel (When (At t) (Or (And ((Scale (Konst k) (One (Cur c)))) (Give a)) Zero)) =
  do σ <- vola a
     s <- price a
     let r = 0.0
     Just $ put s 0 k r σ 1
evalModel _ = Nothing

vola :: Contract -> Maybe Double
vola (One (Stk X)) = Just 0.0
vola _ = Nothing

price :: Contract -> Maybe Double
price (One (Stk X)) = Just 12
price _ = Nothing

call ::
     Double -- ^ Price of Underlying
  -> Double -- ^ Time
  -> Double -- ^ Strike
  -> Double -- ^ Interest Rate
  -> Double -- ^ Volatility
  -> Double -- ^ Maturity
  -> Double -- ^ Option Premium
call s t k r σ m =
  let d1 = (log (s/k) + (r + 0.5*σ^2)*(m-t))/(σ*sqrt(m-t))
      d2 = d1 - σ*sqrt(m-t)
      n1 = 0.5*(1+ erf(d1/sqrt 2))
      n2 = 0.5*(1+ erf(d2/sqrt 2))
   in s*n1 - k*exp(-r*(m-t))*n2

put ::
     Double -- ^ Price of Underlying
  -> Double -- ^ Time
  -> Double -- ^ Strike
  -> Double -- ^ Interest Rate
  -> Double -- ^ Volatility
  -> Double -- ^ Maturity
  -> Double -- ^ Option Premium
put s t k r σ m =
  let d1 = (log (s/k) + (r + 0.5*σ^2)*(m-t))/(σ*sqrt(m-t))
      d2 = d1 - σ*sqrt(m-t)
      n1 = 0.5*(1+ erf(-d1/sqrt 2))
      n2 = 0.5*(1+ erf(-d2/sqrt 2))
   in k*exp(-r*(m-t))*n2 - s*n1

-- examples
main :: IO ()
main = do
  putStrLn "=================== Zero ==================="
  sample zero >>= showAt 10

  putStrLn "=================== One X ==================="
  let oneX = one (Stk X) :: Contract
  toValue <- sample oneX

  print oneX
  print $
    let times = [0..100]
     in zip times (map toValue times)

  putStrLn "=================== 1 CHF ==================="
  sample (scale 1 chf) >>= showAt 10

  putStrLn "=================== 5 CHF ==================="
  sample (scale 5 chf) >>= showAt 10

  putStrLn "=================== ZCB ==================="
  let zcbX = zcb 0 100

  print zcbX
  sample zcbX >>= showAt 10

  putStrLn "=================== European Call on X ==================="
  let euCallOnX = european Call 100 10.0 CHF (one (Stk X)) :: Contract
  f <- sample euCallOnX

  print euCallOnX
  print $
    let times = [0..100]
     in zip times (map f times)

  print $ evalModel euCallOnX

  putStrLn "=================== American Call on X ==================="
  -- TODO: Implement snell
  -- let amCallOnX = american Call (0, 0) 1 CHF (one (Stk X))

  putStrLn "=================== BRC on X ==================="
  let brcX = brc 10 1000 10.0 8.0 CHF (one (Stk X))

  print brcX
  sample brcX >>= showAt 10

  putStrLn "=================== 1 EUR ==================="
  sample (scale 1 eur) >>= showAt 10

  where
    showAt x f = print (f x)

    test :: MonadSample m => Contract -> Process m Double
    test = evalC example_model (Cur CHF)

    sample :: Contract -> IO (Time -> Double)
    sample = sampleIO . test
