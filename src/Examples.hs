{-# LANGUAGE RankNTypes #-}
module Examples where

import Contracts hiding (lift)
import Derivatives
import Valuation

import Prelude hiding (and, or)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Data.Functor ((<&>))
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
  return $ (\t -> s_0 * exp (μ-(σ*σ)/2) * int2Double t) + (const σ * w_t)

example_model :: MonadSample m => Model m
example_model = Model {
    modelStart = 0
  , exch = example_exch
  , disc = example_disc
  , snell = example_snell
  , absorb = example_absorb
}

example_exch :: MonadSample m => Currency -> Currency -> Process m Double
example_exch k1 k2 | k1 == k2 = return $ const 1
example_exch k1 k2 | k2 < k1 = example_exch k2 k1
example_exch _ _ = geometric_brownian_motion 1.0 0.1 0.1

intrest_rate :: MonadSample m => Process m Double
intrest_rate = geometric_brownian_motion 0.0 0.1 0.1

example_disc :: MonadSample m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_disc _ (b, d) = do
  p1 <- d
  p2 <- intrest_rate
  pb <- b
  return $ \t -> if pb t then p1 t else p1 t/(1 + p2 t/100)

example_snell :: MonadSample m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_snell _  = undefined

example_absorb :: MonadSample m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_absorb _ = undefined

test :: MonadSample m => Contract -> Process m Double
test = evalC example_model CHF

test1, test2, test3, test4, test5, test6, test7, test8 :: Process SamplerIO Double
test1 = test zero
test2 = test one_chf
test3 = test $ times 5 one_chf
test4 = test $ zcb 0 100
test5 = test $ european Call 0 0 one_chf
test6 = test $ american Call (0, 0) 1 one_chf
test7 = test $ brc 10 1000 10.0 8.0 one_chf
test8 = test one_eur

main :: IO ()
main = do
  sampleIO test1 >>= \f -> print (f 0)
  sampleIO test2 >>= \f -> print (f 0)
  sampleIO test3 >>= \f -> print (f 0)
  sampleIO test4 >>= \f -> print (f 0)
  sampleIO test5 >>= \f -> print (f 0)
  -- sampleIO test6 >>= \f -> print (f 0)
  sampleIO test7 >>= \f -> print (f 0)
  sampleIO test8 >>= \f -> print (f 1)
