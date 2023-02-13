{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Models.GeometricBrownianMotion where

import Contracts
import Derivatives
import Valuation

import Prelude hiding (and, or, take, zip)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Data.Functor ((<&>))
import Data.Number.Erf
import List.Transformer as T
import GHC.Float

import Graphics.Rendering.Chart.Easy hiding (scale, index)
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Monad.Bayes.Sampler.Strict (sampleIO)

{-# ANN module "HLint: ignore Use camelCase" #-}

-------------------
-- Distributions --
-------------------

wiener :: MonadDistribution m => Process m Double
wiener =
  ListT
    ( do
        r <- normal 0 1
        return (Cons r wiener)
    )

geombm :: MonadDistribution m => Double -> Double -> Double -> Process m Double
geombm μ σ s_0 =
  let f (t, w) = ((μ - (σ * σ) / 2) * t) + (σ * w)
   in bigK s_0 * fmap (exp . f) (zip index wiener)

geombm_model :: MonadDistribution m => Model m
geombm_model = Model {
    modelStart = 0
  , exch = geombm_exch
  , disc = geombm_disc
  , snell = geombm_snell
  , absorb = geombm_absorb
}

-- FIXME: Make sure, no-arbitrage conditions hold:
-- geombm_exch k1 k2 * geombm_exch k2 k3 = geombm_exch k1 k3
geombm_exch :: MonadDistribution m => Asset -> Asset -> Process m Double
geombm_exch k1 k2 | k1 == k2 = bigK 1
geombm_exch (Cur CHF) (Cur EUR) = bigK 1
geombm_exch (Cur EUR) (Cur CHF) = bigK 1
geombm_exch k1 k2 | k2 < k1 = geombm_exch k2 k1
geombm_exch (Cur CHF) (Stk X) = bigK 12
geombm_exch (Cur CHF) (Stk Y) = geombm 0.1 0.1 10.0
geombm_exch (Cur CHF) (Stk Z) = geombm 0.1 0.1 100.0
geombm_exch _ _ = empty

intrest_rate :: MonadDistribution m => Process m Double
intrest_rate = geombm 0.1 0.1 0.0

geombm_disc :: MonadDistribution m => Asset -> (Process m Bool, Process m Double) -> Process m Double
geombm_disc _ (b, d) = do
  p1 <- d
  p2 <- intrest_rate
  pb <- b
  return $ if pb then p1 else p1/(1 + p2/100)

geombm_snell :: MonadDistribution m => Asset -> (Process m Bool, Process m Double) -> Process m Double
geombm_snell _ _ = empty -- TODO

geombm_absorb :: MonadDistribution m => Asset -> (Process m Bool, Process m Double) -> Process m Double
geombm_absorb _ _ = empty -- TODO

-- analytical pricing using Black-Scholes Model

evalModel :: Contract -> Maybe Double
evalModel (When (At t) (Or (And a (Give (Scale (Konst k) (One (Cur c))))) Zero)) =
  do σ <- vola a
     s <- price a
     let r = 0.0
     Just $ call s 0 k r σ t
evalModel (When (At t) (Or (And (Give (Scale (Konst k) (One (Cur c)))) a) Zero)) =
  do σ <- vola a
     s <- price a
     let r = 0.0
     Just $ call s 0 k r σ t
evalModel (When (At t) (Or (And (Give a) ((Scale (Konst k) (One (Cur c))))) Zero)) =
  do σ <- vola a
     s <- price a
     let r = 0.0
     Just $ put s 0 k r σ t
evalModel (When (At t) (Or (And ((Scale (Konst k) (One (Cur c)))) (Give a)) Zero)) =
  do σ <- vola a
     s <- price a
     let r = 0.0
     Just $ put s 0 k r σ t
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
  sample zero >>= printAt 10

  putStrLn "=================== One X ==================="
  let oneX = one (Stk X) :: Contract
  print oneX

  sample oneX >>= print

  putStrLn "=================== 1 CHF ==================="
  sample (scale 1 chf) >>= printAt 10

  putStrLn "=================== 5 CHF ==================="
  sample (scale 5 chf) >>= printAt 10

  putStrLn "=================== ZCB ==================="
  let zcbX = zcb 0 100

  print zcbX
  sample zcbX >>= printAt 10

  putStrLn "=================== European Call on X ==================="
  let euCallOnX = european Call 1 10.0 CHF (one (Stk X)) :: Contract
  print euCallOnX

  sample euCallOnX >>= print
  print $ evalModel euCallOnX

  putStrLn "=================== European Call on Z ==================="
  let euCallOnZ = european Call 120 100.0 CHF (one (Stk Z)) :: Contract
  print euCallOnZ

  sample euCallOnZ >>= print
  print $ evalModel euCallOnZ

  putStrLn "=================== European Put on X ==================="
  let euPutOnX = european Put 1 14.0 CHF (one (Stk X)) :: Contract
  print euPutOnX

  sample euPutOnX >>= print

  print $ evalModel euPutOnX

  putStrLn "=================== American Call on X ==================="
  let amCallOnX = american Call (0, 0) 1 CHF (one (Stk X))
  print amCallOnX

  sample amCallOnX >>= print

  putStrLn "=================== BRC on X ==================="
  let brcX = brc 1 1000 10.0 8.0 CHF (one (Stk X))

  print brcX
  sample brcX >>= printAt 10

  putStrLn "=================== 1 EUR ==================="
  sample (scale 1 eur) >>= printAt 10

  where
    printAt i = print . (!!i)

    test :: MonadDistribution m => Contract -> Process m Double
    test = evalC geombm_model (Cur CHF)

    sample :: Contract -> IO [Double]
    sample = sampleIO . takeOut 100 . test

takeOut :: MonadDistribution m => Int -> ListT m a -> m [a]
takeOut n = run . take n
  where
    run (ListT m) = do
      s <- m
      case s of
        Nil -> return []
        Cons x l -> do
          u <- run l
          return (x : u)

index :: MonadDistribution m => Process m Double
index = select [0,0.01..]

-- Graphs

sampleGBM :: IO ()
sampleGBM = do
  x <- sampleIO (takeOut 100 $ index `zip` geombm 0.1 0.02 20.0)
  toFile def "sample_gbm.svg" $ do
    layout_title .= "Sample GBM"
    plot (line "path" [x])
