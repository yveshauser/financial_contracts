{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Examples where

import Contracts
import Derivatives
import Valuation

import Prelude hiding (and, or, take, zip)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Data.Functor ((<&>))
import Data.Number.Erf
import List.Transformer as T
import GHC.Float

import Graphics.Rendering.Chart.Easy hiding (scale, index)
import Graphics.Rendering.Chart.Backend.Diagrams

{-# ANN module "HLint: ignore Use camelCase" #-}

-------------------
-- Distributions --
-------------------

wiener :: MonadSample m => Process m Double
wiener =
  ListT
    ( do
        r <- normal 0 1
        return (Cons r wiener)
    )

geometric_brownian_motion :: MonadSample m => Double -> Double -> Double -> Process m Double
geometric_brownian_motion μ σ s_0 =
  let f (t, w) = ((μ - (σ * σ) / 2) * t) + (σ * w)
   in bigK s_0 * fmap (exp . f) (zip index wiener)

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
example_exch k1 k2 | k1 == k2 = bigK 1
example_exch (Cur CHF) (Cur EUR) = bigK 1
example_exch (Cur EUR) (Cur CHF) = bigK 1
example_exch k1 k2 | k2 < k1 = example_exch k2 k1
example_exch (Cur CHF) (Stk X) = bigK 12
example_exch (Cur CHF) (Stk Y) = geometric_brownian_motion 0.1 0.1 10.0
example_exch (Cur CHF) (Stk Z) = geometric_brownian_motion 0.1 0.1 100.0
example_exch _ _ = empty

intrest_rate :: MonadSample m => Process m Double
intrest_rate = geometric_brownian_motion 0.1 0.1 0.0

example_disc :: MonadSample m => Asset -> (Process m Bool, Process m Double) -> Process m Double
example_disc _ (b, d) = do
  p1 <- d
  p2 <- intrest_rate
  pb <- b
  return $ if pb then p1 else p1/(1 + p2/100)

example_snell :: MonadSample m => Asset -> (Process m Bool, Process m Double) -> Process m Double
example_snell _ _ = empty -- TODO

example_absorb :: MonadSample m => Asset -> (Process m Bool, Process m Double) -> Process m Double
example_absorb _ _ = empty -- TODO

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

    test :: MonadSample m => Contract -> Process m Double
    test = evalC example_model (Cur CHF)

    sample :: Contract -> IO [Double]
    sample = sampleIO . takeOut 100 . test

takeOut :: Monad m => Int -> ListT m a -> m [a]
takeOut n = run . take n
  where
    run (ListT m) = do
      s <- m
      case s of
        Nil -> return []
        Cons x l -> do
          u <- run l
          return (x : u)

index :: MonadSample m => Process m Double
index = select [0,0.01..]

-- Graphs

sampleGBM :: IO ()
sampleGBM = do
  x <- sampleIO (takeOut 100 $ index `zip` geometric_brownian_motion 0.1 0.02 20.0)
  toFile def "sample_gbm.svg" $ do
    layout_title .= "Sample GBM"
    plot (line "path" [x])
