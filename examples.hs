import Contracts
import Valuation

import Prelude hiding (and, or)
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler

import System.Random

import Data.Number.LogFloat (logFloat)

data OptionKind = Call | Put

-- zero coupon bond
zcb :: Date -> Double -> Currency -> Contract
zcb t x k = when (at t) (scale (konst x) (one k))

-- european options
european :: OptionKind -> Date -> Contract -> Contract
european Call t u = when (at t) (u `or` zero)
european Put t u = when (at t) (give u `or` zero)

-- american options
american :: OptionKind -> (Date, Date) -> Contract -> Contract
american Call (t1, t2) u = anytime (between t1 t2) u
american Put (t1, t2) u = anytime (between t1 t2) (give u)

-- conditionals
down_and_in :: Obs Double -> Obs Double -> Contract -> Contract
down_and_in b u c = when (u %< b) c

-- 1200: discount certificate
fxdc :: Date -> Double -> Currency -> Contract -> Contract
fxdc t r k u = lepo `and` give (european Call t u)
  where
    lepo = european Call t u

-- 1220: reverse convertible
rc :: Date -> Double -> Currency -> Contract -> Contract
rc t r k u = z `and` give o
  where
    z = zcb t r k
    o = european Put t u

-- 1230: barrier reverse convertible
brc :: Date -> Double -> Obs Double -> Currency -> Contract -> Contract
brc t r b k u = z `and` give (down_and_in b v o)
  where
    z = zcb t r k
    o = european Put t u
    v = value u

---------------
-- Valuation --
---------------

example_model :: (MonadDist m) => Date -> Model m
example_model d = Model {
    modelStart = d
  , exch = example_exch
  , disc = example_disc
  , snell = example_snell
  , absorb = example_absorb
}

example_exch :: MonadDist m => Currency -> Currency -> m (Trace Double)
example_exch k1 k2 | k1 == k2 = return $ \t -> 1
example_exch k1 k2 | k2 < k1 = example_exch k2 k1
example_exch k1 k2 = do d <- sequence $ repeat (bernoulli $ logFloat 0.5)
                        let l = map (\x -> if x then -1.0 else 1.0) d
                        return $ \t -> sum (take t l)

intrest_rate :: MonadDist m => m (Trace Double)
intrest_rate = do d <- sequence $ repeat (bernoulli $ logFloat 0.5)
                  let l = map (\x -> if x then -1.0 else 1.0) d
                  return $ \t -> sum (take t l)

example_disc :: MonadDist m => Currency -> (m (Trace Bool), m (Trace Double)) -> m (Trace Double)
example_disc k (b, d) = do p1 <- d
                           p2 <- intrest_rate
                           pb <- b
                           return $ \t -> if (pb t) then (p1 t) else ((p1 t)/(1 + (p2 t)/100))

example_snell :: MonadDist m => Currency -> (m (Trace Bool), m (Trace Double)) -> m (Trace Double)
example_snell k (b, d) = undefined

example_absorb :: MonadDist m => Currency -> (m (Trace Bool), m (Trace Double)) -> m (Trace Double)
example_absorb k (b, d) = undefined

test :: Contract -> IO Double
test c = do g <- newStdGen
            let tr = sample test g
            return $ tr 10
  where test = evalC (example_model 0) CHF c

test1 = test zero
test2 = test (one CHF)
test3 = test $ scale (konst 5) (one CHF)
test4 = test $ zcb 0 100 CHF
test5 = test $ zcb 100 100 CHF
test6 = test $ european Call 0 (one CHF)
test7 = test $ european Call 100 (one CHF)
test8 = test $ american Call (0,0) (one CHF)
test9 = test $ american Call (0,100) (one CHF)

test10 = test $ brc 0 10 (konst 10.0) CHF (one CHF)
test11 = test $ brc 100 10 (konst 10.0) CHF (one CHF)

