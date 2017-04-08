import Contracts
import Assets
import Derivatives
import Valuation

import Prelude hiding (and, or)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler

import System.Random
import Data.Aeson

import Data.Number.LogFloat (logFloat)

---------------
-- Valuation --
---------------
example_model :: MonadDist m => Time -> Model m
example_model d = Model {
    modelStart = d
  , exch = example_exch
  , disc = example_disc
  , snell = example_snell
  , absorb = example_absorb
}

example_exch :: MonadDist m => Currency -> Currency -> Process m Double
example_exch k1 k2 | k1 == k2 = return $ \t -> 1
example_exch k1 k2 | k2 < k1 = example_exch k2 k1
example_exch k1 k2 = do d <- sequence $ repeat (bernoulli $ logFloat 0.5)
                        let l = map (\x -> if x then -1.0 else 1.0) d
                        return $ \t -> sum (take t l)

intrest_rate :: MonadDist m => Process m Double
intrest_rate = do d <- sequence $ repeat (bernoulli $ logFloat 0.5)
                  let l = map (\x -> if x then -1.0 else 1.0) d
                  return $ \t -> sum (take t l)

example_disc :: MonadDist m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_disc k (b, d) = do p1 <- d
                           p2 <- intrest_rate
                           pb <- b
                           return $ \t -> if (pb t) then (p1 t) else ((p1 t)/(1 + (p2 t)/100))

example_snell :: MonadDist m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_snell k (b, d) = undefined

example_absorb :: MonadDist m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_absorb k (b, d) = undefined

test :: Contract -> IO Double
test c = do g <- newStdGen
            let tr = sample (replicateM 10000 test) g
            return $ avg $ map ($10) tr
  where test = evalC (example_model t0) CHF c

avg :: [Double] -> Double
avg xs = (sum xs) / fromIntegral (length xs)

t0, t1 :: Time
t0 = mkdate 2017 1 1
t1 = mkdate 2017 7 1

test1, test2, test3, test4, test5, test6, test7, test8, test9 :: IO Double
test1 = test zero
test2 = test $ chf 1
test3 = test $ chf 5
test4 = test $ zcb t0 100 (Currency CHF)
test5 = test $ zcb t1 100 (Currency CHF)
test6 = test $ european Call t0 1 (chf 1)
test7 = test $ european Call t1 5 (chf 5)
test8 = test $ american Call (t0,t0) 1 (chf 1)
test9 = test $ american Call (t0,t1) 1 (chf 1)

-- test10 = test $ brc 0 10 (konst 10.0) CHF (chf 1)
-- test11 = tet $ brc 100 10 (konst 10.0) CHF (chf 1)

-- brc example (book svsp), p.229
test12 :: Contract
test12 = brc exerciseDate nominalAmount strikePrice barrier currency underlying
  where
    exerciseDate = t1
    nominalAmount = 1000
    strikePrice = 100
    barrier = 80
    currency = Currency CHF
    underlying = one $ Stock A

run = encode test12
