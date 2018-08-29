import Contracts
import Assets
import Derivatives
import Valuation

import Data.ByteString.Lazy as L hiding (repeat, iterate, map, length, foldl)

import Prelude hiding (and, or)
import Control.Monad (replicateM)

import Control.Monad.Bayes.Class

import System.Random
import Data.Aeson

--import Data.Number.LogFloat (logFloat)

---------------
-- Valuation --
---------------
std_normal :: MonadInfer m => Process m Double
std_normal = normal 0 1 >>= return . const

wiener :: MonadInfer m => Int -> Process m Double
wiener 0 = std_normal
wiener n = wiener (n-1) + std_normal

brownian :: MonadInfer m => Process m Double
brownian = iterateM (\dt -> normal 0 1 >>= \n -> return (0 + n * sqrt dt)) 0

iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f = g
    where g x = f x >>= g

{-
--testxx :: IO Double
testxx = do g <- newStdGen
            let step = fmap coin $ bernoulli 0.5
            let tr = sample (replicateM 10 step) g
            return $ Prelude.scanl (+) 0 tr

testyy = do g <- newStdGen
            let tr = sample (brownian) g
            return $ tr 1

coin :: Bool -> Int
coin True = 1
coin False = -1

example_stock :: MonadInfer m => Stock -> Process m Double
example_stock _ = undefined

example_model :: MonadInfer m => Time -> Model m
example_model d = Model {
    modelStart = d
  , stock = example_stock
  --, exch = undefined
  , disc = undefined
  , snell = undefined
  , absorb = undefined
}

example_stock :: MonadInfer m => Stock -> Process m Double
example_stock _ = do d <- sequence $ repeat (bernoulli $ logFloat 0.5)
                     let l = map (\x -> if x then -1.0 else 1.0) d
                     return $ \t -> sum (take t l)

example_exch :: MonadInfer m => Currency -> Currency -> Process m Double
example_exch k1 k2 | k1 == k2 = return $ const 1
example_exch k1 k2 | k2 < k1 = example_exch k2 k1
example_exch _ _ = do d <- sequence $ repeat (bernoulli $ logFloat 0.5)
                      let l = map (\x -> if x then -1.0 else 1.0) d
                      return $ \t -> sum (take t l)

intrest_rate :: MonadInfer m => Process m Double
intrest_rate = do d <- sequence $ repeat (bernoulli $ logFloat 0.5)
                  let l = map (\x -> if x then -1.0 else 1.0) d
                  return $ \t -> sum (take t l)

example_disc :: MonadInfer m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_disc _ (b, d) = do p1 <- d
                           p2 <- intrest_rate
                           pb <- b
                           return $ \t -> if (pb t) then (p1 t) else ((p1 t)/(1 + (p2 t)/100))

example_snell :: MonadInfer m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_snell _  = undefined

example_absorb :: MonadInfer m => Currency -> (Process m Bool, Process m Double) -> Process m Double
example_absorb _ = undefined

test :: Time -> Contract -> IO Double
test t c = do g <- newStdGen
              let tr = sample (replicateM 10000 sim) g
              return $ avg $ map ($t) tr
  where sim = evalC (example_model t0) CHF c

avg :: [Double] -> Double
avg xs = (sum xs) / fromIntegral (length xs)

t0, t1 :: Time
t0 = 0.0 -- mkdate 2017 1 1
t1 = 100.0 -- mkdate 2017 7 1

--test1, test2, test3, test4, test5, test6, test7, test8, test9 :: IO Double
test1 :: IO Double
test1 = test t1 zero
--test2 = test t1 $ chf 1
--test3 = test t1 $ chf 5
--test4 = test t1 $ zcb t0 100 (Currency CHF)
--test5 = test t1 $ zcb t1 100 (Currency CHF)
--test6 = test t1 $ european Call t0 1 (chf 1)
--test7 = test t1 $ european Call t1 5 (chf 5)
--test8 = test t1 $ american Call (t0,t0) 1 (chf 1)
--test9 = test t1 $ american Call (t0,t1) 1 (chf 1)

--test10 = test t1 (one $ Stock A)
-- test10 = test $ brc 0 10 (konst 10.0) CHF (chf 1)
-- test11 = tet $ brc 100 10 (konst 10.0) CHF (chf 1)

-- brc example (book svsp), p.229
test12 :: Contract
test12 = brc exerciseDate nominalAmount strikePrice barrier underlying
  where
    exerciseDate = 100.00 -- mkdate 2018 1 1
    nominalAmount = 1000
    strikePrice = 100
    barrier = 80
    underlying = scale (konst ratio) one
    ratio = 10

run :: IO ()
run = L.putStr $ encode test12
-}
