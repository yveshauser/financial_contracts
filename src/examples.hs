{-# LANGUAGE RankNTypes #-}
import Contracts hiding (lift)
import Derivatives
import Valuation

import Prelude hiding (and, or)
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class

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

random_walk :: MonadInfer m => [Double] -> m [Double]
random_walk ys = do
  s <- gamma 1 1
  let expand xs [] = return xs
      expand (x:xs) (y:ys) = do
        x' <- normal x s
        score (normalPdf x' 1 y)
        expand (x':x:xs) ys
  xs <- expand [0] ys
  return (reverse xs)

example_model :: MonadInfer m => Model m
example_model = Model {
    disc = example_disc
  , snell = example_snell
  , absorb = example_absorb
}

intrest_rate :: MonadInfer m => Process m Double
intrest_rate = do
  d <- sequence $ repeat (bernoulli 0.5)
  let l = map (\x -> if x then -1.0 else 1.0) d
  return $ \t -> sum (take (floor t) l)

example_disc :: MonadInfer m => (Process m Bool, Process m Double) -> Process m Double
example_disc (b, d) = do
  p1 <- d
  p2 <- intrest_rate
  pb <- b
  return $ \t -> if (pb t) then (p1 t) else ((p1 t)/(1 + (p2 t)/100))

example_snell :: MonadInfer m => (Process m Bool, Process m Double) -> Process m Double
example_snell _  = undefined

example_absorb :: MonadInfer m => (Process m Bool, Process m Double) -> Process m Double
example_absorb _ = undefined

test :: MonadInfer m => Contract -> Process m Double
test = evalC example_model

test1, test2, test3, test4, test5, test6, test7 :: MonadInfer m => Process m Double
test1 = test zero
test2 = test one
test3 = test $ times 5 one
test4 = test $ zcb 0.0 100
test5 = test $ european Call 0.0 1 one
test6 = test $ american Call (0.0, 0.0) 1 one
test7 = test $ brc 10.0 1000 10.0 8.0 one
