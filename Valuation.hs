{-# LANGUAGE GADTs, RankNTypes, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms, IncoherentInstances #-}

module Valuation where

import Prelude hiding (and, or)
import Control.Monad (liftM2, liftM3)
import Control.Applicative (liftA2)
import Control.Monad.Bayes.Dist
import Control.Monad.Bayes.Class

import Contracts

-- value process
-- type PR m a = MonadDist m => m (Trace a)
type Time = Int
type Trace a = Time -> a

-- model
data Model m  where
  Model :: MonadDist m => {
    modelStart :: Date
      , exch :: Currency -> Currency -> m (Trace Double)
      , disc :: Currency -> (m (Trace Bool), m (Trace Double)) -> m (Trace Double)
      , snell :: Currency -> (m (Trace Bool), m (Trace Double)) -> m (Trace Double)
      , absorb :: Currency -> (m (Trace Bool), m (Trace Double)) -> m (Trace Double)
} -> Model m

-- evaluation of contracts
evalC :: MonadDist m => Model m -> Currency -> Contract -> m (Trace Double)
evalC m@(Model modelDate exch disc snell absorb) k = eval
  where eval Zero           = bigK 0
        eval (One k2)       = exch k k2
        eval (Give c)       = -(eval c)
        eval (o `Scale` c)  = (evalO m k o) * (eval c)
        eval (c1 `And` c2)  = (eval c1) + (eval c2)
        eval (c1 `Or` c2)   = liftM2 max' (eval c1) (eval c2)
        eval (Cond o c1 c2) = liftM3 cond' (evalO m k o) (eval c1) (eval c2)
        eval (When o c)     = disc k (evalO m k o, eval c)
        eval (Anytime o c)  = snell k (evalO m k o, eval c)
        eval (Until o c)    = absorb k (evalO m k o, eval c)

bigK :: MonadDist m => a -> m (Trace a)
bigK t = fmap const $ categorical [(t, 1.0)]

evalO :: MonadDist m => Model m -> Currency -> Obs a -> m (Trace a)
evalO m k (Konst a) = bigK a
evalO m k (Lift f a) = (.) <$> return f <*> evalO m k a
evalO m k (Lift2 f a b) = liftA2 f <$> evalO m k a <*> evalO m k b
evalO m k (Date) = return id
evalO m k (Value c) = evalC m k c

instance Num a => Num (Trace a) where
  fromInteger i t = fromInteger i
  (+) a b t = a t + b t
  (-) a b t = a t - b t
  (*) a b t = a t * b t
  abs a t = abs (a t)
  signum a t = signum (a t)
  negate a t = negate (a t)

max' :: Ord a => Trace a -> Trace a -> Trace a
max' a b t = max (a t) (b t)

cond' :: Trace Bool -> Trace Double -> Trace Double -> Trace Double
cond' bs xs ys t = if (bs t) then (xs t) else (ys t)

instance (Num a, MonadDist m) => Num (m (Trace a)) where
  fromInteger i = bigK (fromInteger i)
  (+) = liftM2 (+)
  (-) = liftM2 (-)
  (*) = liftM2 (*)
  abs = fmap abs
  signum = fmap signum
