{-# LANGUAGE GADTs, RankNTypes, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms, IncoherentInstances #-}
module Valuation where

import Prelude hiding (and, or)
import Control.Applicative (liftA2, liftA3)

import Contracts

type Trace a = Double -> a

-- value process
type Process m a = m (Trace a)

-- model
data Model m  where
  Model :: Applicative m => {
        disc   :: (Process m Bool, Process m Double) -> Process m Double
      , snell  :: (Process m Bool, Process m Double) -> Process m Double
      , absorb :: (Process m Bool, Process m Double) -> Process m Double
} -> Model m

-- evaluation of contracts
evalC :: Applicative m => Model m -> Contract -> Process m Double
evalC m@(Model disc snell absorb) = eval
  where eval Zero                = bigK 0
        eval One                 = bigK 1
        eval (Give c)            = -(eval c)
        eval (o `Scale` c)       = (evalO m o) * (eval c)
        eval (c1 `And` c2)       = (eval c1) + (eval c2)
        eval (c1 `Or` c2)        = liftA2 max' (eval c1) (eval c2)
        eval (Cond o c1 c2)      = liftA3 cond' (evalO m o) (eval c1) (eval c2)
        eval (When o c)          = disc (evalO m o, eval c)
        eval (Anytime o c)       = snell (evalO m o, eval c)
        eval (Until o c)         = absorb (evalO m o, eval c)

evalO :: Applicative m => Model m -> Obs a -> Process m a
evalO m (Konst a)     = bigK a
evalO m (Lift f a)    = (.) <$> pure f <*> evalO m a
evalO m (Lift2 f a b) = liftA2 f <$> evalO m a <*> evalO m b
evalO m (At t)        = pure (t==)
evalO m (Before t)    = pure (t<)
evalO m (After t)     = pure (t>)
evalO m (Value c)     = evalC m c

instance Num a => Num (Trace a) where
  fromInteger i t = fromInteger i
  (+) a b t       = a t + b t
  (-) a b t       = a t - b t
  (*) a b t       = a t * b t
  abs a t         = abs (a t)
  signum a t      = signum (a t)
  negate a t      = negate (a t)

instance (Num a, Applicative m) => Num (Process m a) where
  fromInteger i = bigK (fromInteger i)
  (+)           = liftA2 (+)
  (-)           = liftA2 (-)
  (*)           = liftA2 (*)
  abs           = fmap abs
  signum        = fmap signum

bigK :: Applicative m => a -> Process m a
bigK t = const <$> pure t

max' :: Ord a => Trace a -> Trace a -> Trace a
max' a b t = max (a t) (b t)

cond' :: Trace Bool -> Trace Double -> Trace Double -> Trace Double
cond' bs xs ys t = if (bs t) then (xs t) else (ys t)
