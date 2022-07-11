{-# LANGUAGE GADTs, RankNTypes, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms, IncoherentInstances #-}
module Valuation where

import Prelude hiding (and, or)
import Control.Applicative (liftA2, liftA3)

import Contracts hiding (cond, lift, lift2)

-- trace
type Trace a = Time -> a

instance Num a => Num (Trace a) where
  fromInteger = const . fromInteger
  (+) a b t   = a t + b t
  (-) a b t   = a t - b t
  (*) a b t   = a t * b t
  abs a t     = abs (a t)
  signum a t  = signum (a t)
  negate a t  = negate (a t)

-- value process
type Process m a = m (Trace a)

-- Process primitives, as defined in Figure 6.6

bigK :: Applicative m => a -> Process m a
bigK = pure . const

date :: Trace Time
date = id

cond :: Trace Bool -> Trace Double -> Trace Double -> Trace Double
cond bs xs ys t = if bs t then xs t else ys t

lift :: (a -> b) -> Trace a -> Trace b
lift f a t = f (a t)

lift2 :: (a -> b -> c) -> Trace a -> Trace b -> Trace c
lift2 f a b t = f (a t) (b t)

instance (Num a, Applicative m) => Num (Process m a) where
  fromInteger = bigK . fromInteger
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum

-- model
data Model m where
  Model :: Applicative m => {
    modelStart :: Time
      , exch   :: Asset -> Asset -> Process m Double
      , disc   :: Asset -> (Process m Bool, Process m Double) -> Process m Double
      , snell  :: Asset -> (Process m Bool, Process m Double) -> Process m Double
      , absorb :: Asset -> (Process m Bool, Process m Double) -> Process m Double
} -> Model m

-- evaluation of contracts
evalC :: Applicative m => Model m -> Asset -> Contract -> Process m Double
evalC m@(Model _ exch disc snell absorb) k = eval
  where
    eval Zero           = bigK 0
    eval (One k1)       = exch k k1
    eval (Give c)       = -(eval c)
    eval (o `Scale` c)  = evalO m k o * eval c
    eval (c1 `And` c2)  = eval c1 + eval c2
    eval (c1 `Or` c2)   = liftA2 max' (eval c1) (eval c2)
    eval (Cond o c1 c2) = liftA3 cond (evalO m k o) (eval c1) (eval c2)
    eval (When o c)     = disc k (evalO m k o, eval c)
    eval (Anytime o c)  = snell k (evalO m k o, eval c)
    eval (Until o c)    = absorb k (evalO m k o, eval c)

evalO :: Applicative m => Model m -> Asset -> Obs a -> Process m a
evalO m k (Konst a)     = bigK a
evalO m k (Lift f a)    = (.) f <$> evalO m k a
evalO m k (Lift2 f a b) = liftA2 f <$> evalO m k a <*> evalO m k b
evalO m k (At t)        = pure (t==)
evalO m k (Before t)    = pure (t<)
evalO m k (After t)     = pure (t>)
evalO m k (Value c)     = evalC m k c

max' :: Ord a => Trace a -> Trace a -> Trace a
max' = lift2 max
