{-# LANGUAGE GADTs, RankNTypes, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms, IncoherentInstances #-}

module Valuation where

import Prelude hiding (and, or)
import Control.Applicative (liftA2, liftA3)
import Control.Monad.Bayes.Class

import Contracts
import Assets

--type Time = Int
-- type Trace a = Time -> a
type Trace a = Double -> a

-- value process
type Process m a = m (Trace a)

-- model
data Model m  where
  Model :: MonadInfer m => {
    modelStart :: Time
      , stock  :: Stock -> Process m Double
      --, exch   :: Currency -> Currency -> Process m Double
      , disc   :: Currency -> (Process m Bool, Process m Double) -> Process m Double
      , snell  :: Currency -> (Process m Bool, Process m Double) -> Process m Double
      , absorb :: Currency -> (Process m Bool, Process m Double) -> Process m Double
} -> Model m

-- evaluation of contracts
evalC :: MonadInfer m => Model m -> Currency -> Contract -> Process m Double
evalC m@(Model modelDate stock disc snell absorb) k = eval
  where eval Zero                = bigK 0
        eval One                 = bigK 1
        eval (Give c)            = -(eval c)
        eval (o `Scale` c)       = (evalO m k o) * (eval c)
        eval (c1 `And` c2)       = (eval c1) + (eval c2)
        eval (c1 `Or` c2)        = liftA2 max' (eval c1) (eval c2)
        eval (Cond o c1 c2)      = liftA3 cond' (evalO m k o) (eval c1) (eval c2)
        eval (When o c)          = disc k (evalO m k o, eval c)
        eval (Anytime o c)       = snell k (evalO m k o, eval c)
        eval (Until o c)         = absorb k (evalO m k o, eval c)

evalO :: MonadInfer m => Model m -> Currency -> Obs a -> Process m a
evalO m k (Konst a)     = bigK a
evalO m k (Lift f a)    = (.) <$> return f <*> evalO m k a
evalO m k (Lift2 f a b) = liftA2 f <$> evalO m k a <*> evalO m k b
evalO m k (At t)        = return (t==)
evalO m k (Before t)    = return (t<)
evalO m k (After t)     = return (t>)
evalO m k (Value c)     = evalC m k c

instance Num a => Num (Trace a) where
  fromInteger i t = fromInteger i
  (+) a b t       = a t + b t
  (-) a b t       = a t - b t
  (*) a b t       = a t * b t
  abs a t         = abs (a t)
  signum a t      = signum (a t)
  negate a t      = negate (a t)

instance (Num a, MonadInfer m) => Num (Process m a) where
  fromInteger i = bigK (fromInteger i)
  (+)           = liftA2 (+)
  (-)           = liftA2 (-)
  (*)           = liftA2 (*)
  abs           = fmap abs
  signum        = fmap signum

bigK :: MonadInfer m => a -> Process m a
bigK t = undefined -- FIXME: fmap const $ categorical t

max' :: Ord a => Trace a -> Trace a -> Trace a
max' a b t = max (a t) (b t)

cond' :: Trace Bool -> Trace Double -> Trace Double -> Trace Double
cond' bs xs ys t = if (bs t) then (xs t) else (ys t)