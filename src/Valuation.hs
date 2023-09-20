{-# LANGUAGE TypeFamilies #-}

module Valuation where

import Contracts

-- | Value Process
-- data RV a = RV
-- newtype PR a = PR (Time -> RV a)

-- | TODO: Figure 6.6 Process primitives
class Process m where
  ifP :: m Bool -> m Double -> m Double -> m Double
  zipP :: (a -> b -> c) -> m a -> m b -> m c
  obs :: (i -> i -> Bool) -> i -> m Bool

-- | Model
class Model m where
  exch   :: Asset -> Asset -> m Double
  disc   :: Asset -> (m Bool, m Double) -> m Double
  snell  :: Asset -> (m Bool, m Double) -> m Double
  absorb :: Asset -> (m Bool, m Double) -> m Double

-- | Eval Contracts
evalC :: (Applicative m, Process m, Model m) => Asset -> Contract -> m Double
evalC k = eval
  where
    eval Zero           = pure 0
    eval (One k1)       = exch k k1
    eval (Give c)       = fmap negate (evalC k c)
    eval (o `Scale` c)  = zipP (*) (evalO k o) (evalC k c)
    eval (c1 `And` c2)  = zipP (+) (evalC k c1) (evalC k c2)
    eval (c1 `Or` c2)   = zipP max (evalC k c1) (evalC k c2)
    eval (Cond o c1 c2) = ifP (evalO k o) (evalC k c1) (evalC k c2)
    eval (When o c)     = disc k (evalO k o, evalC k c)
    eval (Anytime o c)  = snell k (evalO k o, evalC k c)
    eval (Until o c)    = absorb k (evalO k o, evalC k c)

-- | Eval Observations
evalO :: (Applicative m, Process m, Model m) => Asset -> Obs a -> m a
evalO k = eval
  where
    eval (Konst a)     = pure a
    eval (Lift f a)    = f <$> evalO k a
    eval (Lift2 f a b) = f <$> evalO k a <*> evalO k b
    eval (At t)        = obs (==) t
    eval (Before t)    = obs (<) t
    eval (After t)     = obs (>) t
    eval (Value c)     = evalC k c
