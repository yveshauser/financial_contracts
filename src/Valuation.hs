{-# LANGUAGE GADTs, RankNTypes, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms, IncoherentInstances #-}
module Valuation where

import Prelude hiding (and, or, zip)
import List.Transformer

import Contracts

-- value process
type Process m a = ListT m a

-- Process primitives, as defined in Figure 6.6

bigK :: Monad m => a -> Process m a
bigK = select . repeat

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
evalC :: Monad m => Model m -> Asset -> Contract -> Process m Double
evalC m@(Model _ exch disc snell absorb) k = eval
  where
    eval Zero           = bigK 0
    eval (One k1)       = exch k k1
    eval (Give c)       = -(eval c)
    eval (o `Scale` c)  = evalO m k o * eval c
    eval (c1 `And` c2)  = eval c1 + eval c2
    eval (c1 `Or` c2)   = zipP max (eval c1) (eval c2)
    eval (Cond o c1 c2) = ifP (evalO m k o) (eval c1) (eval c2)
    eval (When o c)     = disc k (evalO m k o, eval c)
    eval (Anytime o c)  = snell k (evalO m k o, eval c)
    eval (Until o c)    = absorb k (evalO m k o, eval c)

evalO :: Monad m => Model m -> Asset -> Obs a -> Process m a
evalO m k (Konst a)     = bigK a
evalO m k (Lift f a)    = f <$> evalO m k a
evalO m k (Lift2 f a b) = f <$> evalO m k a <*> evalO m k b
evalO m k (At t)        = obs (==) t
evalO m k (Before t)    = obs (<) t
evalO m k (After t)     = obs (>) t
evalO m k (Value c)     = evalC m k c

ifP :: Monad m => Process m Bool -> Process m Double -> Process m Double -> Process m Double
ifP pb p1 p2 = g <$> zip pb (zip p1 p2)
  where
    g (True, p) = fst p
    g (False, p) = snd p

zipP :: Monad m => (a -> b -> c) -> Process m a -> Process m b -> Process m c
zipP g p1 p2 = uncurry g <$> zip p1 p2

obs :: Monad m => (Time -> Time -> Bool) -> Time -> Process m Bool
obs cmp t = go 0
  where
    go i = ListT (return (Cons (cmp i t) (go (i + 1))))
