{-# LANGUAGE GADTs, RankNTypes, TypeSynonymInstances, FlexibleInstances, LiberalTypeSynonyms, IncoherentInstances, DeriveGeneric #-}

module Contracts where

import Prelude hiding (and, or)

import Assets

type Date = Int

-- Primitives over observables, as defined in Figure 6.3
data Obs a where
    Konst :: a -> Obs a
    Lift :: (a -> b) -> Obs a -> Obs b
    Lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
    Date :: Obs Date
    Value :: Contract -> Obs Double

konst :: a -> Obs a
konst a = Konst a

lift :: (a -> b) -> Obs a -> Obs b
lift = Lift

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 = Lift2

date :: Obs Date
date = Date

at :: Date -> Obs Bool
-- at t = lift2 (==) date (konst t)
at t = lift2 (==) 0 (konst t)

value :: Contract -> Obs Double
value = Value

-- Primitives for defining contracts, see Figure 6.2
data Contract
    = Zero
    | One Asset
    | Give Contract
    | And Contract Contract
    | Or Contract Contract
    | Cond (Obs Bool) Contract Contract
    | Scale (Obs Double) Contract
    | When (Obs Bool) Contract
    | Anytime (Obs Bool) Contract
    | Until (Obs Bool) Contract

zero :: Contract
zero = Zero

one :: Asset -> Contract
one = One

give :: Contract -> Contract
give = Give

and :: Contract -> Contract -> Contract
and = And

or :: Contract -> Contract -> Contract
or = Or

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

scale :: Obs Double -> Contract -> Contract
scale = Scale

when :: Obs Bool -> Contract -> Contract
when = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

until :: Obs Bool -> Contract -> Contract
until = Until

instance Num a => Num (Obs a)
  where
    fromInteger i = konst (fromInteger i)
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    signum = lift signum

(%<), (%<=), (%==), (%>), (%>=) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%<=) = lift2 (<=)
(%==) = lift2 (==)
(%>)  = lift2 (>)
(%>=) = lift2 (>=)
(%&&) = lift2 (&&)

between :: Date -> Date -> Obs Bool
between t1 t2 = (date %>= konst t1) %&& (date %<= konst t2)
