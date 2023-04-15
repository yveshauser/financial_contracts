{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, LiberalTypeSynonyms, IncoherentInstances, OverloadedStrings #-}

module Contracts where

import Prelude hiding (and, or)
import Data.Aeson
import Data.Time

data Currency = CHF | EUR | USD deriving (Ord, Eq, Show)
data Stock = X | Y | Z deriving (Ord, Eq, Show)
data Asset = Cur Currency | Stk Stock deriving (Ord, Eq, Show)

type Time = Integer

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
    deriving (Show)

-- Primitives over observables, as defined in Figure 6.3
data Obs a where
    Konst  :: (Show a) => a -> Obs a
    Lift   :: (a -> b) -> Obs a -> Obs b
    Lift2  :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
    At     :: Time -> Obs Bool
    After  :: Time -> Obs Bool
    Before :: Time -> Obs Bool
    Value  :: Contract -> Obs Double

konst :: (Show a) => a -> Obs a
konst = Konst

lift :: (a -> b) -> Obs a -> Obs b
lift = Lift

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 = Lift2

at :: Time -> Obs Bool
at = At

after :: Time -> Obs Bool
after = After

before :: Time -> Obs Bool
before = Before

value :: Contract -> Obs Double
value = Value

between :: Time -> Time -> Obs Bool
between t1 t2 = after t1 %&& before t2

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

times :: Double -> Contract -> Contract
times = scale . konst

amount :: Double -> Asset -> Contract
amount n k = times n (one k)

short :: Contract -> Contract
short = give

long :: Contract -> Contract
long = id

(%&&) :: Obs Bool -> Obs Bool -> Obs Bool
(%<), (%<=), (%==), (%>), (%>=) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%<=) = lift2 (<=)
(%==) = lift2 (==)
(%>)  = lift2 (>)
(%>=) = lift2 (>=)
(%&&) = lift2 (&&)

instance Show (Obs a)
  where
    show (Konst a)       = show a
    show (Lift _ o)      = "lift: "   ++ show o
    show (Lift2 _ o1 o2) = "lift2: "  ++ show o1 ++ ", " ++ show o2
    show (Value c)       = "value: "  ++ show c
    show (At t)          = "at: "     ++ show t
    show (Before t)      = "before: " ++ show t
    show (After t)       = "after: "  ++ show t

instance (Num a, Show a) => Num (Obs a)
  where
    fromInteger = konst . fromInteger
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    signum = lift signum

instance ToJSON (Obs o) where
  toJSON (Konst a)       = toJSON $ show a
  toJSON (Lift _ o)      = object [ "lift"   .= toJSON o ]
  toJSON (Lift2 _ o1 o2) = object [ "lift2"  .= object [ "o1" .= toJSON o1, "o2" .= toJSON o2 ]]
  toJSON (Value c)       = object [ "value"  .= toJSON c ]
  toJSON (At t)          = object [ "at"     .= toJSON t ]
  toJSON (Before t)      = object [ "before" .= toJSON t ]
  toJSON (After t)       = object [ "when"   .= toJSON t ]

instance ToJSON Contract where
  toJSON Zero           = object []
  toJSON (One k)        = object [ "value"   .= (1 :: Integer) ]
  toJSON (Give c)       = object [ "give"    .= toJSON c ]
  toJSON (o `Scale` c)  = object [ "factor"  .= o, "scale" .= toJSON c ]
  toJSON (c1 `And` c2)  = object [ "and"     .= object [ "c1" .= toJSON c1, "c2" .= toJSON c2 ]]
  toJSON (c1 `Or` c2)   = object [ "or"      .= object [ "c1" .= toJSON c1, "c2" .= toJSON c2 ]]
  toJSON (Cond o c1 c2) = object [ "cond"    .= o, "do"    .= object [ "c1" .= toJSON c1, "c2" .= toJSON c2 ]]
  toJSON (When o c)     = object [ "when"    .= o, "do"    .= toJSON c ]
  toJSON (Anytime o c)  = object [ "anytime" .= o, "do"    .= toJSON c ]
  toJSON (Until o c)    = object [ "until"   .= o, "do"    .= toJSON c ]
