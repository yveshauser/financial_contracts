{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, LiberalTypeSynonyms, IncoherentInstances, OverloadedStrings #-}

module Contracts where

import Prelude hiding (and, or)
import Data.Aeson
import Data.List (intercalate)
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

-- | A readable, indented tree rendering of a contract, e.g.
--
-- > and
-- > ├─ scale 100.0
-- > │  └─ one (Stk X)
-- > └─ give
-- >    └─ one (Cur USD)
instance Show Contract where
  show = intercalate "\n" . renderContract

renderContract :: Contract -> [String]
renderContract c = case c of
    Zero        -> [ "zero" ]
    One a       -> [ "one (" ++ show a ++ ")" ]
    Give x      -> treeNode "give" [renderContract x]
    And a b     -> treeNode "and"  [renderContract a, renderContract b]
    Or a b      -> treeNode "or"   [renderContract a, renderContract b]
    Cond o a b  -> obsNode "cond"    o [a, b]
    Scale o x   -> obsNode "scale"   o [x]
    When o x    -> obsNode "when"    o [x]
    Anytime o x -> obsNode "anytime" o [x]
    Until o x   -> obsNode "until"   o [x]

-- | A contract node carrying an observable. The observable is inlined into the
-- label when it renders to a single line (the common case, e.g. @scale (100.0)@)
-- and otherwise hung as the first child of the node.
obsNode :: String -> Obs a -> [Contract] -> [String]
obsNode label o kids = case renderObs o of
    [single] -> treeNode (label ++ " (" ++ single ++ ")") kidBlocks
    block    -> treeNode label (block : kidBlocks)
  where
    kidBlocks = map renderContract kids

renderObs :: Obs a -> [String]
renderObs o = case o of
    Konst a     -> [ show a ]
    At t        -> [ "at "     ++ show t ]
    After t     -> [ "after "  ++ show t ]
    Before t    -> [ "before " ++ show t ]
    Lift s _ a    -> treeNode ("lift "  ++ s) [renderObs a]
    Lift2 s _ a b -> treeNode ("lift2 " ++ s) [renderObs a, renderObs b]
    Value c       -> treeNode "value" [renderContract c]

-- | A tree node: a label followed by its children's rendered blocks, indented.
treeNode :: String -> [[String]] -> [String]
treeNode label children = label : indentChildren children

-- | Prefix each child's lines with the box-drawing connectors that join it to
-- its parent: the last child gets a corner, the rest get a tee.
indentChildren :: [[String]] -> [String]
indentChildren kids = concat (zipWith draw isLast kids)
  where
    isLast        = replicate (length kids - 1) False ++ [True]
    draw lst ls   = zipWith (++) (connectors lst) ls
    connectors lst
      | lst       = "└─ " : repeat "   "
      | otherwise = "├─ " : repeat "│  "

-- Primitives over observables, as defined in Figure 6.3
data Obs a where
    Konst  :: (Show a) => a -> Obs a
    -- the leading 'String' labels the (unshowable) function for rendering
    Lift   :: String -> (a -> b) -> Obs a -> Obs b
    Lift2  :: String -> (a -> b -> c) -> Obs a -> Obs b -> Obs c
    At     :: Time -> Obs Bool
    After  :: Time -> Obs Bool
    Before :: Time -> Obs Bool
    Value  :: Contract -> Obs Double

konst :: (Show a) => a -> Obs a
konst = Konst

-- | @lift sym f@ / @lift2 sym f@ tag the lifted function with a symbol for
-- display (the function itself cannot be shown).
lift :: String -> (a -> b) -> Obs a -> Obs b
lift = Lift

lift2 :: String -> (a -> b -> c) -> Obs a -> Obs b -> Obs c
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
(%<)  = lift2 "(<)"  (<)
(%<=) = lift2 "(<=)" (<=)
(%==) = lift2 "(==)" (==)
(%>)  = lift2 "(>)"  (>)
(%>=) = lift2 "(>=)" (>=)
(%&&) = lift2 "(&&)" (&&)

instance Show (Obs a) where
  show = intercalate "\n" . renderObs

instance (Num a, Show a) => Num (Obs a)
  where
    fromInteger = konst . fromInteger
    (+) = lift2 "(+)" (+)
    (-) = lift2 "(-)" (-)
    (*) = lift2 "(*)" (*)
    abs = lift "abs" abs
    signum = lift "signum" signum

instance ToJSON (Obs o) where
  toJSON (Konst a)       = toJSON $ show a
  toJSON (Lift s _ o)      = object [ "lift"  .= s, "arg" .= toJSON o ]
  toJSON (Lift2 s _ o1 o2) = object [ "lift2" .= s, "args" .= object [ "o1" .= toJSON o1, "o2" .= toJSON o2 ]]
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
