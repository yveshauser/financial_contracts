module SpecialContracts where

import Contracts
import Valuation

import Prelude hiding (and, or)

-- currency constructor
cur :: Currency -> Double -> Contract
cur k o = scale (konst o) (one $ Currency k)

chf :: Double -> Contract
chf = cur CHF

data OptionKind = Call | Put

-- zero coupon bond
zcb :: Date -> Double -> Asset -> Contract
zcb t x k = when (at t) (scale (konst x) (one k))

-- european options -- FIXME: currency
european :: OptionKind -> Date -> Double -> Contract -> Contract
european Call t s u = when (at t) ((u `and` give (chf s)) `or` zero)
european Put t s u = when (at t) ((give u `and` chf s) `or` zero)

-- american options -- FIXME: currency
american :: OptionKind -> (Date, Date) -> Double -> Contract -> Contract
american Call (t1, t2) s u = anytime (between t1 t2) (u `and` give (chf s))
american Put (t1, t2) s u = anytime (between t1 t2) (give u `and` (chf s))

-- conditionals
down_and_in :: Obs Double -> Obs Double -> Contract -> Contract
down_and_in b u c = when (u %< b) c

-- 1200: discount certificate
fxdc :: Date -> Double -> Double -> Currency -> Contract -> Contract
fxdc t r s k u = lepo `and` give (european Call t s u)
  where
    lepo = european Call t s u

-- 1220: reverse convertible
rc :: Date -> Double -> Double -> Asset -> Contract -> Contract
rc t r s k u = z `and` give o
  where
    z = zcb t r k
    o = european Put t s u

-- 1230: barrier reverse convertible
brc :: Date -> Double -> Double -> Obs Double -> Asset -> Contract -> Contract
brc t r s b k u = z `and` give (down_and_in b v o)
  where
    z = zcb t r k
    o = european Put t s u
    v = value u
