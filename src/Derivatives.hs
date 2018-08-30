module Derivatives where

import Prelude hiding (and, or)
import Contracts

cur :: Currency -> Double -> Contract
cur k o = scale (konst o) (one k)

chf :: Double -> Contract
chf = cur CHF

one_chf :: Contract
one_chf = chf 1.0

data OptionKind = Call | Put

-- | European Options
european :: OptionKind -- ^ Kind of Option
         -> Time       -- ^ Maturity
         -> Double     -- ^ Strike Price
         -> Contract   -- ^ Underlying Asset
         -> Contract   -- ^ European Option Contract
european Call t s u = when (at t) $ (u `and` give (times s one_chf)) `or` zero
european Put  t s u = when (at t) $ (give u `and` (times s one_chf)) `or` zero

-- | American Options
american :: OptionKind   -- ^ Kind of Option
         -> (Time, Time) -- ^ Lifetime
         -> Double       -- ^ Strike Price
         -> Contract     -- ^ Underlying Asset
         -> Contract     -- ^ American Option Contract
american Call (t1, t2) s u = anytime (between t1 t2) $ u `and` give (times s one_chf)
american Put  (t1, t2) s u = anytime (between t1 t2) $ give u `and` (times s one_chf)

-- | Down-and-In Options
down_and_in :: Double     -- ^ Barrier
            -> Contract   -- ^ Underlying
            -> Contract   -- ^ Contract
            -> Contract   -- ^ Conditional Contract
down_and_in b u = when ((value u) %< (konst b))

-- | Down-and-Out Options
down_and_out :: Double    -- ^ Barrier
            -> Contract   -- ^ Underlying
            -> Contract   -- ^ Contract
            -> Contract   -- ^ Conditional Contract
down_and_out b u = when ((value u) %> (konst b))

-- | Zero Coupon Bond
zcb :: Time     -- ^ Maturity
    -> Double   -- ^ Nominal
    -> Contract -- ^ Zero Coupon Bond Contract
zcb t n = when (at t) $ times n one_chf

-- | Low Exercise Price Option
lepo t u = european Call t s u
  where
    s = 0.01

-- SVSP map
-- 12: Yield Enhancement Products

-- | Discount Certificate (1200)
dc :: Time     -- ^ Maturity
   -> Double   -- ^ Strike Price
   -> Contract -- ^ Underlying
   -> Contract -- ^ Discount Certificate Contract
dc t s u = l `and` short o
  where
    l = lepo t u
    o = european Call t s u

-- | Discount Certificate (1210)
bdc :: Time    -- ^ Maturity
   -> Double   -- ^ Strike Price
   -> Double   -- ^ Barrier
   -> Contract -- ^ Underlying
   -> Contract -- ^ Discount Certificate Contract
bdc t b s u = dc t s u `and` o
  where
    o = down_and_out b u $ european Put t s u

-- | Reverse Convertible (1220)
rc :: Time     -- ^ Maturity
   -> Double   -- ^ Nominal
   -> Double   -- ^ Coupon
   -> Double   -- ^ Ratio
   -> Double   -- ^ Strike Price
   -> Contract -- ^ Underlying
   -> Contract -- ^ Reverse Convertible Contract
rc t n c r s u = z `and` times r (short o)
  where
    z = zcb t $ (1+c)*n
    o = european Put t s u

-- | Barrier Reverse Convertible (1230)
brc :: Time       -- ^ Maturity
    -> Double     -- ^ Nominal
    -> Double     -- ^ Strike Price
    -> Double     -- ^ Barrier
    -> Contract   -- ^ Underlying
    -> Contract   -- ^ Barrier Reverse Convertible Contract
brc t n s b u = z `and` short o
  where
    z = zcb t n
    o = down_and_in b u $ european Put t s u

with_costs :: Double -> Contract -> Contract
with_costs v c = amount v CHF `and` c
