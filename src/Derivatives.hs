module Derivatives where

import Prelude hiding (and, or)
import Contracts

{-# ANN module "HLint: ignore Use camelCase" #-}

cur :: Currency -> Contract
cur k = one $ Cur k

chf :: Contract
chf = cur CHF

eur :: Contract
eur = cur EUR

data OptionKind = Call | Put

-- | European Options
european :: OptionKind -- ^ Kind of Option
         -> Time       -- ^ Maturity
         -> Double     -- ^ Strike Price
         -> Currency   -- ^ Currency of the Strike Price
         -> Contract   -- ^ Underlying Asset
         -> Contract   -- ^ European Option Contract
european Call t s c u = when (at t) $ (u `and` give (times s (cur c))) `or` zero
european Put  t s c u = when (at t) $ (give u `and`  times s (cur c) ) `or` zero

-- | American Options
american :: OptionKind   -- ^ Kind of Option
         -> (Time, Time) -- ^ Lifetime
         -> Double       -- ^ Strike Price
         -> Currency     -- ^ Currency of the Strike Price
         -> Contract     -- ^ Underlying Asset
         -> Contract     -- ^ American Option Contract
american Call (t1, t2) s c u = anytime (between t1 t2) $ u `and` give (times s (cur c))
american Put  (t1, t2) s c u = anytime (between t1 t2) $ give u `and`  times s (cur c)

-- | Down-and-In Options
down_and_in :: Double     -- ^ Barrier
            -> Contract   -- ^ Underlying
            -> Contract   -- ^ Contract
            -> Contract   -- ^ Conditional Contract
down_and_in b u = when (value u %< konst b)

-- | Down-and-Out Options
down_and_out :: Double    -- ^ Barrier
            -> Contract   -- ^ Underlying
            -> Contract   -- ^ Contract
            -> Contract   -- ^ Conditional Contract
down_and_out b u = when (value u %> konst b)

-- | Zero Coupon Bond
zcb :: Time     -- ^ Maturity
    -> Double   -- ^ Nominal
    -> Contract -- ^ Zero Coupon Bond Contract
zcb t n = when (at t) $ times n chf

-- | Low Exercise Price Option
lepo :: Time -> Currency -> Contract -> Contract
lepo t = european Call t s
  where
    s = 0.01

-- SVSP map
-- 12: Yield Enhancement Products

-- | Discount Certificate (1200)
dc :: Time     -- ^ Maturity
   -> Double   -- ^ Strike Price
   -> Currency -- ^ Currency
   -> Contract -- ^ Underlying
   -> Contract -- ^ Discount Certificate Contract
dc t s c u = l `and` short o
  where
    l = lepo t c u
    o = european Call t s c u

-- | Discount Certificate (1210)
bdc :: Time    -- ^ Maturity
   -> Double   -- ^ Strike Price
   -> Double   -- ^ Barrier
   -> Currency -- ^ Currency
   -> Contract -- ^ Underlying
   -> Contract -- ^ Discount Certificate Contract
bdc t b s c u = dc t s c u `and` o
  where
    o = down_and_out b u $ european Put t s c u

-- | Reverse Convertible (1220)
rc :: Time     -- ^ Maturity
   -> Double   -- ^ Nominal
   -> Double   -- ^ Coupon
   -> Double   -- ^ Ratio
   -> Double   -- ^ Strike Price
   -> Currency -- ^ Currency
   -> Contract -- ^ Underlying
   -> Contract -- ^ Reverse Convertible Contract
rc t n p r s c u = z `and` times r (short o)
  where
    z = zcb t $ (1+p)*n
    o = european Put t s c u

-- | Barrier Reverse Convertible (1230)
brc :: Time       -- ^ Maturity
    -> Double     -- ^ Nominal
    -> Double     -- ^ Strike Price
    -> Double     -- ^ Barrier
    -> Currency   -- ^ Currency
    -> Contract   -- ^ Underlying
    -> Contract   -- ^ Barrier Reverse Convertible Contract
brc t n s b c u = z `and` short o
  where
    z = zcb t n
    o = down_and_in b u $ european Put t s c u

with_costs :: Double -> Contract -> Contract
with_costs v c = amount v (Cur CHF) `and` c
