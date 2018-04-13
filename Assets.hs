{-# LANGUAGE DeriveGeneric #-}

module Assets where

import Data.Aeson
import GHC.Generics

data Currency = CHF | EUR | USD deriving (Ord, Eq, Show, Generic)
data Stock    = A | B | C deriving (Ord, Eq, Show, Generic)

data Asset = Currency Currency | Stock Stock deriving (Show, Generic)

-- currency constructor
-- cur :: Currency -> Double -> Contract
-- cur k o = scale (konst o) (one $ Currency k)

-- chf :: Double -> Contract
-- chf = cur CHF

instance ToJSON Currency
instance ToJSON Stock
