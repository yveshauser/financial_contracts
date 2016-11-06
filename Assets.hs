{-# LANGUAGE DeriveGeneric #-}

module Assets where

import Currencies
import Stocks

import GHC.Generics -- TODO: currently used for JSON instances - do these manually instead

data Asset = Currency Currency | Stock Stock deriving (Show, Generic)
