{-# LANGUAGE DeriveGeneric #-}

module Assets where

import Data.Aeson
import GHC.Generics

data Currency = CHF | EUR | USD deriving (Ord, Eq, Show, Generic)
data Stock    = A | B | C deriving (Ord, Eq, Show, Generic)

instance ToJSON Currency
instance ToJSON Stock
