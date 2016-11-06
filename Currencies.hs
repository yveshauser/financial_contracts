{-# LANGUAGE DeriveGeneric #-}

module Currencies where

import GHC.Generics -- TODO: currently used for JSON instances - do these manually instead

data Currency = CHF | EUR | USD deriving (Ord, Eq, Show, Generic)

