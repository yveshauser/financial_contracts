{-# LANGUAGE DeriveGeneric #-}

module Stocks where

import GHC.Generics -- TODO: currently used for JSON instances - do these manually instead

data Stock = A | B | C deriving (Ord, Eq, Show, Generic)
