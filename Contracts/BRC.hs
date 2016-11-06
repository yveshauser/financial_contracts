module Contracts.BRC where

data BRC = BRC {
    valor_ :: String
  , underlyings_ :: [String]
  , barrier_ :: Double
}
